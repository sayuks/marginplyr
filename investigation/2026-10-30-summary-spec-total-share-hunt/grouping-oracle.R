suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
options(lifecycle_verbosity = "quiet", width = 110)
set.seed(20261030L)
constructors <- c(set = "grouping_set", sets = "grouping_sets", rollup = "rollup", cube = "cube", product = "grouping_spec")
name_pool <- c("a", ".data", ".env", "a,b", "a=b", "a`b", "a\nb", "日本語", "é", "é", "..0", "..-1", ".", ".BY")
canon <- function(xs) {
  dimensions <- unique(unlist(xs, use.names = FALSE))
  lapply(xs, function(x) dimensions[dimensions %in% x])
}
parens <- function(expr, n) {
  for (i in seq_len(n)) expr <- rlang::call2("(", expr)
  expr
}
selection <- function(cols) {
  form <- sample.int(4L, 1L)
  expr <- switch(form,
    rlang::call2("c", !!!lapply(cols, function(x) if (x %in% c(".data", ".env")) rlang::expr(tidyselect::all_of(!!x)) else rlang::sym(x))),
    rlang::expr(tidyselect::all_of(!!cols)),
    rlang::call2("c", !!!lapply(cols, function(x) rlang::expr(.data[[!!x]]))),
    rlang::new_quosure(rlang::expr(tidyselect::all_of(!!cols)), rlang::env(globalenv()))
  )
  expr <- parens(expr, sample(0:2, 1L))
  list(expr = expr, family = list(cols), units = as.list(cols))
}
generate <- function(nms, depth = 0L) {
  kind <- sample(if (depth >= 2L) c("set", "rollup", "cube") else names(constructors), 1L)
  count <- sample(if (kind %in% c("set", "product")) 0:2 else 1:2, 1L)
  args <- lapply(seq_len(count), function(i) {
    if (kind %in% c("sets", "product") && depth < 2L && runif(1L) < 0.65) {
      return(generate(nms, depth + 1L))
    }
    cols <- sample(nms, sample(1:2, 1L))
    if (kind %in% c("rollup", "cube") && runif(1L) < 0.5) {
      return(list(expr = parens(rlang::call2("grouping_set", selection(cols)$expr), sample(0:2, 1L)),
                  family = list(cols), units = list(cols)))
    }
    selection(cols)
  })
  expr <- parens(rlang::call2(constructors[[kind]], !!!lapply(args, `[[`, "expr")), sample(0:2, 1L))
  if (kind == "set") {
    family <- list(unique(unlist(lapply(args, function(x) x$family[[1L]]), use.names = FALSE)))
  } else if (kind == "sets") {
    family <- unlist(lapply(args, `[[`, "family"), recursive = FALSE)
  } else if (kind == "product") {
    family <- list(character())
    for (arg in args) family <- unlist(lapply(family, function(x) {
      lapply(arg$family, function(y) unique(c(x, y)))
    }), recursive = FALSE)
  } else {
    units <- unlist(lapply(args, `[[`, "units"), recursive = FALSE)
    if (kind == "rollup") {
      family <- lapply(rev(0:length(units)), function(n) unique(unlist(units[seq_len(n)], use.names = FALSE)))
    } else {
      family <- unlist(lapply(rev(seq_along(units)), function(k) {
        lapply(combn(seq_along(units), k, simplify = FALSE), function(ids) unique(unlist(units[ids], use.names = FALSE)))
      }), recursive = FALSE)
      family <- c(family, list(character()))
    }
  }
  family <- lapply(family, function(x) if (is.null(x)) character() else x)
  list(expr = expr, family = canon(family), units = NULL)
}
connections <- list(sqlite = DBI::dbConnect(RSQLite::SQLite()), duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)))
checks <- 0L
for (trial in seq_len(700L)) {
  nms <- sample(name_pool, 5L)
  source <- stats::setNames(data.frame(x = 1:2, y = c("a", "b"), z = 1:2, w = 3:4, v = c(TRUE, FALSE)), nms)
  node <- generate(nms)
  spec <- eval(node$expr, globalenv())
  dims <- unique(unlist(node$family, use.names = FALSE))
  expected <- canon(node$family)
  bits <- lapply(expected, function(x) if (length(dims)) stats::setNames(as.integer(!dims %in% x), dims) else integer())
  ids <- vapply(bits, function(x) as.integer(sum(unname(x) * 2^rev(seq_along(x) - 1L))), integer(1))
  backends <- list(local = source, dtplyr = dtplyr::lazy_dt(source), arrow = arrow::arrow_table(source), simulated = dbplyr::tbl_lazy(source, con = dbplyr::simulate_postgres()))
  if (trial <= 100L) {
    backends$sqlite <- dplyr::copy_to(connections$sqlite, source, "oracle_input", overwrite = TRUE, temporary = TRUE)
    backends$duckdb <- dplyr::copy_to(connections$duckdb, source, "oracle_input", overwrite = TRUE, temporary = TRUE)
  }
  for (backend in names(backends)) {
    actual <- tryCatch(inspect_grouping(backends[[backend]], .grouping = spec, .duplicates = "keep", .format = "list"), error = function(cnd) { print(trial); print(backend); print(node$expr); stop(cnd) })
    if (!identical(actual$grouping_bits, bits)) { print(trial); print(backend); print(node$expr); str(actual$grouping_bits); str(bits) }; stopifnot(identical(actual$included, expected), identical(actual$grouping_bits, bits),
              identical(actual$grouping_id, ids), identical(actual$set_id, seq_along(expected)))
    drop <- inspect_grouping(backends[[backend]], .grouping = spec, .duplicates = "drop", .format = "list")
    stopifnot(identical(drop$included, unique(expected)))
    checks <- checks + 2L
  }
  if (trial %% 100L == 0L) cat("PASS", trial, "plans;", checks, "backend/duplicate-policy comparisons\n")
}
DBI::dbDisconnect(connections$sqlite)
DBI::dbDisconnect(connections$duckdb, shutdown = TRUE)
cat("FINAL PASS", checks, "comparisons\n")
