suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
set.seed(291017)
con_duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
base <- data.frame(
  f = c("a", "a", "b", "b", NA, NA),
  g = c("x", "y", "x", "y", "x", "y"),
  h = c(1L, 2L, 2L, 1L, 1L, 2L), x = c(1, 3, -1, 7, 0, 9)
)
inputs <- list(
  local = base, dtplyr = dtplyr::lazy_dt(base),
  duckdb = dplyr::copy_to(con_duck, base, "facts"),
  sqlite = dplyr::copy_to(con_sqlite, base, "facts"),
  arrow = arrow::arrow_table(base)
)
transforms <- list(
  identity = function(x) x,
  joined = function(x) {
    dplyr::inner_join(x, dplyr::select(x, g, h), by = c("g", "h"))
  },
  anti = function(x) {
    dplyr::anti_join(x, dplyr::filter(x, h == 2L), by = c("f", "g", "h"))
  },
  union = function(x) {
    dplyr::union_all(dplyr::filter(x, h == 1L), dplyr::filter(x, h == 2L))
  },
  distinct = function(x) dplyr::distinct(x),
  derived = function(x) dplyr::mutate(dplyr::filter(x, x >= 0), x = x * 2 + 1),
  order = function(x) dplyr::arrange(x, dplyr::desc(h), g),
  renamed = function(x) dplyr::rename(dplyr::rename(x, x2 = x), x = x2)
)
plans <- list(
  rollup(g, h), cube(g, h),
  grouping_sets(grouping_set(h), grouping_set(), grouping_set(g, h))
)
normalize <- function(x) {
  x <- as.data.frame(x)
  for (name in names(x)) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  }
  ordering <- do.call(
    order, c(x[c("f", "g", "h", "sid")], list(na.last = TRUE))
  )
  x <- x[ordering, , drop = FALSE]
  rownames(x) <- NULL
  x
}
checks <- 0L
errors <- list()
mismatches <- list()
for (transformation in names(transforms)) for (backend in names(inputs)) {
  source <- tryCatch(
    transforms[[transformation]](inputs[[backend]]), error = identity
  )
  if (inherits(source, "error")) {
    cat("TRANSFORM", transformation, backend, conditionMessage(source), "\n")
    next
  }
  local <- tryCatch(dplyr::collect(source), error = identity)
  if (inherits(local, "error")) {
    cat("UPSTREAM", transformation, backend, conditionMessage(local), "\n")
    next
  }
  for (plan_index in seq_along(plans)) {
    for (sorting in c("none", "last", "first")) {
      for (duplicates in c("error", "keep")) {
        invoke <- function(input) {
          summarize_with_margins(
            input, total = sum(x), count = dplyr::n(), lower = min(x),
            upper = max(x), mean = mean(x), bit = grouping_id(g, h), .by = f,
            .grouping = plans[[plan_index]], .margin_label = NULL, .id = "sid",
            .sort = sorting, .duplicates = duplicates
          )
        }
        expected <- normalize(invoke(local))
        actual <- tryCatch(
          normalize(dplyr::collect(invoke(source))), error = identity
        )
        key <- paste(
          transformation, backend, plan_index, sorting, duplicates, sep = "/"
        )
        checks <- checks + 1L
        if (inherits(actual, "error")) {
          errors[[key]] <- conditionMessage(actual)
        } else {
          comparison <- all.equal(actual, expected, check.attributes = FALSE)
          if (!isTRUE(comparison)) mismatches[[key]] <- comparison
        }
      }
    }
  }
}
cat("CHECKS", checks, "ERRORS", length(errors), "MISMATCHES",
    length(mismatches), "\n")
print(errors)
print(mismatches)
DBI::dbDisconnect(con_duck, shutdown = TRUE)
DBI::dbDisconnect(con_sqlite)
