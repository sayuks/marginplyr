Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_SOURCE", unset = getwd()), quiet = TRUE)
library(dplyr)
set.seed(20261109)
outdir <- tempfile("marginplyr-grouping-oracle-"); dir.create(outdir)
results <- list()
record <- function(kind, case, detail = "") {
  results[[length(results) + 1L]] <<- data.frame(kind, case, detail)
}
unit <- function() sample(letters[1:4], sample.int(3L, 1L))
node <- function(depth = 0L) {
  kind <- if (depth > 1L) "set" else sample(c("set", "sets", "rollup", "cube", "product"), 1L)
  ctor <- c(set = "grouping_set", sets = "grouping_sets", rollup = "rollup", cube = "cube", product = "grouping_spec")[[kind]]
  if (kind == "set") {
    dims <- if (runif(1) < .25) character() else unit()
    return(list(expr = as.call(c(list(as.name(ctor)), lapply(dims, as.name))), dims = dims, sets = list(dims)))
  }
  n <- sample.int(3L, 1L)
  if (kind %in% c("rollup", "cube")) {
    units <- lapply(seq_len(n), function(i) unit())
    exprs <- lapply(units, function(cols) {
      if (length(cols) == 1L) as.name(cols) else as.call(c(list(as.name("grouping_set")), lapply(cols, as.name)))
    })
    dims <- unique(unlist(units, use.names = FALSE))
    if (kind == "rollup") {
      sets <- lapply(n:0, function(k) if (k) unique(unlist(units[seq_len(k)], use.names = FALSE)) else character())
    } else {
      choices <- unlist(lapply(n:0, function(k) combn(seq_len(n), k, simplify = FALSE)), recursive = FALSE)
      sets <- lapply(choices, function(ids) if (length(ids)) unique(unlist(units[ids], use.names = FALSE)) else character())
    }
  } else {
    children <- lapply(seq_len(n), function(i) node(depth + 1L))
    exprs <- lapply(children, `[[`, "expr")
    dims <- unique(unlist(lapply(children, `[[`, "dims"), use.names = FALSE))
    if (kind == "sets") {
      sets <- unlist(lapply(children, `[[`, "sets"), recursive = FALSE)
    } else {
      sets <- list(character())
      for (child in children) {
        sets <- unlist(lapply(sets, function(prefix) lapply(child$sets, function(suffix) union(prefix, suffix))), recursive = FALSE)
      }
    }
  }
  list(expr = as.call(c(list(as.name(ctor)), exprs)), dims = dims, sets = sets)
}
key <- function(x) paste(sort(x), collapse = ":")
data <- tibble(k = c("u", "u", NA, NA, "v", "v"),
               a = c("x", "y", "x", NA, "x", "x"),
               b = c("m", "m", "n", "n", NA, "m"),
               c = c("p", "q", "p", "q", "p", "p"),
               d = c("t", "s", "t", "s", "t", "t"),
               x = c(1, 3, -2, 2, NA, 0))
for (case in seq_len(100L)) {
  item <- node()
  spec <- eval(item$expr)
  duplicates <- duplicated(vapply(item$sets, key, character(1)))
  for (policy in c("error", "drop", "keep")) {
    observed <- tryCatch(inspect_grouping(data, .grouping = spec, .duplicates = policy, .format = "list"), error = identity)
    expected_error <- policy == "error" && any(duplicates)
    if (expected_error) {
      stopifnot(inherits(observed, "marginplyr_error"))
    } else {
      if (inherits(observed, "error")) stop("Unexpected plan refusal: ", deparse1(item$expr), " ", conditionMessage(observed))
      sets <- if (policy == "drop") item$sets[!duplicates] else item$sets
      sets <- lapply(sets, function(s) item$dims[item$dims %in% s])
      if (!identical(observed$included, sets)) {
        saveRDS(list(item = item, observed = observed), file.path(outdir, "plan-failure.rds"))
        stop("Set order mismatch: ", deparse1(item$expr))
      }
      stopifnot(identical(observed$set_id, seq_along(sets)))
      expected_id <- vapply(sets, function(s) as.integer(sum(as.integer(!item$dims %in% s) * 2^rev(seq_along(item$dims) - 1L))), integer(1))
      stopifnot(identical(observed$grouping_id, expected_id))
    }
    record("plan", paste(case, policy))
  }
  # Each explicit grain is evaluated with ordinary dplyr; no plan internals
  # or other Margin verb supply expected values.
  for (fixed in c(FALSE, TRUE)) {
    policy <- "drop"
    sets <- item$sets[!duplicates]
    has_total <- any(lengths(sets) == 0L)
    by <- if (fixed) "k" else character()
    actual <- tryCatch(summarize_with_margins(
      data, x = sum(x, na.rm = TRUE), count = n(),
      px = share_of_total(x), pn = share_of_total(count),
      .by = all_of(by), .grouping = spec, .duplicates = policy,
      .margin_label = NULL, .id = "occurrence"
    ), error = identity)
    if (!has_total) {
      stopifnot(inherits(actual, "marginplyr_error"))
      record("no-root-refusal", paste(case, fixed))
      next
    }
    denominator <- data |>
      group_by(pick(all_of(by))) |>
      summarize(dx = sum(x, na.rm = TRUE), dn = n(), .groups = "drop")
    parts <- lapply(seq_along(sets), function(i) {
      grouping <- c(by, sets[[i]])
      result <- data |>
        group_by(pick(all_of(grouping))) |>
        summarize(x = sum(x, na.rm = TRUE), count = n(), .groups = "drop")
      for (col in setdiff(item$dims, sets[[i]])) result[[col]] <- rep(NA_character_, nrow(result))
      if (fixed) {
        result <- left_join(result, denominator, by = by, na_matches = "na")
      } else {
        result$dx <- rep(denominator$dx, nrow(result))
        result$dn <- rep(denominator$dn, nrow(result))
      }
      root <- length(sets[[i]]) == 0L
      result$px <- if (root) rep(1, nrow(result)) else ifelse(is.na(result$x) | is.na(result$dx) | result$dx == 0, NA_real_, result$x / result$dx)
      result$pn <- if (root) rep(1, nrow(result)) else ifelse(is.na(result$count) | is.na(result$dn) | result$dn == 0, NA_real_, result$count / result$dn)
      result$occurrence <- rep(as.integer(i), nrow(result))
      select(result, all_of(c(by, item$dims, "occurrence", "x", "count", "px", "pn")))
    })
    expected <- bind_rows(parts)
    if (inherits(actual, "error")) stop("Unexpected share refusal: ", deparse1(item$expr), " ", conditionMessage(actual))
    order_cols <- c(by, item$dims, "occurrence")
    actual <- arrange(actual, !!!rlang::syms(order_cols))
    expected <- arrange(expected, !!!rlang::syms(order_cols))
    if (!isTRUE(all.equal(actual, expected, check.attributes = TRUE))) {
      saveRDS(list(item = item, actual = actual, expected = expected), file.path(outdir, "share-failure.rds"))
      stop("Share mismatch: ", deparse1(item$expr), " ", paste(all.equal(actual, expected), collapse = "; "))
    }
    record("share", paste(case, fixed))
  }
}
write.csv(bind_rows(results), file.path(outdir, "grouping-oracle-results.csv"), row.names = FALSE)
cat("PASS", length(results), "independent plan/share comparisons\n")
print(sessionInfo())
