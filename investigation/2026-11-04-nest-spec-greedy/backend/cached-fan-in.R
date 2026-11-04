Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile(fileext = ".csv")
observe <- function(expr) tryCatch(force(expr), error = identity)
normal <- function(x) {
  x <- as_tibble(ungroup(x), .name_repair = "minimal")
  x$cells <- lapply(x$cells, function(y) as_tibble(y, .name_repair = "minimal"))
  x
}
graphs <- list(
  cache = function(x) compute(x),
  double_cache = function(x) compute(compute(x)),
  cache_rename = function(x) rename(compute(x), h = g, g = h),
  rename_cache = function(x) compute(rename(x, h = g, g = h)),
  cache_order = function(x) arrange(compute(x), desc(row)),
  order_cache = function(x) compute(arrange(x, desc(row))),
  cache_filter = function(x) filter(compute(x), row > 1L),
  cache_select = function(x) select(compute(x), h, g, row),
  shared_union = function(x) { y <- compute(x); union_all(filter(y, row < 3L), filter(y, row > 1L)) },
  renamed_union = function(x) { y <- compute(x); union_all(rename(y, h = g, g = h), y) },
  double_cached_union = function(x) { y <- compute(x); z <- compute(arrange(y, desc(row))); union_all(y, z) },
  union_cache = function(x) compute(union_all(x, x)),
  joined_cache = function(x) compute(inner_join(x, rename(x, row2 = row), by = c("g", "h"))),
  cache_join = function(x) { y <- compute(x); inner_join(y, rename(y, row2 = row), by = c("g", "h")) },
  cache_cross = function(x) { y <- compute(x); cross_join(y, rename(y, g2 = g, h2 = h, row2 = row)) },
  cache_semi = function(x) { y <- compute(x); semi_join(y, filter(y, row > 1L), by = c("g", "h")) },
  cache_anti = function(x) { y <- compute(x); anti_join(y, filter(y, row > 1L), by = c("g", "h")) },
  cache_intersect = function(x) { y <- compute(x); intersect(y, filter(y, row > 1L)) },
  cache_setdiff = function(x) { y <- compute(x); setdiff(y, filter(y, row > 1L)) },
  cache_union = function(x) { y <- compute(x); union(y, y) },
  cache_distinct = function(x) distinct(compute(x), g, h, .keep_all = TRUE),
  distinct_cache = function(x) compute(distinct(x, g, h, .keep_all = TRUE)),
  cache_slice = function(x) slice(compute(x), c(3L, 1L, 2L)),
  slice_cache = function(x) compute(slice(x, c(3L, 1L, 2L))),
  grouped_cache = function(x) compute(group_by(x, h)),
  cache_grouped = function(x) group_by(compute(x), h),
  rename_order_cache = function(x) compute(arrange(rename(compute(x), h = g, g = h), g)),
  shared_joined_union = function(x) { y <- compute(x); z <- compute(inner_join(y, rename(y, row2 = row), by = c("g", "h"))); union_all(z, arrange(z, desc(row2))) }
)
results <- list()
for (size in c(0L, 1L, 4L)) for (special in c(FALSE, TRUE)) {
  input <- tibble(g = c("b", "a", NA, "a")[seq_len(size)],
                  h = c("u", "v", "u", "v")[seq_len(size)], row = seq_len(size))
  if (special) input[[".SD"]] <- seq_len(size)
  root <- dtplyr::lazy_dt(input)
  original <- serialize(root[["parent"]], NULL)
  for (graph in names(graphs)) {
    q <- observe(graphs[[graph]](root))
    native <- if (inherits(q, "error")) q else observe(collect(q))
    if (inherits(native, "error")) {
      results[[length(results) + 1L]] <- data.frame(size, special, graph,
        operation = "native", status = "native_error", detail = conditionMessage(native))
      next
    }
    for (operation in c("inspect", "nest_with_margins", "nest_by_with_margins")) {
      f <- if (operation == "inspect") function(x) inspect_grouping(
        x, .grouping = rollup(where(is.character)), .format = "list") else function(x) {
          group <- setdiff(names(native)[vapply(as.list(native), is.character, logical(1))], group_vars(x))
          ans <- get(operation)(x, .grouping = rollup(all_of(group)),
                                .keep = TRUE, .key = "cells", .id = "set", .sort = "last")
          if (inherits(ans, "dtplyr_step")) ans <- as_tibble(ans, .name_repair = "minimal")
          normal(ans)
        }
      expected <- observe(f(native))
      actual <- observe(f(q))
      status <- if (inherits(expected, "error")) "local_error" else
        if (inherits(actual, "marginplyr_error")) "refused" else
          if (inherits(actual, "error")) "error" else
            if (isTRUE(all.equal(expected, actual))) "match" else "mismatch"
      detail <- if (inherits(actual, "error")) conditionMessage(actual) else
        if (status == "mismatch") paste(all.equal(expected, actual), collapse = "; ") else ""
      results[[length(results) + 1L]] <- data.frame(size, special, graph, operation, status, detail)
      if (status %in% c("mismatch", "error")) print(tail(results, 1L))
    }
    stopifnot(identical(original, serialize(root[["parent"]], NULL)))
  }
}
results <- do.call(rbind, results)
write.csv(results, output, row.names = FALSE)
print(table(results$operation, results$status))
print(sessionInfo())
