Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
set.seed(61106)
run <- function(con, backend) {
  on.exit(DBI::dbDisconnect(con))
  count <- 0L
  for (iteration in seq_len(30L)) {
    data <- tibble(p = sample(c("east", "west", NA), 13, TRUE),
                   a = sample(c("A", "B", NA), 13, TRUE),
                   b = sample(c("x", "y", NA), 13, TRUE),
                   v = sample(c(-5, -1, 0, 0.25, 2, 7, NA), 13, TRUE))
    if (iteration %% 10L == 0L) data <- data[0L, ]
    src <- copy_to(con, data, "facts", overwrite = TRUE, temporary = TRUE)
    specs <- list(rollup(a, b), cube(a, b),
                  grouping_sets(grouping_set(), grouping_set(b), grouping_set(a, b), grouping_set(a)),
                  grouping_spec(rollup(a), rollup(b)),
                  grouping_sets(grouping_set(a), grouping_set(), grouping_set(a), grouping_set()))
    for (index in seq_along(specs)) for (fixed in c(FALSE, TRUE)) {
      spec <- specs[[index]]
      f <- function(x) summarize_with_margins(
        x, value = coalesce(sum(v, na.rm = TRUE), 0), share = share_of_total(value),
        .by = all_of(if (fixed) "p" else character()), .grouping = spec,
        .duplicates = "keep", .id = "set", .margin_label = NULL,
        .check_share_source = FALSE)
      canonical <- function(x) {
        x <- as.data.frame(x)
        x <- x[do.call(order, x[c(intersect("p", names(x)), "set", intersect(c("a", "b"), names(x)))]), , drop = FALSE]
        rownames(x) <- NULL
        x[] <- lapply(x, function(v) if (is.numeric(v) || is.logical(v)) as.double(v) else v)
        x
      }
      expected <- canonical(f(data))
      actual <- tryCatch(canonical(collect(f(src))), error = identity)
      if (inherits(actual, "error") || !isTRUE(all.equal(actual, expected))) {
        cat("FAIL", backend, iteration, index, fixed, "\n")
        print(list(actual = actual, expected = expected))
        print(all.equal(actual, expected))
        str(actual)
        str(expected)
        stop("oracle mismatch")
      }
      count <- count + 1L
    }
  }
  cat(backend, count, "Total-share comparisons matched\n")
}
run(DBI::dbConnect(RSQLite::SQLite()), "SQLite")
run(DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)), "DuckDB")
