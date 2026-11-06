Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
DBI::dbWriteTable(con, "facts", data.frame(g = "a", v = 3L))
ordinary <- dplyr::tbl(con, dbplyr::sql("SELECT * FROM facts"))
opts <- dbplyr::sql_options(cte = TRUE)
actual <- expand_with_margins(dplyr::tbl(con, "facts"),
                             .grouping = rollup(g), .margin_label = NULL)
capture <- function(x) tryCatch(dplyr::collect(x, sql_options = opts), error = conditionMessage)
plain_error <- capture(ordinary)
margin_error <- capture(actual)
wrapped <- capture(dplyr::filter(actual, TRUE))
stopifnot(identical(plain_error, margin_error), is.character(plain_error),
          grepl("query_list", plain_error, fixed = TRUE), nrow(wrapped) == 2L)
print(list(ordinary_bare_sql = plain_error, expansion_bare_sql = margin_error,
           ordinary_filtered_control = capture(dplyr::filter(ordinary, TRUE)),
           filtered_expansion_control = wrapped))

# Nonfinite source values retain the same included-branch ordering as ordinary DuckDB.
src <- dplyr::copy_to(con, tibble::tibble(g = c(NaN, NA_real_, -Inf, Inf, 0, -0, 1),
                                       rid = seq_len(7L)), "nonfinite")
ordered <- dplyr::collect(dplyr::arrange(src, is.na(g), g))
for (sorting in c("first", "last")) {
  query <- expand_with_margins(src, .grouping = rollup(g),
                              .margin_label = NULL, .sort = sorting, .id = "sid")
  result <- dplyr::collect(query)
  included <- result[result$sid == 1L, c("g", "rid")]
  stopifnot(identical(included$g, ordered$g), identical(included$rid, ordered$rid),
            sum(result$sid == 2L) == 7L, all(is.na(result$g[result$sid == 2L])))
  print(list(sorting = sorting, result = result))
}
DBI::dbDisconnect(con)
