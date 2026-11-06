Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con))
  keys <- c(date = "DATE '2020-01-01'", timestamp = "TIMESTAMP '2020-01-01 02:03:04'",
            boolean = "TRUE", decimal = "1.234567890123456789::DECIMAL(38,18)",
            blob = "'abc'::BLOB", list = "[1,NULL,2]", struct = "{'field': 1, 'other': 'a'}",
            map = "MAP(['a'],[1])", integer64 = "9007199254740993::BIGINT")
  count <- 0L
  for (key in names(keys)) for (fixed in c(FALSE, TRUE)) {
    for (aggregate in c("auto", "portable")) for (sort in c("none", "first", "last")) {
      source <- tbl(con, dbplyr::sql(paste0("SELECT ", keys[[key]], " AS g, child, v FROM ",
                                           "(VALUES ('x', 2), ('y', 6), ('x', 10)) AS data(child,v)")))
      query <- summarize_with_margins(source, value = sum(v),
                                      parent = share_of_parent(value), total = share_of_total(value),
                                      .by = all_of(if (fixed) "g" else character()),
                                      .grouping = if (fixed) rollup(child) else rollup(g, child),
                                      .aggregate = aggregate, .sort = sort, .id = "set",
                                      .margin_label = NULL)
      result <- collect(query)
      root <- if (fixed) 2L else 3L
      denominator <- result$value[result$set == root]
      stopifnot(length(denominator) == 1L, denominator == 18)
      stopifnot(isTRUE(all.equal(result$total, as.double(result$value) / 18)),
                isTRUE(all.equal(result$parent, as.double(result$value) / 18)))
      count <- count + 1L
    }
  }
  cat(count, "DuckDB typed-key Parent/Total-share comparisons matched\n")
}
main()
