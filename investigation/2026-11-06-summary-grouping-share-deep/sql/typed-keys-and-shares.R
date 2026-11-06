Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)

main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con))
  key_sql <- list(
    int64 = "(9007199254740992 + k)::BIGINT",
    decimal = "(k / 1000000000000000000.0)::DECIMAL(38,18)",
    time = "TIME '01:00:00' + k * INTERVAL '1 microsecond'",
    date = "DATE '2020-01-01' + k::INTEGER",
    timestamp = "TIMESTAMP '2020-01-01' + k * INTERVAL '1 microsecond'",
    interval = "k * INTERVAL '1 month'",
    blob = "('abc' || k::VARCHAR)::BLOB",
    list = "[k, NULL]",
    struct = "{'field': k, 'other': 'a'}",
    map = "MAP(['key'],[k])"
  )
  sources <- list(
    ordinary = "v::DOUBLE", decimal = "v::DECIMAL(38,18)",
    big = "(9007199254740992 + v)::BIGINT",
    small = "v * 0.000000000000000001::DECIMAL(38,18)"
  )
  n <- 0L
  failures <- list()
  for (key in names(key_sql)) for (measure in names(sources)) {
    sql <- paste0("SELECT ", key_sql[[key]], " AS g, CASE WHEN k = 3 THEN NULL ELSE k % 2 END AS p, ",
                  "CASE WHEN k % 2 = 0 THEN 'x' ELSE 'y' END AS child, ", sources[[measure]], " AS v ",
                  "FROM (VALUES (0,2), (1,4), (2,6), (3,8), (0,-1)) AS facts(k,v)")
    x <- tbl(con, dbplyr::sql(sql))
    for (fixed in c(FALSE, TRUE)) for (agg in c("auto", "portable")) {
      for (sorting in c("none", "first", "last")) {
        n <- n + 1L
        out <- tryCatch(collect(summarize_with_margins(
          x, value = sum(v), small = min(v), total = share_of_total(value),
          parent = share_of_parent(value), min_share = share_of_total(small),
          .grouping = if (fixed) rollup(g, child) else rollup(p, g, child),
          .by = all_of(if (fixed) "p" else character()), .id = "set",
          .margin_label = NULL, .aggregate = agg, .sort = sorting)), error = identity)
        if (inherits(out, "error")) {
          failures[[length(failures)+1L]] <- data.frame(key,measure,fixed,agg,sorting,error=conditionMessage(out))
          next
        }
        root <- if (fixed) 3L else 4L
        den <- out[out$set == root, ]
        part <- if (fixed) match(out$p, den$p) else rep(1L, nrow(out))
        expected <- as.double(out$value) / as.double(den$value[part])
        expected[out$set == root] <- 1
        min_expected <- as.double(out$small) / as.double(den$small[part])
        min_expected[out$set == root] <- 1
        parent_expected <- rep(NA_real_, nrow(out))
        dims <- if (fixed) c("g", "child") else c("p", "g", "child")
        for (child_set in seq_len(root - 1L)) {
          rows <- which(out$set == child_set)
          parents <- which(out$set == child_set + 1L)
          kept <- head(dims, length(dims) - child_set)
          keys <- c(if (fixed) "p", kept)
          index <- if (length(keys)) vctrs::vec_match(out[rows, keys], out[parents, keys]) else rep(1L,length(rows))
          parent_expected[rows] <- as.double(out$value[rows]) / as.double(out$value[parents[index]])
        }
        parent_expected[out$set == root] <- 1
        if (!isTRUE(all.equal(out$total, expected, tolerance=1e-12)) ||
            !isTRUE(all.equal(out$min_share, min_expected, tolerance=1e-12)) ||
            !isTRUE(all.equal(out$parent, parent_expected, tolerance=1e-12))) {
          failures[[length(failures)+1L]] <- data.frame(key,measure,fixed,agg,sorting,error="ratio mismatch")
          print(list(case=c(key,measure,fixed,agg,sorting),out=out,expected=expected,min_expected=min_expected,parent_expected=parent_expected))
        }
      }
    }
  }
  cat("Completed",n,"DuckDB typed-key/multi-source comparisons\n")
  if (length(failures)) print(bind_rows(failures)) else cat("All matched visible denominators\n")
  stopifnot(length(failures) == 0L)
}
main()
