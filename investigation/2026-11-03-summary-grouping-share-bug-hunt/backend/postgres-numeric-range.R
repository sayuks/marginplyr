pkgload::load_all(quiet = TRUE)
source("investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-fixture.R")
with_postgres(function(con) {
  cat(DBI::dbGetQuery(con, "SELECT version()")[[1L]], "\n")
  rows <- list()
  for (exponent in c(-400L, -310L, -300L, 0L, 300L, 310L, 400L)) {
    DBI::dbExecute(con, "DROP TABLE IF EXISTS facts")
    DBI::dbExecute(con, paste0("CREATE TEMP TABLE facts AS SELECT 'a'::text g, 1e", exponent,
      "::numeric x UNION ALL SELECT 'b', 3e", exponent, "::numeric"))
    source <- dplyr::tbl(con, "facts")
    for (kind in c("total", "parent")) {
      share <- if (kind == "total") rlang::expr(share_of_total(value)) else rlang::expr(share_of_parent(value))
      query <- summarize_with_margins(source, value = sum(x, na.rm = TRUE), fraction = !!share,
        .grouping = rollup(g), .id = "set", .margin_label = NULL)
      # Divide inside NUMERIC before converting the finite ratio to double.
      oracle <- DBI::dbGetQuery(con, paste0("SELECT CAST(SUM(x)/(SELECT SUM(x) FROM facts) AS double precision) AS fraction FROM facts GROUP BY g ORDER BY fraction"))$fraction
      value <- tryCatch(dplyr::collect(query), error = identity)
      expected <- c(oracle, 1)
      actual <- if (inherits(value, "error")) conditionMessage(value) else paste(sort(value$fraction), collapse = ",")
      pass <- !inherits(value, "error") && isTRUE(all.equal(sort(value$fraction), expected))
      rows[[length(rows) + 1L]] <- data.frame(exponent, kind, pass, actual)
    }
  }
  results <- do.call(rbind, rows)
  print(results, row.names = FALSE)
  utils::write.csv(results, "investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-numeric-range.csv", row.names = FALSE)
  if ("--assert" %in% commandArgs(TRUE)) stopifnot(all(results$pass))
})
