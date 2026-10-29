pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbExecute(con, "SET threads = 1")
keys <- list(
  array = "[i % 2, NULL]", struct = "struct_pack(a := i % 2, b := NULL::INTEGER)",
  map = "map(['a', 'b'], [i % 2, NULL])", blob = "encode(CAST(i % 2 AS VARCHAR))",
  uuid = "CAST(CASE WHEN i % 2 = 0 THEN '00000000-0000-0000-0000-000000000001' ELSE '00000000-0000-0000-0000-000000000002' END AS UUID)",
  interval = "i % 2 * INTERVAL 1 DAY", json = "CAST(i % 2 AS JSON)",
  timetz = "CAST(CASE WHEN i % 2 = 0 THEN '12:00:00+02:00' ELSE '15:00:00+03:00' END AS TIMETZ)",
  decimal = "CAST(i % 2 AS DECIMAL(18,5))", date = "DATE '2026-01-01' + CAST(i % 2 AS INTEGER)",
  nan = "CASE WHEN i % 2 = 0 THEN 'NaN'::DOUBLE ELSE 0::DOUBLE END",
  infinity = "CASE WHEN i % 2 = 0 THEN 'Infinity'::DOUBLE ELSE '-Infinity'::DOUBLE END"
)
checks <- 0L
signals <- list()
for (kind in names(keys)) {
  sql <- paste0("SELECT ", keys[[kind]], " AS k, 'g' || CAST(i % 2 AS VARCHAR) AS g, i AS v FROM range(1,5) t(i)")
  source <- tryCatch(tbl(con, dbplyr::sql(sql)), error = identity)
  if (inherits(source, "error")) {
    cat("FIXTURE_REFUSED", kind, conditionMessage(source), "\n")
    next
  }
  for (role in c("fixed", "dimension")) {
    for (sort in c("none", "first", "last")) {
      for (duplicates in c("drop", "keep")) {
        for (share in c(FALSE, TRUE)) {
          for (helper in c("total", "parent")) {
            if (!share && helper == "parent") next
            checks <- checks + 1L
            key <- paste(kind, role, sort, duplicates, share, helper, sep = "/")
            result <- tryCatch({
              dots <- rlang::quos(amount = sum(v, na.rm = TRUE))
              if (share) dots$fraction <- if (helper == "total") rlang::quo(share_of_total(amount)) else rlang::quo(share_of_parent(amount))
              query <- summarize_with_margins(
                source, !!!dots,
                .by = if (role == "fixed") "k" else NULL,
                .grouping = if (role == "fixed") rollup(g) else rollup(k, g),
                .sort = sort, .duplicates = duplicates,
                .margin_label = NULL, .id = "sid"
              )
              direct <- collect(query)
              materialized <- collect(compute(query))
              if (sort == "none") {
                direct <- direct[vctrs::vec_order(direct), ]
                materialized <- materialized[vctrs::vec_order(materialized), ]
              }
              comparison <- all.equal(direct, materialized, check.attributes = FALSE)
              if (!isTRUE(comparison)) stop(paste(comparison, collapse = "; "))
              if (share) {
                for (row in seq_len(nrow(direct))) {
                  root <- direct$sid == if (helper == "total") max(direct$sid) else min(direct$sid[row] + 1L, max(direct$sid))
                  if (role == "fixed") root <- root & vctrs::vec_equal(direct$k, vctrs::vec_slice(direct$k, row), na_equal = TRUE)
                  if (role == "dimension" && helper == "parent" && direct$sid[row] == 1L) root <- root & vctrs::vec_equal(direct$k, vctrs::vec_slice(direct$k, row), na_equal = TRUE)
                  expected <- direct$amount[row] / direct$amount[which(root)[1L]]
                  stopifnot(isTRUE(all.equal(direct$fraction[row], expected)))
                }
              }
              TRUE
            }, error = identity)
            if (inherits(result, "error")) {
              signals[[key]] <- conditionMessage(result)
              cat("SIGNAL", key, conditionMessage(result), "\n")
            }
          }
        }
      }
    }
  }
  cat("COMPLETED_TYPE", kind, "checks", checks, "signals", length(signals), "\n")
}
DBI::dbDisconnect(con, shutdown = TRUE)
cat("CHECKS", checks, "SIGNALS", length(signals), "\n")
