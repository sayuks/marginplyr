Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
out <- "investigation/2026-11-02-summary-grouping-share-bug-hunt/backend"
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), array = "matrix")
keys <- c(
  integer = "i%2", bigint = "CAST(i%2 AS BIGINT)",
  decimal = "CAST(i%2 AS DECIMAL(18,3))", hugeint = "CAST(i%2 AS HUGEINT)",
  date = "DATE '2026-01-01'+CAST(i%2 AS INTEGER)",
  timestamp = "TIMESTAMP '2026-01-01'+(i%2)*INTERVAL '1 second'",
  time = "CAST(CASE WHEN i%2=0 THEN '12:00:00' ELSE '13:00:00' END AS TIME)",
  interval = "(i%2)*INTERVAL '1 day'",
  uuid = "CAST(CASE WHEN i%2=0 THEN '00000000-0000-0000-0000-000000000001' ELSE '00000000-0000-0000-0000-000000000002' END AS UUID)",
  blob = "CAST(CAST(i%2 AS VARCHAR) AS BLOB)",
  list = "[i%2, NULL]", array = "CAST([i%2, 2] AS INTEGER[2])",
  struct = "{a:i%2,b:NULL::INTEGER}", map = "MAP([i%2], [i%2])")
capture <- function(f) tryCatch(list(ok = TRUE, value = f(), message = ""),
                               error = function(e) list(ok = FALSE, message = conditionMessage(e)))
rows <- list()
for (kind in names(keys)) {
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TABLE typed_keys AS SELECT ", keys[[kind]],
    " AS k, CASE WHEN i<3 THEN 'a' ELSE 'b' END AS g, CAST(i AS DOUBLE) AS x FROM range(1,5) t(i)"))
  src <- tbl(con, "typed_keys")
  for (role in c("fixed", "dimension")) for (adapter in c("native", "portable")) for (sorting in c("none", "last")) {
    ordinary <- capture(function() collect(summarise(group_by(src, k), v = sum(x, na.rm = TRUE))))
    result <- capture(function() {
      q <- if (role == "fixed") {
        summarize_with_margins(src, v = sum(x, na.rm = TRUE), p = share_of_total(v),
          .by = k, .grouping = rollup(g), .margin_label = NULL, .id = "set",
          .duplicates = if (adapter == "portable") "keep" else "error", .sort = sorting)
      } else {
        summarize_with_margins(src, v = sum(x, na.rm = TRUE), p = share_of_total(v),
          .grouping = rollup(k), .margin_label = NULL, .id = "set",
          .duplicates = if (adapter == "portable") "keep" else "error", .sort = sorting)
      }
      val <- collect(q)
      material <- collect(compute(q, name = "key_result", overwrite = TRUE))
      if (sorting == "last") stopifnot(isTRUE(all.equal(as_tibble(val), as_tibble(material))))
      # The fixture partitions into k=1 with total 4 and k=0 with total 6.
      # Every detail row is one original x. The dimension-only details total 4 and 6.
      if (role == "fixed") {
        stopifnot(nrow(val) == 6L,
                  isTRUE(all.equal(sort(val$p[val$set == 1L]), sort(c(1/4,3/4,2/6,4/6)))),
                  all(val$p[val$set == 2L] == 1))
      } else {
        stopifnot(nrow(val) == 3L,
                  isTRUE(all.equal(sort(val$p[val$set == 1L]), c(0.4,0.6))),
                  all(val$p[val$set == 2L] == 1))
      }
      TRUE
    })
    rows[[length(rows)+1L]] <- data.frame(kind=kind, role=role, adapter=adapter, sort=sorting,
      ordinary=ordinary$ok, margin=result$ok, message=result$message)
  }
}
DBI::dbDisconnect(con)
ans <- bind_rows(rows)
write.csv(ans, file.path(out,"duckdb-key-types.csv"), row.names = FALSE)
print(with(ans, table(kind, margin)))
print(ans[!ans$margin, ], row.names = FALSE)
