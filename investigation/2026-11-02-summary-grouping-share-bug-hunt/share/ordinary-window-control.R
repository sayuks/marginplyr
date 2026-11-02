# Independent ordinary DuckDB SQL, without a Margin operation.
run <- function() {
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
DBI::dbExecute(con, "CREATE TABLE facts AS SELECT * FROM (VALUES ('a','x'),('a','y'),('b','x'),('b','y')) t(g,h)")
base <- "SELECT g,h,GROUPING_ID(g,h) AS gid, CAST(nextval('probe') AS DOUBLE) AS v
         FROM facts GROUP BY GROUPING SETS ((g,h),(g),())"
windowed <- function(source) paste0(
  "SELECT g,h,gid,v,CASE WHEN gid=3 THEN 1.0 ELSE v/den END AS p FROM (",
  "SELECT g,h,gid,v,CASE WHEN gid=1 THEN MAX(CASE WHEN gid=3 THEN v END) OVER () ELSE den END AS den FROM (",
  "SELECT *, MAX(CASE WHEN gid=1 THEN v END) OVER (PARTITION BY g) AS den FROM ",
  source, "))"
)
for (materialized in c(FALSE, TRUE)) {
  DBI::dbExecute(con, "CREATE OR REPLACE SEQUENCE probe")
  query <- if (materialized) {
    paste0("WITH staged AS MATERIALIZED (", base, ") ", windowed("staged"))
  } else windowed(paste0("(", base, ") q"))
  result <- DBI::dbGetQuery(con, query)
  expected <- vapply(seq_len(nrow(result)), function(i) {
    row <- result[i, ]
    if (row$gid == 3) return(1)
    denominator <- if (row$gid == 1) result$v[result$gid == 3] else
      result$v[result$gid == 1 & !is.na(result$g) & result$g == row$g]
    row$v / denominator
  }, numeric(1))
  matches <- isTRUE(all.equal(result$p, expected))
  sequence <- DBI::dbGetQuery(con, "SELECT currval('probe')")[[1L]]
  stopifnot(identical(matches, materialized), nrow(result) == 7L,
            as.numeric(sequence) == if (materialized) 7 else 14)
  cat("materialized", materialized, "matches", matches,
      "sequence", sequence, "\n")
  print(result)
  cat("Expected:", expected, "\n")
}
}
run()
