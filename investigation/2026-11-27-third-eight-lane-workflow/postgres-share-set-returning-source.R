# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A PostgreSQL share source with a set-returning function other than a bare UNNEST() is collected with multiplied rows (#1817).
# A PostgreSQL share whose dbplyr::sql() source returns several rows per group
# through a set-returning function other than a bare UNNEST() (generate_series,
# jsonb_array_elements, a schema-qualified or quoted unnest) is collected
# silently with multiplied rows and shares, where share_of_parent()'s
# "Lazy execution boundaries" says it "surfaces as a database error when the
# staged query executes". A bare UNNEST() is the positive control.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
if (inherits(try(DBI::dbDisconnect(DBI::dbConnect(RPostgres::Postgres(), host = "/tmp/claude/mp-pg/sock", port = 55432, user = "marginplyr", dbname = "postgres")), silent = TRUE), "try-error")) {
  cat("The temporary PostgreSQL cluster of the investigation is not reachable; nothing was run.\n")
  quit(status = 0)
}
suppressMessages(library(dplyr))
con <- DBI::dbConnect(RPostgres::Postgres(), host = "/tmp/claude/mp-pg/sock",
                      port = 55432, user = "marginplyr", dbname = "postgres")
t <- tbl(con, dbplyr::sql("SELECT * FROM (VALUES ('a', 1), ('a', 2), ('b', 5)) AS s(g, v)"),
         vars = c("g", "v"))
cat("Expected (Rd): every source below is not one value per grouping row, so collect()\n",
    "raises a database error; no share is returned.\n\n", sep = "")
for (s in c("SUM(v) + UNNEST(ARRAY[10, 20])",            # positive control
            "SUM(v) * generate_series(1, 2)",
            "SUM(v) + (jsonb_array_elements('[1, 2]'::jsonb))::int",
            "SUM(v) + pg_catalog.unnest(ARRAY[10, 20])",
            "SUM(v) + \"unnest\"(ARRAY[10, 20])")) {
  cat("source:", s, "\n")
  native <- t |> group_by(g) |> summarise(n = dbplyr::sql(s)) |> collect()
  cat("  native dbplyr summarise() per group:", nrow(native), "rows for 2 groups (not one value per group)\n")
  r <- tryCatch(collect(summarize_with_margins(t, n = dbplyr::sql(s), p = share_of_total(n),
                                               .grouping = rollup(g))),
                error = function(e) e)
  if (inherits(r, "error")) {
    cat("  marginplyr: database error:", sub(".*ERROR: +", "", gsub("\n", " ", conditionMessage(r))) |> substr(1, 90), "\n")
  } else {
    cat("  marginplyr: returned", nrow(r), "rows, no condition:",
        paste0(r$g, " n=", r$n, " p=", round(r$p, 3), collapse = "; "), "\n")
  }
}
DBI::dbDisconnect(con)
