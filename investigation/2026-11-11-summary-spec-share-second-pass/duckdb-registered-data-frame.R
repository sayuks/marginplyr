# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Repro: a data frame registered with duckdb::duckdb_register() is refused by every Margin verb
args <- commandArgs(TRUE)
repo <- if (length(args)) args[[1]] else "."
suppressMessages(pkgload::load_all(repo, quiet = TRUE)); suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:", shared_home = FALSE))
d <- data.frame(k = c("p", "p", "q"), a = c("x", "y", "x"), v = c(1, 2, 4))
duckdb::duckdb_register(con, "d_reg", d)          # zero-copy view over the R data frame
DBI::dbWriteTable(con, "d_tab", d)                # control: same rows as a table
show <- function(tag, expr) cat(sprintf("%-28s", tag), tryCatch({ r <- expr; paste("ok,", nrow(as.data.frame(r)), "rows") },
  error = function(e) paste0("ERROR <", class(e)[1], ">: ", conditionMessage(e))), "\n")
show("native summarise, registered", collect(summarise(tbl(con, "d_reg"), s = sum(v, na.rm = TRUE), .by = c(k, a))))
show("margins, table",      collect(summarize_with_margins(tbl(con, "d_tab"), s = sum(v, na.rm = TRUE), .by = k, .grouping = rollup(a))))
show("margins, registered", collect(summarize_with_margins(tbl(con, "d_reg"), s = sum(v, na.rm = TRUE), .by = k, .grouping = rollup(a))))
show("margins, registered, .by only", collect(summarize_with_margins(tbl(con, "d_reg"), s = sum(v, na.rm = TRUE), .by = k)))
show("share, registered",   collect(summarize_with_margins(tbl(con, "d_reg"), s = sum(v, na.rm = TRUE), sh = share_of_total(s), .grouping = rollup(a))))
show("expand, registered",  collect(expand_with_margins(tbl(con, "d_reg"), .grouping = rollup(a))))
show("inspect (name-only)", inspect_grouping(tbl(con, "d_reg"), .grouping = rollup(a)))
show("inspect (typed where())", inspect_grouping(tbl(con, "d_reg"), .grouping = rollup(where(is.character))))
print(DBI::dbGetQuery(con, "SELECT view_name, sql, temporary FROM duckdb_views() WHERE NOT internal"))
DBI::dbDisconnect(con, shutdown = TRUE)
