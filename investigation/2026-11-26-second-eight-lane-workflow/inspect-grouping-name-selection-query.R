# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A name selection computed inside a helper, such as all_of(sort(dims)), sends DuckDB's metadata queries although ?inspect_grouping says selecting by name sends none.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
sent <- new.env(); sent$sql <- character()
invisible(suppressMessages(trace("rethrow_rapi_prepare", where = asNamespace("duckdb"), print = FALSE,
  tracer = quote(assign("sql", c(get("sql", envir = .GlobalEnv$sent), query), envir = .GlobalEnv$sent)))))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "sales", data.frame(region = c("e", "w"), store = c("s1", "s2"), revenue = c(1, 2)))
sales <- tbl(con, "sales")
options(marginplyr.audit_sql = TRUE)
dims <- c("store", "region")
probe <- function(label, expr) {
  sent$sql <- character()
  p <- force(expr)
  cat(sprintf("%-48s queries sent: %d  recorded: %-34s plan: %s\n", label, length(sent$sql),
              paste(last_sent_queries(include_result = FALSE)$purpose, collapse = ", "), paste(p$included, collapse = " ")))
}
cat("EXPECTED (?inspect_grouping, Lazy inputs): 'Selecting columns by name sends no query.' -> 0 queries for every line.\n\n")
probe("CONTROL  rollup(region, store)", inspect_grouping(sales, .grouping = rollup(region, store)))
probe("CONTROL  rollup(all_of(dims))", inspect_grouping(sales, .grouping = rollup(all_of(dims))))
probe("OBSERVED rollup(all_of(sort(dims)))", inspect_grouping(sales, .grouping = rollup(all_of(sort(dims)))))
probe("OBSERVED rollup(all_of(rev(dims)))", inspect_grouping(sales, .grouping = rollup(all_of(rev(dims)))))
probe("OBSERVED rollup(all_of(unique(dims)))", inspect_grouping(sales, .grouping = rollup(all_of(unique(dims)))))
probe("OBSERVED rollup(all_of(setdiff(colnames(sales), 'revenue')))", inspect_grouping(sales, .grouping = rollup(all_of(setdiff(colnames(sales), "revenue")))))
probe("OBSERVED .by = all_of(head(dims, 1)), rollup(region)", inspect_grouping(sales, .by = all_of(head(dims, 1)), .grouping = rollup(region)))
cat("\nStatements sent by the last call:\n"); for (s in sent$sql) cat("   ", substr(gsub("\n", " ", s), 1, 110), "\n")
DBI::dbDisconnect(con, shutdown = TRUE)
invisible(0)

cat("\nConsequence on inputs whose types cannot be read without rows (DuckDB CSV auto-detect, derived dtplyr step):\n")
f <- file.path(tempdir(), "cand4-sales.csv")
write.csv(data.frame(region = c("e", "w"), store = c("s1", "s2"), revenue = c(1, 2)), f, row.names = FALSE)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
csv <- tbl(con, dbplyr::sql(sprintf("SELECT * FROM read_csv_auto('%s')", f)))
step <- dtplyr::lazy_dt(data.frame(region = c("e", "w"), store = c("s1", "s2"), revenue = c(1, 2)), immutable = TRUE) |>
  dplyr::mutate(running = cumsum(revenue))
first_line <- function(e) strsplit(conditionMessage(e), "\n")[[1]][1]
for (nm in c("csv", "step")) {
  x <- get(nm)
  a <- tryCatch({p <- inspect_grouping(x, .grouping = rollup(all_of(dims))); paste(p$included, collapse = " ")}, error = first_line)
  b <- tryCatch({p <- inspect_grouping(x, .grouping = rollup(all_of(sort(dims)))); paste(p$included, collapse = " ")}, error = first_line)
  n <- tryCatch({dplyr::select(x, dplyr::all_of(sort(dims))); "builds"}, error = first_line)
  cat(sprintf("  %-4s CONTROL all_of(dims): %s\n       OBSERVED all_of(sort(dims)): %s\n       NATIVE select(x, all_of(sort(dims))): %s\n", nm, a, b, n))
}
DBI::dbDisconnect(con, shutdown = TRUE)
