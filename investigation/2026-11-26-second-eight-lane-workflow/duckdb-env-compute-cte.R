# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# compute() of a DuckDB Margin result of class marginplyr_duckdb_env refuses dbplyr's deprecated cte argument.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), "dbplyr", as.character(packageVersion("dbplyr")), "\n")
src <- data.frame(g = c("a", "b"), v = 1:2, .env = c("x", "y"))
with_env <- copy_to(con, src, "with_env")
without <- copy_to(con, src[c("g", "v")], "without")
env_x <- expand_with_margins(with_env, .grouping = rollup(g), .margin_label = NULL, .id = "sid")
ord_x <- expand_with_margins(without, .grouping = rollup(g), .margin_label = NULL, .id = "sid")
env_s <- summarize_with_margins(with_env, n = n(), .by = tidyselect::all_of(".env"), .grouping = rollup(g))
cat("classes:", class(env_x)[1], "/", class(env_s)[1], "/", class(ord_x)[1], "\n\n")
run <- function(label, expr) {
  warned <- character()
  r <- tryCatch(withCallingHandlers(expr, warning = function(w) { warned <<- c(warned, class(w)[1]); invokeRestart("muffleWarning") }),
    error = function(e) paste0("ERROR <", class(e)[1], "> ", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 90)))
  if (inherits(r, "tbl_lazy")) r <- paste("computed,", nrow(collect(r)), "rows")
  cat(sprintf("%-48s %s%s\n", label, r, if (length(warned)) paste0("  [warning: ", paste(warned, collapse = ","), "]") else ""))
}
cat("Expected: as the result without `.env`: computed with lifecycle_warning_deprecated for cte;\n",
    "         rlang's `temporary` must be `TRUE` or `FALSE` for temporary = NA / \"yes\".\n\n")
run("without .env: compute(cte = TRUE)", compute(ord_x, cte = TRUE))
run("with .env expansion: compute(cte = TRUE)", compute(env_x, cte = TRUE))
run("with .env expansion: compute(cte = FALSE)", compute(env_x, cte = FALSE))
run("with .env summary: compute(cte = TRUE)", compute(env_s, cte = TRUE))
run("with .env: compute(sql_options(cte = TRUE))", compute(env_x, sql_options = dbplyr::sql_options(cte = TRUE)))
run("without .env: compute(temporary = NA)", compute(ord_x, temporary = NA))
run("with .env: compute(temporary = NA)", compute(env_x, temporary = NA))
run("without .env: compute(temporary = 'yes')", compute(ord_x, temporary = "yes"))
run("with .env: compute(temporary = 'yes')", compute(env_x, temporary = "yes"))
run("with .env: compute(name = 'n1', temporary = NA)", compute(env_x, name = "n1", temporary = NA))
cat("\nNative control on a plain lazy query (no Margin verb, no `.env`):\n")
run("compute(mutate(without, w = v), cte = TRUE)", compute(mutate(without, w = v), cte = TRUE))
run("compute(mutate(without, w = v), temporary = NA)", compute(mutate(without, w = v), temporary = NA))
cat("\nA native lazy query holding `.env` cannot be computed by dbplyr at all:\n")
run("compute(mutate(with_env, w = v))", compute(mutate(with_env, w = v)))
DBI::dbDisconnect(con, shutdown = TRUE)
