# Run from the marginplyr checkout: Rscript investigation/2026-10-25-expand-grouping-bug-hunt/duckdb-scalars.R
# The last assertion describes the desired behavior and fails on b6f9fab.
suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
})

con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))

DBI::dbExecute(con, "CREATE TEMP TABLE typed_keys AS SELECT TIME '12:30:00' AS clock, INTERVAL '2 days 3 hours' AS elapsed, 'abc'::BLOB AS binary, 1 AS value")

failures <- character()

for (column in c("clock", "elapsed", "binary")) {
    source <- select(tbl(con, "typed_keys"), g = all_of(column), value)
    input <- collect(source)
    control <- collect(union_all(source, mutate(source, g = NA)))
    stopifnot(nrow(control) == 2L)
    cat("\nTYPE", column, "\nINPUT\n")
    print(input)
    cat("SQL NULL CONTROL\n")
    print(control)
    plan <- inspect_grouping(source, .grouping = rollup(g))
    stopifnot(identical(plan$included, c("(g)", "()")))
    summary <- collect(summarize_with_margins(source, n = n(), .grouping = rollup(g),
        .margin_label = NULL))
    stopifnot(nrow(summary) == 2L)
    cat("SUMMARY CONTROL\n")
    print(summary)
    expanded <- tryCatch(expand_with_margins(source, .grouping = rollup(g), .margin_label = NULL),
        error = identity)
    result <- if (inherits(expanded, "error"))
        expanded
    else tryCatch(collect(expanded), error = identity)
    if (inherits(result, "error")) {
        failures <- c(failures, column)
        cat("EXPANSION ERROR\n", conditionMessage(result), "\n")
    }
    else {
        stopifnot(nrow(result) == 2L)
        cat("EXPANSION\n")
        print(result)
    }
}

DBI::dbDisconnect(con, shutdown = TRUE)

cat("Desired-contract failures:", length(failures), "\n")

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(length(failures) ==
    0L)
