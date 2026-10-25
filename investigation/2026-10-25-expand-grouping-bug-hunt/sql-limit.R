# Run from the marginplyr checkout: Rscript investigation/2026-10-25-expand-grouping-bug-hunt/sql-limit.R
# Final assertions describe the desired behavior and fail on b6f9fab.
suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
})

failures <- character()

for (backend in c("sqlite", "duckdb")) {
    con <- if (backend == "sqlite")
        DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    else DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    source <- head(arrange(copy_to(con, tibble(g = c("c", "b", "a"), v = 1:3), "facts"),
        v), 2L)
    input <- collect(source)
    stopifnot(identical(input$v, 1:2))
    id <- if (backend == "duckdb")
        "set"
    else NULL
    specifications <- list(empty_first = grouping_sets(grouping_set(), grouping_set(g)),
        detail_first = rollup(g))
    materialized <- compute(source, name = "selected_rows")
    control <- collect(expand_with_margins(materialized, .grouping = specifications$empty_first,
        .margin_label = NULL, .id = id))
    stopifnot(nrow(control) == 4L, identical(sort(control$v), c(1L, 1L, 2L, 2L)))
    for (name in names(specifications)) {
        query <- expand_with_margins(source, .grouping = specifications[[name]],
            .margin_label = NULL, .id = id)
        cat("\n", backend, name, "\n", as.character(sql_render(query)), "\n")
        result <- tryCatch(collect(query), error = identity)
        if (inherits(result, "error")) {
            failures <- c(failures, paste(backend, name, "error"))
            cat("ERROR\n", conditionMessage(result), "\n")
        }
        else {
            print(result)
            if (nrow(result) != 4L || !identical(sort(result$v), c(1L, 1L, 2L, 2L))) {
                failures <- c(failures, paste(backend, name, "cardinality"))
            }
        }
    }
    DBI::dbDisconnect(con)
}

print(failures)

cat("Desired-contract failures:", length(failures), "\n")

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(length(failures) ==
    0L)
