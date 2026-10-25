suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
})

cs <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

cd <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))

problems <- list()

k <- 0L

for (nm in c(".env", ".data", "..marginplyr_sort_1", "..MARGINPLYR_SORT_1", "..marginplyr_arrow_dimension_1",
    "rowid", "_rowid_", "oid", "a`b", "a\"b", "a\nb", "a\rb", "a b", "NULL", "NA",
    "1", "...", "..1", "x,y", "x=0", "é", "É")) {
    data <- tibble(g = c("b", "a", NA), v = 1:3)
    names(data)[2] <- nm
    for (b in c("sqlite", "duck", "arrow")) {
        k <- k + 1L
        source <- tryCatch(switch(b, sqlite = copy_to(cs, data, paste0("t", k)),
            duck = copy_to(cd, data, paste0("t", k)), arrow = arrow::Table$create(data)),
            error = function(e) e)
        if (inherits(source, "error")) {
            cat("SOURCE", b, nm, conditionMessage(source), "\n")
            next
        }
        for (dim in c("g", nm)) for (s in c("none", "last")) for (label in c("Total",
            NA_character_)) {
            ky <- paste(b, encodeString(nm), dim, s, label, sep = "/")
            got <- tryCatch(expand_with_margins(source, .grouping = rollup(all_of(dim)),
                .sort = s, .margin_label = label, .id = "id"), error = function(e) e)
            if (!inherits(got, "error"))
                got <- tryCatch(collect(got), error = function(e) e)
            if (inherits(got, "error")) {
                cat("ERROR", ky, conditionMessage(got), "\n")
                problems[[ky]] <- conditionMessage(got)
            }
        }
    }
}

cat("FAILURES", length(problems), "\n")

saveRDS(problems, "/tmp/margin-lazy-names.rds")

DBI::dbDisconnect(cs)

DBI::dbDisconnect(cd, shutdown = TRUE)
