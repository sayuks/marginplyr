suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
})

con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

con_duck <- DBI::dbConnect(duckdb::duckdb())

clean <- function(x) {
    x <- as.data.frame(x)
    rownames(x) <- NULL
    x
}

canonical <- function(x) {
    x <- clean(x)
    if (nrow(x))
        x <- x[do.call(order, c(lapply(x, function(col) if (is.list(col)) vapply(col,
            toString, "") else as.character(col)), list(na.last = TRUE))), , drop = FALSE]
    rownames(x) <- NULL
    x
}

normal <- tibble(fixed = c("z", "a", "z", NA), g = c("x", "y", NA, "x"), h = c(2L,
    1L, 2L, NA_integer_), value = c(11, 12, 13, 14))

shapes <- list(normal = normal, empty = normal[0, ], factor = mutate(normal, g = factor(g,
    levels = c("y", "x", "q"))), typed = tibble(fixed = c("b", "a"), g = as.Date(c("2000-01-01",
    "2000-01-02")), h = as.POSIXct(c("2000-01-01", "2000-01-02"), tz = "UTC"), value = c(1,
    2)))

failures <- list()

count <- 0L

for (shape in names(shapes)) {
    data <- shapes[[shape]]
    sources <- list(sqlite = copy_to(con_sqlite, data, paste0("sq_", shape), temporary = TRUE),
        duck = copy_to(con_duck, data, paste0("dk_", shape), temporary = TRUE), arrow = arrow::Table$create(data))
    for (backend in names(sources)) for (derived in c(FALSE, TRUE)) {
        source <- sources[[backend]]
        if (derived)
            source <- filter(mutate(source, value = value * 2), is.na(h) | h > min(h,
                na.rm = TRUE))
        base <- tryCatch(collect(source), error = function(e) e)
        if (inherits(base, "error"))
            next
        for (group in c("rollup", "cube", "reverse", "dup", "none")) for (sort in c("none",
            "first", "last")) for (missing in c(FALSE, TRUE)) for (fixed in c(FALSE,
            TRUE)) {
            count <- count + 1L
            spec <- switch(group, rollup = rollup(g, h), cube = cube(g, h), reverse = grouping_sets(grouping_set(),
                grouping_set(g, h), grouping_set(g)), dup = grouping_sets(grouping_set(g),
                grouping_set(g)), none = NULL)
            invoke <- function(x) expand_with_margins(x, .by = if (fixed)
                all_of("fixed")
            else NULL, .grouping = spec, .sort = sort, .margin_label = if (missing)
                NULL
            else "Total", .id = "id", .duplicates = "keep")
            got <- tryCatch(collect(invoke(source)), error = function(e) e)
            expected <- tryCatch(invoke(base), error = function(e) e)
            key <- paste(shape, backend, derived, group, sort, missing, fixed, sep = "/")
            if (inherits(got, "error") || inherits(expected, "error")) {
                if (inherits(got, "error") && !inherits(expected, "error")) {
                  cat("ERROR ", key, " ", conditionMessage(got), "\n")
                  failures[[key]] <- conditionMessage(got)
                }
            }
            else {
                a <- canonical(got)
                b <- canonical(expected)
                if (!isTRUE(all.equal(a, b, check.attributes = FALSE))) {
                  cat("DIFF ", key, " ", paste(all.equal(a, b, check.attributes = FALSE),
                    collapse = "; "), "\n")
                  failures[[key]] <- list(got = got, expected = expected)
                }
                if (sort != "none" && !isTRUE(all.equal(clean(got), clean(expected),
                  check.attributes = FALSE))) {
                  cat("ORDER ", key, " ", paste(all.equal(clean(got), clean(expected),
                    check.attributes = FALSE), collapse = "; "), "\n")
                  failures[[paste0(key, "/ORDER")]] <- list(got = got, expected = expected)
                }
            }
        }
    }
}

cat("TOTAL", count, "FAILURES", length(failures), "\n")

saveRDS(failures, "/tmp/margin-lazy-hunt-results.rds")

DBI::dbDisconnect(con_sqlite)

DBI::dbDisconnect(con_duck, shutdown = TRUE)
