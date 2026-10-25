suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
})

cs <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

cd <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))

data <- tibble(g = c("z", "a", "z", "b", NA), h = c(3L, 5L, 1L, 2L, NA), v = 1:5)

transforms <- list(identity = function(x) x, limit = function(x) head(x, 2L), order_limit = function(x) head(arrange(x,
    desc(h)), 2L), window = function(x) mutate(window_order(x, h), rn = row_number()),
    arrange = function(x) arrange(x, desc(h)), group = function(x) group_by(x, h),
    distinct = function(x) distinct(x, g, .keep_all = TRUE), summarise = function(x) summarise(x,
        h = min(h, na.rm = TRUE), v = sum(v), .by = g), union = function(x) union_all(x,
        filter(x, v < 3L)), join = function(x) left_join(x, select(filter(x, v <
        3L), g, h2 = h), by = "g"), rename = function(x) rename(x, g2 = g), filter_window = function(x) filter(x,
        min_rank(v) < 3L))

problems <- list()

i <- 0L

for (b in c("sqlite", "duck", "arrow")) {
    src <- switch(b, sqlite = copy_to(cs, data, "a"), duck = copy_to(cd, data, "a"),
        arrow = arrow::Table$create(data))
    for (tn in names(transforms)) {
        x <- tryCatch(transforms[[tn]](src), error = function(e) e)
        if (inherits(x, "error")) {
            cat("TRANSFORM", b, tn, conditionMessage(x), "\n")
            next
        }
        base <- tryCatch(collect(x), error = function(e) e)
        if (inherits(base, "error")) {
            cat("BASE", b, tn, conditionMessage(base), "\n")
            next
        }
        dim <- if (tn == "rename")
            "g2"
        else "g"
        for (specn in c("rollup", "set", "reverse")) for (sort in c("none", "last")) for (id in c(FALSE,
            TRUE)) for (label in c("Total", NA_character_)) {
            key <- paste(b, tn, specn, sort, id, label, sep = "/")
            spec <- switch(specn, rollup = rollup(all_of(dim)), set = grouping_set(all_of(dim)),
                reverse = grouping_sets(grouping_set(), grouping_set(all_of(dim))))
            invoke <- function(z) expand_with_margins(z, .grouping = spec, .id = if (id)
                "id"
            else NULL, .sort = sort, .margin_label = label)
            got <- tryCatch(collect(invoke(x)), error = function(e) e)
            want <- tryCatch(invoke(base), error = function(e) e)
            i <- i + 1L
            if (inherits(got, "error")) {
                cat("ERROR", key, conditionMessage(got), "\n")
                problems[[key]] <- got
                next
            }
            if (inherits(want, "error"))
                next
            same <- function(a, b) {
                a <- as.data.frame(a)
                b <- as.data.frame(b)
                rownames(a) <- NULL
                rownames(b) <- NULL
                if (sort == "none") {
                  sorter <- function(z) {
                    z <- z[do.call(order, c(lapply(z, as.character), list(na.last = TRUE))),
                      , drop = FALSE]
                    rownames(z) <- NULL
                    z
                  }
                  a <- sorter(a)
                  b <- sorter(b)
                }
                isTRUE(all.equal(a, b, check.attributes = FALSE))
            }
            if (!same(got, want)) {
                cat("DIFF", key, "\n")
                problems[[key]] <- list(got = got, want = want)
            }
        }
    }
}

cat("TOTAL", i, "FAILURES", length(problems), "\n")

saveRDS(problems, "/tmp/margin-lazy-derived.rds")

DBI::dbDisconnect(cs)

DBI::dbDisconnect(cd, shutdown = TRUE)
