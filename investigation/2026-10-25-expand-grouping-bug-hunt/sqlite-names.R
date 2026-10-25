pkgload::load_all(".", quiet = TRUE)

library(dplyr, warn.conflicts = FALSE)

set.seed(4471)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

weird <- c("normal", ".env", ".data", "a...1", "a b", "select", "x`y", "a\"b", "é",
    "é", "TRUE", "NA", "..marginplyr_sort_1")

fail <- list()

n <- 0L

for (name in weird) for (id in weird) for (sort in c("none", "first", "last")) {
    if (id %in% c(name, "v"))
        next
    d <- tibble::tibble(g = c("z", NA, "a"), v = 1:3)
    names(d)[1] <- name
    spec <- rollup(all_of(name))
    expected <- expand_with_margins(d, .grouping = spec, .id = id, .sort = sort,
        .margin_label = NULL)
    s <- copy_to(con, d, "source", overwrite = TRUE, temporary = TRUE)
    got <- tryCatch(collect(expand_with_margins(s, .grouping = spec, .id = id, .sort = sort,
        .margin_label = NULL)), error = identity)
    n <- n + 1L
    same <- !inherits(got, "error") && identical(names(got), names(expected))
    if (same) {
        keys <- order(expected[[id]], expected$v)
        gkeys <- order(got[[id]], got$v)
        same <- isTRUE(all.equal(as.data.frame(got[gkeys, ]), as.data.frame(expected[keys,
            ]), check.attributes = FALSE))
        if (sort != "none")
            same <- same && identical(got$v, expected$v) && identical(got[[id]],
                expected[[id]])
    }
    if (!same)
        fail[[length(fail) + 1]] <- list(name = name, id = id, sort = sort, result = if (inherits(got,
            "error")) conditionMessage(got) else got, expected = expected)
}

cat("CASES", n, "FAILURES", length(fail), "\n")

print(utils::head(fail, 10))

DBI::dbDisconnect(con)
