pkgload::load_all(".", quiet = TRUE)

suppressPackageStartupMessages(library(dplyr))

levels <- c(NA_character_, "b", "a", "..marginplyr_missing_factor_code", "..marginplyr_missing_factor_code_",
    "unused", "")

f <- structure(c(1L, NA_integer_, 3L, 2L, 4L, 5L, 7L), class = "factor", levels = levels)

ct <- 0L

failures <- 0L

for (ord in c(FALSE, TRUE)) for (label in list(NULL, "T")) for (pos in c("first",
    "last")) for (sort in c("none", "first", "last")) for (n in c(0L, 1L, 7L)) for (role in c("dimension",
    "fixed", "payload")) {
    g <- f[seq_len(n)]
    if (ord)
        class(g) <- c("ordered", "factor")
    x <- tibble(g = g, other = rep(c("b", "a", NA), length.out = n), rid = seq_len(n))
    run <- function(x) expand_with_margins(x, .by = all_of(if (role == "fixed")
        "g"
    else character()), .grouping = if (role == "dimension")
        rollup(g)
    else rollup(other), .margin_label = label, .margin_label_position = pos, .sort = sort,
        .id = "set")
    a <- run(x)
    b <- collect(run(dtplyr::lazy_dt(x)))
    ct <- ct + 1L
    if (!isTRUE(all.equal(a, b))) {
        failures <- failures + 1L
        cat("BAD", ord, if (is.null(label))
            "NULL"
        else label, pos, sort, n, role, "\n")
        print(all.equal(a, b))
        str(a)
        str(b)
    }
}

cat("CASES", ct, "\n")

stopifnot(ct == 216L, failures == 0L)
