pkgload::load_all(".", quiet = TRUE)

library(dplyr, warn.conflicts = FALSE)

d <- tibble::tibble(x = 1:3, y = c(1, 2, 3), g = c("a", "b", "c"))

exprs <- rlang::exprs(x/2, x + 0.5, x * 1.5, x - x, x/0, 1/x, +x, -x, x + 1L, x *
    2L, x + y, x/y, 2L, 2.5, x, y)

fail <- list()

accepted <- 0L

for (e in exprs) for (target in c("x", "z")) {
    s <- mutate(dtplyr::lazy_dt(d), `:=`(!!target, !!e))
    actual <- collect(s)
    expected <- names(actual)[vapply(actual, is.integer, logical(1))]
    got <- tryCatch(inspect_grouping(s, .grouping = grouping_set(where(is.integer)),
        .format = "list"), error = identity)
    if (inherits(got, "marginplyr_error"))
        next
    accepted <- accepted + 1L
    if (inherits(got, "error") || !identical(got$included[[1L]], expected))
        fail[[length(fail) + 1L]] <- list(target = target, expr = rlang::as_label(e),
            expected = expected, got = if (inherits(got, "error")) conditionMessage(got) else got$included)
}

cat("ACCEPTED", accepted, "FAILURES", length(fail), "\n")

print(fail)
