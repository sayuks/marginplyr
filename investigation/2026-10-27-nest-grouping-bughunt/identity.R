pkgload::load_all(".", quiet = TRUE)

options(warn = 2)

checks <- 0L

for (n in c(0L, 1L, 2L, 3L, 30L, 31L, 32L)) {
    cols <- if (n)
        paste0("x", seq_len(n))
    else character()
    dat <- tibble::as_tibble(stats::setNames(rep(list(1L), n), cols), .rows = 1)
    spec <- if (n)
        rollup(dplyr::all_of(cols))
    else grouping_set()
    plan <- inspect_grouping(dat, .grouping = spec, .format = "list")
    expected <- if (n > 31L)
        rep(NA_integer_, n + 1L)
    else as.integer(2^(seq_len(nrow(plan)) - 1L) - 1)
    stopifnot(identical(plan$grouping_id, expected))
    for (j in seq_len(nrow(plan))) stopifnot(identical(unname(plan$grouping_bits[[j]]), c(rep(0L,
        n - j + 1L), rep(1L, j - 1L))))
    if (n <= 31L) {
        res <- summarize_with_margins(dat, mask = grouping_id(), .grouping = spec, .margin_label = NULL,
            .id = "set")
        stopifnot(identical(res$mask, plan$grouping_id[res$set]))
    }
    else {
        err <- tryCatch(summarize_with_margins(dat, mask = grouping_id(), .grouping = spec,
            .margin_label = NULL), error = identity)
        stopifnot(inherits(err, "marginplyr_error"), grepl("at most 31", conditionMessage(err)))
    }
    checks <- checks + 1L
}

set.seed(271027)

for (i in seq_len(100)) {
    n <- sample(1:8, 1)
    cols <- if (n)
        paste0("x", seq_len(n))
    else character()
    dat <- tibble::as_tibble(stats::setNames(rep(list(1L), n), cols))
    subsets <- replicate(sample(1:20, 1), sample(cols, sample(0:n, 1)), simplify = FALSE)
    items <- lapply(subsets, function(x) rlang::inject(grouping_set(dplyr::all_of(!!x))))
    spec <- rlang::inject(grouping_sets(!!!items))
    plan <- inspect_grouping(dat, .grouping = spec, .duplicates = "keep", .format = "list")
    actual_cols <- unique(unlist(subsets, use.names = FALSE))
    expected <- vapply(subsets, function(x) as.integer(sum((!actual_cols %in% x) * 2^rev(seq_along(actual_cols) -
        1L))), integer(1))
    stopifnot(identical(plan$grouping_id, expected))
    res <- summarize_with_margins(dat, mask = grouping_id(), .grouping = spec, .duplicates = "keep",
        .margin_label = NULL, .id = "set")
    stopifnot(identical(res$mask, expected[res$set]))
    checks <- checks + 1L
}

cat("PASS: identity/inspection correspondence:", checks, "cases\n")
