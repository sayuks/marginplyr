# Run from the repository root; --assert requires the desired repairs.
pkgload::load_all(".", quiet = TRUE)

checks <- logical()

data <- tibble::tibble(g = list(1L, 2L), value = 1:2)

lazy <- dtplyr::lazy_dt(data)

control <- dplyr::collect(dplyr::union_all(lazy, dplyr::mutate(lazy, g = list(NULL))))

stopifnot(identical(control$g, list(1L, 2L, NULL, NULL)))

for (label in list(NULL, NA_character_)) {
    result <- tryCatch(dplyr::collect(expand_with_margins(lazy, .grouping = rollup(g),
        .margin_label = label)), error = identity)
    name <- if (is.null(label))
        "list_NULL"
    else "list_NA"
    checks[[name]] <- !inherits(result, "error") && identical(result, control)
    if (inherits(result, "error"))
        cat(name, ": ", conditionMessage(result), "\n")
}

stopifnot(identical(dplyr::collect(lazy), data))

stopifnot(nrow(dplyr::collect(expand_with_margins(lazy, .grouping = grouping_set(g),
    .margin_label = NULL))) == 2L)

stopifnot(nrow(dplyr::collect(expand_with_margins(lazy, .grouping = rollup(g), .margin_label = "All"))) ==
    4L)

for (empty in c(FALSE, TRUE)) {
    value <- tibble::tibble(g = array(1:8, c(2L, 2L, 2L)), value = 1:2)
    if (empty)
        value <- value[0, ]
    stopifnot(nrow(dplyr::summarise(value, n = dplyr::n(), .by = g)) == nrow(value))
    result <- tryCatch(expand_with_margins(value, .grouping = rollup(g)), error = identity)
    checks[[paste0("array_refusal_empty_", empty)]] <- inherits(result, "marginplyr_error")
    if (inherits(result, "error"))
        cat(conditionMessage(result), "\n")
    else cat("Empty array display label was accepted as", typeof(result$g), "\n")
    for (label in list(NULL, NA_character_)) {
        missing <- expand_with_margins(value, .grouping = rollup(g), .margin_label = label)
        stopifnot(identical(dim(missing$g), c(2L * nrow(value), 2L, 2L)))
    }
}

print(checks)

cat("Desired-contract failures:", sum(!checks), "\n")

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(all(checks))
