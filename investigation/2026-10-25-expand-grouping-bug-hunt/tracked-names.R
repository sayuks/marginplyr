pkgload::load_all(".", quiet = TRUE)

data <- tibble::tibble(g = "a", v = 1L)

for (id in c("...", "..1")) {
    control <- dplyr::mutate(data, !!!stats::setNames(list(1L), id))
    stopifnot(id %in% names(control))
    actual <- tryCatch(expand_with_margins(data, .grouping = rollup(g), .id = id),
        error = identity)
    stopifnot(inherits(actual, "error"))
    cat("Tracked #827 id", id, ":", conditionMessage(actual), "\n")
}

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

source <- dplyr::copy_to(con, data, "facts")

for (id in c("TRUE", "FALSE")) {
    control <- dplyr::collect(dplyr::mutate(source, !!!stats::setNames(list(1L),
        id)))
    stopifnot(id %in% names(control))
    actual <- tryCatch(dplyr::collect(expand_with_margins(source, .grouping = rollup(g),
        .id = id, .margin_label = NULL)), error = identity)
    stopifnot(inherits(actual, "error"))
    cat("Tracked #826 id", id, ":", conditionMessage(actual), "\n")
}

DBI::dbDisconnect(con)
