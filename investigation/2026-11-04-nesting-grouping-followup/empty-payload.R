pkgload::load_all(quiet = TRUE)

input <- stats::setNames(tibble::tibble(v = integer()), "..0")
native <- dplyr::nest_by(input)
stopifnot(nrow(native) == 1L, identical(native$data[[1L]], input))
stopifnot(nrow(nest_with_margins(input)) == 0L)
regular <- nest_by_with_margins(tibble::tibble(v = integer()))
stopifnot(nrow(regular) == 1L, identical(regular$data[[1L]]$v, integer()))
populated <- nest_by_with_margins(stats::setNames(tibble::tibble(v = 1L), "..0"))
stopifnot(identical(populated$data[[1L]][["..0"]], 1L))

result <- tryCatch(nest_by_with_margins(input), error = identity)
if (inherits(result, "error")) cat(conditionMessage(result), "\n")
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(!inherits(result, "error"), inherits(result, "rowwise_df"),
    nrow(result) == 1L, identical(result$data[[1L]], input))
} else {
  stopifnot(inherits(result, "error"),
    grepl("indexing '...' with non-positive index 0", conditionMessage(result), fixed = TRUE))
}
