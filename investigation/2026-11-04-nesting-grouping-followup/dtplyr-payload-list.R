pkgload::load_all(quiet = TRUE)

reproduce <- function() {
  input <- tibble::tibble(g = "a", v = 1L)
  lazy <- dtplyr::lazy_dt(input)
  pending <- nest_with_margins(lazy, .grouping = grouping_set(g), .keep = FALSE)
  cell <- function(result) result$data[[1L]]$v
  stopifnot(identical(cell(dplyr::collect(pending)), 1L))

  list <- function(...) {
    args <- base::list(...)
    if (identical(names(args), "v")) args$v <- rep(999L, length(args$v))
    args
  }
  native <- dplyr::summarize(
    lazy, data = list(data.table::as.data.table(base::list(v = v))), .by = g
  )
  stopifnot(identical(cell(dplyr::collect(native)), 1L),
    identical(cell(dplyr::collect(tidyr::nest(lazy, data = v))), 1L),
    identical(dplyr::collect(dplyr::summarize(lazy, rows = dplyr::n(), .by = g))$rows, 1L),
    identical(dplyr::collect(lazy), input),
    identical(cell(nest_with_margins(input, .grouping = grouping_set(g))), 1L))
  observed <- c(
    lazy_collection = cell(dplyr::collect(pending)),
    rowwise = cell(nest_by_with_margins(lazy, .grouping = grouping_set(g)))
  )
  rm(list)
  stopifnot(identical(cell(dplyr::collect(pending)), 1L))
  print(observed)
  expected <- if ("--assert" %in% commandArgs(TRUE)) 1L else 999L
  stopifnot(identical(unname(observed), rep(expected, 2L)))
}
reproduce()
