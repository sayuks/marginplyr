pkgload::load_all(quiet = TRUE)
visits <- integer()
predicate <- function(x) {
  visits <<- c(visits, length(x))
  if (length(visits) > 1L) stop("source predicate evaluated twice")
  rep(FALSE, length(x))
}
input <- dtplyr::lazy_dt(tibble::tibble(value = 1:2))
source <- dplyr::filter(input, predicate(value))

stopifnot(nrow(dplyr::collect(source)) == 0L, identical(visits, 2L))
visits <- integer()
lazy <- nest_with_margins(source)
stopifnot(length(visits) == 0L, nrow(dplyr::collect(lazy)) == 0L,
          identical(visits, 2L))
visits <- integer()
actual <- tryCatch(nest_by_with_margins(source), error = identity)
if (inherits(actual, "error")) cat(conditionMessage(actual), "\n")
cat("Source predicate input sizes:", paste(visits, collapse = ", "), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"), inherits(actual, "rowwise_df"),
            nrow(actual) == 1L, identical(actual$data[[1L]]$value, integer()),
            identical(visits, 2L))
}
