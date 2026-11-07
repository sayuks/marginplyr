Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
mode <- commandArgs(trailingOnly = TRUE)
if (length(mode) == 0L) mode <- "observe"
stopifnot(mode %in% c("observe", "contract", "controls"))

fixture <- tibble::tibble(g = c("a", "b"), x = 1:2)
inputs <- list(as.data.frame(fixture), fixture, data.table::as.data.table(fixture))
for (input in inputs) {
  if (mode != "controls") {
    condition <- tryCatch(
      summarize_with_margins(
        input,
        if (grouping_bit(g) == 0L) {
          tibble::tibble(detail = sum(x))
        } else {
          tibble::tibble(total = sum(x))
        },
        .grouping = rollup(g), .id = "set", .sort = "last"
      ),
      error = identity
    )
    stopifnot(inherits(condition, "error"))
    if (mode == "observe") {
      stopifnot(inherits(condition, "simpleError"), is.null(conditionCall(condition)))
      stopifnot(grepl("Only in branch 2: total", conditionMessage(condition), fixed = TRUE))
      stopifnot(grepl("Only in branch 1: detail", conditionMessage(condition), fixed = TRUE))
      cat(conditionMessage(condition), "\n")
    } else {
      stopifnot(inherits(condition, "marginplyr_error"))
      stopifnot(!is.null(conditionCall(condition)))
    }
  }

  common <- summarize_with_margins(
    input,
    if (grouping_bit(g) == 0L) {
      tibble::tibble(value = sum(x))
    } else {
      tibble::tibble(value = sum(x))
    },
    .grouping = rollup(g), .id = "set", .sort = "last"
  )
  packed <- summarize_with_margins(
    input,
    measures = if (grouping_bit(g) == 0L) {
      tibble::tibble(detail = sum(x))
    } else {
      tibble::tibble(total = sum(x))
    },
    .grouping = rollup(g), .id = "set", .sort = "last"
  )
  stopifnot(identical(common$value, c(1L, 2L, 3L)))
  stopifnot(identical(packed$measures$detail, c(1L, 2L, NA_integer_)))
  stopifnot(identical(packed$measures$total, c(NA_integer_, NA_integer_, 3L)))
}
cat("Three eager classes, six passing controls.\n")
