suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
source <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), .BY = c(1, 3)))
identity_value <- function(value) value
capture <- function(value) tryCatch(force(value), error = identity)
for (aggregate in c("sum", "mean", "min", "max", "median", "sd", "var")) {
  expr <- rlang::call2(aggregate, quote(.BY))
  ordinary <- rlang::inject(dplyr::summarise(source, value = !!expr, .by = g))
  value <- dplyr::collect(ordinary)
  stopifnot(identical(names(value), c("g", "value")), nrow(value) == 2L)
  wrapped <- rlang::inject(dplyr::summarise(
    source, value = identity_value(!!expr), .by = g
  ))
  failure <- capture(dplyr::collect(wrapped))
  stopifnot(inherits(failure, "error"),
            grepl("locked binding for '.BY'", conditionMessage(failure),
                  fixed = TRUE))
  cat(aggregate, "ordinary: PASS; wrapped:", conditionMessage(failure), "\n")
}
