# Investigated 2026-11-03, source SHA 4b34fdecb2d8a41034b3d9b5d5af8e51592555e1.
options(lifecycle_verbosity = "quiet", width = 100)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
data <- data.frame(x = 1:3, y = 5:7)
source <- dtplyr::lazy_dt(data)
template <- "{.col}_{seq_along(.col)}"
ordinary <- dplyr::collect(dplyr::summarise(
  source, dplyr::across(c(x, y), sum, .names = template)
))
margin <- dplyr::collect(marginplyr::summarize_with_margins(
  source, dplyr::across(c(x, y), sum, .names = template)
))
local <- marginplyr::summarize_with_margins(
  data, dplyr::across(c(x, y), sum, .names = template)
)
share <- tryCatch(marginplyr::summarize_with_margins(
  data, dplyr::across(c(x, y), sum, .names = template),
  fraction = marginplyr::share_of_total(y_2)
), error = identity)
cat("ordinary dtplyr:\n"); print(ordinary)
cat("Margin dtplyr:\n"); print(margin)
cat("no-share local control:\n"); print(local)
cat("local share source:\n")
if (inherits(share, "error")) cat(conditionMessage(share), "\n") else print(share)
stopifnot(identical(names(ordinary), c("x_1", "y_2")))
stopifnot(identical(names(local), names(ordinary)))
stopifnot(identical(unname(unlist(margin)), unname(unlist(ordinary))))
# A vectorized glue mask reproduces dplyr's names; scalar-pair expansion does not.
whole <- as.character(glue::glue_data(list(.col = c("x", "y"), .fn = c("1", "1")), template))
expand_name <- get("expand_across_name", envir = asNamespace("marginplyr"))
pairs <- vapply(c("x", "y"), function(column) {
  expand_name(template, column, "1", environment())
}, character(1))
cat("whole glue context:", whole, "\nscalar-pair context:", pairs, "\n")
stopifnot(identical(whole, names(ordinary)), identical(unname(pairs), names(margin)))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(names(margin), names(ordinary)))
  stopifnot(!inherits(share, "error"), identical(share$fraction, 1))
} else {
  stopifnot(identical(names(margin), c("x_1", "y_1")))
  stopifnot(inherits(share, "marginplyr_error"))
  stopifnot(grepl("unknown preceding ordinary summary `y_2`", conditionMessage(share), fixed = TRUE))
}
