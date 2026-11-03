# Investigated 2026-11-03, source SHA 4b34fdecb2d8a41034b3d9b5d5af8e51592555e1.
options(lifecycle_verbosity = "quiet", width = 100)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
data <- data.frame(x = 1:3)
ordinary <- dplyr::summarise(
  data, value = (function(...) sum(dplyr::pick(..1)))(x)
)
margin <- tryCatch(marginplyr::summarize_with_margins(
  data, value = (function(...) sum(dplyr::pick(..1)))(x)
), error = identity)
control_dots <- marginplyr::summarize_with_margins(
  data, value = (function(...) sum(dplyr::pick(...)))(x)
)
control_across <- marginplyr::summarize_with_margins(
  data, value = (function(...) sum(dplyr::across(..1, sum)))(x)
)
stopifnot(identical(ordinary$value, 6L))
stopifnot(identical(control_dots, ordinary), identical(control_across, ordinary))
cat("ordinary result:\n"); print(ordinary)
cat("all-dots pick control:\n"); print(control_dots)
cat("across first-dots control:\n"); print(control_across)
cat("investigated Margin result:\n")
if (inherits(margin, "error")) cat(conditionMessage(margin), "\n") else print(margin)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(margin, "error"), identical(margin, ordinary))
} else {
  stopifnot(inherits(margin, "error"), grepl("Column `..1` doesn't exist", conditionMessage(margin), fixed = TRUE))
}
