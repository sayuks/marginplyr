pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

data <- data.frame(x = 1:3)
ordinary <- dplyr::summarise(
  data, value = (function(...) sum(pick(..01)))(x)
)
margin <- tryCatch(
  marginplyr::summarize_with_margins(
    data, value = (function(...) sum(pick(..01)))(x)
  ),
  error = identity
)
canonical <- marginplyr::summarize_with_margins(
  data, value = (function(...) sum(pick(..1)))(x)
)
stopifnot(identical(ordinary$value, 6L), identical(canonical$value, 6L))
cat("Ordinary value:", ordinary$value, "\n")
cat("Canonical Margin value:", canonical$value, "\n")
if (inherits(margin, "error")) {
  cat("Noncanonical Margin error:\n", conditionMessage(margin), "\n")
} else {
  cat("Noncanonical Margin value:", margin$value, "\n")
}
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(margin, "error"), identical(margin$value, ordinary$value))
} else {
  stopifnot(inherits(margin, "error"),
            grepl("..01", conditionMessage(margin), fixed = TRUE))
}
