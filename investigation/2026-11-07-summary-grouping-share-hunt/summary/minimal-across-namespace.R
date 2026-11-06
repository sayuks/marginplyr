args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else getwd()
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_root, quiet = TRUE)
data <- data.frame(x = c(1L, 3L))
hits <- 0L
`::` <- function(pkg, name) {
  package <- as.character(substitute(pkg))
  member <- as.character(substitute(name))
  if (identical(package, "base") && identical(member, "sum")) {
    hits <<- hits + 1L
    return(list(s = sum, m = max))
  }
  get(member, envir = asNamespace(package))
}
expected <- dplyr::summarise(
  data, dplyr::across(x, base::sum, .names = "answer_{.fn}")
)
stopifnot(identical(expected, data.frame(answer_s = 4L, answer_m = 3L)),
          identical(hits, 1L))
expected <- dplyr::mutate(expected, answer_1 = 1L, .before = 1L)
control <- summarize_with_margins(
  data, dplyr::across(x, list(s = sum, m = max), .names = "answer_{.fn}"),
  .id = "answer_1"
)
stopifnot(identical(control, expected))
hits <- 0L
actual <- tryCatch(summarize_with_margins(
  data, dplyr::across(x, base::sum, .names = "answer_{.fn}"),
  .id = "answer_1"
), error = identity)
print(expected)
if (inherits(actual, "error")) {
  cat(class(actual), "\n", conditionMessage(actual), "\n", sep = " ")
} else {
  print(actual)
}
cat("Namespace calls during Margin request:", hits, "\n")
if ("--assert" %in% args) {
  stopifnot(identical(actual, expected))
} else {
  stopifnot(inherits(actual, "marginplyr_error"),
            grepl("conflicts with a summary output", conditionMessage(actual), fixed = TRUE),
            identical(hits, 0L))
}
