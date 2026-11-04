pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
data <- data.frame(a = c("x", "y"), value = 1:2)
data[["..01"]] <- c("p", "p")

included <- function(spec) {
  inspect_grouping(data, .grouping = spec, .format = "list")$included
}

expected <- list("..01")
direct <- tidyselect::eval_select(quote(..01), data)
stopifnot(
  identical(names(direct), "..01"),
  identical(inspect_grouping(data, .by = (..01), .format = "list")$fixed,
            expected),
  identical(included(grouping_set(tidyselect::all_of("..01"))), expected),
  identical(included(grouping_set(c(..01))), expected)
)

# The real-dots counterpart continues to use the forwarded specification.
forwarded <- function(...) included(grouping_sets((..01)))
if (!"--before-fix" %in% args) {
  stopifnot(identical(forwarded(grouping_set(a)), list("a")))
}

observed <- tryCatch(included(grouping_set((..01))), error = identity)
if ("--assert" %in% args || "--before-fix" %in% args) {
  stopifnot(identical(observed, expected))
} else {
  stopifnot(inherits(observed, "rlang_error"))
  stopifnot(grepl("used in an incorrect context", conditionMessage(observed),
                 fixed = TRUE))
  cat(conditionMessage(observed), "\n")
}
