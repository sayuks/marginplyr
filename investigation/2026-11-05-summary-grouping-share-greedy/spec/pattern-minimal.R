Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- tibble(a = c("x", "y"), b = c(1L, 2L))
pattern <- stringr::fixed("a")
length.stringr_fixed <- function(x) {
  if (is.character(tidyselect::peek_data()$a)) 1L else 2L
}
reference <- tidyselect::eval_select(rlang::quo(matches(pattern)), data)
stopifnot(identical(names(reference), "a"))
run <- function(selector, verb) {
  q <- rlang::new_quosure(selector, globalenv())
  spec <- rlang::inject(grouping_set(!!q))
  if (verb == "summary") {
    return(summarize_with_margins(data, count = n(), .grouping = spec))
  }
  do.call(getExportedValue("marginplyr", verb), list(.data = data, .grouping = spec))
}
for (verb in c("inspect_grouping", "summary", "expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
  actual <- tryCatch(run(quote(matches(pattern)), verb), error = identity)
  control <- run(quote(matches(base::identity(pattern))), verb)
  stopifnot(!inherits(control, "error"))
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(!inherits(actual, "error"))
  } else {
    stopifnot(inherits(actual, "error"), grepl("stringr patterns must be length 1", conditionMessage(actual), fixed = TRUE))
    cat(verb, ": false pattern-length refusal; typed wrapper control succeeded\n", sep = "")
  }
}
