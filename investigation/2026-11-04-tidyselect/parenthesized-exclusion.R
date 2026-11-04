# Run from the root of the archived package revision.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
suppressPackageStartupMessages(library(dplyr))

data <- tibble(x = 1:2, y = -3:-4, z = 100:101)
selectors <- rlang::exprs(
  parens = -c(tidyselect::any_of("missing"), (-tidyselect::starts_with("x"))),
  bare = -c(tidyselect::any_of("missing"), -tidyselect::starts_with("x"))
)
consumers <- list(
  across = function(s) rlang::exprs(dplyr::across(!!s, sum)),
  pick = function(s) rlang::exprs(value = sum(dplyr::pick(!!s))),
  if_any = function(s) rlang::exprs(value = sum(dplyr::if_any(!!s, ~ .x > 10))),
  if_all = function(s) rlang::exprs(value = sum(dplyr::if_all(!!s, ~ .x > 0)))
)
divergences <- 0L
for (form in names(selectors)) {
  selector <- selectors[[form]]
  want <- names(tidyselect::eval_select(selector, data))
  stopifnot(identical(want, if (form == "parens") "x" else names(data)))
  stopifnot(
    identical(inspect_grouping(data, .by = !!selector,
                               .format = "list")$fixed[[1L]], want),
    identical(inspect_grouping(data, .grouping = grouping_set(!!selector),
                               .format = "list")$included[[1L]], want)
  )
  for (consumer in names(consumers)) {
    expressions <- consumers[[consumer]](selector)
    for (prefix in list(list(), rlang::exprs(n = dplyr::n()))) {
      native <- summarise(data, !!!prefix, !!!expressions)
      margin <- summarize_with_margins(data, !!!prefix, !!!expressions)
      if (!identical(native, margin)) {
        divergences <- divergences + 1L
        print(list(form = form, consumer = consumer,
                   preceding = length(prefix), native = native, margin = margin))
      }
      if (form == "bare") stopifnot(identical(native, margin))
    }
  }
}
cat("Divergences:", divergences, "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(divergences == 0L)
}
