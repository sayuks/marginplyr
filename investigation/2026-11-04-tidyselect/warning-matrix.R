# Run from the root of the archived package revision.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(tidyselect))
old <- options(lifecycle_verbosity = "warning")
data <- data.frame(a = 1L, b = "x", value = 1)
warn <- function(x) {
  warnings <- character()
  result <- withCallingHandlers(tryCatch(x, error = identity), warning = function(cnd) {
    warnings <<- c(warnings, paste(class(cnd)[1L], conditionMessage(cnd)))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warnings)
}
helpers <- c(
  'one_of(c("a","absent"))', 'one_of("absent")', 'starts_with("a", TRUE)',
  'all_of(c("a","absent"))', 'any_of(c("a","absent"))'
)
forms <- c(
  '%1$s', '!(%1$s)', '-(%1$s)', 'c(everything(), -(%1$s))', 'c(%1$s, %1$s)',
  'c(%1$s, -(%1$s))', 'c(-(%1$s), !(%1$s))', 'c(%1$s, where(is.numeric))',
  'c(where(is.numeric), %1$s)', 'where(is.numeric) & !(%1$s)'
)
for (helper in helpers) {
  for (form in forms) {
    case <- sprintf(form, helper)
    q <- rlang::new_quosure(str2lang(case), environment())
    expected <- warn(eval_select(q, data))
    fixed <- warn(inspect_grouping(data, .by = !!q))
    included <- warn(inspect_grouping(data, .grouping = grouping_set(!!q)))
    if (!identical(expected$warnings, fixed$warnings) ||
        !identical(expected$warnings, included$warnings)) {
      print(list(case = case, tidyselect = expected$warnings,
                 fixed = fixed$warnings, included = included$warnings))
    }
  }
}
options(old)
