args <- commandArgs(trailingOnly = TRUE)
copy <- args[!grepl("^--", args)]
pkgload::load_all(if (length(copy)) copy[[1L]] else ".", quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
run <- function() {
  old <- options(warn = 1L)
  on.exit(options(old))
  d <- tibble::tibble(g = 1L, payload = "v")
  lazy <- dplyr::mutate(dtplyr::lazy_dt(d), payload = "new")
  caller_error <- structure(list(message = "caller stopped a selection warning", call = NULL,
    marker = "independent-20261104"), class = c("caller_hunt_error", "error", "condition"))
  capture <- function(expr) tryCatch(withCallingHandlers(force(expr),
    warning = function(w) stop(caller_error)), error = identity)
  native <- capture(tidyselect::eval_select(rlang::expr(one_of("absent")), d))
  stopifnot(identical(native, caller_error))
  answers <- list()
  for (name in c("inspect_grouping", "nest_with_margins", "nest_by_with_margins")) {
    verb <- get(name)
    local <- capture(verb(d, .grouping = grouping_set(one_of("absent"), where(is.numeric))))
    fixed <- capture(verb(lazy, .by = one_of("absent")))
    variable <- capture(verb(lazy, .grouping = grouping_set(one_of("absent"), where(is.numeric))))
    stopifnot(identical(local, caller_error), identical(fixed, caller_error))
    cat(name, "variable condition:", class(variable)[[1L]],
      "; identical to caller condition:", identical(variable, caller_error), "\n")
    answers[[name]] <- variable
  }
  cat("Native selection, local Grouping specification and derived fixed-key controls preserved the caller condition.\n")
  if ("--assert" %in% args) {
    stopifnot("Grouping specification must preserve the caller-promoted error before metadata refusal." =
      all(vapply(answers, identical, logical(1), caller_error)))
  } else {
    stopifnot(all(vapply(answers, inherits, logical(1), "marginplyr_error")))
  }
}
run()
