# Observation mode pins the dated failure; --assert requires the typed contract.
# Run from an archive of the named package SHA.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
main <- function() {
  contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
  input <- tibble::tibble(a = "east")
  seen <- character()
  stopifnot(!exists("length.stringr_pattern", globalenv(), inherits = FALSE))
  makeActiveBinding("hunt_dynamic_length", function() {
    kind <- typeof(tidyselect::peek_data()$a)
    seen <<- c(seen, kind)
    if (kind == "character") 1L else 2L
  }, globalenv())
  cleanup <- function() {
    if (exists("length.stringr_pattern", globalenv(), inherits = FALSE)) {
      rm("length.stringr_pattern", envir = globalenv())
    }
  }
  on.exit({
    cleanup()
    rm("hunt_dynamic_length", envir = globalenv())
  }, add = TRUE)
  selection <- quote(matches(stringr::regex("a"), vars = {
    "length.stringr_pattern" <<- as.vector(as.vector(
      list(x = NULL, as.vector("hunt_dynamic_length", "symbol")), "pairlist"
    ), "function")
    tidyselect::peek_vars()
  }))
  direct <- tidyselect::eval_select(selection, input)
  stopifnot(identical(names(direct), "a"))
  cat("Direct typed selection:", names(direct), "\n")
  cleanup()
  seen <- character()
  spec <- rlang::eval_tidy(rlang::expr(marginplyr::grouping_set(!!selection)))
  actual <- tryCatch(marginplyr::inspect_grouping(
    dtplyr::lazy_dt(input), .grouping = spec, .format = "list"
  ), error = identity)
  cat("Unwrapped outcome:", if (inherits(actual, "error")) {
    conditionMessage(actual)
  } else paste(actual$included[[1L]], collapse = ", "), "\n")
  cat("Unwrapped metadata:", paste(seen, collapse = ", "), "\n")
  unwrapped_types <- seen
  cleanup()
  seen <- character()
  typed_selection <- selection
  typed_selection$vars <- rlang::expr(base::identity(!!selection$vars))
  typed_spec <- rlang::eval_tidy(rlang::expr(
    marginplyr::grouping_set(!!typed_selection)
  ))
  control <- marginplyr::inspect_grouping(
    dtplyr::lazy_dt(input), .grouping = typed_spec, .format = "list"
  )
  stopifnot(identical(control$included, list("a")), all(seen == "character"))
  cat("Typed-wrapper selection:", control$included[[1L]], "\n")
  cat("Typed-wrapper metadata:", paste(seen, collapse = ", "), "\n")
  if (contract) {
    stopifnot(!inherits(actual, "error"), identical(actual, control))
  } else {
    stopifnot(inherits(actual, "error"), identical(unwrapped_types, "integer"),
      grepl("patterns must be length 1", conditionMessage(actual), fixed = TRUE))
  }
}
main()
for (pkg in c("dplyr", "tidyselect", "stringr", "vctrs", "rlang", "dtplyr", "pkgload")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
cat(R.version.string, "\n", R.version$platform, "\n")
