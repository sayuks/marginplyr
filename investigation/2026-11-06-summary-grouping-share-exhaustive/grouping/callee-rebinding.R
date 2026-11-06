# Run from a source archive of the tested revision. --assert requires the
# intended typed-selection contract rather than the dated failure.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L), value = c(2, 3))
seen <- character()
makeActiveBinding("hunt_pattern_source", function() {
  kind <- typeof(tidyselect::peek_data()$a)
  seen <<- c(seen, kind)
  if (kind == "character") "^a$" else "^b$"
}, globalenv())
caller <- rlang::env(globalenv(), paste0 = "ordinary nonfunction binding")
pattern <- eval(quote(function() {
  paste0 <- as.vector(as.vector("hunt_pattern_source", "symbol"), "function")
  paste0()
}), caller)
selection <- rlang::expr(matches(pattern()))
direct <- tidyselect::eval_select(selection, input)
stopifnot(identical(names(direct), "a"))
cat("Direct typed selection:", names(direct), "\n")
seen <- character()
actual <- tryCatch(marginplyr::summarize_with_margins(
  input, n = dplyr::n(), .by = b,
  .grouping = marginplyr::grouping_set(matches(pattern()))
), error = identity)
cat("Summary outcome:", if (inherits(actual, "error")) conditionMessage(actual) else "success", "\n")
cat("Summary callback metadata:", paste(seen, collapse = ", "), "\n")
seen <- character()
control <- marginplyr::summarize_with_margins(
  input, n = dplyr::n(), .by = b,
  .grouping = marginplyr::grouping_set(matches(base::identity(pattern())))
)
stopifnot(identical(control$a, c("east", "west")), identical(control$n, c(1L, 1L)))
cat("Typed-wrapper control:", paste(control$a, collapse = ", "), "\n")
cat("Control callback metadata:", paste(seen, collapse = ", "), "\n")
if (contract) {
  stopifnot(!inherits(actual, "error"), identical(actual, control))
} else {
  stopifnot(inherits(actual, "marginplyr_error"), grepl("Columns cannot appear", conditionMessage(actual), fixed = TRUE))
}
cat("Versions:\n")
for (pkg in c("dplyr", "tidyselect", "rlang", "vctrs", "pkgload")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
cat(R.version.string, "\n", R.version$platform, "\n")
