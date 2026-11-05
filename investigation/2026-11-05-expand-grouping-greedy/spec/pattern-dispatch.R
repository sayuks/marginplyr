# Run in a fresh R process from the investigated package root. The process
# owns the S3 method and observer bindings; they disappear on process exit.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
input <- data.frame(a = "a", b = 1L)
pattern <- stringr::regex("^a")
method_reads <- logical()
length.stringr_regex <- function(x) {
  typed <- is.character(tidyselect::peek_data()$a)
  method_reads <<- c(method_reads, typed)
  if (typed) 1L else 2L
}
direct <- tidyselect::eval_select(rlang::quo(matches(pattern)), input)
stopifnot(identical(names(direct), "a"), all(method_reads))
print(direct)
check <- function(expr) {
  method_reads <<- logical()
  result <- tryCatch(eval(expr), error = identity)
  list(result = if (inherits(result,"error")) conditionMessage(result) else result,
       reads = method_reads, error = inherits(result,"error"))
}
controls <- list(
  inspect = check(quote(inspect_grouping(input, .grouping=grouping_set(matches(identity(pattern)))))),
  expand = check(quote(expand_with_margins(input, .grouping=grouping_set(matches(identity(pattern)))))))
stopifnot(!controls$inspect$error,!controls$expand$error,
          all(controls$inspect$reads),all(controls$expand$reads))
print(controls)
actual <- list(
  inspect = check(quote(inspect_grouping(input, .grouping=grouping_set(matches(pattern))))),
  expand = check(quote(expand_with_margins(input, .grouping=grouping_set(matches(pattern))))),
  fixed = check(quote(inspect_grouping(input, .by=matches(pattern)))))
print(actual)
if ("--assert" %in% commandArgs(TRUE)) stopifnot(!any(vapply(actual, `[[`, logical(1), "error")))
