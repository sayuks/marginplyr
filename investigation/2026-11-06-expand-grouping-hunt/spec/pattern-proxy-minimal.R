# Run in a fresh process from the package root. The local method disappears
# with this process; it inspects column types and no source values.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
input <- data.frame(a = "x")
pattern <- stringr::fixed("a")
reads <- character()
vec_proxy.stringr_fixed <- function(x, ...) {
  type <- typeof(tidyselect::peek_data()$a)
  reads <<- c(reads, type)
  if (type != "character") stop("needs character selection metadata")
  unclass(x)
}
direct <- tidyselect::eval_select(rlang::quo(matches(pattern)), input)
stopifnot(identical(names(direct), "a"), identical(reads, "character"))
print(direct)
reads <- character()
actual <- tryCatch(
  inspect_grouping(input, .grouping = grouping_set(matches(pattern))),
  error = identity
)
print(if (inherits(actual, "error")) conditionMessage(actual) else actual)
print(reads)
stopifnot(inherits(actual, "error"), identical(reads, "integer"))
reads <- character()
control <- inspect_grouping(
  input, .grouping = grouping_set(matches(identity(pattern))), .format = "list"
)
stopifnot(identical(control$included[[1L]], "a"),
          identical(reads, "character"))
print(control)
if ("--assert" %in% commandArgs(TRUE)) stopifnot(!inherits(actual, "error"))
