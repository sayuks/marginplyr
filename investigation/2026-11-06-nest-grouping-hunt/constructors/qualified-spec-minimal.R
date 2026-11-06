# Dated research reproduction. --assert requires the desired contract.
Sys.setenv(TESTTHAT_PARALLEL = "false")
source <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd())
pkgload::load_all(source, quiet = TRUE)
input <- data.frame(a = c("x", "y"), b = c(1L, 1L), value = 11:12)
reads <- 0L
caller <- rlang::env(s = grouping_set(a))
caller[["::"]] <- function(package, name) {
  stopifnot(identical(substitute(package), quote(base)))
  stopifnot(identical(substitute(name), quote(`(`)))
  reads <<- reads + 1L
  function(x) "b"
}
selector <- rlang::new_quosure(quote(base::`(`(s)), caller)
expected <- names(tidyselect::eval_select(selector, input[0, , drop = FALSE]))
stopifnot(identical(expected, "b"), identical(reads, 1L))
reads <- 0L
actual <- inspect_grouping(input, .grouping = grouping_sets(!!selector), .format = "list")$included
cat("direct typed selection:", expected, "\n")
cat("Grouping dimension:", actual[[1L]], "\n")
cat("caller namespace lookups:", reads, "\n")
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(identical(actual, list(expected)), identical(reads, 1L))
} else {
  stopifnot(identical(actual, list("a")), identical(reads, 0L))
}
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  result <- verb(input, .grouping = grouping_sets(!!selector))
  reference <- verb(input, .grouping = grouping_sets(grouping_set(b)))
  cat("actual nesting rows:", nrow(result), "; expected:", nrow(reference), "\n")
  stopifnot(identical(nrow(result), 2L), identical(nrow(reference), 1L))
}
# The genuine base-qualified identity wrapper retains the bound specification.
plain <- rlang::new_quosure(quote(base::`(`(s)), rlang::env(s = grouping_set(a)))
stopifnot(identical(inspect_grouping(input, .grouping = grouping_sets(!!plain), .format = "list")$included, list("a")))
# An unwrapped constructor-shaped callee runs the caller's actual function.
caller$grouping_set <- function(...) "b"
unwrapped <- rlang::new_quosure(quote(grouping_set(a)), caller)
stopifnot(identical(inspect_grouping(input, .grouping = grouping_sets(!!unwrapped), .format = "list")$included, list("b")))
