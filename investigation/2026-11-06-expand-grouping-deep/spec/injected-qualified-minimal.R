Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)

# A wrapper that uses the public embracing idiom. Qualified parentheses are
# ordinary calls to base's identity function within tidyselect evaluation.
make_spec <- function(column) {
  grouping_sets(base::`(`({{ column }}))
}
reference <- function(column, data) {
  tidyselect::eval_select(
    rlang::expr(base::`(`({{ column }})), data
  )
}

data <- data.frame(a = "x", b = 1L)
a <- "b"
expected <- names(reference(a, data))
actual <- inspect_grouping(
  data, .grouping = make_spec(a), .format = "list"
)$included[[1L]]
plain <- inspect_grouping(
  data, .grouping = grouping_sets(base::`(`(a)), .format = "list"
)$included[[1L]]
syntactic <- function(column) grouping_sets(({{ column }}))
bare <- inspect_grouping(
  data, .grouping = syntactic(a), .format = "list"
)$included[[1L]]
identity_wrapper <- function(column) grouping_sets(identity({{ column }}))
identity_control <- inspect_grouping(
  data, .grouping = identity_wrapper(a), .format = "list"
)$included[[1L]]
fixed <- function(column) {
  inspect_grouping(data, .by = base::`(`({{ column }}), .format = "list")
}
fixed_control <- fixed(a)$fixed[[1L]]
stopifnot(
  identical(expected, "b"), identical(plain, "b"),
  identical(bare, "a"), identical(identity_control, "b"),
  identical(fixed_control, "b")
)
cat("direct typed tidyselect:", expected, "\n")
cat("qualified selection without embracing:", plain, "\n")
cat("syntactic parentheses with embracing:", bare, "\n")
cat("identity wrapper with embracing:", identity_control, "\n")
cat("fixed-key qualified selection with embracing:", fixed_control, "\n")
cat("Grouping qualified selection with embracing:", actual, "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(actual, expected))
} else {
  stopifnot(identical(actual, "a"))
}
