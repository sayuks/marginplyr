Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
data <- data.frame(a = "x", b = 1L)
pattern <- stringr::fixed("a")
reads <- character()
method <- function(x) {
  type <- typeof(tidyselect::peek_data()$a)
  reads <<- c(reads, type)
  if (type != "character") stop("pattern proxy requires character column metadata")
  unclass(x)
}
rlang::local_bindings(
  vec_proxy.stringr_fixed = method,
  .env = asNamespace("vctrs")$.__S3MethodsTable__.
)
direct <- tidyselect::eval_select(rlang::quo(matches(pattern)), data)
stopifnot(identical(names(direct), "a"))
reads <- character()
actual <- tryCatch(
  inspect_grouping(data, .grouping = grouping_set(matches(pattern))),
  error = identity
)
actual_reads <- reads
control <- inspect_grouping(data, .grouping = grouping_set(matches(base::identity(pattern))))
stopifnot(identical(control$included, "(a)"))
cat("direct selected:", names(direct), "\n")
cat("actual proxy metadata:", actual_reads, "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"), identical(actual, control))
} else {
  stopifnot(inherits(actual, "error"), identical(actual_reads, "integer"))
  cat(conditionMessage(actual), "\n")
  cat("typed wrapper selected:", control$included, "\n")
}
