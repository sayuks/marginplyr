# Run from the investigated source root.
pkgload::load_all(quiet = TRUE)

run <- function() {
  assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
  data <- data.frame(a = "x", b = 1L)
  input <- dtplyr::lazy_dt(data)
  reads <- character()
  predicate <- function(x) {
    reads <<- base::c(reads, typeof(x))
    is.character(x)
  }
  # The explicitly qualified parenthesis is an ordinary evaluation context,
  # so it uses this caller-defined c(), unlike a tidyselect walk of c().
  c <- function(...) predicate
  selector <- rlang::quo(base::`(`(c("a")))
  stopifnot(identical(names(tidyselect::eval_select(selector, data)), "a"))
  stopifnot(identical(reads, base::c("character", "integer")))
  reads <- character()
  inspect <- function(data, selector) {
    inspect_grouping(
      data, .grouping = grouping_set(!!selector), .format = "list"
    )$included
  }
  control <- inspect(input, rlang::quo(where(predicate)))
  stopifnot(identical(control, list("a")))
  stopifnot(identical(reads, base::c("character", "integer")))
  reads <- character()
  actual <- inspect(input, selector)
  cat("dtplyr included:", paste(actual[[1L]], collapse = ","),
      "predicate input types:", paste(reads, collapse = ","), "\n")
  stopifnot(identical(
    actual, if (assert_contract) list("a") else list(character())
  ))
  stopifnot(identical(
    reads, if (assert_contract) base::c("character", "integer") else rep("integer", 4L)
  ))

  reads <- character()
  local <- tryCatch(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selector, grouping_set()), .format = "list"
    )$included,
    error = identity
  )
  cat("local union:", if (inherits(local, "error")) conditionMessage(local) else paste(lengths(local), collapse = ","), "\n")
  if (assert_contract) {
    stopifnot(identical(local, list("a", character())))
  } else {
    stopifnot(inherits(local, "marginplyr_error"))
    stopifnot(grepl("Duplicate grouping sets", conditionMessage(local), fixed = TRUE))
    stopifnot(identical(reads, rep("integer", 2L)))
  }
  stopifnot(identical(
    inspect_grouping(
      data, .grouping = grouping_sets(where(predicate), grouping_set()), .format = "list"
    )$included,
    list("a", character())
  ))
}
run()
