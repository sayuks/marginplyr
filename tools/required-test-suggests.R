# Returns the test suite's registered optional Suggests after checking their
# declared versions with the same guard the tests use.
required_test_suggests <- function(root = ".") {
  suite <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests/testthat/helper-optional-backends.R"),
    envir = suite)
  guard <- new.env(parent = globalenv())
  sys.source(file.path(root, "inst/suggests/guard.R"), envir = guard)
  packages <- names(suite$optional_suggest_spec())
  if (length(packages) == 0L) {
    stop("No optional Suggests are registered for the test suite.", call. = FALSE)
  }
  suggests <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Suggests")[1L, 1L]
  status <- lapply(packages, guard$marginplyr_suggest_status,
    suggests = suggests)
  missing <- vapply(status, function(item) !isTRUE(item$available), logical(1))
  if (any(missing)) {
    stop("The test suite requires usable optional Suggests: ",
      paste(vapply(status[missing], `[[`, character(1), "reason"),
        collapse = "; "), call. = FALSE)
  }
  packages
}
