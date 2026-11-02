Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
d <- tibble::tibble(a = 1L)
probe <- function(...) {
  env <- environment()
  cat("Dots populated:", ...length() > 0L, "\n")
  cat("Binding deferred:", marginplyr:::selection_binding_deferred(env, "..."), "\n")
  reader <- tryCatch(marginplyr:::is_name_only_helper_arg(quote(...), env), error = identity)
  cat("Helper-argument reader:\n")
  print(reader)
  plain <- tryCatch(inspect_grouping(d, .by = all_of(c(...)), .format = "list"), error = identity)
  wrapped <- inspect_grouping(d, .by = identity(all_of(c(...))), .format = "list")
  literal <- inspect_grouping(d, .by = all_of(character()), .format = "list")
  cat("Wrapped helper fixed keys:", wrapped$fixed[[1L]], "\n")
  cat("Explicit empty-vector fixed keys:", literal$fixed[[1L]], "\n")
  if (...length() == 0L) {
    stopifnot(inherits(reader, "error"), inherits(plain, "getvarError"),
      length(wrapped$fixed[[1L]]) == 0L, length(literal$fixed[[1L]]) == 0L)
  } else {
    stopifnot(!inherits(reader, "error"), !inherits(plain, "error"),
      identical(plain$fixed[[1L]], "a"), identical(wrapped$fixed[[1L]], "a"))
  }
}
probe()
probe("a")
cat("Three hypotheses tested: classifier ordinary binding read (confirmed);\n")
cat("tidy-eval capture loses the dots environment (refuted by same-quosure direct selection);\n")
cat("constructor retention is responsible (refuted by the .by-only minimal call).\n")
