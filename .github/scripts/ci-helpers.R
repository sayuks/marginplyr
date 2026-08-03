# Shared by the check helper scripts in this directory, which all read the same
# two environment variables and write to the same job summary. Sourced by path
# because every workflow step runs from the repository root.
#
# These scripts are excluded from the built package by `.Rbuildignore`, so
# `lintr::lint_package()` does not see them, but the repository's style still
# applies. Lint them by calling `lintr::lint_dir()` on ".github", passing
# `lintr::linters_with_defaults()` with `object_usage_linter` set to NULL.
#
# `object_usage_linter()` cannot follow `source()`, so it reports every helper
# defined here as an undefined global at each call site. Disabling that one
# linter is the accurate fix: there is no namespace for it to resolve against,
# unlike the package itself, where `pkgload::load_all()` supplies one.

# Where `rcmdcheck` was told to put its output.
check_directory <- function() {
  Sys.getenv("MARGINPLYR_CHECK_DIR", "check")
}

# Names the job in the summary, so a run page with several checks stays
# readable.
check_label <- function() {
  Sys.getenv("MARGINPLYR_CHECK_LABEL", "R CMD check")
}

# The `<package>.Rcheck` directory the check produced. Found by globbing rather
# than by assuming the package name, so a rename cannot leave a script silently
# looking at nothing.
rcheck_directory <- function(required = TRUE) {
  found <- Sys.glob(file.path(check_directory(), "*.Rcheck"))
  if (length(found) == 1L) {
    return(found)
  }
  if (!required) {
    return(NA_character_)
  }
  stop(sprintf(
    "Expected exactly one *.Rcheck directory under '%s', found %d.",
    check_directory(),
    length(found)
  ))
}

# R CMD check keeps a test's output in `<test>.Rout`, but renames it to
# `<test>.Rout.fail` when the test exits non-zero. Reading only the first name
# would report a failing suite as a suite that never ran.
test_output_path <- function(rcheck, test = "testthat") {
  names <- paste0(test, c(".Rout", ".Rout.fail"))
  candidates <- file.path(rcheck, "tests", names)
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) NA_character_ else found[1]
}

write_step_summary <- function(lines) {
  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
  if (nzchar(summary_path)) {
    write(lines, file = summary_path, append = TRUE)
  }
  cat(lines, sep = "\n")
  cat("\n")
  invisible(lines)
}

# Wraps text as a fenced block for the job summary.
as_summary_block <- function(text) {
  c("```", unlist(strsplit(text, "\n", fixed = TRUE)), "```", "")
}
