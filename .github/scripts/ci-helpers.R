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

# `optional_suggests()` and `optional_backends()`, the one list of optional
# backends the release matrix reasons about. It is defined with the guards that
# consume it rather than here, because `.Rbuildignore` excludes `^\.github$`
# and does not exclude `tests/`: the tarball ships the helper, so this file can
# read it and a list kept here could not be read back from the tests. Every job
# that runs these scripts checks out the repository, so the path resolves.
#
# `source()` evaluates top-level expressions only, and the helper's sole
# testthat call sits inside `skip_if_backend_absent()`'s body, so this works
# from a bare `Rscript` with testthat unattached. It also brings in
# `backend_available()` and `required_suggests()`; nothing here calls them, and
# separating the list into its own file would trade that for a file whose only
# purpose is the separation.
source("tests/testthat/helper-optional-backends.R")

# Reads a delimited list out of the environment, the form the workflow's matrix
# entries are written in. Package lists are comma-separated; test names are
# separated by `;`, because a test name may contain a comma.
env_list <- function(name, sep = ",") {
  declared <- trimws(strsplit(Sys.getenv(name, ""), sep, fixed = TRUE)[[1]])
  declared[nzchar(declared)]
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
