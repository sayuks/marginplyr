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

# testthat's final result tally, for example
# "[ FAIL 0 | WARN 0 | SKIP 68 | PASS 1083 ]", as a named integer vector.
#
# Shared because `verify-depends-only.R` and `verify-backend.R` both read it out
# of a check's testthat log and both draw the same two conclusions from it --
# the suite completed, and it passed something. Two copies of this pattern would
# be two places to update when testthat changes the line, and the failure of the
# one left behind is silence: a tally that stops matching reports a suite that
# never ran, which is indistinguishable from the real thing this guards against.
#
# `useBytes` because the surrounding report is box-drawn UTF-8, and a runner
# without a UTF-8 locale would otherwise turn a parse into an encoding error.
# `NULL` when there is no tally at all, which each caller words for itself.
test_tally <- function(test_log) {
  tally <- grep("\\[ FAIL [0-9]+ \\|", test_log, value = TRUE, useBytes = TRUE)
  if (length(tally) == 0L) {
    return(NULL)
  }
  tally <- tally[length(tally)]
  counts <- as.integer(regmatches(tally, gregexpr("[0-9]+", tally))[[1]])
  names(counts) <- c("fail", "warn", "skip", "pass")
  list(line = tally, counts = counts)
}

# `optional_suggests()` and `optional_backends()`, the one list of optional
# packages the release matrix reasons about. It is defined with the guards that
# consume it rather than here; the helper's own comment records why that
# direction is forced, and why those two names describe different sets. This
# reads the working tree rather than the built tarball, and every job that runs
# these scripts checks out the repository, so the path resolves.
#
# `source()` evaluates top-level expressions only, and the helper's sole
# testthat call sits inside `skip_if_suggest_absent()`'s body, so this works
# from a bare `Rscript` with testthat unattached. Its reading of DESCRIPTION and
# of `inst/suggests/guard.R` is inside function bodies for the same reason, one
# step further on: `generate-backend-matrix.R` runs before marginplyr is
# installed and asks only for the list, so resolving either at source time would
# fail a script that has no version question to ask. `verify-suite-coverage.R`
# does ask one, and runs from the repository root where both are present.
#
# The helper also brings in `suggest_available()`, `suggest_status()`, and
# `required_suggests()`; separating the list into its own file would trade that
# for a file whose only purpose is the separation.
source("tests/testthat/helper-optional-backends.R")

# Reads a comma-separated list out of the environment, which is how a generated
# matrix entry reaches the job that acts on it. It splits the same way
# `required_suggests()` in `tests/testthat/helper-optional-backends.R` does, so
# a `required` value means one package set to the CI scripts and to the tests
# that run under them.
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
