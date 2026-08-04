# Every backend behind these helpers is a Suggested package, so a check run
# without it must skip rather than fail. That is right for CRAN's minimal
# flavors, but it makes a skip indistinguishable from a proof: a release job
# whose whole purpose is to execute DuckDB would pass green with fourteen
# silently skipped tests if duckdb failed to install.
#
# `MARGINPLYR_REQUIRED_SUGGESTS` is how a job states which backends it exists
# to prove. A package named there must be installed, and its absence fails the
# test instead of skipping it. Generic jobs leave the variable unset and keep
# skipping. `.github/workflows/release-matrix.yaml` sets it per dedicated job.
required_suggests <- function() {
  declared <- strsplit(
    Sys.getenv("MARGINPLYR_REQUIRED_SUGGESTS", ""),
    ",",
    fixed = TRUE
  )[[1]]
  declared <- trimws(declared)
  declared[nzchar(declared)]
}

# The optional Suggests these helpers guard on, and the only list the release
# matrix reads. Which side of the repository it lives on is forced, not
# preferred: `R CMD check` runs these tests from the unpacked tarball, and
# `.Rbuildignore` excludes `^\.github$` from that tarball, so a list kept in
# `.github/scripts/` would not exist where the tests execute. The CI scripts run
# from the checkout instead and can read this file, which makes `tests/` the
# only placement both sides reach. Adding a backend starts here, because
# `backend_available()` below refuses a name this list does not carry.
#
# The value answers whether the release matrix can assert the package absent by
# name. `DBI` cannot. `dbplyr` is an Import and declares `Imports: DBI`, so DBI
# is inside the hard dependency closure and is installed in every job,
# `_R_CHECK_DEPENDS_ONLY_=true` included. `verify-depends-only.R` would then
# require a `{DBI} is not installed` line that no run can produce, and
# `verify-library-isolation.R` would find DBI on `.libPaths()` in every job that
# withheld it.
optional_suggests <- function() {
  c(
    arrow = TRUE,
    duckdb = TRUE,
    dtplyr = TRUE,
    RSQLite = TRUE,
    DBI = FALSE
  )
}

# The subset a job can be asked to withhold, which is what the release matrix's
# absence assertions iterate over. Keeps the name it had while it lived in
# `ci-helpers.R`, so `verify-depends-only.R` and `verify-library-isolation.R`
# call it unchanged.
optional_backends <- function() {
  asserted <- optional_suggests()
  names(asserted)[asserted]
}

# Reports whether an optional backend can be used, and refuses to report FALSE
# for a backend the running job promised to exercise. Callers that select among
# several backends use this directly, because dropping one from a list records
# no skip at all and would otherwise be invisible.
#
# A package `known` does not name is an error rather than FALSE. Nothing in the
# release matrix executes such a package and nothing asserts it absent, so a
# guard on it would do nothing while reading as protection; erroring is what
# makes this test suite the place a backend gets registered. The check runs
# before `requireNamespace()` so that it fires the same way on a fully
# provisioned developer machine, where the package is installed and a check
# placed after it would never be reached.
#
# `known` is a parameter only so that this helper's own tests can drive both
# outcomes. They need a sentinel name no CRAN package uses and a base package
# that is always installed, and neither belongs in `optional_suggests()`. Every
# other call site takes the default.
backend_available <- function(package, known = optional_suggests()) {
  if (!package %in% names(known)) {
    stop(sprintf(
      paste0(
        "{%s} is not named in `optional_suggests()`, so no release-matrix job ",
        "executes it and no job asserts it is absent. Add it there, and add a ",
        "`backend` entry in `release-matrix.yaml`."
      ),
      package
    ))
  }
  if (requireNamespace(package, quietly = TRUE)) {
    return(TRUE)
  }
  if (package %in% required_suggests()) {
    stop(sprintf(
      paste0(
        "{%s} is named in MARGINPLYR_REQUIRED_SUGGESTS but is not installed, ",
        "so this job cannot prove its backend contract."
      ),
      package
    ))
  }
  FALSE
}

# Skips unless every named backend is installed, keeping testthat's own wording
# so the skip summary reads the same as it did under `skip_if_not_installed()`.
#
# `known` sits after `...` and so can only be supplied by full name, which keeps
# it from ever swallowing a backend argument.
skip_if_backend_absent <- function(..., known = optional_suggests()) {
  for (package in c(...)) {
    if (!backend_available(package, known = known)) {
      skip(sprintf("{%s} is not installed", package))
    }
  }
  invisible(NULL)
}
