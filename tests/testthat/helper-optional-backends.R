# Every backend behind these helpers is a Suggested package, so a check run
# without it must skip rather than fail. That is right for CRAN's minimal
# flavors, but it makes a skip indistinguishable from a proof: a release job
# whose whole purpose is to execute DuckDB would pass green with fourteen
# silently skipped tests if duckdb failed to install.
#
# Both variables below arrive as a comma-separated package list, and the CI
# scripts read the same values through `env_list()` in `.github/scripts/`. That
# file cannot be shared with the tests -- `.Rbuildignore` excludes `^\.github$`,
# so it is absent from the tarball these tests run from -- so the split is
# forced, and keeping the two readings identical is what makes a `required`
# value mean one package set on both sides.
suggests_env_list <- function(name) {
  declared <- trimws(strsplit(Sys.getenv(name, ""), ",", fixed = TRUE)[[1]])
  declared[nzchar(declared)]
}

# `MARGINPLYR_REQUIRED_SUGGESTS` is how a job states which backends it exists
# to prove. A package named there must be installed, and its absence fails the
# test instead of skipping it. Generic jobs leave the variable unset and keep
# skipping. `.github/workflows/release-matrix.yaml` sets it per dedicated job.
required_suggests <- function() {
  suggests_env_list("MARGINPLYR_REQUIRED_SUGGESTS")
}

# Simulated absence. `verify-suite-coverage.R` runs the whole suite once per
# optional backend with the others named here, which is how it establishes that
# every test executes in some single-backend configuration without waiting for
# the `backend` jobs to report. Unset everywhere else, so an ordinary run is
# unaffected.
#
# Hiding is a claim about a package this process could otherwise see, so it is
# deliberately not the same mechanism as `.libPaths()` surgery: a test that
# reaches its backend through anything but these helpers would keep working and
# be reported as running, which is the honest answer -- the guards are what
# decide whether a test skips, and the guards are what this reads.
hidden_suggests <- function() {
  suggests_env_list("MARGINPLYR_HIDE_SUGGESTS")
}

# The one table describing the optional Suggests these helpers guard on, and the
# only list the release matrix reads. Which side of the repository it lives on
# is forced, not preferred: `R CMD check` runs these tests from the unpacked
# tarball, and `.Rbuildignore` excludes `^\.github$` from that tarball, so a
# table kept in `.github/scripts/` would not exist where the tests execute. The
# CI scripts run from the checkout instead and can read this file, which makes
# `tests/` the only placement both sides reach. Adding a backend starts here,
# because `backend_available()` below refuses a name this table does not carry,
# and `release-matrix.yaml` generates its `backend` job from it.
#
# `asserted` answers whether the release matrix can assert the package absent by
# name. `DBI` cannot. `dbplyr` is an Import and declares `Imports: DBI`, so DBI
# is inside the hard dependency closure and is installed in every job,
# `_R_CHECK_DEPENDS_ONLY_=true` included. `verify-depends-only.R` would then
# require a `{DBI} is not installed` line that no run can produce, and
# `verify-library-isolation.R` would find DBI on `.libPaths()` in every job that
# withheld it. An entry that is not `asserted` gets no job of its own; it
# reaches CI as another backend's companion.
#
# `companions` names the packages a job proving this backend must install
# alongside it. They are not the backend, so they are not what the job promises
# to execute -- but a driver package without DBI installs and then does nothing.
optional_backend_spec <- function() {
  list(
    arrow = list(asserted = TRUE, companions = character()),
    duckdb = list(asserted = TRUE, companions = "DBI"),
    dtplyr = list(asserted = TRUE, companions = character()),
    RSQLite = list(asserted = TRUE, companions = "DBI"),
    DBI = list(asserted = FALSE, companions = character())
  )
}

# The table's `asserted` column, in the shape every existing caller reads.
optional_suggests <- function() {
  vapply(optional_backend_spec(), function(entry) entry$asserted, logical(1))
}

# The subset a job can be asked to withhold, which is what the release matrix's
# absence assertions iterate over and what its `backend` matrix is generated
# from. Keeps the name it had while it lived in `ci-helpers.R`, so
# `verify-depends-only.R` and `verify-library-isolation.R` call it unchanged.
optional_backends <- function() {
  asserted <- optional_suggests()
  names(asserted)[asserted]
}

# Every package a job proving `package` must install and name in
# `MARGINPLYR_REQUIRED_SUGGESTS`. The backend leads, so a reader of the
# generated matrix sees which entry a job is for before its companions.
backend_job_packages <- function(package) {
  spec <- optional_backend_spec()
  if (!package %in% names(spec)) {
    stop(sprintf(
      "{%s} is not named in `optional_backend_spec()`.",
      package
    ))
  }
  unique(c(package, spec[[package]]$companions))
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
        "executes it and no job asserts it is absent. Add it to ",
        "`optional_backend_spec()`, which is what generates its job."
      ),
      package
    ))
  }
  # A simulated absence may never contradict a promise. The two variables are
  # set by different things -- `MARGINPLYR_HIDE_SUGGESTS` by
  # `verify-suite-coverage.R`, `MARGINPLYR_REQUIRED_SUGGESTS` by a `backend`
  # job -- so nothing structural stops them from naming the same package, and if
  # they did the hook would turn the job's proof into a skip and the job would
  # pass. Refusing per queried package rather than per variable is what keeps
  # this from firing on `test-optional-backends.R`, which sets
  # `MARGINPLYR_REQUIRED_SUGGESTS` to names the coverage run has no reason to
  # hide.
  hidden <- package %in% hidden_suggests()
  if (hidden && package %in% required_suggests()) {
    stop(sprintf(
      paste0(
        "{%s} is named in both MARGINPLYR_HIDE_SUGGESTS and ",
        "MARGINPLYR_REQUIRED_SUGGESTS, so hiding it would mask the contract ",
        "this run promised to prove."
      ),
      package
    ))
  }
  if (!hidden && requireNamespace(package, quietly = TRUE)) {
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
