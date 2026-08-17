# Every package behind these helpers is a Suggested one, so a check run without
# it must skip rather than fail. That is right for CRAN's minimal flavors, but
# it makes a skip indistinguishable from a proof: a release job whose whole
# purpose is to execute DuckDB would pass green with fourteen silently skipped
# tests if duckdb failed to install.
#
# Two words are in use here and they are not synonyms (#185). A *Suggest* is any
# optional package a guard may name, which is what `optional_suggest_spec()`
# below holds and what every helper taking a package name speaks of. A *backend*
# is narrower -- an entry the release matrix generates a job for -- which is why
# `optional_backends()` keeps its name for the subset those jobs iterate over
# and no helper accepting an entry is named for one. `DBI` is a Suggest and not
# a backend; `data.table` is both, and what its job proves is an input class.
#
# This file and `test-optional-backends.R` keep the narrower word in their
# names, which is the one place it stands without being accurate: a testthat
# helper file is reached by path -- from the CI scripts, the workflow,
# `inst/suggests/guard.R`, and the design documents, `grep` being the way to
# find them all -- and no reader meets either name while writing a guard.
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

# `MARGINPLYR_REQUIRED_SUGGESTS` is how a job states which Suggests it exists
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
# reaches its package through anything but these helpers would keep working and
# be reported as running, which is the honest answer -- the guards are what
# decide whether a test skips, and the guards are what this reads.
hidden_suggests <- function() {
  suggests_env_list("MARGINPLYR_HIDE_SUGGESTS")
}

# Whether a package is usable is not the same question as whether it is
# installed, because six of this package's Suggests carry a version constraint
# (#123). `inst/suggests/guard.R` is what reads one, and these helpers source
# that file rather than carrying a second reading of DESCRIPTION: no vignette
# and no example can reach `tests/`, so a copy here would be the copy every one
# of those sites drifts from.
#
# Repository copies are preferred over installed ones throughout, for the reason
# `test-package-metadata.R` records: `system.file()` reads whichever marginplyr
# happens to be installed, so a working tree that moved a version floor, or
# changed the guard, would otherwise be tested against neither change. Under
# `R CMD check` no repository copy is reachable and the installed package is the
# only answer, which is the same package the tarball built.
#
# The lookup is a function rather than a value because `.github/scripts/`
# sources this file from a bare `Rscript` before marginplyr is installed --
# `generate-backend-matrix.R` needs only `optional_suggest_spec()` -- so
# resolving a path at source time would fail a script that never asks a version
# question. Both candidate paths are relative, and both possible working
# directories are covered: the repository root under those scripts, and
# `tests/testthat` under testthat, from where the repository root is `../..`.
#
# The plain path is tried first because the `../..` form is the one that can
# reach outside the repository: run from the root it names the parent of the
# checkout, where a stray `DESCRIPTION` would quietly win. Neither
# `tests/testthat/DESCRIPTION` nor `tests/testthat/inst/` exists, so trying the
# plain path first costs nothing under testthat.
repository_file <- function(path) {
  candidates <- c(path, file.path("..", "..", path))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    return(NA_character_)
  }
  found[[1]]
}

# The `Suggests` field the guard reads its constraints out of.
declared_suggests <- function() {
  path <- repository_file("DESCRIPTION")
  if (is.na(path)) {
    path <- system.file("DESCRIPTION", package = "marginplyr")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("No DESCRIPTION is reachable to read Suggests versions from.")
  }
  suggests <- unname(read.dcf(path, fields = "Suggests")[1L, 1L])
  # A DESCRIPTION stating no Suggests would make every guard below report every
  # package unconstrained, which reads exactly like a package whose constraints
  # are all satisfied.
  if (is.na(suggests)) {
    stop(sprintf("%s states no Suggests field.", path))
  }
  suggests
}

# The shipped guard, sourced once into an environment of its own so that its
# definitions cannot be mistaken for helpers defined here. Loaded on demand for
# the reason above, and cached because every guarded test asks for it.
suggest_guard <- local({
  loaded <- NULL
  function() {
    if (!is.null(loaded)) {
      return(loaded)
    }
    path <- repository_file(file.path("inst", "suggests", "guard.R"))
    if (is.na(path)) {
      path <- system.file("suggests", "guard.R", package = "marginplyr")
    }
    if (!nzchar(path) || !file.exists(path)) {
      stop(
        "`inst/suggests/guard.R` is not reachable, so no guard here can tell ",
        "an installed package from a usable one."
      )
    }
    loaded <<- new.env(parent = globalenv())
    sys.source(path, envir = loaded)
    loaded
  }
})

# What the guard reports about one Suggested package. `suggests` is a seam
# rather than a convenience: it is how `test-optional-backends.R` drives the
# too-old branch with a package that is certainly installed, which no real
# constraint in DESCRIPTION would ever produce in a passing environment.
suggest_status <- function(package, suggests = declared_suggests()) {
  suggest_guard()$marginplyr_suggest_status(package, suggests = suggests)
}

# The one table describing the optional Suggests these helpers guard on, and the
# only list the release matrix reads. Which side of the repository it lives on
# is forced, not preferred: `R CMD check` runs these tests from the unpacked
# tarball, and `.Rbuildignore` excludes `^\.github$` from that tarball, so a
# table kept in `.github/scripts/` would not exist where the tests execute. The
# CI scripts run from the checkout instead and can read this file, which makes
# `tests/` the only placement both sides reach. Adding an entry starts here,
# because `suggest_available()` below refuses a name this table does not carry,
# and `release-matrix.yaml` generates its `backend` job from it.
#
# `asserted` answers whether the release matrix can assert the package absent by
# name. `DBI` cannot. `dbplyr` is an Import and declares `Imports: DBI`, so DBI
# is inside the hard dependency closure and is installed in every job,
# `_R_CHECK_DEPENDS_ONLY_=true` included. `verify-depends-only.R` would then
# require a `{DBI} is not installed` line that no run can produce, and
# `verify-library-isolation.R` would find DBI on `.libPaths()` in every job that
# withheld it. An entry that is not `asserted` gets no job of its own; it
# reaches CI as another entry's companion.
#
# `companions` names the packages a job proving this entry must install
# alongside it. They are not the entry the job is for, so they are not what it
# promises to execute -- but a driver package without DBI installs and then does
# nothing. It is also how a job declares what its own entry drags in: dtplyr
# declares `Imports: data.table`, so a job installing dtplyr installs data.table
# whether it asked for it or not, and `verify-library-isolation.R` would read
# that as a leak from a shared cache. Naming it here is the job saying it
# expected it.
#
# `data.table` is an entry of its own as well, because it is not only dtplyr's
# dependency here: raw `data.table` input reaches the local backend as an
# ordinary data frame subclass (#176), and it is genuinely absent under
# `_R_CHECK_DEPENDS_ONLY_=true`, which is what `asserted` claims. What its
# `backend` job proves is that input class, not a translation.
optional_suggest_spec <- function() {
  list(
    arrow = list(asserted = TRUE, companions = character()),
    duckdb = list(asserted = TRUE, companions = "DBI"),
    dtplyr = list(asserted = TRUE, companions = "data.table"),
    data.table = list(asserted = TRUE, companions = character()),
    RSQLite = list(asserted = TRUE, companions = "DBI"),
    DBI = list(asserted = FALSE, companions = character())
  )
}

# The table's `asserted` column, in the shape every existing caller reads.
optional_suggests <- function() {
  vapply(optional_suggest_spec(), function(entry) entry$asserted, logical(1))
}

# The subset a job can be asked to withhold, which is what the release matrix's
# absence assertions iterate over and what its `backend` matrix is generated
# from. Keeps the name it had while it lived in `ci-helpers.R`, so
# `verify-depends-only.R` and `verify-library-isolation.R` call it unchanged.
#
# #185 renamed the table and the guards around it without touching this, because
# here the narrower word is the accurate one: what it returns is exactly the set
# `backend` jobs are generated from, and it takes no package name, so it cannot
# be read as a claim that any particular entry translates queries.
# `suggest_job_packages()` below does take one, which is why that one moved.
optional_backends <- function() {
  asserted <- optional_suggests()
  names(asserted)[asserted]
}

# Every package a job proving `package` must install and name in
# `MARGINPLYR_REQUIRED_SUGGESTS`. The entry leads, so a reader of the
# generated matrix sees which one a job is for before its companions.
#
# Defined over the whole table rather than over `optional_backends()`, because
# an entry with no job of its own still has an answer -- `DBI` installs itself
# and nothing else -- and refusing a name the table does not carry is the check
# worth making here. `generate-backend-matrix.R` asks it only about entries a
# job exists for.
suggest_job_packages <- function(package) {
  spec <- optional_suggest_spec()
  if (!package %in% names(spec)) {
    stop(sprintf(
      "{%s} is not named in `optional_suggest_spec()`.",
      package
    ))
  }
  unique(c(package, spec[[package]]$companions))
}

# Reports whether an optional Suggest can be used, and refuses to report FALSE
# for one the running job promised to exercise. Callers that select among
# several packages use this directly, because dropping one from a list records
# no skip at all and would otherwise be invisible.
#
# "Can be used" includes the version DESCRIPTION requires, which is what
# `requireNamespace()` here could not say (#123). An installed-but-too-old
# package now reports FALSE and skips, where it used to report TRUE and let the
# test call an API the installed version does not have.
#
# A package `known` does not name is an error rather than FALSE. Nothing in the
# release matrix executes such a package and nothing asserts it absent, so a
# guard on it would do nothing while reading as protection; erroring is what
# makes this test suite the place an optional Suggest gets registered. The check
# runs before the guard is consulted so that it fires the same way on a fully
# provisioned developer machine, where the package is installed and a check
# placed after it would never be reached.
#
# `known` and `suggests` are parameters only so that this helper's own tests can
# drive every outcome. They need a sentinel name no CRAN package uses, a base
# package that is always installed, and a constraint no installed version can
# satisfy; none of the three belongs in `optional_suggests()` or in DESCRIPTION.
# Every other call site takes the defaults.
suggest_available <- function(package,
                              known = optional_suggests(),
                              suggests = declared_suggests()) {
  if (!package %in% names(known)) {
    stop(sprintf(
      paste0(
        "{%s} is not named in `optional_suggests()`, so no release-matrix job ",
        "executes it and no job asserts it is absent. Add it to ",
        "`optional_suggest_spec()`, which is what generates its job."
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
  status <- suggest_status(package, suggests = suggests)
  if (!hidden && status$available) {
    return(TRUE)
  }
  if (package %in% required_suggests()) {
    stop(sprintf(
      paste0(
        "%s, and it is named in MARGINPLYR_REQUIRED_SUGGESTS, so this job ",
        "cannot prove the contract it exists for."
      ),
      suggest_absence_reason(package, suggests = suggests)
    ))
  }
  FALSE
}

# Why a package is unusable, in the wording a skip carries.
#
# A hidden package reports the absent wording rather than the guard's, because
# `MARGINPLYR_HIDE_SUGGESTS` claims a package this process could otherwise see
# is gone: `verify-suite-coverage.R` and `verify-depends-only.R` both attribute
# a skip by matching `{pkg} is not installed`, and a simulated absence that
# announced itself differently would be a skip neither could attribute.
#
# The too-old wording is deliberately not that phrase, and not because a
# `backend` job needs it to be: a package that job named is required, so
# `suggest_available()` above stops rather than skipping, and one it withheld is
# not installed at all. What the distinction is for is the reader of a skip --
# "not installed" would send someone looking for a package sitting in their
# library -- and for `verify-backend.R`, which fails a job on any skip it
# cannot attribute. Sharing the absent wording would let a version failure pass
# there as a withheld package if one ever reached that path.
suggest_absence_reason <- function(package, suggests = declared_suggests()) {
  if (package %in% hidden_suggests()) {
    return(sprintf("{%s} is not installed", package))
  }
  suggest_status(package, suggests = suggests)$reason
}

# Skips unless every named package is usable, keeping testthat's own wording for
# an absent package so the skip summary reads the same as it did under
# `skip_if_not_installed()`.
#
# `known` and `suggests` sit after `...` and so can only be supplied by full
# name, which keeps them from ever swallowing a package argument.
skip_if_suggest_absent <- function(...,
                                   known = optional_suggests(),
                                   suggests = declared_suggests()) {
  for (package in c(...)) {
    if (!suggest_available(package, known = known, suggests = suggests)) {
      skip(suggest_absence_reason(package, suggests = suggests))
    }
  }
  invisible(NULL)
}
