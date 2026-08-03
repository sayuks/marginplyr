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

# Reports whether an optional backend can be used, and refuses to report FALSE
# for a backend the running job promised to exercise. Callers that select among
# several backends use this directly, because dropping one from a list records
# no skip at all and would otherwise be invisible.
backend_available <- function(package) {
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
skip_if_backend_absent <- function(...) {
  for (package in c(...)) {
    if (!backend_available(package)) {
      skip(sprintf("{%s} is not installed", package))
    }
  }
  invisible(NULL)
}
