# Generates `release-matrix.yaml`'s `backend` matrix from the one table.
#
# Those jobs used to be four hand-written entries, each repeating what
# `optional_suggest_spec()` in `tests/testthat/helper-optional-backends.R`
# already says: which package the job installs, which packages its absence must
# fail on, and a `cache-version` spelled from its name. Repetition is what the
# `coverage` job existed to police -- it read the workflow file back against the
# same table and failed when a tracked backend had no entry (#71). Generating
# the entries removes the duplication and the job that guarded it (#93): a
# backend cannot be tracked without a job, because the job is the table.
#
# It also closes #73. That ticket asked for an assertion that every job
# withholding optional backends checks a tarball, and recorded its own closing
# condition: close it if the shape of the workflow changes such that the
# question stops making sense. With one generated job body there is no second
# shape for a `backend` job to have, so the assertion has nothing left to guard.
#
# What the cost is, and where it is paid: the workflow file no longer shows its
# own jobs. So this writes the matrix it produced to the step summary, where a
# run page shows what was actually generated rather than what a reader of the
# YAML would infer.
#
# Emitted as JSON for `fromJSON` in the consuming job's `strategy.matrix`, which
# is the only shape Actions accepts for a computed matrix. `jsonlite` rather
# than pasted strings: this is the file that decides which backends CI runs at
# all, and a quoting mistake here would produce a syntactically valid matrix
# with the wrong contents. It has no dependencies of its own and is not in
# DESCRIPTION -- `.github/scripts/` may use packages installed only for CI, and
# promoting a serializer used by one CI script into the package's metadata would
# be exactly the false Import `AGENTS.md` warns about.

source(".github/scripts/ci-helpers.R")

backends <- optional_backends()
if (length(backends) == 0L) {
  stop(call. = FALSE, paste0(
    "`optional_backends()` names nothing, so no job would execute any ",
    "backend and the whole `backend` matrix would silently disappear."
  ))
}

entries <- lapply(backends, function(package) {
  packages <- suggest_job_packages(package)
  list(
    name = package,
    # What `setup-r-dependencies` installs on top of the hard dependencies.
    packages = paste(sprintf("any::%s", packages), collapse = ", "),
    # What the job promises to execute: `MARGINPLYR_REQUIRED_SUGGESTS` turns a
    # failed install of any of these into a failed job instead of a skip, and
    # `verify-library-isolation.R` reads the same value as the job's declaration
    # of which optional backends it asked for.
    required = paste(packages, collapse = ","),
    # One cache per backend. These jobs share an operating system, R version,
    # and architecture, so a single `cache-version` would put every library
    # behind one `restore-keys` prefix and each job would start from whichever
    # ran last (#64).
    cache = sprintf("backend-%s-1", package)
  )
})

matrix_json <- jsonlite::toJSON(entries, auto_unbox = TRUE)

output_path <- Sys.getenv("GITHUB_OUTPUT", "")
if (nzchar(output_path)) {
  write(sprintf("backend=%s", matrix_json), file = output_path, append = TRUE)
}

write_step_summary(c(
  "## Generated backend matrix",
  "",
  sprintf(
    "%d job(s), one per member of `optional_backends()`.",
    length(entries)
  ),
  "",
  unlist(lapply(entries, function(entry) {
    sprintf(
      "- **%s** — installs `%s`, requires `%s`, cache `%s`",
      entry$name,
      entry$packages,
      entry$required,
      entry$cache
    )
  })),
  "",
  as_summary_block(jsonlite::prettify(matrix_json))
))

message(sprintf(
  "Generated %d backend job(s): %s.",
  length(entries),
  paste(backends, collapse = ", ")
))
