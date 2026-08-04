# Confirms that every optional backend the test suite tracks is executed by a
# `backend` job.
#
# `AGENTS.md`'s "Adding an optional backend" list has three entries, and #69
# made three of the four ways to get it wrong fail loudly. This is the fourth
# (#71): a backend added to `optional_suggests()` and guarded in the tests, with
# no `backend` matrix entry, produces a green workflow. Every job correctly
# asserts it absent -- that half works -- and no job ever runs it. Its contracts
# are written and never executed, which is the failure
# `MARGINPLYR_REQUIRED_SUGGESTS` and `verify-backend.R` exist to rule out,
# arrived at from the other direction.
#
# This has to be a CI job rather than a test in the package's own suite.
# `.Rbuildignore` excludes `^\.github$`, so the tarball does not ship
# `release-matrix.yaml` and a test run from the tarball cannot read it -- the
# direction that made #69 work does not work here. This script runs from the
# checkout instead, and reads `optional_suggests()` out of
# `tests/testthat/helper-optional-backends.R` through `ci-helpers.R`, so #69's
# single source stays single and this file carries no list of its own.
#
# The matrix is parsed with the `yaml` package rather than matched with a regex
# over the `backend:` block. The failure modes decide it: a regex that stops
# matching -- because an entry gained a comment, changed its quoting, or moved
# -- finds no entries to complain about and passes, which is the same silent
# green this script exists to remove. `yaml` has no dependencies and the job
# installs it directly; DESCRIPTION does not need it, because `.github/scripts/`
# may use packages installed only for CI.
#
# #71 named three candidate assertions. One ships:
#
#   Every package `optional_backends()` names has exactly one `backend` matrix
#   entry naming it in `required`. Nothing covers this today. "Exactly one"
#   rather than "at least one" because an entry's `proves` list is the only
#   place a backend's contracts are named and its `cache-version` is derived
#   from its `name`, so a backend split across two entries has no single answer
#   to either.
#
# Two do not:
#
#   Every name in `required` is named by `optional_suggests()`.
#   `verify-library-isolation.R` already refuses this, at the point the claim is
#   made and against the library the job actually got. #64's reasoning keeps it
#   there: `check-tarball.R` sources it, so a job cannot drop the assertion
#   while still checking anything. A static copy would give one mistake a second
#   wording to keep in step with the first and would buy only an earlier
#   failure.
#
#   Every job that runs with optional backends withheld runs `check-tarball.R`.
#   This guards a job that does not exist yet, and it is not the same kind of
#   static property as the assertion above: withholding is the default, so the
#   check would first have to decide which jobs make a claim worth asserting,
#   which is a judgement about a job's shape rather than a fact the file states.
#   Deferred rather than dismissed, and #73 is where it is recorded -- the case
#   to act on it is a job that declares `MARGINPLYR_REQUIRED_SUGGESTS` and
#   checks no tarball.
#
# This script runs as a workflow step rather than being sourced by another
# script, which is the arrangement #64 argued against for
# `verify-library-isolation.R` on the grounds that a step can be deleted. There
# is nothing here to source it from: no other job reads a workflow file, and
# attaching it to `check-tarball.R` would make a claim about this repository
# depend on a tarball that does not contain the file being read. Deleting the
# step deletes the only job that has one, which is visible in a way that
# dropping one assertion from a job that still checks a tarball is not.

source(".github/scripts/ci-helpers.R")

# Fixed rather than read from the environment, as in the sibling scripts, which
# name the file they assert against. The failure path is reachable without a
# knob: adding a name to `optional_suggests()` is the mistake this script exists
# to catch, and doing it locally prints the message.
workflow_path <- ".github/workflows/release-matrix.yaml"

if (!file.exists(workflow_path)) {
  stop(call. = FALSE, sprintf("No workflow file at '%s'.", workflow_path))
}

workflow <- yaml::read_yaml(workflow_path)
entries <- workflow$jobs$backend$strategy$matrix$backend

# A missing or empty matrix says the file moved out from under this script,
# which is a different problem from a backend having no entry and reads better
# as its own message than as four identical "no entry" lines.
if (!is.list(entries) || length(entries) == 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "`jobs.backend.strategy.matrix.backend` in '%s' is missing or empty, so ",
      "this check cannot tell which backends the release matrix executes."
    ),
    workflow_path
  ))
}

entry_name <- function(entry) {
  if (is.null(entry$name)) "(unnamed entry)" else entry$name
}

# `required` rather than `packages` is what makes an entry count as executing a
# backend: it is what the job sets `MARGINPLYR_REQUIRED_SUGGESTS` from, so it is
# the field that turns a failed install into a failed job instead of a skip. An
# entry that installed a backend without naming it there would prove nothing.
# `unlist()` before splitting because YAML gives `required` back as a character
# vector when it is written as a block sequence and as one string when it is
# written inline, as every entry is today. Without it the parsed list would keep
# only its first element, and a backend named second would be reported missing
# with a message blaming the wrong thing.
entries_naming <- function(package, matrix_entries) {
  declared <- vapply(
    matrix_entries,
    function(entry) {
      required <- paste(unlist(entry$required), collapse = ",")
      package %in% split_list(required)
    },
    logical(1)
  )
  vapply(matrix_entries[declared], entry_name, character(1))
}

backends <- optional_backends()
covering <- lapply(backends, entries_naming, matrix_entries = entries)
names(covering) <- backends
counts <- lengths(covering)

describe <- function(package) {
  if (counts[[package]] == 0L) {
    return("no `backend` entry")
  }
  sprintf(
    "%s %s",
    if (counts[[package]] == 1L) "entry" else "entries",
    paste(sprintf("`%s`", covering[[package]]), collapse = ", ")
  )
}

verdict <- rep("OK", length(backends))
verdict[counts == 0L] <- "MISSING"
verdict[counts > 1L] <- "DUPLICATE"

write_step_summary(c(
  "## Backend matrix coverage",
  "",
  sprintf(
    "%d of %d tracked backends have exactly one `backend` entry in `%s`.",
    sum(counts == 1L),
    length(backends),
    workflow_path
  ),
  "",
  paste0(
    "- ", verdict,
    " **", backends, "** — ",
    vapply(backends, describe, character(1))
  )
))

problems <- character()

# Collective phrasing throughout, as in `verify-library-isolation.R`. A message
# that reads correctly for one backend and for several needs no branch on the
# count, and a branch that pluralized only part of a sentence would leave the
# rest disagreeing.
uncovered <- backends[counts == 0L]
if (length(uncovered) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These backends are named by `optional_suggests()` in ",
      "`tests/testthat/helper-optional-backends.R` but have no `backend` ",
      "matrix entry in `%s`, so no job executes them and their contracts ",
      "skip everywhere: %s. Add an entry for each, naming it in `required` ",
      "and its contract tests in `proves`."
    ),
    workflow_path,
    paste(uncovered, collapse = ", ")
  ))
}

duplicated_backends <- backends[counts > 1L]
if (length(duplicated_backends) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These backends are named in `required` by more than one `backend` ",
      "matrix entry in `%s`, which leaves their contracts and dependency ",
      "cache without one entry to read them from: %s."
    ),
    workflow_path,
    paste(
      sprintf("%s (%s)", duplicated_backends, vapply(
        duplicated_backends,
        function(package) paste(covering[[package]], collapse = ", "),
        character(1)
      )),
      collapse = "; "
    )
  ))
}

if (length(problems) > 0L) {
  stop(call. = FALSE, paste(problems, collapse = " "))
}

message(sprintf(
  "Verified: %s each have a `backend` matrix entry in %s.",
  paste(backends, collapse = ", "),
  workflow_path
))
