# Confirms that every verifier in this directory is still run by something.
#
# Each `verify-*.R` here asserts a property nothing else in the repository
# fails on, and each is reached by a workflow step. Deleting the step deletes
# the assertion and nothing reports it: the job still runs, still passes, and
# the diff is a few lines of YAML that read like cleanup. A gate that stops
# asserting reports nothing, which is the shape every one of these scripts
# exists to refuse (#292).
#
# The rule is one sentence: every `verify-*.R` under `.github/scripts/` is
# either named by a workflow step or sourced by a script that is. The set is
# derived from the directory rather than listed, so a verifier added tomorrow
# is covered without an edit here -- and a list would name what it named when
# it was written, which is the failure this script is about.
#
# `check-tarball.R` sources it rather than a workflow calling it as a step,
# which is what `verify-library-isolation.R` already does and for the same
# reason: a script reached only by a step is a script one deleted step
# silences. The regress stops there rather than at a count of levels. Removing
# this `source()` means editing a script whose job is to check a tarball, and
# removing that job's step means no tarball is checked -- which no diff can
# make look like cleanup. Guarding `check-tarball.R`'s own step is therefore
# deliberately not attempted.
#
# Run it locally with:
#
#     Rscript .github/scripts/verify-verifier-invocation.R

source(".github/scripts/ci-helpers.R")

scripts_dir <- ".github/scripts"
workflows_dir <- ".github/workflows"

# A full-line comment is not code, in YAML or in R -- both spell one the same
# way, so one rule covers both files. It has to be dropped before anything is
# read: a workflow comment quoting a gate's command for a reader to run it
# locally is not a step, and counting one would report a verifier as invoked by
# prose. A comment placed after code on the same line is not stripped, and a
# call commented out that way would still be counted; the probe below fixes
# what this reader does rather than leaving it to be inferred.
executable_lines <- function(lines) {
  lines[!grepl("^[[:space:]]*#", lines)]
}

# The two forms that actually run a script here: `Rscript <path>` in a workflow
# step, and `source("<path>")` from another script. The prefix is what
# separates either from a bare path, which a step's `name:` or an artifact
# value may carry without running anything. A workflow value outside `run:`
# that spells the whole command would still be counted; nothing here does, and
# narrowing the match to a line beginning `run:` would miss the later lines of
# a block scalar, which is a shape a step may legitimately take.
#
# `scripts_dir` is escaped into the pattern rather than pasted, so its `.`
# matches a dot and not any character. The match ends at the file name, so
# `basename()` is what reads it back out.
invocation <- paste0(
  "(Rscript[[:space:]]+|source\\([[:space:]]*[\"'])",
  gsub(".", "\\.", scripts_dir, fixed = TRUE),
  "/[A-Za-z0-9._-]+\\.R"
)

invoked <- function(lines) {
  found <- regmatches(lines, gregexpr(invocation, lines))
  unique(basename(unlist(found)))
}

invoked_by_file <- function(path) {
  invoked(executable_lines(readLines(path, warn = FALSE)))
}

# The mechanism, before the verdict, in all three ways it can arrive vacuous.
verifiers <- sort(list.files(scripts_dir, pattern = "^verify-.*\\.R$"))
workflows <- sort(
  list.files(workflows_dir, pattern = "\\.ya?ml$", full.names = TRUE)
)

if (length(verifiers) == 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "No `verify-*.R` was found under `%s`, so the loop below iterates over ",
      "nothing and this script passes on any repository. Fix the derivation ",
      "in this file before trusting what it reports."
    ),
    scripts_dir
  ))
}

if (length(workflows) == 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "No workflow was found under `%s`, so nothing could be found invoking ",
      "anything. That is this script reading the wrong place, not %d ",
      "verifier(s) that stopped running."
    ),
    workflows_dir, length(verifiers)
  ))
}

# The reader itself, asked the two questions it has to answer differently. A
# reader that stopped discarding comments reports every verifier as invoked and
# passes; one that stopped matching reports every verifier as dead and fails
# naming the repository. Both are answered here, where the diagnosis is this
# file.
probe <- c(
  sprintf("        run: Rscript %s/probe-invoked.R", scripts_dir),
  sprintf("      # Rscript %s/probe-mentioned.R", scripts_dir)
)
if (!identical(invoked(executable_lines(probe)), "probe-invoked.R")) {
  stop(call. = FALSE, paste0(
    "The invocation reader in this script does not separate a call from a ",
    "mention of one, so every verdict below would be about the reader. Fix ",
    "`executable_lines()` and the `invocation` pattern here before reading ",
    "the repository."
  ))
}

# Walks out from the workflow steps: a script a step names is reached, and so
# is anything a reached script sources. `via` records how each was reached, so
# the summary says which step or which script carries it and a reader chasing a
# failure has the chain rather than a verdict.
via <- character()
pending <- character()

for (workflow in workflows) {
  for (name in invoked_by_file(workflow)) {
    if (!(name %in% names(via))) {
      via[[name]] <- sprintf("step in `%s`", basename(workflow))
      pending <- c(pending, name)
    }
  }
}

# A path that is run but answers to no file: the step or the `source()` fails
# at run time, so this is reported rather than skipped over, and reported here
# because skipping it silently is what would let the walk end early.
absent <- character()

while (length(pending) > 0L) {
  name <- pending[[1L]]
  pending <- pending[-1L]
  path <- file.path(scripts_dir, name)
  if (!file.exists(path)) {
    absent <- c(absent, name)
    next
  }
  for (sourced in invoked_by_file(path)) {
    if (!(sourced %in% names(via))) {
      via[[sourced]] <- sprintf("sourced by `%s`", name)
      pending <- c(pending, sourced)
    }
  }
}

route <- function(name) {
  if (name %in% names(via)) via[[name]] else "**run by nothing**"
}

write_step_summary(c(
  "## Verifier invocation",
  "",
  sprintf(
    "%d verifier(s) under `%s`, against %d workflow(s).",
    length(verifiers), scripts_dir, length(workflows)
  ),
  "",
  sprintf("- **%s** — %s", verifiers, vapply(verifiers, route, character(1)))
))

problems <- character()

uninvoked <- setdiff(verifiers, names(via))
if (length(uninvoked) > 0L) {
  # The remedy is part of the message, as `verify-suite-coverage.R`'s is: a
  # gate that only refuses invites the script to be deleted rather than run.
  problems <- c(problems, sprintf(
    paste0(
      "These verifiers are named by no workflow step and sourced by no ",
      "script that is, so what each of them asserts is asserted nowhere: %s. ",
      "Give each one a `run: Rscript` step in the workflow whose job it ",
      "belongs to, or source it from a script that already has one, as ",
      "`check-tarball.R` sources `verify-library-isolation.R` and this ",
      "script. ",
      "Prefer the second where a job's own work would be incomplete without ",
      "the assertion: a step can be deleted on its own, and a `source()` ",
      "cannot be, without editing the script that does the work."
    ),
    paste(sprintf("`%s`", uninvoked), collapse = ", ")
  ))
}

if (length(absent) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These paths are run as scripts under `%s` but no file answers them: ",
      "%s. Point the caller at the file's current name, or delete the call ",
      "with the script it named."
    ),
    scripts_dir,
    paste(sprintf("`%s`", sort(unique(absent))), collapse = ", ")
  ))
}

if (length(problems) > 0L) {
  # `call. = FALSE` because `check-tarball.R` sources this file; what the
  # wrapping looks like without it is in `verify-library-isolation.R`.
  stop(call. = FALSE, paste(problems, collapse = " "))
}

message(sprintf(
  paste0(
    "Verified: all %d verifier(s) under %s are named by a workflow step or ",
    "sourced by a script that is."
  ),
  length(verifiers), scripts_dir
))
