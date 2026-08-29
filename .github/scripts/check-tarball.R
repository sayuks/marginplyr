# Checks a built source tarball and records the outcome where a release
# reviewer can audit it.
#
# Every release-matrix job runs this script rather than
# `r-lib/actions/check-r-package`, for three reasons. The jobs must check the
# tarball the `build` job produced instead of rebuilding one from the working
# tree, so that what passes is the artifact a submission would carry. They must
# agree on which NOTEs are understood, which is a policy that belongs in one
# reviewed place rather than in six copies of an inline step. And a matrix job
# cannot pass R arguments, so configuration arrives through the environment.
#
# Exits non-zero on any ERROR or WARNING. An unexpected NOTE is annotated and
# written to the job summary but does not fail the job: R-devel and CRAN
# incoming checks introduce NOTEs that are outside this package's control, and
# a gate that cries wolf gets ignored. Judging the recorded NOTEs is part of
# the release review.

source(".github/scripts/ci-helpers.R")

# Asserts that this job's library holds the optional backends it declared and
# no others, and stops before the check when it does not. It runs here rather
# than as its own workflow step so that it cannot be dropped from a job while
# the job goes on claiming to check a tarball; see the script's own header. A
# separate environment keeps its working names off this one.
source(".github/scripts/verify-library-isolation.R", local = new.env())

# Asserts that every other verifier in that directory is still reached by a
# workflow step or by a script that is. It runs here for the same reason the
# assertion above does, one level out: a verifier reached only by a step is
# silenced by deleting that step, and deleting this `source()` means editing a
# script whose job is to check a tarball. That is where the regress stops, and
# `verify-verifier-invocation.R`'s header says why it stops there (#292).
source(".github/scripts/verify-verifier-invocation.R", local = new.env())

tarball_dir <- Sys.getenv("MARGINPLYR_TARBALL_DIR", "tarball")
check_dir <- check_directory()
label <- check_label()

split_words <- function(value) {
  words <- trimws(strsplit(value, "[[:space:]]+")[[1]])
  words[nzchar(words)]
}

# NOTEs this package has already accounted for. Each entry pairs a literal
# fragment of the NOTE's text with the reason it is expected, so the job
# summary explains itself without a reviewer reconstructing the history.
#
# Fragments are matched literally and kept narrow on purpose. Matching the
# "checking CRAN incoming feasibility" header instead would classify every
# future incoming finding -- misspellings, unreachable URLs -- as understood,
# which is the opposite of what this list is for.
understood_notes <- c(
  "New submission" =
    "marginplyr 0.1.0 is a first CRAN release, so incoming checks say so.",
  "Days since last update" =
    "Only meaningful for resubmissions during a review cycle."
)

describe_note <- function(note) {
  matched <- vapply(
    names(understood_notes),
    function(fragment) grepl(fragment, note, fixed = TRUE),
    logical(1)
  )
  if (!any(matched)) {
    return(NA_character_)
  }
  understood_notes[[which(matched)[1]]]
}

tarballs <- list.files(tarball_dir, pattern = "[.]tar[.]gz$", full.names = TRUE)
if (length(tarballs) != 1L) {
  stop(sprintf(
    "Expected exactly one source tarball in '%s', found %d.",
    tarball_dir,
    length(tarballs)
  ))
}
message("Checking source tarball: ", tarballs)

result <- rcmdcheck::rcmdcheck(
  tarballs,
  args = unique(c("--no-manual", split_words(
    Sys.getenv("MARGINPLYR_CHECK_ARGS", "--as-cran")
  ))),
  # The tarball is already built; rebuilding it here would reintroduce exactly
  # the working-tree dependency this workflow exists to remove.
  build_args = NULL,
  check_dir = check_dir,
  error_on = "never"
)

summary_lines <- c(
  sprintf("## %s", label),
  "",
  sprintf(
    "%d ERROR(s), %d WARNING(s), %d NOTE(s).",
    length(result$errors),
    length(result$warnings),
    length(result$notes)
  ),
  ""
)

append_section <- function(lines, heading, entries) {
  if (length(entries) == 0L) {
    return(lines)
  }
  c(
    lines,
    sprintf("### %s", heading),
    "",
    unlist(lapply(entries, as_summary_block)),
    ""
  )
}

summary_lines <- append_section(summary_lines, "Errors", result$errors)
summary_lines <- append_section(summary_lines, "Warnings", result$warnings)

unexpected <- character()
for (note in result$notes) {
  reason <- describe_note(note)
  heading <- if (is.na(reason)) "Unexpected NOTE" else "Understood NOTE"
  summary_lines <- c(
    summary_lines,
    sprintf("### %s", heading),
    "",
    if (is.na(reason)) "No recorded explanation for this NOTE." else reason,
    "",
    as_summary_block(note)
  )
  if (is.na(reason)) {
    unexpected <- c(unexpected, note)
  }
}

write_step_summary(summary_lines)

for (note in unexpected) {
  # `::warning::` surfaces the NOTE in the Actions run header, where a release
  # reviewer sees it without opening the uploaded check directory.
  cat(sprintf(
    "::warning title=Unexpected NOTE in %s::%s\n",
    label,
    gsub("\n", " ", trimws(note))
  ))
}

if (length(result$errors) > 0L || length(result$warnings) > 0L) {
  stop(sprintf(
    "%s failed with %d ERROR(s) and %d WARNING(s).",
    label,
    length(result$errors),
    length(result$warnings)
  ))
}
