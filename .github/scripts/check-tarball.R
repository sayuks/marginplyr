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
# Exits non-zero on any ERROR or WARNING. An unexpected NOTE is always
# annotated and written to the job summary. It also fails a manually dispatched
# strict release run; routine push and pull-request checks retain their
# lower-noise reporting behavior (#505).

source(".github/scripts/ci-helpers.R")
source(".github/scripts/cran-note-policy.R")

# Asserts that this job's library holds the optional backends it declared and
# no others, and stops before the check when it does not. It runs here rather
# than as its own workflow step so that it cannot be dropped from a job while
# the job goes on claiming to check a tarball; see the script's own header. A
# separate environment keeps its working names off this one.
source(".github/scripts/verify-library-isolation.R", local = new.env())

# Asserts that every other verifier in that directory is still reached by a
# workflow step or by a script that is. It runs here rather than as its own
# step for the same reason the assertion above does, one level out; the
# script's own header says where that regress stops (#292).
source(".github/scripts/verify-verifier-invocation.R", local = new.env())

tarball_dir <- Sys.getenv("MARGINPLYR_TARBALL_DIR", "tarball")
check_dir <- check_directory()
label <- check_label()

split_words <- function(value) {
  words <- trimws(strsplit(value, "[[:space:]]+")[[1]])
  words[nzchar(words)]
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
cran_status <- cran_status_from_tarball(tarballs)
strict_release <- identical(
  tolower(Sys.getenv("MARGINPLYR_STRICT_RELEASE", "false")),
  "true"
)

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

classifications <- lapply(
  result$notes,
  classify_cran_note,
  cran_status = cran_status
)
unexpected <- character()
for (index in seq_along(result$notes)) {
  note <- result$notes[[index]]
  classification <- classifications[[index]]
  allowed <- identical(classification$status, "allowed-note")
  heading <- if (allowed) "Allowed NOTE" else "Unexpected NOTE"
  summary_lines <- c(
    summary_lines,
    sprintf("### %s", heading),
    "",
    if (allowed) {
      classification$rationale
    } else {
      "No release-policy allowance matches this complete NOTE."
    },
    "",
    as_summary_block(note)
  )
  if (!allowed) {
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

if (cran_notes_block(classifications, strict_release)) {
  stop(sprintf(
    "%s failed strict release classification with %d unknown NOTE(s).",
    label,
    length(unexpected)
  ))
}
