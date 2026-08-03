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

tarball_dir <- Sys.getenv("MARGINPLYR_TARBALL_DIR", "tarball")
check_dir <- Sys.getenv("MARGINPLYR_CHECK_DIR", "check")
label <- Sys.getenv("MARGINPLYR_CHECK_LABEL", "R CMD check")

split_words <- function(value) {
  words <- trimws(strsplit(value, "[[:space:]]+")[[1]])
  words[nzchar(words)]
}

# NOTEs this package has already accounted for. Each entry pairs a regular
# expression with the reason the NOTE is expected, so the job summary explains
# itself without a reviewer having to reconstruct the history.
understood_notes <- c(
  "New submission" =
    "marginplyr 0.1.0 is a first CRAN release, so incoming checks say so.",
  "checking CRAN incoming feasibility" =
    "Header of the first-release incoming NOTE above.",
  "Days since last update" =
    "Only meaningful for resubmissions during a review cycle."
)

describe_note <- function(note) {
  matched <- vapply(
    names(understood_notes),
    function(pattern) grepl(pattern, note, fixed = TRUE),
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
    unlist(lapply(entries, function(entry) {
      c("```", strsplit(entry, "\n", fixed = TRUE)[[1]], "```", "")
    })),
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
    "```",
    strsplit(note, "\n", fixed = TRUE)[[1]],
    "```",
    ""
  )
  if (is.na(reason)) {
    unexpected <- c(unexpected, note)
  }
}

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_path)) {
  write(summary_lines, file = summary_path, append = TRUE)
}
cat(summary_lines, sep = "\n")

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
