#!/usr/bin/env Rscript
# Assemble a hunt finding's issue body from its candidate, its refutation and
# the coordinator's spec, so the coordinator writes only what it decided.
#
#   Rscript tools/hunt/issue-body.R SCRATCH SPEC.md    write one body
#   Rscript tools/hunt/issue-body.R SCRATCH --numbers  resolve cross-references
#
# SPEC.md holds `## name` sections. Required: `id` (the candidate's, as in
# SCRATCH/cands/<id>.md and SCRATCH/refs/<id>-refute.md), `title`, `labels`,
# `refuter` (the model), `what`, `acceptance`. Optional: `decision`,
# `interacts`, `blocked`, and `contract`, `repro`, `expected`, `observed`,
# `control`, `cause`, `executed`, `rendered`, each replacing the candidate's
# own text. `environment.md` in SCRATCH is the paragraph every body ends with:
# the pinned SHA, the versions and where the evidence is recorded.
#
# The body goes to SCRATCH/issues/<id>.body.md and the title and labels to
# SCRATCH/issues/<id>.meta. Another finding is written `{{<its id>}}`;
# `--numbers` replaces each with the issue number `numbers.tsv` beside the
# bodies gives it (two tab-separated columns, id and number) and stops on one
# left over. glue's own `{{` is left alone: only a listed id is a reference.
args <- commandArgs(trailingOnly = TRUE)
scratch <- args[[1L]]
issues <- file.path(scratch, "issues")
dir.create(issues, showWarnings = FALSE)

if (identical(args[[2L]], "--numbers")) {
  numbers <- utils::read.delim(file.path(issues, "numbers.tsv"), header = FALSE,
                               col.names = c("id", "number"))
  for (file in Sys.glob(file.path(issues, "*.body.md"))) {
    body <- readLines(file, warn = FALSE)
    for (i in seq_len(nrow(numbers))) {
      body <- gsub(paste0("{{", numbers$id[[i]], "}}"),
                   paste0("#", numbers$number[[i]]), body, fixed = TRUE)
    }
    ids <- sub("[.]body[.]md$", "", basename(Sys.glob(
      file.path(issues, "*.body.md")
    )))
    left <- unlist(regmatches(body, gregexpr("\\{\\{[^{}]+\\}\\}", body)))
    left <- intersect(gsub("[{}]", "", left), ids)
    if (length(left)) {
      stop(basename(file), " refers to a finding without a number: ",
           paste(left, collapse = ", "), call. = FALSE)
    }
    writeLines(body, file)
  }
  quit(status = 0L)
}

read_sections <- function(path) {
  lines <- readLines(path, warn = FALSE)
  heads <- grep("^## ", lines)
  ends <- c(heads[-1L] - 1L, length(lines))
  bodies <- lapply(seq_along(heads), function(i) {
    trimws(paste(lines[seq.int(heads[[i]] + 1L, ends[[i]])], collapse = "\n"),
           whitespace = "\n")
  })
  stats::setNames(bodies, sub("^## ", "", lines[heads]))
}
spec <- read_sections(args[[2L]])
id <- spec$id
candidate <- read_sections(file.path(scratch, "cands", paste0(id, ".md")))
refutation <- read_sections(
  file.path(scratch, "refs", paste0(id, "-refute.md"))
)
# The spec's text where it has one, else the candidate's.
chosen <- function(name, field) {
  if (is.null(spec[[name]])) candidate[[field]] else spec[[name]]
}
section <- function(title, text) {
  if (!is.null(text) && nzchar(text)) c(paste("##", title), "", text, "")
}
body <- c(
  section("What to build", spec$what),
  "## Contract and minimal reproduction", "",
  chosen("contract", "contract_source"), "",
  "```r", chosen("repro", "repro_code"), "```", "",
  paste("**Expected.**", chosen("expected", "expected")), "",
  paste("**Observed.**", chosen("observed", "observed")), "",
  paste("**Native control.**", chosen("control", "native_control")), "",
  "## Cause and counterevidence", "",
  if (is.null(spec$cause)) {
    c(paste0("Cause site: `", candidate$cause_file, "`, ",
             candidate$cause_function, "."), "", refutation$cause_reading)
  } else {
    spec$cause
  }, "",
  paste0(
    "The strongest argument that this is not a defect, from the independent ",
    "refutation (", spec$refuter, ", verdict `", refutation$verdict, "`): ",
    refutation$strongest_argument_against
  ), "",
  section("Decision", spec$decision),
  section("Remedies replayed in memory by the refuter",
          refutation$remedy_tradeoffs),
  section("Acceptance criteria", spec$acceptance),
  "## Backends", "",
  paste("Executed:", chosen("executed", "executed_backends")), "",
  paste("Rendered or read only:",
        chosen("rendered", "rendered_only_backends")), "",
  section("Interacts with", spec$interacts),
  "## Environment and dependencies", "",
  readLines(file.path(scratch, "environment.md"), warn = FALSE), "",
  "## Blocked by", "",
  if (is.null(spec$blocked)) "None (can start immediately)." else spec$blocked
)
# A reader has no use for the session's own paths.
body <- gsub(scratch, "<scratch>", body, fixed = TRUE)
writeLines(body, file.path(issues, paste0(id, ".body.md")))
writeLines(c(spec$title, spec$labels), file.path(issues, paste0(id, ".meta")))
cat(id, ":", sum(nchar(body)), "characters |", spec$title, "|", spec$labels,
    "\n")
