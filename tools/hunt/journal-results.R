#!/usr/bin/env Rscript
# Turn a hunt workflow's journal into one Markdown file per result, so the
# coordinator reads a section instead of a whole return.
#
#   Rscript tools/hunt/journal-results.R SCRATCH JOURNAL.jsonl [PREFIX]
#
# A finder's return (a `lane` and its `candidates`) becomes
# SCRATCH/cands/<PREFIX><lane>-<k>.md, PREFIX defaulting to "L"; a refuter's
# return (an `id` and a `verdict`) becomes SCRATCH/refs/<id>-refute.md. Each
# field of a result is a `## field` section. A task's output file cuts a long
# result short; the journal holds all of it.
args <- commandArgs(trailingOnly = TRUE)
scratch <- args[[1L]]
prefix <- if (length(args) >= 3L) args[[3L]] else "L"
entries <- lapply(readLines(args[[2L]], warn = FALSE), function(line) {
  tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
           error = function(cnd) NULL)
})
results <- lapply(Filter(function(entry) {
  identical(entry$type, "result") && !is.null(entry$result)
}, entries), function(entry) {
  value <- entry$result
  if (is.character(value)) {
    value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  }
  value
})
sections <- function(title, fields) {
  c(paste("#", title), "", unlist(lapply(names(fields), function(name) {
    c(paste("##", name), "", as.character(unlist(fields[[name]])), "")
  })))
}
for (directory in c("cands", "refs")) {
  dir.create(file.path(scratch, directory), showWarnings = FALSE,
             recursive = TRUE)
}
for (result in results) {
  if (!is.null(result$candidates)) {
    cat(sprintf("lane %s: %d candidates, %d not reproduced\n", result$lane,
                length(result$candidates), length(result$not_reproduced)))
    for (k in seq_along(result$candidates)) {
      candidate <- result$candidates[[k]]
      id <- paste0(prefix, result$lane, "-", k)
      writeLines(sections(paste("Candidate", id), candidate),
                 file.path(scratch, "cands", paste0(id, ".md")))
      cat(sprintf("  %s [%s | %s] %s\n", id, candidate$severity,
                  candidate$direction, candidate$title))
    }
  } else if (!is.null(result$verdict)) {
    writeLines(sections(paste("Refutation", result$id), result),
               file.path(scratch, "refs", paste0(result$id, "-refute.md")))
    cat(sprintf("refutation %s: %s (%d characters)\n", result$id,
                result$verdict, sum(nchar(unlist(result)))))
  }
}
