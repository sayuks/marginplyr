#!/usr/bin/env Rscript
# What the tracker, the investigation notes and the tests already hold about
# one hunt candidate, for its refuter to judge. Replaces a lookup agent.
#
#   Rscript tools/hunt/known-issues.R CANDIDATE.md
#
# Run from the repository root. CANDIDATE.md is a file
# `tools/hunt/journal-results.R` wrote; the result is appended to it as a
# `## known` section, and a second run replaces that section.
path <- commandArgs(trailingOnly = TRUE)[[1L]]
lines <- readLines(path, warn = FALSE)
known <- which(lines == "## known")
if (length(known)) lines <- lines[seq_len(known[[1L]] - 1L)]
section <- function(name) {
  start <- which(lines == paste("##", name))
  if (!length(start)) return("")
  rest <- lines[-seq_len(start[[1L]])]
  end <- grep("^## ", rest)
  paste(if (length(end)) rest[seq_len(end[[1L]] - 1L)] else rest,
        collapse = " ")
}
# Search terms: the names the candidate writes as code, cause first.
identifiers <- function(text) {
  quoted <- unlist(regmatches(text, gregexpr("`[^`]+`", text)))
  bare <- unlist(regmatches(text, gregexpr(
    "[A-Za-z.][A-Za-z0-9._]*(?=\\()|[a-z]+(_[a-z0-9]+)+", text, perl = TRUE
  )))
  terms <- gsub("[`()]", "", c(bare, quoted))
  unique(terms[nchar(terms) >= 4L & !grepl("[ /]", terms)])
}
terms <- utils::head(unique(c(
  identifiers(section("cause_function")), identifiers(section("title"))
)), 6L)

issues <- do.call(rbind, lapply(terms, function(term) {
  json <- system2("gh", c(
    "issue", "list", "--state", "all", "--limit", "8", "--search",
    shQuote(paste0('"', term, '" in:title,body')), "--json",
    "number,title,state,stateReason,labels"
  ), stdout = TRUE, stderr = FALSE)
  found <- jsonlite::fromJSON(paste(json, collapse = ""))
  if (!length(found) || !nrow(found)) return(NULL)
  data.frame(
    term = term, number = found$number, state = found$state,
    reason = ifelse(is.na(found$stateReason), "", found$stateReason),
    labels = vapply(found$labels, function(label) {
      paste(label$name, collapse = ",")
    }, character(1)), title = found$title
  )
}))
files <- c(Sys.glob("investigation/*.md"), Sys.glob("tests/testthat/*.R"),
           Sys.glob("design/adr/*.md"))
mentions <- lapply(stats::setNames(terms, terms), function(term) {
  hit <- vapply(files, function(file) {
    any(grepl(term, readLines(file, warn = FALSE), fixed = TRUE))
  }, logical(1))
  files[hit]
})

report <- c("## known", "")
if (is.null(issues)) {
  report <- c(report, "No issue matches a search term.", "")
} else {
  merged <- stats::aggregate(term ~ number + state + reason + labels + title,
                             issues, paste, collapse = ", ")
  merged <- merged[order(-merged$number), ]
  report <- c(report, sprintf(
    "- #%d (%s%s; %s) %s [matched: %s]", merged$number, merged$state,
    ifelse(nzchar(merged$reason), paste0(", ", merged$reason), ""),
    merged$labels, merged$title, merged$term
  ), "")
}
for (term in terms) {
  report <- c(report, sprintf(
    "- `%s`: %s", term, if (length(mentions[[term]])) {
      paste(utils::head(mentions[[term]], 12L), collapse = ", ")
    } else {
      "in no note, test or ADR"
    }
  ))
}
writeLines(c(lines, if (nzchar(utils::tail(lines, 1L))) "", report), path)
cat(path, ":", length(terms), "terms,",
    if (is.null(issues)) 0L else length(unique(issues$number)), "issues\n")
