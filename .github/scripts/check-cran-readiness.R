# Runs checktor over the package directory or source tarball given as the first
# argument and writes a Markdown report to the optional second argument. CI and
# local release checks both pass the built source tarball:
#
#   R CMD build .
#   Rscript .github/scripts/check-cran-readiness.R marginplyr_*.tar.gz
#
# checktor 0.1.0 has no suppression interface. The baseline therefore records
# reviewed findings exactly, and this script fails for either a new finding or
# a stale baseline entry. The latter makes a checktor upgrade or a resolved
# false positive remove its exception instead of leaving it able to mask a
# future recurrence.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: check-cran-readiness.R <package-path> [report-path]")
}

input_path <- args[[1L]]
report_path <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path("checktor-report", "report.md")
}
baseline_path <- file.path(".github", "checktor-baseline.tsv")

package_path <- input_path
if (file.exists(input_path) && grepl("[.]tar[.]gz$", input_path)) {
  extracted <- tempfile("marginplyr-checktor-")
  dir.create(extracted)
  utils::untar(input_path, exdir = extracted)
  candidates <- list.dirs(extracted, recursive = FALSE, full.names = TRUE)
  candidates <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))]
  if (length(candidates) != 1L) {
    stop("The source tarball must contain exactly one R package.")
  }
  package_path <- candidates[[1L]]
}

required_columns <- c("category", "check", "location", "reason")
baseline <- utils::read.delim(
  baseline_path,
  colClasses = "character",
  check.names = FALSE,
  na.strings = character()
)
if (!identical(names(baseline), required_columns)) {
  stop(
    "checktor baseline columns must be: ",
    paste(required_columns, collapse = ", ")
  )
}
if (any(!nzchar(baseline$reason))) {
  stop("Every checktor baseline entry must state a reason.")
}

finding_key <- function(x) {
  paste(x$category, x$check, x$location, sep = "\t")
}

baseline_keys <- finding_key(baseline)
if (anyDuplicated(baseline_keys)) {
  stop("The checktor baseline contains duplicate entries.")
}

dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)

results <- tryCatch(
  checktor::checktor(package_path, verbose = FALSE, progress = FALSE),
  error = function(cnd) cnd
)
if (inherits(results, "condition")) {
  report <- c(
    "# checktor CRAN preflight",
    "",
    "**Gate: FAILED**",
    "",
    "checktor could not complete:",
    "",
    "```text",
    conditionMessage(results),
    "```"
  )
  writeLines(report, report_path)
  writeLines(report)
  quit(status = 1L)
}

findings <- checktor::issues(results)
finding_columns <- c("category", "check", "location", "message")
missing_columns <- setdiff(finding_columns, names(findings))
if (length(missing_columns) > 0L) {
  stop(
    "checktor issues are missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

finding_keys <- finding_key(findings)
unexpected <- !finding_keys %in% baseline_keys
stale <- !baseline_keys %in% finding_keys
gate_passed <- !any(unexpected) && !any(stale)

escape_markdown <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("[\r\n]+", " ", x)
  gsub("|", "\\\\|", x, fixed = TRUE)
}

markdown_table <- function(x, columns) {
  if (nrow(x) == 0L) {
    return("_None._")
  }
  x <- x[, columns, drop = FALSE]
  x[] <- lapply(x, escape_markdown)
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  )
}

if (nrow(findings) > 0L) {
  baseline_match <- match(finding_keys, baseline_keys)
  findings$disposition <- ifelse(unexpected, "unexpected", "accepted baseline")
  findings$reason <- ifelse(unexpected, "", baseline$reason[baseline_match])
} else {
  findings$disposition <- character()
  findings$reason <- character()
}

stale_entries <- baseline[stale, , drop = FALSE]
raw_report <- checktor::health_report(results, format = "markdown")
report <- c(
  "# checktor CRAN preflight",
  "",
  paste0("**Gate: ", if (gate_passed) "PASSED" else "FAILED", "**"),
  "",
  paste0("- Package: `", normalizePath(package_path), "`"),
  paste0("- checktor: ", as.character(utils::packageVersion("checktor"))),
  paste0("- Findings: ", nrow(findings)),
  paste0("- Accepted baseline: ", sum(!unexpected)),
  paste0("- Unexpected: ", sum(unexpected)),
  paste0("- Stale baseline entries: ", sum(stale)),
  "",
  "## Findings and dispositions",
  "",
  markdown_table(
    findings,
    c("category", "check", "location", "message", "disposition", "reason")
  ),
  "",
  "## Stale baseline entries",
  "",
  markdown_table(stale_entries, required_columns),
  "",
  "## Native checktor report",
  "",
  raw_report
)

writeLines(report, report_path)
writeLines(report)
if (!gate_passed) {
  quit(status = 1L)
}
