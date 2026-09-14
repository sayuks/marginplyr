# Line numbers locate a checktor finding for a reviewer, but they are not its
# identity: unrelated edits move them. Code findings are instead anchored to
# their file and the detected source line; all other findings retain their
# checktor location because it is already line-independent.

checktor_baseline_columns <- c(
  "category", "check", "location", "source", "reason"
)

checktor_baseline_key <- function(x) {
  paste(x$category, x$check, x$location, x$source, sep = "\t")
}

checktor_finding_source_path <- function(package_path, file) {
  direct_path <- file.path(package_path, file)
  if (file.exists(direct_path)) {
    return(direct_path)
  }

  candidates <- list.files(package_path, recursive = TRUE, full.names = TRUE)
  candidates <- candidates[basename(candidates) == file]
  if (length(candidates) != 1L) {
    stop(
      "checktor finding source is not uniquely located: ",
      file,
      " (found ",
      length(candidates), ")"
    )
  }
  candidates[[1L]]
}

checktor_finding_anchors <- function(findings, package_path) {
  required_columns <- c("file", "line", "location")
  missing_columns <- setdiff(required_columns, names(findings))
  if (length(missing_columns) > 0L) {
    stop(
      "checktor issues are missing anchor columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  anchors <- data.frame(
    location = findings$location,
    source = rep("", nrow(findings)),
    stringsAsFactors = FALSE
  )
  source_findings <- !is.na(findings$file) & !is.na(findings$line)
  source_findings <- source_findings & nzchar(findings$file)
  source_findings <- source_findings & findings$line >= 1L
  source_findings <- source_findings & findings$line == as.integer(findings$line)

  for (index in which(source_findings)) {
    source_path <- checktor_finding_source_path(
      package_path,
      findings$file[[index]]
    )
    source_lines <- readLines(source_path, warn = FALSE)
    line <- findings$line[[index]]
    if (line > length(source_lines)) {
      stop(
        "checktor finding line is outside its source file: ",
        findings$location[[index]]
      )
    }
    anchors$location[[index]] <- findings$file[[index]]
    anchors$source[[index]] <- trimws(source_lines[[line]])
  }

  anchors
}

# Compares every current finding to a reviewed baseline exactly once. A second
# finding with the same anchor remains unexpected, so an additional copy of a
# reviewed pattern cannot be accepted merely by sharing its source text.
checktor_baseline_match <- function(baseline, findings, package_path) {
  baseline_keys <- checktor_baseline_key(baseline)
  anchors <- checktor_finding_anchors(findings, package_path)
  finding_keys <- checktor_baseline_key(data.frame(
    category = findings$category,
    check = findings$check,
    location = anchors$location,
    source = anchors$source,
    stringsAsFactors = FALSE
  ))
  baseline_match <- match(finding_keys, baseline_keys)
  unexpected <- is.na(baseline_match) | duplicated(finding_keys)

  list(
    baseline_match = baseline_match,
    unexpected = unexpected,
    stale = !baseline_keys %in% finding_keys
  )
}
