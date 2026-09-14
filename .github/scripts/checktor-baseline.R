# Line numbers locate a checktor finding for a reviewer, but they are not its
# identity: unrelated edits move them. Code findings are instead anchored to
# their file and the detected source line; all other findings retain their
# checktor location because it is already line-independent.

checktor_baseline_columns <- c(
  "category", "check", "location", "scope", "source", "reason"
)

# Turns a baseline or anchor data frame's identity columns into comparable
# keys. The caller supplies character category, check, location, and source
# columns plus scope, each already normalized for the kind of finding it
# represents.
checktor_baseline_key <- function(x) {
  paste(x$category, x$check, x$location, x$scope, x$source, sep = "\t")
}

# Locates the one package source checktor's file field names. The caller holds
# a package root and a checktor path or a basename unique within that root.
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

# Returns the lexical block openings that contain one source line. The caller
# supplies parsed R source and its lines, and holds that `line` names one of
# those lines. Their ordered spelling distinguishes an unchanged statement
# moved into a different closure from a line-number-only relocation.
checktor_finding_scope <- function(parsed, source_lines, line) {
  opening_braces <- parsed[parsed$token == "'{'", , drop = FALSE]
  blocks <- parsed[match(opening_braces$parent, parsed$id), , drop = FALSE]
  blocks <- blocks[blocks$line1 <= line & blocks$line2 >= line, , drop = FALSE]
  if (nrow(blocks) == 0L) {
    return("")
  }
  blocks <- blocks[order(blocks$line1, blocks$col1), , drop = FALSE]
  paste(trimws(source_lines[blocks$line1]), collapse = " > ")
}

# Returns the complete block statement that holds one finding line. The caller
# supplies parsed R source and its lines, and holds that `line` belongs to one
# statement. Normalizing that statement makes a changed continuation of a
# multiline call distinct without treating earlier line movement as a change.
checktor_finding_expression <- function(parsed, source_lines, line) {
  opening_braces <- parsed[parsed$token == "'{'", , drop = FALSE]
  block_parents <- opening_braces$parent
  statements <- parsed[
    parsed$token == "expr" & parsed$parent %in% block_parents,
    ,
    drop = FALSE
  ]
  statements <- statements[
    statements$line1 <= line & statements$line2 >= line,
    ,
    drop = FALSE
  ]
  if (nrow(statements) == 0L) {
    return(trimws(source_lines[[line]]))
  }
  statements <- statements[order(
    statements$line2 - statements$line1,
    statements$col2 - statements$col1
  ), , drop = FALSE]
  statement <- statements[1L, , drop = FALSE]
  paste(
    trimws(source_lines[seq.int(statement$line1, statement$line2)]),
    collapse = " "
  )
}

# Gives code findings a file-and-source anchor and leaves other checktor
# locations unchanged. The caller supplies checktor's file, line, and location
# columns and a package root containing every named source; code anchors also
# record their lexical scope so one identical line cannot change owners unseen.
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
    scope = rep("", nrow(findings)),
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
    parsed <- utils::getParseData(parse(source_path, keep.source = TRUE))
    line <- findings$line[[index]]
    if (line > length(source_lines)) {
      stop(
        "checktor finding line is outside its source file: ",
        findings$location[[index]]
      )
    }
    anchors$location[[index]] <- findings$file[[index]]
    anchors$scope[[index]] <- checktor_finding_scope(
      parsed,
      source_lines,
      line
    )
    anchors$source[[index]] <- checktor_finding_expression(
      parsed,
      source_lines,
      line
    )
  }

  anchors
}

# Compares checktor findings against a schema-validated reviewed baseline. The
# caller supplies checktor's category, check, file, line, and location columns
# plus the package root they name; the result marks unexpected and stale rows.
# A second finding with one anchor remains unexpected, so an additional copy of
# a reviewed pattern cannot be accepted merely by sharing its source text.
checktor_baseline_match <- function(baseline, findings, package_path) {
  baseline_keys <- checktor_baseline_key(baseline)
  anchors <- checktor_finding_anchors(findings, package_path)
  finding_keys <- checktor_baseline_key(data.frame(
    category = findings$category,
    check = findings$check,
    location = anchors$location,
    scope = anchors$scope,
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
