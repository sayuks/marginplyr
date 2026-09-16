# Parses one DCF dependency field into package, operator, and version columns.
dependency_requirements <- function(field) {
  if (length(field) == 0L || is.na(field) || !nzchar(field)) {
    return(data.frame(
      package = character(),
      operator = character(),
      version = character()
    ))
  }
  entries <- trimws(strsplit(gsub("\n", " ", field), ",", fixed = TRUE)[[1L]])
  pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)",
    "(?:[[:space:]]*\\((>=|<=|==|>|<)[[:space:]]*([^()]+)\\))?$"
  )
  matched <- regexec(pattern, entries, perl = TRUE)
  parts <- regmatches(entries, matched)
  if (any(lengths(parts) == 0L)) {
    stop(
      "Cannot parse dependency requirement(s): ",
      paste(entries[lengths(parts) == 0L], collapse = ", "),
      call. = FALSE
    )
  }
  data.frame(
    package = vapply(parts, `[[`, character(1), 2L),
    operator = vapply(parts, function(part) {
      if (length(part) >= 3L) part[[3L]] else ""
    }, character(1)),
    version = vapply(parts, function(part) {
      if (length(part) >= 4L) trimws(part[[4L]]) else ""
    }, character(1)),
    stringsAsFactors = FALSE
  )
}
