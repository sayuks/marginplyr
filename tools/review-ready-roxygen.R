# Proves R documentation eligibility after admission, without evaluating code.
# Unknown tags and ordinary comments remain part of the unchanged source proof.
review_ready_roxygen_source <- function(path) {
  text <- rawToChar(readBin(path, "raw", n = file.info(path)$size))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (!all(validUTF8(lines)) || any(grepl("[[:cntrl:]]", gsub("[\t\r]", "", lines)))) {
    stop("Unsupported R source encoding or control character.", call. = FALSE)
  }
  parsed <- parse(text = text, keep.source = TRUE)
  tokens <- utils::getParseData(parsed)
  comments <- tokens[tokens$token == "COMMENT", , drop = FALSE]
  documentation <- rep(FALSE, length(lines))
  allowed <- c("title", "description", "details", "param", "return", "returns",
    "examples", "section", "seealso", "note", "references")
  supported <- TRUE
  previous <- 0L
  for (i in seq_len(nrow(comments))) {
    line <- comments$line1[[i]]
    line_text <- lines[[line]]
    if (!grepl("^[ \t]*#'", line_text) || comments$line2[[i]] != line) next
    if (line > previous + 1L &&
        any(nzchar(trimws(lines[seq.int(previous + 1L, line - 1L)])))) supported <- TRUE
    content <- sub("^[ \t]*#'[ ]?", "", line_text)
    if (grepl("@", content, fixed = TRUE)) {
      tag <- regmatches(content, regexec("^[ \t]*@([A-Za-z]+)([ \t]|$)", content))[[1L]]
      supported <- length(tag) > 0L && tag[[2L]] %in% allowed &&
        !grepl("@", substring(content, nchar(tag[[1L]]) + 1L), fixed = TRUE)
    }
    documentation[[line]] <- supported && !grepl("#[ \t]*(nolint|nocov)", content)
    previous <- line
  }
  blank <- !nzchar(trimws(lines))
  adjacent <- documentation
  repeat {
    expanded <- adjacent | (blank &
      (c(FALSE, head(adjacent, -1L)) | c(tail(adjacent, -1L), FALSE)))
    if (identical(expanded, adjacent)) break
    adjacent <- expanded
  }
  endings <- rep("\n", length(lines))
  if (length(lines) && !endsWith(text, "\n")) endings[[length(lines)]] <- ""
  list(expressions = parse(text = text, keep.source = FALSE),
    retained = paste0(lines, endings)[!adjacent])
}

# The caller pins both commits. Archives also expose added/deleted R inputs and
# package configuration that a comment-only expression comparison cannot prove.
review_ready_roxygen_path <- function(root, base, sha) {
  workspace <- tempfile("review-ready-roxygen-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  trees <- file.path(workspace, c("base", "submitted"))
  for (i in seq_along(trees)) {
    dir.create(trees[[i]])
    archive <- file.path(workspace, paste0(i, ".tar"))
    result <- review_ready_run_system("git",
      c("archive", "--format=tar", paste0("--output=", archive), c(base, sha)[[i]]),
      directory = root, capture = TRUE)
    if (result$status != 0L) stop("Cannot archive pinned R comparison.", call. = FALSE)
    utils::untar(archive, exdir = trees[[i]])
  }
  compare <- function() {
    inputs <- lapply(trees, function(tree) list.files(file.path(tree, "R"),
      recursive = TRUE, all.files = TRUE))
    if (!identical(inputs[[1L]], inputs[[2L]])) return(FALSE)
    config <- c("DESCRIPTION", "NAMESPACE", ".Rbuildignore", ".Rinstignore",
      ".Rprofile", ".RData")
    for (name in config) {
      paths <- file.path(trees, name)
      exists <- file.exists(paths)
      if (!identical(exists[[1L]], exists[[2L]])) return(FALSE)
      if (all(exists) && length(unique(unname(tools::md5sum(paths)))) != 1L) return(FALSE)
    }
    for (name in inputs[[1L]]) {
      paths <- file.path(trees, "R", name)
      if (length(unique(unname(tools::md5sum(paths)))) == 1L) next
      if (!grepl("^[^/]+[.]R$", name)) return(FALSE)
      before <- review_ready_roxygen_source(paths[[1L]])
      after <- review_ready_roxygen_source(paths[[2L]])
      if (!identical(before, after)) return(FALSE)
    }
    TRUE
  }
  eligible <- tryCatch(compare(), error = function(cnd) {
    cat("R documentation proof unavailable: ", conditionMessage(cnd), "\n", sep = "")
    FALSE
  })
  path <- if (eligible) "Documentation" else "Full"
  cat("R documentation proof: ", path,
    if (eligible) " (expressions and package configuration unchanged).\n" else
      " (unsupported or behavior-changing R/configuration edit).\n", sep = "")
  path
}

if (sys.nframe() == 0L) {
  source("tools/review-ready-check-lib.R")
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L || !all(grepl("^[0-9a-f]{40}$", args[1:2]))) {
    stop("Pinned base, submitted commit and result path are required.", call. = FALSE)
  }
  writeLines(review_ready_roxygen_path(".", args[[1L]], args[[2L]]), args[[3L]])
}
