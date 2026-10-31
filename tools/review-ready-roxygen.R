# Proves documentation-equivalent R edits without evaluation.
# Parsed expressions and unsafe directive placement must both remain unchanged.
review_ready_roxygen_source <- function(path) {
  text <- rawToChar(readBin(path, "raw", n = file.info(path)$size))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (!all(validUTF8(lines)) || any(grepl("[[:cntrl:]]", gsub("[\t\r]", "", lines)))) {
    stop("Unsupported R source encoding or control character.", call. = FALSE)
  }
  expressions <- parse(file = path, keep.source = FALSE, encoding = "UTF-8")
  tokens <- utils::getParseData(parse(file = path, keep.source = TRUE, encoding = "UTF-8"))
  if (is.null(tokens)) return(list(expressions = expressions, retained = character()))
  tokens <- tokens[tokens$terminal, , drop = FALSE]
  tokens <- tokens[order(tokens$line1, tokens$col1), , drop = FALSE]
  keep <- tokens$token != "COMMENT"
  allowed <- c("title", "description", "details", "param", "return", "returns",
    "examples", "section", "seealso", "note", "references")
  supported <- TRUE
  previous <- 0L
  for (i in which(tokens$token == "COMMENT")) {
    line <- tokens$line1[[i]]
    comment <- sub("\r$", "", tokens$text[[i]])
    if (grepl("\\bnocov\\b", comment, perl = TRUE)) keep[[i]] <- TRUE
    if (!startsWith(comment, "#'")) {
      keep[[i]] <- keep[[i]] || grepl("^#+['|]", comment)
      next
    }
    if (!grepl("^[ \t]*#'", lines[[line]]) || tokens$line2[[i]] != line) {
      keep[[i]] <- TRUE
      next
    }
    if (line > previous + 1L &&
        any(nzchar(trimws(lines[seq.int(previous + 1L, line - 1L)])))) supported <- TRUE
    content <- sub("^#'[ ]?", "", comment)
    if (grepl("@", content, fixed = TRUE)) {
      tag <- regmatches(content, regexec("^[ \t]*@([A-Za-z]+)([ \t]|$)", content))[[1L]]
      supported <- length(tag) > 0L && tag[[2L]] %in% allowed &&
        !grepl("@", substring(content, nchar(tag[[1L]]) + 1L), fixed = TRUE)
    }
    keep[[i]] <- keep[[i]] || !supported
    previous <- line
  }
  # Code token kinds anchor unsafe directives while ignoring source formatting.
  retained <- ifelse(tokens$token == "COMMENT",
    paste0("COMMENT:", sub("\r$", "", tokens$text)), tokens$token)
  list(expressions = expressions, retained = unname(retained[keep]))
}

# The caller pins both commits. Archives also expose added/deleted R inputs and
# package configuration that an expression comparison alone cannot prove.
# Command mode refuses submitted parse errors before any capacity promotion.
review_ready_roxygen_path <- function(root, base, sha, fail_parse = FALSE) {
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
  if (fail_parse) {
    sources <- list.files(file.path(trees[[2L]], "R"), "^[^/]+[.]R$",
      full.names = TRUE)
    for (source in sources) parse(file = source, keep.source = FALSE, encoding = "UTF-8")
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
  writeLines(review_ready_roxygen_path(".", args[[1L]], args[[2L]],
    fail_parse = TRUE), args[[3L]])
}
