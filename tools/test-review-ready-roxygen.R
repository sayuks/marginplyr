# Real committed-tree fixtures at the admitted R eligibility boundary.
source("tools/review-ready-check-lib.R")
source("tools/review-ready-roxygen.R")

local({
  root <- tempfile("roxygen-eligibility-fixtures-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  git <- function(...) review_ready_git_output(root, c(...))
  writeLines(c("Package: fixture", "Version: 0.0.1", "Collate: 'fixture.R'"),
    file.path(root, "DESCRIPTION"))
  writeLines("export(fixture)", file.path(root, "NAMESPACE"))
  original <- c("#' Fixture", "#'", "#' Description.", "#' @param x Input.",
    "#' @return The input.", "#' @export", "#' @examples", "#' fixture(1)",
    "fixture <- function(x) {", "  # ordinary comment", "  x", "}",
    "literal <- \"#' @export # string\"", "", "other <- function() 1")
  source_path <- file.path(root, "R", "fixture.R")
  writeLines(original, source_path)
  git("init", "-q")
  git("config", "user.name", "Fixture")
  git("config", "user.email", "fixture@example.com")
  git("add", ".")
  git("commit", "-qm", "baseline")
  base <- git("rev-parse", "HEAD")[[1L]]
  check <- function(lines = original, expected, label, configure = function() NULL) {
    git("reset", "--hard", base)
    git("clean", "-fd")
    writeLines(lines, source_path)
    configure()
    git("add", "-A")
    git("commit", "--allow-empty", "-qm", label)
    actual <- review_ready_roxygen_path(root, base, git("rev-parse", "HEAD")[[1L]])
    if (!identical(actual, expected)) stop(label, ": expected ", expected, ", got ", actual)
  }
  check(sub("Description.", "A new description.", original, fixed = TRUE),
    "Documentation", "description")
  check(sub("fixture(1)", "fixture(2) # example comment", original, fixed = TRUE),
    "Documentation", "example")
  check(c("", original), "Documentation", "associated whitespace")
  check(sub("  x", "  x + 1", original, fixed = TRUE), "Full", "executable change")
  check(sub("# string", "# changed string", original, fixed = TRUE),
    "Full", "comment markers inside strings")
  check(sub("ordinary comment", "nocov start", original, fixed = TRUE),
    "Full", "ordinary coverage directive")
  check(sub("ordinary comment", "nolint: object_usage_linter.", original, fixed = TRUE),
    "Full", "ordinary lint directive")
  check(sub("Description.", "Description. # nolint: line_length_linter.", original,
    fixed = TRUE), "Full", "roxygen lint directive")
  for (tag in c("export other", "importFrom stats median", "evalNamespace 'export(other)'",
      "evalRd 'text'", "rawNamespace export(other)", "useDynLib fixture", "include other.R",
      "order 1", "unknown text")) {
    check(sub("@export", paste0("@", tag), original, fixed = TRUE), "Full", tag)
  }
  check(sub("  x", "  x # inline comment", original, fixed = TRUE),
    "Full", "inline comment")
  check(c(original[1:14], "", original[15]), "Full", "unassociated whitespace")
  check(expected = "Documentation", label = "final newline", configure = function() {
    cat(paste(original, collapse = "\n"), file = source_path)
  })
  check(expected = "Full", label = "executable line endings", configure = function() {
    cat(paste0(paste(original, collapse = "\r\n"), "\r\n"), file = source_path)
  })
  check(sub("@export", "@export\n#' continuation", original, fixed = TRUE),
    "Full", "unsafe directive continuation")
  check(sub("  x", "  (", original, fixed = TRUE), "Full", "parse failure")
  check(expected = "Full", label = "configuration", configure = function() {
    writeLines("export(other)", file.path(root, "NAMESPACE"))
  })
  check(expected = "Full", label = "load order", configure = function() {
    cat("\nImports: stats\n", file = file.path(root, "DESCRIPTION"), append = TRUE)
  })
  check(expected = "Full", label = "added source", configure = function() {
    writeLines("#' Only prose", file.path(root, "R", "added.R"))
  })
  check(expected = "Full", label = "deleted source", configure = function() unlink(source_path))
  check(expected = "Full", label = "unsupported R input", configure = function() {
    dir.create(file.path(root, "R", "nested"))
    writeLines("#' Prose", file.path(root, "R", "nested", "source.R"))
  })
  cat("Pinned R documentation eligibility fixtures passed.\n")
})
