# Returns committed-tree byte hashes, including all generated artifact paths.
review_ready_generated_hashes <- function(root) {
  paths <- sort(list.files(root, recursive = TRUE, all.files = TRUE))
  # Quarto's execution cache is not a generated publication artifact.
  paths <- paths[!grepl("(^|/)[.]quarto/", paths)]
  hashes <- tools::md5sum(file.path(root, paths))
  stats::setNames(unname(hashes), paths)
}

# Regeneration owns its installation in a temporary library inherited by Quarto.
run_review_ready_generated <- function(root = ".") {
  workspace <- tempfile("marginplyr-generated-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  archive <- file.path(workspace, "source.tar")
  status <- system2("git", c("-C", shQuote(normalizePath(root)), "archive",
    "--format=tar", shQuote(paste0("--output=", archive)), "HEAD"))
  if (status != 0L) stop("Cannot archive generated-documentation inputs.", call. = FALSE)
  copy <- file.path(workspace, "source")
  dir.create(copy)
  utils::untar(archive, exdir = copy)
  before <- review_ready_generated_hashes(copy)
  unlink(list.files(file.path(copy, "man", "figures"), "^README-", full.names = TRUE))
  library <- file.path(workspace, "library")
  dir.create(library)
  original <- setwd(copy)
  on.exit(setwd(original), add = TRUE)
  cat("Regenerating README/help; renderer versions:\n")
  code <- paste0(
    ".libPaths(c(", deparse(library), ", .libPaths())); ",
    "Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)); ",
    "cat(system2('quarto', '--version', stdout = TRUE), '\\n'); ",
    "cat(system2('quarto', c('pandoc', '--version'), stdout = TRUE)[1L], '\\n'); ",
    "roxygen2::roxygenise(); devtools::build_readme(quiet = FALSE)"
  )
  status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(code)))
  if (status != 0L) stop("Documentation regeneration failed.", call. = FALSE)
  after <- review_ready_generated_hashes(copy)
  changed <- union(names(before), names(after))
  changed <- changed[is.na(before[changed]) | is.na(after[changed]) |
    before[changed] != after[changed]]
  if (length(changed)) {
    stop("Stale generated documentation (or Quarto/Pandoc writer drift): ",
      paste(changed, collapse = ", "), ". Regenerate and commit all outputs.", call. = FALSE)
  }
  cat("Committed README, NAMESPACE, help and figures match regeneration.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) run_review_ready_generated()
