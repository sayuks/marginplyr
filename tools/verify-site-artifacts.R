# Installs this source in an isolated library before rendering and verifying
# site output. The common admission manager owns this process and its children.
verify_site_artifacts <- function() {
  workspace <- tempfile("marginplyr-site-")
  dir.create(workspace)
  original_directory <- getwd()
  on.exit({
    setwd(original_directory)
    unlink(workspace, recursive = TRUE)
  }, add = TRUE)
  archive <- file.path(workspace, "source.tar")
  status <- system2("git", shQuote(c(
    "archive", "--format=tar", paste0("--output=", archive), "HEAD"
  )))
  if (status != 0L) stop("Cannot archive pinned site source.", call. = FALSE)
  source <- file.path(workspace, "source")
  dir.create(source)
  utils::untar(archive, exdir = source)
  # A hidden checkout ancestor hides Quarto inputs; render the committed archive.
  setwd(source)
  library <- file.path(workspace, "library")
  dir.create(library)
  previous <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(previous)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = previous)
    }
  }, add = TRUE)
  libraries <- .libPaths()
  on.exit(.libPaths(libraries), add = TRUE)
  Sys.setenv(R_LIBS = paste(
    c(library, libraries), collapse = .Platform$path.sep
  ))
  .libPaths(c(library, libraries))
  status <- system2(file.path(R.home("bin"), "R"), shQuote(c(
    "CMD", "INSTALL", paste0("--library=", library), "."
  )))
  if (status != 0L) stop("Site package installation failed.", call. = FALSE)
  altdoc::render_docs(parallel = FALSE, freeze = FALSE)
  source(".github/scripts/verify-site.R", local = new.env(parent = globalenv()))
  cat(
    "Completed site artifact verification:",
    "installed source, rendered site and output markers.\n"
  )
}

verify_site_artifacts()
