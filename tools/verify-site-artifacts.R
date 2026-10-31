# Installs this source in an isolated library before rendering and verifying
# site output. The common admission manager owns this process and its children.
verify_site_artifacts <- function() {
  library <- tempfile("marginplyr-site-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)
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
