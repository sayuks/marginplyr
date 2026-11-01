# Installs this source in an isolated library before rendering and verifying
# site output. The common admission manager owns this process and its children.

# Verify the current rendered archive with the pinned checker and its helper.
verify_rendered_site <- function(verification_root) {
  paths <- c(".github", ".github/scripts", ".github/scripts/ci-helpers.R")
  links <- Sys.readlink(file.path(getwd(), paths))
  if (any(!is.na(links) & nzchar(links))) stop("Site verification destination crosses a symlink.", call. = FALSE)
  helper <- file.path(verification_root, ".github/scripts/ci-helpers.R")
  if (!file.copy(helper, ".github/scripts/ci-helpers.R", overwrite = TRUE))
    stop("Pinned site verification helper is unavailable.", call. = FALSE)
  source(file.path(verification_root, ".github/scripts/verify-site.R"), local = new.env(parent = globalenv()))
}

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
  # Quarto's default directory walk skips an absolute hidden ancestor. The
  # request root is under .local; explicit generated inputs preserve its normal
  # nonhidden inventory without replacing a submitted project.render policy.
  project <- file.path("_quarto", "_quarto.yml")
  configuration <- yaml::read_yaml(project)
  hidden <- grepl("(^|/)[.][^/]", normalizePath(getwd()))
  if (hidden && is.null(configuration$project$render)) {
    inputs <- list.files("_quarto", recursive = TRUE,
      pattern = "[.](qmd|md|Rmd|ipynb)$")
    inputs <- inputs[!grepl("(^|/)[._]|(^|/)(README[.]|AGENTS[.]|CLAUDE[.])|[.]llms[.]md$", inputs)]
    if (!length(inputs)) stop("No generated site inputs to render.", call. = FALSE)
    configuration$project$render <- as.list(inputs)
    yaml::write_yaml(configuration, project,
      handlers = list(logical = yaml::verbatim_logical))
    quarto::quarto_render(input = "_quarto", quiet = FALSE,
      as_job = FALSE, use_freezer = FALSE)
  }
  verify_rendered_site(original_directory)
  cat(
    "Completed site artifact verification:",
    "installed source, rendered site and output markers.\n"
  )
}

verify_site_artifacts()
