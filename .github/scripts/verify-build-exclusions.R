# Builds a disposable checkout containing synthetic local worktrees, then checks
# the source tarball and its installation. A clean archive alone cannot expose
# local-checkout contamination (#788); the payload is added after extraction.
# Run from the repository root with Rscript .github/scripts/verify-build-exclusions.R.

verify_build_exclusions <- function() {
  repository <- normalizePath(".", mustWork = TRUE)
  fixture <- tempfile("marginplyr build exclusions ")
  dir.create(fixture)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  original_directory <- getwd()
  on.exit(setwd(original_directory), add = TRUE, after = FALSE)

  run <- function(command, arguments, label) {
    log <- file.path(fixture, paste0(label, ".log"))
    status <- system2(command, shQuote(arguments), stdout = log, stderr = log)
    if (status != 0L) {
      cat(readLines(log, warn = FALSE), sep = "\n")
      stop(sprintf("%s failed with status %d.", label, status), call. = FALSE)
    }
  }

  checkout <- file.path(fixture, "marginplyr")
  dir.create(checkout)
  archive <- file.path(fixture, "checkout.tar")
  run("git", c("archive", "HEAD", "-o", archive), "archive")
  utils::untar(archive, exdir = checkout)
  # Exercise the current exclusion file even before its change is committed.
  stopifnot(file.copy(
    file.path(repository, ".Rbuildignore"), checkout, overwrite = TRUE
  ))

  payload <- file.path(checkout, ".worktrees", c(
    "alpha/.git", "alpha/R/local-only.R", "beta/nested/.git",
    "beta/non-portable:name.txt"
  ))
  for (path in payload) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(
      if (basename(path) == ".git") {
        "gitdir: /synthetic/local/worktree/reference"
      } else {
        "synthetic local worktree payload"
      },
      path
    )
  }
  before <- tools::md5sum(payload)

  source_files <- c(
    "DESCRIPTION", "NAMESPACE", "LICENSE",
    paste0("R/", list.files(file.path(checkout, "R"), recursive = TRUE)),
    paste0("man/", list.files(file.path(checkout, "man"), recursive = TRUE))
  )
  installed_files <- list.files(file.path(checkout, "inst"), recursive = TRUE)
  setwd(fixture)
  r <- file.path(R.home("bin"), "R")
  run(r, c("CMD", "build", "--no-build-vignettes", checkout), "build")
  tarballs <- list.files(fixture, pattern = "^marginplyr_.*\\.tar\\.gz$")
  stopifnot(length(tarballs) == 1L)
  tarball <- tarballs[[1L]]
  members <- utils::untar(tarball, list = TRUE)
  members <- sub("^marginplyr/", "", members)
  if (any(grepl("^\\.worktrees(/|$)", members))) {
    stop("Local worktrees entered the source tarball.", call. = FALSE)
  }
  stopifnot(all(c(source_files, paste0("inst/", installed_files)) %in% members))

  # R CMD check installs this same tarball into its isolated check directory.
  run(r, c(
    "CMD", "check", "--no-tests", "--no-examples", "--no-manual",
    "--ignore-vignettes", tarball
  ), "check")
  check_directory <- file.path(fixture, "marginplyr.Rcheck")
  check_log <- readLines(file.path(check_directory, "00check.log"))
  for (diagnostic in c(
    "checking for hidden files and directories ... OK",
    "checking for portable file names ... OK"
  )) {
    stopifnot(any(grepl(diagnostic, check_log, fixed = TRUE)))
  }
  installation <- file.path(check_directory, "marginplyr")
  installed <- list.files(
    installation, recursive = TRUE, all.files = TRUE, include.dirs = TRUE
  )
  stopifnot(
    dir.exists(installation),
    !any(grepl("(^|/)\\.worktrees(/|$)", installed)),
    all(c("DESCRIPTION", "NAMESPACE", "R/marginplyr", installed_files) %in%
      installed),
    identical(before, tools::md5sum(payload))
  )
  stopifnot(identical(
    unname(tools::md5sum(file.path(checkout, "inst", installed_files))),
    unname(tools::md5sum(file.path(installation, installed_files)))
  ))
  message(paste(
    "Verified: synthetic local worktrees are unchanged and absent from the",
    "source tarball and installation; package sources and installed files",
    "are present; hidden-file and portable-path checks pass."
  ))
}

verify_build_exclusions()
