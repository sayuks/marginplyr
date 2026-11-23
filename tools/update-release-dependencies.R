#!/usr/bin/env Rscript

# Keeps the release library current: a repository-external library holding the
# newest repository version of every package the formal preflight depends on
# -- DESCRIPTION's dependencies plus Config/Needs/preflight, closed over their
# hard dependencies -- and marginplyr installed from the working tree. Stage 4
# of `tools/cran-release.md` owns when it runs, who may run it, and how the
# formal preflight is pointed at the library. It also refuses an R older than
# the current R release. Nothing tracked changes, nor any other library.
#
#   Rscript tools/update-release-dependencies.R            # preview
#   Rscript tools/update-release-dependencies.R --apply    # install
#   Rscript tools/update-release-dependencies.R --library  # print the path

source(file.path("tools", "dependency-requirements.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || !all(args %in% c("--apply", "--library"))) {
  stop(
    "Usage: update-release-dependencies.R [--apply | --library]",
    call. = FALSE
  )
}

# One library per R minor version, because installed packages are built for it.
release_library <- file.path(
  tools::R_user_dir("marginplyr", "cache"),
  "release-library",
  paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = ".")
)
if (identical(args, "--library")) {
  cat(release_library, "\n", sep = "")
  quit(status = 0L)
}
install <- identical(args, "--apply")

# A non-interactive session stops on an unset CRAN mirror.
repos <- getOption("repos")
repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
options(repos = repos)
# R's 60-second default cuts off the larger binaries, such as arrow and duckdb.
options(timeout = max(1800, getOption("timeout")))

# The R sources, not the package mirror, say which R is the current release.
version_info <- read.dcf(url(
  "https://cloud.r-project.org/src/base/VERSION-INFO.dcf"
))
current_r <- package_version(version_info[[1L, "Release"]])
running_r <- getRversion()
if (running_r < current_r) {
  stop(
    "R ", running_r, " is older than the current R release ", current_r,
    "; install it before preparing a CRAN submission.",
    call. = FALSE
  )
}
cat("R ", format(running_r), " (current release ", format(current_r), ")\n",
    sep = "")

# Compare against the build type install.packages() would fetch, so a source
# release whose binary is not built yet is not reported as installable.
type <- getOption("pkgType")
if (identical(type, "both")) type <- .Platform$pkgType
available <- utils::available.packages(type = type)

description <- read.dcf("DESCRIPTION")
fields <- intersect(
  c("Depends", "Imports", "LinkingTo", "Suggests", "Config/Needs/preflight"),
  colnames(description)
)
direct <- unique(unlist(lapply(fields, function(field) {
  dependency_requirements(description[[1L, field]])$package
})))
base_packages <- rownames(utils::installed.packages(
  lib.loc = .Library,
  priority = "base"
))
direct <- setdiff(direct, c("R", base_packages))
closure <- tools::package_dependencies(
  direct,
  db = available,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
packages <- setdiff(
  unique(c(direct, unlist(closure, use.names = FALSE))),
  base_packages
)
unavailable <- setdiff(packages, rownames(available))
if (length(unavailable) > 0L) {
  stop(
    "Not available as ", type, " packages: ",
    paste(unavailable, collapse = ", "),
    call. = FALSE
  )
}

# Answers which of `packages` neither the release library nor R's own library
# holds at the repository version, as package, installed, and available
# columns. The caller holds `packages` within `available`. R's library counts
# because R puts it on every library path, so a current recommended package
# there needs no copy.
pending <- function(packages) {
  installed <- utils::installed.packages(lib.loc = c(release_library, .Library))
  newest <- tapply(
    installed[, "Version"],
    installed[, "Package"],
    function(versions) as.character(max(package_version(versions)))
  )
  plan <- data.frame(
    package = packages,
    installed = unname(newest[packages]),
    available = unname(available[packages, "Version"])
  )
  held <- package_version(ifelse(is.na(plan$installed), "0.0", plan$installed))
  plan <- plan[package_version(plan$available) > held, , drop = FALSE]
  plan[order(plan$package), , drop = FALSE]
}

plan <- pending(packages)
cat("Release library:", release_library, "\n")
if (nrow(plan) == 0L) {
  cat("All", length(packages), "release dependencies are current.\n")
} else {
  print(plan, row.names = FALSE)
}

if (!install) {
  cat(
    "\nPreview only: ", nrow(plan), " of ", length(packages),
    " packages to install, then marginplyr from the working tree.",
    " Rerun with --apply.\n",
    sep = ""
  )
  quit(status = 0L)
}

dir.create(release_library, recursive = TRUE, showWarnings = FALSE)
if (file.access(release_library, 2L) != 0L) {
  stop("The release library is not writable: ", release_library, call. = FALSE)
}
if (nrow(plan) > 0L) {
  utils::install.packages(
    plan$package,
    lib = release_library,
    type = type,
    dependencies = FALSE
  )
  remaining <- pending(packages)
  if (nrow(remaining) > 0L) {
    stop(
      "Still missing or outdated after installation: ",
      paste(remaining$package, collapse = ", "),
      call. = FALSE
    )
  }
}

# The vignettes render against the installed marginplyr, and an unchanged
# version can name a different candidate, so the working tree always installs.
status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-test-load", paste0("--library=", shQuote(release_library)), ".")
)
if (status != 0L) {
  stop("Installing marginplyr into the release library failed.", call. = FALSE)
}
cat("Release library is current:", release_library, "\n")
