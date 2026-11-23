#!/usr/bin/env Rscript

# Keeps the release library current: a repository-external library holding the
# newest repository version of every package the release depends on --
# DESCRIPTION's dependencies plus Config/Needs/preflight and
# Config/Needs/release, closed over their hard dependencies. Stage 4 of
# `tools/cran-release.md` owns when it runs, who may run it, and how the formal
# preflight is pointed at the library. It also refuses an R older than the
# current R release. Nothing tracked changes, nor any other library.
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
library <- file.path(
  tools::R_user_dir("marginplyr", "cache"),
  "release-library",
  paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = ".")
)
if (identical(args, "--library")) {
  cat(library, "\n", sep = "")
  quit(status = 0L)
}
apply <- identical(args, "--apply")

# An unset CRAN mirror would make a non-interactive session prompt for one.
repos <- getOption("repos")
repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
options(repos = repos)
# R's 60-second default cuts off the larger binaries, such as arrow and duckdb.
options(timeout = max(1800, getOption("timeout")))

version_info <- read.dcf(url(paste0(
  repos[["CRAN"]], "/src/base/VERSION-INFO.dcf"
)))
release <- package_version(version_info[[1L, "Release"]])
running <- getRversion()
if (running < release) {
  stop(
    "R ", running, " is older than the current R release ", release,
    "; install it before preparing a CRAN submission.",
    call. = FALSE
  )
}
cat("R ", format(running), " (current release ", format(release), ")\n", sep = "")

# Compare against the build type install.packages() would fetch, so a source
# release whose binary is not built yet is not reported as installable.
type <- getOption("pkgType")
if (identical(type, "both")) type <- .Platform$pkgType
available <- utils::available.packages(type = type)

description <- read.dcf("DESCRIPTION")
fields <- intersect(
  c(
    "Depends", "Imports", "LinkingTo", "Suggests",
    "Config/Needs/preflight", "Config/Needs/release"
  ),
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
unavailable <- setdiff(direct, rownames(available))
if (length(unavailable) > 0L) {
  stop(
    "Not available as ", type, " packages: ",
    paste(unavailable, collapse = ", "),
    call. = FALSE
  )
}
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

# Returns the packages among `packages` that neither the release library nor
# R's own library holds at the repository version. R's library counts because
# R itself puts it on every library path, so a current recommended package
# there needs no copy.
pending <- function(packages) {
  installed <- utils::installed.packages(lib.loc = c(library, .Library))
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
  plan[package_version(plan$available) > held, , drop = FALSE]
}

plan <- pending(packages)
cat("Release library:", library, "\n")
if (nrow(plan) == 0L) {
  cat("All", length(packages), "release dependencies are current.\n")
  quit(status = 0L)
}
plan <- plan[order(plan$package), , drop = FALSE]
print(plan, row.names = FALSE)

if (!apply) {
  cat(
    "\nPreview only: ", nrow(plan), " of ", length(packages),
    " packages to install. Rerun with --apply.\n",
    sep = ""
  )
  quit(status = 0L)
}

dir.create(library, recursive = TRUE, showWarnings = FALSE)
if (file.access(library, 2L) != 0L) {
  stop("The release library is not writable: ", library, call. = FALSE)
}
utils::install.packages(
  plan$package,
  lib = library,
  type = type,
  dependencies = FALSE,
  Ncpus = max(1L, parallel::detectCores(logical = FALSE), na.rm = TRUE)
)

remaining <- pending(packages)
if (nrow(remaining) > 0L) {
  stop(
    "Still missing or outdated after installation: ",
    paste(remaining$package, collapse = ", "),
    call. = FALSE
  )
}
cat("Installed", nrow(plan), "package(s) into", library, "\n")
