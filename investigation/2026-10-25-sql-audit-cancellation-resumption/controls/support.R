# Helpers for the retained investigation probes, run from the repository root.
control_dir <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])))

capture_output <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply a fresh output path explicitly.")
  }
  parent <- normalizePath(dirname(path), mustWork = TRUE)
  target <- file.path(parent, basename(path))
  retained <- normalizePath(file.path(control_dir, "evidence"))
  if (identical(target, retained) || startsWith(target, paste0(retained, "/"))) {
    stop("Fresh probes must not write into retained evidence.")
  }
  if (file.exists(target)) stop("Output exists; choose a fresh output path.")
  target
}

load_checkout <- function() {
  if (!file.exists("DESCRIPTION") ||
      read.dcf("DESCRIPTION")[[1L, "Package"]] != "marginplyr") {
    stop("Run this probe from the marginplyr repository root.")
  }
  pkgload::load_all(quiet = TRUE)
}

capture_root <- function(args) {
  if (length(args)) normalizePath(args[[1L]]) else file.path(control_dir, "evidence")
}
