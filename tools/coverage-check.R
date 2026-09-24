#!/usr/bin/env Rscript

# One line-coverage verdict for local review and CI. An optional output path
# writes the same measured result as Cobertura after the verdict is calculated.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0L &&
    (length(args) != 2L || !identical(args[[1L]], "--cobertura"))) {
  stop("Usage: coverage-check.R [--cobertura <output>]", call. = FALSE)
}

source("tools/coverage-check-lib.R")
status <- coverage_check(if (length(args) == 2L) args[[2L]] else NULL)
quit(status = status, save = "no")
