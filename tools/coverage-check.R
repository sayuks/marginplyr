#!/usr/bin/env Rscript

# One line-coverage verdict for CI and explicit local measurements. An output path
# writes the same measured result as Cobertura after the verdict is calculated.
# A shard measures part of the test files and saves its raw result; a merge of
# every shard's result reaches the verdict the unsharded run would.

args <- commandArgs(trailingOnly = TRUE)
usage <- paste0("Usage: coverage-check.R [--cobertura <output>]\n",
  "       coverage-check.R --shard <index>/<count> --output <result.rds>\n",
  "       coverage-check.R --merge <directory> [--cobertura <output>]")
option <- function(name) {
  position <- match(name, args)
  if (is.na(position)) NULL else args[[position + 1L]]
}
flags <- if (length(args) %% 2L == 0L) sort(args[c(TRUE, FALSE)]) else NA
mode <- if (identical(flags, character()) || identical(flags, "--cobertura")) {
  "check"
} else if (identical(flags, c("--output", "--shard")) &&
           grepl("^[0-9]+/[0-9]+$", option("--shard"))) {
  "shard"
} else if (identical(flags, "--merge") ||
           identical(flags, c("--cobertura", "--merge"))) {
  "merge"
} else {
  stop(usage, call. = FALSE)
}

source("tools/coverage-check-lib.R")
status <- switch(mode,
  check = coverage_check(option("--cobertura")),
  shard = {
    shard <- as.integer(strsplit(option("--shard"), "/", fixed = TRUE)[[1L]])
    coverage_check_shard(shard[[1L]], shard[[2L]], option("--output"))
  },
  merge = coverage_verdict(coverage_merge(option("--merge")),
    option("--cobertura"))
)
quit(status = status, save = "no")
