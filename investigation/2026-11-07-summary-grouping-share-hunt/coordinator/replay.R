# Explicit research replay, independent of package checks.
# Arguments: archived source root, retained evidence root, output directory.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
source_root <- normalizePath(args[[1L]], mustWork = TRUE)
evidence_root <- normalizePath(args[[2L]], mustWork = TRUE)
dir.create(args[[3L]], recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(args[[3L]], mustWork = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
jobs <- list()
add <- function(name, script, arguments = character(), expected = 0L) {
  jobs[[length(jobs) + 1L]] <<- list(name = name, script = script,
                                  args = arguments, expected = expected)
}
add("share-observation", "share/value-boundaries.R")
add("share-controls", "share/value-boundaries.R", c("all", "--assert", "--controls-only"))
for (family in c("listflow", "listwrite", "partial", "environment", "deferred")) {
  add(paste0("share-", family, "-contract"), "share/value-boundaries.R", c(family, "--assert"), 1L)
}
add("summary-observation", "summary/across-namespace-shape.R")
add("summary-controls", "summary/across-namespace-shape.R", "--controls-only")
add("summary-contract", "summary/across-namespace-shape.R", "--assert", 1L)
add("grouping-observation", "grouping/dynamic-dispatch.R")
add("grouping-contract", "grouping/dynamic-dispatch.R", "--assert", 1L)
add("grouping-matrix", "grouping/dynamic-dispatch-matrix.R")
add("grouping-controls", "grouping/dynamic-dispatch-matrix.R", "--controls-only")
add("sql-numeric", "sql/sql-boundaries.R", source_root)
add("sql-names", "sql/sql-names.R", source_root)
add("sql-upstream-controls", "sql/sql-upstream-controls.R")
setwd(source_root)
rows <- list()
for (job in jobs) {
  script <- file.path(output, job$script)
  dir.create(dirname(script), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file.path(evidence_root, job$script), script, overwrite = TRUE))
  log <- file.path(output, paste0(job$name, ".log"))
  result <- processx::run(file.path(R.home("bin"), "Rscript"),
    c(script, job$args), stdout = log, stderr = "2>&1", error_on_status = FALSE,
    timeout = 600)
  rows[[length(rows) + 1L]] <- data.frame(name = job$name, script = job$script,
    args = paste(job$args, collapse = " "), md5 = unname(tools::md5sum(script)),
    expected = job$expected, observed = result$status)
  write.csv(do.call(rbind, rows), file.path(output, "replay-results.csv"), row.names = FALSE)
  cat(job$name, "expected", job$expected, "observed", result$status, "\n")
  stopifnot(identical(as.integer(result$status), job$expected))
}
