#!/usr/bin/env Rscript
# Prepares one matching JSON cache entry or explicitly removes a selected entry.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L) stop("Cannot locate DuckDB JSON setup.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
source(file.path(dirname(entry), "duckdb-json.R"))

main <- function(args) {
  if (length(args) == 0L) {
    lease <- duckdb_json_acquire()
    on.exit(duckdb_json_release(lease), add = TRUE)
    cat("Prepared DuckDB JSON extension: ", lease$path, "\n", sep = "")
  } else if (length(args) == 3L && identical(args[[1L]], "--remove")) {
    removed <- duckdb_json_cleanup(args[[2L]], args[[3L]])
    cat(if (removed) "Removed DuckDB JSON cache entry.\n" else "No DuckDB JSON cache entry exists.\n")
  } else {
    stop("Usage: Rscript tools/setup-duckdb-json.R [--remove VERSION PLATFORM]",
         call. = FALSE)
  }
}
status <- tryCatch({ main(commandArgs(TRUE)); 0L }, error = function(cnd) {
  cat(conditionMessage(cnd), "\n", file = stderr(), sep = "")
  1L
})
quit(status = status, save = "no")
