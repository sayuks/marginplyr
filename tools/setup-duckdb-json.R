#!/usr/bin/env Rscript
# Prepares one matching JSON cache entry or explicitly removes a selected entry.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L) stop("Cannot locate DuckDB JSON setup.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
source(file.path(dirname(entry), "duckdb-json.R"))

main <- function(args) {
  cache <- duckdb_json_default_cache()
  cache_option <- which(args == "--cache")
  if (length(cache_option)) {
    if (length(cache_option) != 1L || cache_option[[1L]] == length(args)) {
      stop("Usage: Rscript tools/setup-duckdb-json.R [--remove VERSION PLATFORM] [--cache DIR]",
           call. = FALSE)
    }
    position <- cache_option[[1L]]
    cache <- args[[position + 1L]]
    args <- args[-c(position, position + 1L)]
  }
  if (length(args) == 0L) {
    lease <- duckdb_json_acquire(cache = cache)
    on.exit(duckdb_json_release(lease), add = TRUE)
    cat("Prepared DuckDB JSON extension: ", lease$path, "\n", sep = "")
  } else if (length(args) == 3L && identical(args[[1L]], "--remove")) {
    removed <- duckdb_json_cleanup(args[[2L]], args[[3L]], cache)
    cat(if (removed) "Removed DuckDB JSON cache entry.\n" else "No DuckDB JSON cache entry exists.\n")
  } else {
    stop("Usage: Rscript tools/setup-duckdb-json.R [--remove VERSION PLATFORM] [--cache DIR]",
         call. = FALSE)
  }
}
status <- tryCatch({ main(commandArgs(TRUE)); 0L }, error = function(cnd) {
  cat(conditionMessage(cnd), "\n", file = stderr(), sep = "")
  1L
})
quit(status = status, save = "no")
