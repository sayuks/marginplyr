# Temporary native-process probe for issue #1566.
main <- function() {
  root <- tempfile("json-native-probe-", tmpdir = Sys.getenv("RUNNER_TEMP", tempdir()))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  worker <- file.path(root, "probe.R")
  writeLines(r"---(
args <- commandArgs(trailingOnly = TRUE)
m <- new.env(parent = globalenv())
sys.source('tools/duckdb-json.R', m)
con <- m$duckdb_json_connection()
native <- m$duckdb_json_identity(con)
DBI::dbDisconnect(con, shutdown = TRUE)
print(list(R = R.version.string, duckdb = as.character(packageVersion('duckdb')),
           native = native, mode = args[[2L]]))
paths <- m$duckdb_json_cache_paths(native$version, native$platform, args[[1L]])
real_validate <- m$duckdb_json_validate
if (args[[2L]] == 'install-only') m$duckdb_json_validate <- function(path) path
m$unlink <- function(x, recursive = FALSE, ...) {
  result <- base::unlink(x, recursive = recursive, ...)
  if (result != 0L) {
    cat('[probe] unlink failure:', x, '\n')
    remaining <- list.files(x, all.files = TRUE, recursive = TRUE, full.names = TRUE)
    print(file.info(remaining)[, c('size', 'isdir', 'mode')])
    for (path in remaining) {
      cat('[probe] normalized:', normalizePath(path, winslash = '/'),
          'write access:', file.access(path, 2), '\n')
      cat('[probe] remove remaining:', path, file.remove(path), '\n')
    }
    gc()
    cat('[probe] after gc:', base::unlink(x, recursive = TRUE), '\n')
  }
  result
}
withCallingHandlers(m$duckdb_json_publish(paths, native), error = function(cnd) {
  cat('[probe] signaled:', conditionMessage(cnd), '\n')
})
cat('[probe] publication returned\n')
)---", worker)
  for (mode in c("install-only", "load-validation")) {
    cache <- file.path(root, mode)
    result <- processx::run(file.path(R.home("bin"), "Rscript"),
      c(worker, cache, mode), error_on_status = FALSE, timeout = 180000,
      cleanup_tree = TRUE)
    cat(result$stdout, result$stderr, sep = "")
    cat("[probe] child status:", result$status, "\n")
    cat("[probe] after child exit removal:", unlink(cache, recursive = TRUE),
        "still exists:", dir.exists(cache), "\n")
  }
}
main()
