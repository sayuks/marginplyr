# Run reached-checkpoint SIGINT acceptance separately from controlled injection.
args <- commandArgs(TRUE)
entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
root <- dirname(dirname(dirname(normalizePath(entry))))
source(file.path(root, "tools/interruption-supervisor.R"))
cases <- list()
add <- function(checkpoint, ...) {
  case <- utils::modifyList(list(checkpoint = checkpoint, mode = "sigint", flag = FALSE,
    schema = "main", overwrite = TRUE, sorted = TRUE, expansion = FALSE,
    outer = FALSE, commit = FALSE), list(...))
  if (!any(vapply(cases, identical, logical(1), case))) cases[[length(cases) + 1L]] <<- case
}
for (flag in c(FALSE, TRUE)) {
  for (checkpoint in c("before", "acquire", "drop", "create", "index", "insert", "analyze", "result", "rollback", "cleanup_release", "release")) {
    add(checkpoint, flag = flag)
  }
  for (checkpoint in c("acquire", "drop", "create", "index", "insert", "analyze", "result", "release")) {
    for (commit in c(FALSE, TRUE)) add(checkpoint, flag = flag, outer = TRUE, commit = commit)
  }
  for (schema in c("main", "temp", "other")) for (overwrite in c(FALSE, TRUE)) {
    for (checkpoint in c("insert", "analyze", "result")) add(checkpoint, flag = flag, schema = schema, overwrite = overwrite)
  }
  for (schema in c("main", "temp", "other")) for (overwrite in c(FALSE, TRUE)) {
    for (commit in c(FALSE, TRUE)) add("insert", flag = flag, schema = schema, overwrite = overwrite,
      sorted = FALSE, outer = TRUE, commit = commit)
  }
  for (sorted_result in c(FALSE, TRUE)) for (expansion in c(FALSE, TRUE)) {
    add("result", flag = flag, sorted = sorted_result, expansion = expansion)
  }
  for (mode in c("healthy", "error", "controlled")) add("insert", flag = flag, mode = mode)
}
tryCatch({
  if (length(args) != 1L) stop("Usage: Rscript tools/sqlite-interruption/run.R <evidence directory>")
  supervise_interruption(root, args[[1L]], cases, "sqlite-interruption", timeout = 45)
}, error = function(cnd) { message(conditionMessage(cnd)); quit(status = 1L) })
