args <- commandArgs(TRUE)
root <- if (length(args)) args[[1L]] else getwd()
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
connections <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
failures <- character()
for (backend in names(connections)) {
  con <- connections[[backend]]
  input <- copy_to(con, data.frame(x = 1), 'facts')
  ordinary <- collect(summarise(input, ..0 = sum(x, na.rm = TRUE)))
  stopifnot(identical(names(ordinary), '..0'), ordinary[[1L]] == 1)
  actual <- tryCatch(
    collect(summarize_with_margins(input, ..0 = sum(x, na.rm = TRUE),
                                  .sort = 'last')),
    error = identity
  )
  ok <- !inherits(actual, 'error') && identical(names(actual), '..0') &&
    actual[[1L]] == 1
  cat(backend, 'ordinary=1; Margin=',
      if (inherits(actual, 'error')) conditionMessage(actual) else actual[[1L]],
      '; desired-behavior=', ok, '\n', sep = '')
  if (!ok) failures <- c(failures, backend)
  DBI::dbDisconnect(con)
}
if (length(failures)) stop('Valid SQL summary outputs failed: ',
                           paste(failures, collapse = ', '), call. = FALSE)
