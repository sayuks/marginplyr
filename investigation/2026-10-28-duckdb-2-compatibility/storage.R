# Disposable cross-version storage fixture. MODE is create, read, update, or reject.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
mode <- args[[1L]]
path <- args[[2L]]
stopifnot(mode %in% c('create', 'read', 'update', 'reject'))
if (mode == 'create') stopifnot(!file.exists(path)) else stopifnot(file.exists(path))
pkgload::load_all('.', quiet = TRUE)
con <- tryCatch(DBI::dbConnect(duckdb::duckdb(path, shared_home = FALSE)),
  error = identity)
if (mode == 'reject') {
  stopifnot(inherits(con, 'error'),
    grepl('version', conditionMessage(con), ignore.case = TRUE))
  cat('Expected incompatible storage:', conditionMessage(con), '\n')
} else {
  if (inherits(con, 'error')) stop(con)
  if (mode == 'create') {
    source <- dplyr::copy_to(con, data.frame(g = c('a', 'b'), value = c(2L, 6L)),
      'facts', temporary = FALSE)
    result <- summarize_with_margins(source, total = sum(value, na.rm = TRUE),
      .grouping = rollup(g), .sort = 'last')
    dplyr::compute(result, name = 'margin_result', temporary = FALSE)
  }
  if (mode == 'update') DBI::dbExecute(con, 'UPDATE facts SET value = value')
  result <- dplyr::collect(dplyr::arrange(dplyr::tbl(con, 'margin_result'), .data$g))
  stopifnot(identical(result$g, c('Total', 'a', 'b')),
    identical(as.numeric(result$total), c(8, 2, 6)))
  print(DBI::dbGetQuery(con, 'PRAGMA version'))
  DBI::dbDisconnect(con, shutdown = TRUE)
  cat('PASS', mode, '\n')
}
