args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
normal <- readRDS(file.path(args[[1L]], 'sites-matrix.rds'))
failure <- readRDS(file.path(args[[1L]], 'sites-failures.rds'))
stopifnot(identical(normal$metadata$commit, failure$metadata$commit))
normal_cases <- normal$cases
has_error <- function(pair) {
  value <- pair[['TRUE']]$value
  is.list(value) && 'message' %in% names(value)
}
expected_collision <- vapply(names(normal_cases), function(name) {
  grepl('/colliding/', name, fixed = TRUE) && !endsWith(name, '/inspect')
}, logical(1))
stopifnot(identical(unname(vapply(normal_cases, has_error, logical(1))),
                    unname(expected_collision)))
for (index in which(expected_collision)) {
  value <- normal_cases[[index]][['TRUE']]$value
  stopifnot('marginplyr_error' %in% value$class,
            grepl('already present in grouping column', value$message, fixed = TRUE))
}
rows <- function(cases) do.call(rbind, lapply(cases, function(pair) pair[['TRUE']]$record))
normal_rows <- rows(normal_cases)
failure_rows <- rows(failure$cases)
normalize_sql <- function(sql) gsub('marginplyr_share_[A-Za-z0-9]+', 'marginplyr_share_NAME', sql)
stopifnot(length(normal_cases) == 726L, length(failure$cases) == 13L,
          sum(expected_collision) == 249L,
          sum(!expected_collision) == 477L,
          sum(normal_rows$purpose != 'result') == 758L,
          sum(normal_rows$purpose == 'result') == 267L,
          sum(failure_rows$purpose != 'result') == 18L,
          identical(as.integer(table(failure_rows$status)), c(9L, 5L, 9L)))
for (cases in list(normal_cases, failure$cases)) for (pair in cases) {
  fields <- setdiff(names(pair[['FALSE']]), 'record')
  stopifnot(identical(pair[['FALSE']][fields], pair[['TRUE']][fields]))
  audited <- pair[['TRUE']]
  internal <- audited$record[audited$record$purpose != 'result', ]
  stopifnot(identical(unname(normalize_sql(internal$sql)), unname(audited$sent)),
    all(audited$record$status[audited$record$purpose == 'result'] == 'prepared'),
    all(internal$source[internal$purpose == 'share_dialect_transaction'] == 'literal'),
    all(internal$source[internal$purpose != 'share_dialect_transaction'] == 'render'))
}
stopifnot(all(normal_rows$status[normal_rows$purpose != 'result'] == 'returned'),
          all(vapply(normal_rows$conditions, nrow, integer(1)) == 0L))
for (index in seq_len(nrow(failure_rows))) {
  summaries <- failure_rows$conditions[[index]]
  if (failure_rows$status[[index]] == 'errored') {
    expected_handling <- if (failure_rows$purpose[[index]] %in%
      c('share_dialect', 'share_dialect_transaction')) 'handled' else 'propagated'
    stopifnot(nrow(summaries) == 1L, summaries$phase == 'invocation',
              summaries$handling == expected_handling,
              'error' %in% summaries$classes[[1L]])
  } else {
    stopifnot(nrow(summaries) == 0L)
  }
}
cat('Snapshot:', normal$metadata$commit, '\n')
cat('Normal pairs: 726; expected collision refusals: 249; successful calls: 477\n')
cat('Normal rows per audit-on set: 758 internal (516 counts, 242 metadata), 267 prepared results\n')
cat('Failure/notice/timeout/transaction pairs: 13; internal rows: 18; prepared results: 5\n')
cat('Failure-set statuses: 9 errored, 9 returned, 5 prepared\n')
cat('Total public calls in paired capture: 1478; total internal DBI sends: 1552\n')
cat('No new bug confirmed by the tested paths. All assertions passed.\n')
