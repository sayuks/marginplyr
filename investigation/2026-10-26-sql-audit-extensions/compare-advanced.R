# Replay all saved audit off/on observations. This compares row multisets;
# it does not establish Margin order or ordering within tied keys.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
evidence_dir <- args[[1L]]
norm <- function(x) {
  if (is.character(x)) {
    return(gsub('marginplyr_share_[[:alnum:]]+', 'marginplyr_share_<id>', x))
  }
  if (is.data.frame(x)) {
    for (name in names(x)) x[[name]] <- norm(x[[name]])
    return(x)
  }
  if (is.list(x)) return(lapply(x, norm))
  x
}
canonical <- function(x) {
  if (is.data.frame(x) && nrow(x) && !any(vapply(x, is.list, logical(1)))) {
    x <- x[do.call(order, c(as.list(x), list(na.last = TRUE))), , drop = FALSE]
    rownames(x) <- NULL
  }
  x
}
failures <- character()
failed <- function(message) {
  failures <<- c(failures, message)
  cat(message, '\n')
}
for (backend in c('sqlite', 'duckdb', 'postgres')) {
  path <- function(suffix) file.path(evidence_dir, paste0(backend, '-', suffix, '.rds'))
  stopifnot(file.exists(path('off')), file.exists(path('on')))
  off <- readRDS(path('off')); on <- readRDS(path('on'))
  expected_cases <- if (backend == 'postgres') 70L else 69L
  stopifnot(length(off) == expected_cases, length(on) == expected_cases)
  stopifnot(identical(vapply(off, `[[`, character(1), 'label'),
                      vapply(on, `[[`, character(1), 'label')))
  for (i in seq_along(on)) {
    a <- off[[i]]; b <- on[[i]]; rows <- b$record
    a$record <- b$record <- NULL
    same_row_order <- identical(a$data, b$data)
    a$data <- canonical(a$data); b$data <- canonical(b$data)
    if (!identical(norm(a), norm(b))) {
      failed(paste('DIFFERENCE', backend, a$label))
      print(all.equal(norm(a), norm(b)))
    } else cat('PASS', backend, a$label, if (!same_row_order) '(row multiset)', '\n')
    sql <- norm(b$sent)
    # RPostgres forwards dbExecute through dbSendQuery. Deduplicate this
    # fixture's consecutive forwarding copies only for the transaction case.
    if (backend == 'postgres' && grepl('transaction:', a$label, fixed = TRUE) &&
        length(sql) > 1L) {
      sql <- sql[c(TRUE, sql[-1L] != sql[-length(sql)])]
    }
    internal <- rows[rows$purpose != 'result', ]
    if (!identical(norm(internal$sql), sql)) {
      failed(paste('COVERAGE DIFFERENCE', backend, a$label))
      print(sql); print(internal)
    }
    stopifnot(all(rows$status %in% c('returned', 'errored', 'interrupted', 'prepared')))
    if (is.character(b$render) && any(rows$purpose == 'result')) {
      stopifnot(identical(rows$sql[rows$purpose == 'result'], b$render))
    }
  }
}
if (length(failures)) stop(length(failures), ' audit comparisons failed')
cat('All 208 audit off/on comparisons and internal SQL checks passed.\n')
