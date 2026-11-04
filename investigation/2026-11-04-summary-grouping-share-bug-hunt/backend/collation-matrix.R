pkgload::load_all(quiet = TRUE)
library(dplyr)
out_dir <- 'investigation/2026-11-04-summary-grouping-share-bug-hunt/backend'
rows <- list()
for (backend in c('sqlite', 'duckdb')) {
  con <- if (backend == 'sqlite') DBI::dbConnect(RSQLite::SQLite(), ':memory:') else DBI::dbConnect(duckdb::duckdb(), dbdir = ':memory:', shared_home = FALSE)
  DBI::dbExecute(con, 'CREATE TABLE facts (f TEXT COLLATE NOCASE, g TEXT COLLATE NOCASE, h INTEGER, x DOUBLE)')
  dat <- data.frame(f = rep(c('A','a',NA), 4), g = rep(c('X','x',NA,'Y'), 3),
    h = rep(c(1L,2L), 6), x = seq_len(12L))
  DBI::dbAppendTable(con, 'facts', dat)
  src <- tbl(con, 'facts')
  local <- dat
  local$f <- tolower(local$f); local$g <- tolower(local$g)
  key <- function(x) paste(ifelse(is.na(x$f), '<NA>', tolower(x$f)),
    ifelse(is.na(x$g), '<NA>', tolower(x$g)), ifelse(is.na(x$h), '<NA>', x$h), x$set, sep = ':')
  for (portable in c(FALSE,TRUE)) for (sorted in c(FALSE,TRUE)) for (label in c(FALSE,TRUE)) {
    label_value <- if (label) 'Total' else NULL
    args <- list(.grouping = rollup(g,h), .id = 'set', .margin_label = label_value,
      .sort = if (sorted) 'last' else 'none', .duplicates = if(portable) 'keep' else 'error')
    run <- function(x) summarize_with_margins(x, value = sum(x, na.rm=TRUE),
      p = share_of_parent(value), t = share_of_total(value), .by = f,
      .grouping = args$.grouping, .id = args$.id, .margin_label = args$.margin_label,
      .sort = args$.sort, .duplicates = args$.duplicates, .check_share_source = FALSE)
    expected <- run(local)
    actual <- collect(run(src))
    idx <- match(key(expected), key(actual))
    pass <- nrow(expected) == nrow(actual) && !anyNA(idx) &&
      isTRUE(all.equal(expected$value, actual$value[idx])) &&
      isTRUE(all.equal(expected$p, actual$p[idx])) && isTRUE(all.equal(expected$t, actual$t[idx]))
    rows[[length(rows)+1L]] <- data.frame(backend, portable, sorted, label, pass)
  }
  if (backend == 'duckdb') DBI::dbDisconnect(con, shutdown=TRUE) else DBI::dbDisconnect(con)
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out_dir,'collation-matrix.csv'), row.names=FALSE)
print(results, row.names=FALSE)
stopifnot(all(results$pass))
