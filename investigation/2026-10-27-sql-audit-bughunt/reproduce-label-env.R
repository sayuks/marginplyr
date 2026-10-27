# Run from the checkout root. Exit 1 reproduces the unfixed public contract.
main <- function(args) {
  stopifnot(length(args) <= 1L)
  if (length(args)) stopifnot(!file.exists(args[[1L]]))
  pkgload::load_all('.', quiet = TRUE)
  old <- options(marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  observations <- list()
  for (value in c('a', 'Total')) {
    input <- dplyr::copy_to(con, data.frame(.env = value),
                            paste0('env_source_', length(observations)))
    sql <- 'SELECT SUM(CASE WHEN CAST(".env" AS TEXT) = \'Total\' THEN 1 ELSE 0 END) AS collisions FROM '
    raw <- DBI::dbGetQuery(con, paste0(sql, DBI::dbQuoteIdentifier(con, dbplyr::remote_name(input))))
    stopifnot(raw$collisions == as.integer(value == 'Total'))
    for (audited in c(FALSE, TRUE)) for (checked in c(FALSE, TRUE)) {
      options(marginplyr.audit_sql = audited)
      outcome <- tryCatch(summarize_with_margins(
        input, rows = dplyr::n(),
        .grouping = rollup(dplyr::all_of('.env')),
        .check_margin_label = checked
      ), error = identity)
      record <- if (audited) last_sent_queries(details = TRUE) else NULL
      if (!inherits(outcome, 'error')) outcome <- dplyr::collect(outcome)
      if (audited) stopifnot(identical(record, last_sent_queries(details = TRUE)))
      if (!checked) stopifnot(is.data.frame(outcome), nrow(outcome) == 2L,
                              identical(sort(outcome$rows), c(1L, 1L)))
      key <- paste(value, audited, checked, sep = '/')
      observations[[key]] <- list(
        value = value, audited = audited, checked = checked,
        outcome = if (inherits(outcome, 'error')) list(
          classes = class(outcome), message = conditionMessage(outcome)
        ) else outcome,
        record = record,
        desired = if (!checked) TRUE else if (value == 'a') {
          is.data.frame(outcome) && nrow(outcome) == 2L &&
            identical(sort(outcome$.env), sort(c('a', 'Total')))
        } else {
          inherits(outcome, 'marginplyr_error') &&
            grepl('.env', conditionMessage(outcome), fixed = TRUE)
        }
      )
      cat(key, ': ', if (inherits(outcome, 'error')) conditionMessage(outcome) else 'returned correct rows', '\n', sep = '')
      if (audited && checked) print(record[c('purpose', 'sql', 'status')])
    }
  }
  ordinary <- dplyr::copy_to(con, data.frame(g = 'a'), 'ordinary_control')
  control <- summarize_with_margins(ordinary, rows = dplyr::n(),
    .grouping = rollup(g), .check_margin_label = TRUE)
  stopifnot(nrow(dplyr::collect(control)) == 2L)
  if (length(args)) saveRDS(observations, args[[1L]])
  failed <- names(observations)[!vapply(observations, `[[`, logical(1), 'desired')]
  cat('Unsatisfied public-contract cases:', length(failed), '\n')
  print(failed)
  if (length(failed)) stop('SQL .env collision checking violates the public contract.', call. = FALSE)
}
main(commandArgs(TRUE))
