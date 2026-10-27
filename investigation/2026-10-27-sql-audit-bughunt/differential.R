pkgload::load_all('.', quiet = TRUE)
set.seed(20261027)
failures <- list()
cases <- 0L
returned <- 0L
refused <- 0L
run <- function(audit, remote, backend, operation, spec, check, sort, label, share) {
  options(marginplyr.audit_sql = audit)
  cache <- getFromNamespace('share_dialect_verdicts', 'marginplyr')
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  warnings <- character()
  phase <- 'construction'
  ans <- tryCatch(withCallingHandlers({
    if (operation == 'summary') {
      if (share) {
        query <- summarize_with_margins(remote, value = sum(.data$v, na.rm = TRUE),
          portion = share_of_total(value), .grouping = spec,
          .sort = sort, .margin_label = label, .check_margin_label = check,
          .check_share_source = FALSE)
      } else {
        query <- summarize_with_margins(remote, value = sum(.data$v, na.rm = TRUE),
          .grouping = spec, .sort = sort, .margin_label = label,
          .check_margin_label = check)
      }
    } else {
      query <- expand_with_margins(remote, .grouping = spec, .sort = sort,
        .margin_label = label, .check_margin_label = check)
    }
    rows <- if (audit) last_sent_queries(details = TRUE) else NULL
    phase <- 'collection'
    value <- dplyr::collect(query)
    if (audit && !identical(rows, last_sent_queries(details = TRUE))) stop('record changed during collect')
    value <- as.data.frame(value)
    if (nrow(value)) value <- value[do.call(order, lapply(value, as.character)), , drop = FALSE]
    rownames(value) <- NULL
    list(value = value, warnings = warnings)
  }, warning = function(w) {
    warnings <<- c(warnings, paste(phase, paste(class(w), collapse='/'), conditionMessage(w)))
    invokeRestart('muffleWarning')
  }), error = function(e) list(error = conditionMessage(e), classes = class(e), phase = phase, warnings = warnings))
  ans
}
for (backend in c('sqlite','duckdb')) {
  con <- if (backend == 'sqlite') DBI::dbConnect(RSQLite::SQLite(), ':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  for (iteration in seq_len(20)) {
    data <- data.frame(g = sample(c('a','b','Total',NA_character_), 12, replace=TRUE),
      h = sample(c('x','y',NA_character_),12,replace=TRUE), v=sample(c(1:3,NA),12,replace=TRUE))
    remote <- dplyr::copy_to(con,data,paste0('source_',iteration), temporary=TRUE)
    spec <- switch(1L + iteration %% 5L, rollup(g,h), cube(g,h), grouping_sets(grouping_set(g),grouping_set()), grouping_sets(grouping_set(g,h),grouping_set(g,h),grouping_set()), grouping_spec(rollup(g),rollup(h)))
    for (operation in c('summary','expand')) for (check in c(FALSE,TRUE)) for (sort in c("none","first","last")) {
      cases <- cases + 1L
      label <- if (iteration %% 2L) 'Total' else NA_character_
      off <- run(FALSE,remote,backend,operation,spec,check,sort,label,iteration %% 3L == 0L)
      on <- run(TRUE,remote,backend,operation,spec,check,sort,label,iteration %% 3L == 0L)
      if (is.null(off$error)) returned <- returned + 1L else refused <- refused + 1L
      if (!identical(off,on)) failures[[length(failures)+1L]] <- list(case=cases,backend=backend,iteration=iteration,operation=operation,check=check,sort=sort,off=off,on=on)
    }
  }
  DBI::dbDisconnect(con)
}
cat('Differential cases:', cases, '\nReturned:', returned, '\nRefused or collection error:', refused, '\nFailures:', length(failures),'\n')
if (length(failures)) {print(failures); quit(status=1)}
