suppressPackageStartupMessages(pkgload::load_all('.', quiet=TRUE))
options(lifecycle_verbosity='quiet',width=120)
warning_messages <- character()
observe <- function(x) tryCatch(withCallingHandlers(force(x), warning=function(cnd) {
  warning_messages <<- c(warning_messages, conditionMessage(cnd))
  invokeRestart('muffleWarning')
}),error=identity)
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
source <- dplyr::copy_to(con,data.frame(x=1L,g='a'),'preflight',temporary=TRUE)
unsafe <- dtplyr::lazy_dt(data.frame(x=1L,g='a')) |> dplyr::mutate(z=as.character(x))
query_log <- character()
trace('dbSendQuery',signature=c('duckdb_connection','character'),tracer=quote({.GlobalEnv$query_log <- c(.GlobalEnv$query_log,statement)}),where=asNamespace('DBI'),print=FALSE)
effects <- 0L
later <- quote(where({effects <<- effects+1L; is.numeric}))
invalid <- list(absent=quote(absent),all_of=quote(tidyselect::all_of('absent')),operator=quote(x*x))
consumers <- c('inspect_grouping','summarize_with_margins','expand_with_margins','nest_with_margins','nest_by_with_margins')
constructors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
rows <- list()
for (constructor in constructors) for (consumer in consumers) for (invalid_name in names(invalid)) {
  bad <- invalid[[invalid_name]]
  plain <- eval(rlang::call2(constructor,bad,quote(x)))
  mixed <- eval(rlang::call2(constructor,bad,later))
  run <- function(data,spec) {
    query_log <<- character();effects <<- 0L
    result <- suppressMessages(observe(do.call(get(consumer),list(.data=data,.grouping=spec))))
    list(result=result,queries=query_log,effects=effects)
  }
  duck_control <- run(source,plain)
  duck_mixed <- run(source,mixed)
  local_control <- run(data.frame(x=1L,g='a'),plain)
  dt_control <- run(unsafe,plain)
  dt_mixed <- run(unsafe,mixed)
  duck_supported <- !consumer %in% c('nest_with_margins','nest_by_with_margins')
  if (duck_supported) {
    stopifnot(inherits(duck_control$result,'error'),identical(class(duck_control$result),class(duck_mixed$result)), identical(conditionMessage(duck_control$result),conditionMessage(duck_mixed$result)),length(duck_control$queries)==0L,length(duck_mixed$queries)==1L,grepl('LIMIT 0',duck_mixed$queries,fixed=TRUE),duck_mixed$effects==0L)
  } else stopifnot(identical(conditionMessage(duck_control$result),conditionMessage(duck_mixed$result)), length(duck_mixed$queries)==0L)
  stopifnot(identical(class(dt_control$result),class(local_control$result)),inherits(dt_mixed$result,'marginplyr_error'),grepl("Can't safely determine column metadata",conditionMessage(dt_mixed$result),fixed=TRUE),dt_mixed$effects==0L)
  rows[[length(rows)+1L]] <- data.frame(constructor=constructor,consumer=consumer,invalid=invalid_name,duck_supported=duck_supported,duck_control_queries=length(duck_control$queries),duck_mixed_queries=length(duck_mixed$queries),dt_control_class=class(dt_control$result)[1],dt_mixed_class=class(dt_mixed$result)[1],later_effects=duck_mixed$effects)
}
untrace('dbSendQuery',signature=c('duckdb_connection','character'),where=asNamespace('DBI'))
DBI::dbDisconnect(con,shutdown=TRUE)
rows <- do.call(rbind,rows)
cat('MIXED PREFLIGHT:',nrow(rows),'matrix cases; each had 5 controls/investigated calls.\n')
cat('DuckDB:',sum(rows$duck_control_queries),'control queries;',sum(rows$duck_mixed_queries),'mixed queries.\n')
cat('dtplyr:',sum(rows$dt_mixed_class=='marginplyr_error'),'masked leading-selection errors.\n')
cat('Later predicate effects:',sum(rows$later_effects),'\n')
cat('Warnings:',length(warning_messages),'\n')
if (length(warning_messages)) cat(paste(unique(warning_messages), collapse='\n'),'\n')
utils::write.csv(rows, 'investigation/2026-10-31-summary-grouping-share-hunt/grouping-mixed-preflight-matrix.csv', row.names=FALSE)
