args <- commandArgs(TRUE)
if (length(args) != 1L || file.exists(args[[1L]])) stop('Supply one fresh RDS output path.')
pkgload::load_all('.', quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, data.frame(g = 'a', v = 1), 'matrix_source')
child <- dbplyr::lazy_frame(h = 'b', z = 2, .name = 'nested_source')
leaf <- function(kind, at) switch(kind,
 warning = simpleWarning(paste(at, 'warning'), call = NULL),
 error = simpleError(paste(at, 'error'), call = NULL),
 interrupt = structure(list(message = paste(at, 'interrupt'), call = NULL), class = c(paste0(at, '_interrupt'), 'interrupt', 'condition')))
run <- function(audited, prefix, ending, cleanup, nested, warn) {
 options(marginplyr.audit_sql = audited, warn = warn)
 observed <- list(); actions <- character(); snapshots <- list()
 effect <- function(kind, at) {
   actions <<- c(actions, paste(at, kind))
   switch(kind,
     warning = warning(leaf(kind, at)),
     error = stop(leaf(kind, at)),
     interrupt = stop(leaf(kind, at)),
     escape = invokeRestart('escape_operation', paste(at, 'escaped')),
     normal = NULL)
 }
 invoke_child <- function() {
   if (nested == 'none') return(invisible(NULL))
   before <- if (audited) last_sent_queries(FALSE, TRUE)
   old <- getOption('marginplyr.audit_sql')
   options(marginplyr.audit_sql = !audited)
   if (nested == 'local') inspect_grouping(data.frame(h = 'x'), .grouping = rollup(h))
   if (nested == 'sql') summarize_with_margins(child, total = sum(z, na.rm = TRUE), .grouping = rollup(h))
   if (nested == 'failure') tryCatch(inspect_grouping(child, .grouping = stop('child controlled failure')), error = function(e) NULL)
   options(marginplyr.audit_sql = old)
   if (audited) stopifnot(identical(last_sent_queries(FALSE, TRUE), before))
 }
 testthat::local_mocked_bindings(collect = function(...) {
   on.exit({ invoke_child(); effect(cleanup, 'cleanup') }, add = TRUE)
   invoke_child()
   if (prefix != 'none') {
     withRestarts({
       if (prefix == 'native') rlang::interrupt() else signalCondition(leaf('interrupt', 'prefix'))
     }, resume_operation = function() { actions <<- c(actions, 'resumed'); NULL })
   }
   effect(ending, 'body')
   data.frame(g = 0)
 }, .package = 'dplyr')
 outcome <- tryCatch(withRestarts(withCallingHandlers(
   summarize_with_margins(remote, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE),
   condition = function(cnd) {
     observed[[length(observed) + 1L]] <<- list(classes = class(cnd), message = conditionMessage(cnd))
     if (inherits(cnd, 'interrupt') && !is.null(findRestart('resume_operation'))) invokeRestart('resume_operation')
     if (inherits(cnd, 'warning') && !is.null(findRestart('muffleWarning')) && warn != 2) invokeRestart('muffleWarning')
   }
 ), escape_operation = identity), error = identity, interrupt = identity)
 rows <- if (audited) last_sent_queries(FALSE, TRUE)
 if (inherits(outcome, 'tbl_sql')) outcome <- as.character(dbplyr::sql_render(outcome))
 if (inherits(outcome, 'condition')) outcome <- list(classes = class(outcome), message = conditionMessage(outcome))
 list(outcome = outcome, observed = observed, actions = actions, rows = rows)
}
combinations <- expand.grid(prefix=c('none','signal','native'), ending=c('normal','warning','error','interrupt','escape'), cleanup=c('normal','warning','error','interrupt','escape'), nested=c('none','local','sql','failure'), warn=c(0,2), stringsAsFactors=FALSE)
failures <- list(); statuses <- list(); observations <- list(); status_checks <- 0L
for (i in seq_len(nrow(combinations))) {
 x <- as.list(combinations[i,])
 off <- do.call(run,c(list(audited=FALSE),x))
 on <- do.call(run,c(list(audited=TRUE),x))
 if (!identical(on[1:3],off[1:3])) failures[[length(failures)+1L]] <- list(case=x,off=off,on=on)
 statuses[[i]] <- list(case=x,status=on$rows$status,classes=on$rows$conditions[[1]]$classes,messages=on$rows$conditions[[1]]$message)
 observations[[i]] <- list(case=x,off=off[1:3],on=on[1:3],rows=on$rows)
 # A cleanup restart after a pending error/interrupt does not establish that
 # the interrupted body resumed. Keep those observations without inventing an
 # unknown-versus-terminal oracle for that unselected contract interpretation.
 pending <- x$ending %in% c('error','interrupt') || (x$ending == 'warning' && x$warn == 2)
 if (!(x$cleanup == 'escape' && pending)) {
   body_status <- switch(x$ending, normal='returned', warning=if(x$warn==2) 'errored' else 'returned', error='errored', interrupt='interrupted', escape='unknown')
   expected <- switch(x$cleanup, normal=body_status, warning=if(x$warn==2) 'errored' else body_status, error='errored', interrupt='interrupted', escape='unknown')
   stopifnot(identical(on$rows$status,expected))
   status_checks <- status_checks + 1L
 }
 stopifnot(all(on$rows$conditions[[1]]$phase == 'invocation'))
 if (i %% 100L == 0L) cat('tested',i,'pairs\n')
}
saveRDS(list(cases=combinations,failures=failures,statuses=statuses,observations=observations,status_checks=status_checks),args[[1L]])
print(failures)
cat('PASS',nrow(combinations),'off/on public lifecycle pairs;',status_checks,'status assertions\n')
print(table(vapply(statuses,function(x) x$status,character(1))))
DBI::dbDisconnect(con)
stopifnot(length(failures)==0L,status_checks==540L)
