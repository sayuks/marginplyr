pkgload::load_all('.', quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, data.frame(g = c('a','b'), v = c(1,3)), 'scope_retention')
verbs <- list(summarize = summarize_with_margins, summarise = summarise_with_margins,
              expand = expand_with_margins, nest = nest_with_margins,
              nest_by = nest_by_with_margins, inspect = inspect_grouping)
# Early `.data` forcing: success, nested public refusal, arbitrary failure,
# and active bindings must leave the last completed input owner until entry.
checks <- 0L
for (verb in verbs) {
  for (audit in c(FALSE, TRUE)) {
    options(marginplyr.audit_sql = TRUE)
    summarize_with_margins(remote, total = sum(v), .grouping = rollup(g))
    previous <- last_sent_queries(details = TRUE)
    leaf <- errorCondition('data input failed', class = 'scope_data_failure')
    got <- tryCatch(verb({stop(leaf)}, .grouping = rollup(g)), error = identity)
    stopifnot(identical(got, leaf), identical(last_sent_queries(details=TRUE), previous))
    forced <- 0L
    input <- new.env()
    makeActiveBinding('value', function() {
      forced <<- forced + 1L
      options(marginplyr.audit_sql = audit)
      summarize_with_margins(remote, total = sum(v), .grouping = rollup(g))
      snapshot <- if (audit) last_sent_queries(details=TRUE) else NULL
      if (forced == 1L) {
        # Reading an outer expression's input has not yet started its record.
        stopifnot(!audit || nrow(snapshot) == 1L)
      }
      data.frame(g = 'local', v = 5)
    }, input)
    result <- tryCatch(verb(input$value, .grouping = rollup(g)), error = identity)
    stopifnot(forced == 1L)
    if (audit) stopifnot(nrow(last_sent_queries(details=TRUE)) == 0L) else {
      stopifnot(inherits(tryCatch(last_sent_queries(),error=identity),'marginplyr_error'))
    }
    checks <- checks + 2L
  }
}
# Copies from the active operation remain snapshots after later row updates,
# replacement, filtering, changed options, and normal caller modifications.
options(marginplyr.audit_sql=TRUE)
query <- summarize_with_margins(remote, total=sum(v), .grouping=rollup(g), .check_margin_label=TRUE)
saved <- last_sent_queries(details=TRUE)
serialized <- serialize(saved, NULL)
for (i in seq_len(30L)) {
  summarize_with_margins(remote, total=sum(v), .grouping=rollup(g), .check_margin_label=TRUE)
  stopifnot(identical(serialize(saved,NULL), serialized))
  for (include in c(FALSE,TRUE)) for (details in c(FALSE,TRUE)) last_sent_queries(include,details)
  x <- last_sent_queries(details=TRUE)
  x$sql[] <- 'caller text'; x$conditions[[1L]]$message <- character()
  stopifnot(!identical(x,last_sent_queries(details=TRUE)))
}
# Weak references distinguish call/query/connection retention from SQL text.
refs <- local({
  holder <- new.env(parent=emptyenv())
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  q <- dplyr::copy_to(con2,data.frame(g='x',v=1),'weak_scope_source')
  environment_ref <- rlang::new_weakref(holder)
  connection_holder <- new.env(parent=emptyenv()); connection_holder$con <- con2
  connection_ref <- rlang::new_weakref(connection_holder)
  pointer_ref <- rlang::new_weakref(con2@ptr)
  factory <- function() { force(holder); force(connection_holder); rollup(g) }
  summarize_with_margins(q,total=sum(v),.grouping=factory(),.check_margin_label=TRUE)
  DBI::dbDisconnect(con2)
  list(environment_ref,connection_ref,pointer_ref)
})
for(i in 1:3) invisible(gc())
stopifnot(all(vapply(refs,function(x)is.null(rlang::wref_key(x)),logical(1))))
DBI::dbDisconnect(con)
cat('PASS promise forcing:',checks,'checks; copy replacements:30; released weakrefs:',length(refs),'\n')
