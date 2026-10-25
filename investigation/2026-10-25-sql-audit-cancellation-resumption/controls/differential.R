script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) != 3L || !args[[1L]] %in% c("sqlite", "duckdb") ||
    !args[[2L]] %in% c("on", "off")) {
  stop("Usage: Rscript differential.R <sqlite|duckdb> <on|off> <fresh-output.rds>")
}
backend <- args[[1L]]
audited <- identical(args[[2L]], 'on')
out <- capture_output(args[[3L]])
load_checkout()
options(marginplyr.audit_sql = audited)
con <- switch(backend,
 sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'),
 duckdb = DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
on.exit(DBI::dbDisconnect(con), add=TRUE)
data <- data.frame(g=c('a','b','a',NA_character_,'Total'), h=c('x','x','y','y','x'), v=c(1,2,3,NA_real_,5), i=1:5, flag=c(TRUE,FALSE,NA,TRUE,FALSE))
data$f <- factor(c('foo','bar','foo',NA,'bar'),levels=c('bar','foo','unused'))
src <- dplyr::copy_to(con,data,'differential_source',temporary=TRUE)
empty <- dplyr::filter(src,FALSE)
grouped <- dplyr::group_by(src,h)
filtered <- dplyr::filter(src,i < 4)
limit <- utils::head(dplyr::arrange(src,v),3)
weird <- dplyr::rename(src, '.env'=g, 'a" b'=v, '一 二'=h)
all_sql <- character()
trace('dbSendQuery',where=asNamespace('DBI'),tracer=quote({all_sql <<- c(all_sql,as.character(statement))}), print=FALSE)
trace('dbExecute',where=asNamespace('DBI'),tracer=quote({all_sql <<- c(all_sql,as.character(statement))}), print=FALSE)
verdicts <- getFromNamespace('share_dialect_verdicts','marginplyr')
cases <- list(
 summary = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE), n=dplyr::n(), .grouping=rollup(g,h))),
 summary_sorted = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.sort='last')),
 summary_checked_good = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.margin_label='ALL',.check_margin_label=TRUE)),
 summary_checked_bad = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE)),
 summary_missing = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.margin_label=NA_character_,.check_margin_label=TRUE)),
 summary_factor = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.grouping=rollup(f),.sort='first',.check_margin_label=TRUE)),
 summary_nogroups = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.check_margin_label=TRUE)),
 summary_nosummary = quote(summarize_with_margins(S,.grouping=rollup(g,h))),
 summary_cube = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),id=grouping_id(g,h),bit=grouping_bit(g),.grouping=cube(g,h),.id='setid',.sort='first')),
 summary_share = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),share=share_of_total(total),parent=share_of_parent(total),.grouping=rollup(g,h),.check_share_source=FALSE,.sort='last')),
 summary_share_checked = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g,h))),
 summary_by = quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),.by=flag,.grouping=rollup(g,h),.sort='last')),
 expand = quote(expand_with_margins(S,.grouping=rollup(g,h))),
 expand_sorted = quote(expand_with_margins(S,.grouping=rollup(g,h),.sort='first')),
 expand_checked = quote(expand_with_margins(S,.grouping=rollup(g,h),.check_margin_label=TRUE,.margin_label='ALL')),
 expand_factor = quote(expand_with_margins(S,.grouping=rollup(f),.sort='last')),
 inspect = quote(inspect_grouping(S,.grouping=rollup(g,h))),
 inspect_typed = quote(inspect_grouping(S,.grouping=rollup(where(is.character)))),
 inspect_factor = quote(inspect_grouping(S,.grouping=rollup(where(is.factor)))),
 inspect_empty = quote(inspect_grouping(S)),
 nest = quote(nest_with_margins(S,.grouping=rollup(g,h))),
 nestby = quote(nest_by_with_margins(S,.grouping=rollup(g,h)))
)
inputs <- list(source=src,empty=empty,grouped=grouped,filtered=filtered,limit=limit)
run <- function(input,label,expr) {
 S <- input
 rm(list=ls(verdicts),envir=verdicts)
 all_sql <<- character()
 conditions <- list()
 value <- withCallingHandlers(tryCatch(eval(expr),error=identity,interrupt=identity),condition=function(cnd) {
  if(inherits(cnd,c('warning','error','interrupt'))) conditions[[length(conditions)+1L]] <<- list(class=class(cnd),message=conditionMessage(cnd))
 })
 construction_sql <- all_sql
 rec <- if(audited) last_sent_queries(details=TRUE) else NULL
 value_class <- class(value)
 rendered <- if(inherits(value,'tbl_lazy')) tryCatch(as.character(dbplyr::sql_render(value)),error=identity) else NULL
 collected <- if(inherits(value,'tbl_lazy')) tryCatch(dplyr::collect(value),error=identity) else value
 if(inherits(collected,'condition')) collected <- list(class=class(collected),message=conditionMessage(collected))
 list(label=label,conditions=conditions,sql=construction_sql,record=rec,class=value_class,rendered=rendered,value=collected)
}
results <- list()
for(nm in names(inputs)) for(case in names(cases)) {
 label <- paste(nm,case,sep='/')
 results[[label]] <- run(inputs[[nm]],label,cases[[case]])
 cat(label,length(results[[label]]$sql),if(audited)nrow(results[[label]]$record) else '', '\n')
}
S <- weird
results[['weird']] <- run(weird,'weird',quote(summarize_with_margins(S,total=sum(.data[['a" b']],na.rm=TRUE),.grouping=rollup(.data[['.env']],.data[['一 二']]),.margin_label='ALL',.check_margin_label=TRUE,.sort='last')))
saveRDS(results,out)
