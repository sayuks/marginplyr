script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript transaction-failures.R <fresh-output.rds>")
}
out <- capture_output(args[[1L]])
load_checkout()
run <- function(backend,audit,scenario) {
 options(marginplyr.audit_sql=audit)
 verdicts <- getFromNamespace('share_dialect_verdicts','marginplyr')
 rm(list=ls(verdicts),envir=verdicts)
 con <- switch(backend,sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
 on.exit(DBI::dbDisconnect(con),add=TRUE)
 src <- dplyr::copy_to(con,data.frame(g=c('a','b'),v=c(1,2)),'tx_source')
 if(scenario=='missing') src <- dplyr::tbl(con, dbplyr::sql('SELECT g,v FROM missing_table'),vars=c('g','v'))
 if(scenario=='invalid_expr') src <- dplyr::mutate(src,g=DOES_NOT_EXIST(g))
 DBI::dbBegin(con)
 DBI::dbExecute(con,'CREATE TABLE marker (v INTEGER)')
 DBI::dbExecute(con,'INSERT INTO marker VALUES (19)')
 conditions <- list()
 value <- withCallingHandlers(tryCatch(switch(scenario,
  share=summarize_with_margins(src,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g)),
  inspect=inspect_grouping(src,.grouping=rollup(where(is.character))),
  summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE)
 ),error=identity),condition = function(cnd) {
    if (inherits(cnd, c('warning', 'error', 'interrupt'))) {
      conditions[[length(conditions) + 1L]] <<-
        list(class = class(cnd), message = conditionMessage(cnd))
    }
  })
 rows <- if(audit) last_sent_queries(details=TRUE) else NULL
 state <- tryCatch(DBI::dbGetQuery(con,'SELECT v FROM marker'),error=function(cnd) list(class=class(cnd),message=conditionMessage(cnd)))
 DBI::dbRollback(con)
 gone <- tryCatch(DBI::dbGetQuery(con,'SELECT v FROM marker'),error=function(cnd) list(class=class(cnd),message=conditionMessage(cnd)))
 value <- if(inherits(value,'condition')) list(class=class(value),message=conditionMessage(value)) else if(inherits(value,'tbl_lazy')) list(class=class(value),vars=as.character(dplyr::tbl_vars(value))) else value
 list(conditions=conditions,value=value,state=state,gone=gone,rows=rows)
}
all <- list()
for(backend in c('sqlite','duckdb')) for(scenario in c('normal','missing','invalid_expr','inspect','share')) {
 off <- run(backend,FALSE,scenario);on <- run(backend,TRUE,scenario)
 stopifnot(identical(off[1:4],on[1:4]))
 all[[paste(backend,scenario,sep='/')]] <- list(off=off,on=on)
 cat(backend,scenario,'PASS\n');print(on$rows[c('purpose','source','status')])
}
stopifnot(length(all) == 10L)
saveRDS(all,out)
cat("PASS", length(all), "transaction audit off/on pairs\n")
