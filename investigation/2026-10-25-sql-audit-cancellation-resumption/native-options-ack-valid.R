# Run this diagnostic in a disposable Rscript process; it loads this checkout.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(normalizePath(sub("^--file=", "", script_arg))),
                 "reproduction-setup.R"))
# Unmodified package code. A single-flight sender targets only this disposable
# R process; a request/ack handshake prevents accumulated native SIGINTs.
args <- commandArgs(trailingOnly=TRUE)
mode <- if(length(args))args[[1L]] else 'internal'
seconds <- if(length(args)>1L)as.numeric(args[[2L]]) else 35
stopifnot(mode %in% c('internal','public'))
pkgload::load_all(hunt_checkout, quiet = TRUE)
expected <- c(rlib_warning_verbosity='default',lifecycle_verbosity='default')
options(c(list(marginplyr.audit_sql=TRUE),as.list(expected)))
input<-dbplyr::tbl_lazy(data.frame(v=1))
invisible(inspect_grouping(input))
ns<-asNamespace('marginplyr')
render<-get('render_sent_query_sql',ns)
record<-get('sent_queries',ns)$latest$record
record$rows<-dplyr::tibble(purpose='fixture',sql=NA_character_,source='render',status='prepared',conditions=list(get('empty_sent_query_conditions',ns)()))
entry<-list(record=record,index=1L)
query<-input
root<-tempfile('native-options-ack-');dir.create(root)
request<-file.path(root,'request');ack<-file.path(root,'ack')
writeLines('0',request);writeLines('0',ack)
atomic_write<-function(value,path) {
 temp<-paste0(path,'.tmp');writeLines(as.character(value),temp);file.rename(temp,path)
}
read_number<-function(path) {
 value<-suppressWarnings(tryCatch(readLines(path,warn=FALSE),error=function(cnd)'0'))
 if(length(value)!=1L)return(0L)
 as.integer(value)
}
run<-function() {
 target<-Sys.getpid()
 sender<-parallel::mcparallel({
  last<-0L
  repeat {
   req<-read_number(request)
   if(!is.na(req)&&req>last) {
    Sys.sleep(runif(1L,0.0005,0.0025))
    tools::pskill(target,2L)
    atomic_write(req,ack)
    last<-req
   } else Sys.sleep(0.0002)
  }
 },silent=TRUE,mc.set.seed=FALSE)
 clean<-function() {
  if(!is.null(sender)) {
   tools::pskill(sender$pid,9L)
   suppressWarnings(parallel::mccollect(sender,wait=TRUE))
   sender<<-NULL
  }
 }
 on.exit(clean(),add=TRUE)
 end<-proc.time()[['elapsed']]+seconds
 caught<-0L
 for(i in seq_len(100000L)) {
  if(proc.time()[['elapsed']]>=end)break
  if(mode=='internal')record$rows$conditions<-list(get('empty_sent_query_conditions',ns)())
  atomic_write(i,request)
  outcome<-tryCatch(allowInterrupts({
   for(k in seq_len(100000L)) {
    if(mode=='internal')render(query,entry) else summarize_with_margins(input,answer=sum(v,na.rm=TRUE))
   }
   NULL
  }),interrupt=identity)
  caught<-caught+inherits(outcome,'interrupt')
  deadline<-proc.time()[['elapsed']]+5
  while(!identical(read_number(ack),i)) {
   if(proc.time()[['elapsed']]>deadline)stop('sender acknowledgement timed out')
   Sys.sleep(0.001)
  }
  actual<-vapply(names(expected),getOption,character(1))
  if(!identical(actual,expected)) {
   clean()
   rows<-if(mode=='internal')record$rows else last_sent_queries(details=TRUE)
   evidence<-list(mode=mode,iteration=i,caught=caught,expected=expected,actual=actual,rows=rows,outcome_class=class(outcome),native_interrupt=inherits(outcome,'interrupt'),session=sessionInfo(),package_functions_replaced=FALSE)
   destination<-hunt_output(paste0('native-options-ack-',mode,'.rds'))
   saveRDS(evidence,destination)
   # The sender has exited and its one acknowledged signal was already caught.
   drained<-0L
   tryCatch(allowInterrupts(Sys.sleep(0.01)),interrupt=function(cnd) { drained <<- drained + 1L })
   invisible(inspect_grouping(data.frame(v=1)))
   evidence$after_independent_local_call<-vapply(names(expected),getOption,character(1))
   evidence$arithmetic<-1L+1L
   evidence$extra_interrupts_after_cleanup<-drained
   evidence$sender_reaped<-is.null(sender)
   saveRDS(evidence,destination)
   cat('VIOLATION mode',mode,'iteration',i,'caught',caught,'actual',paste(actual,collapse=','),'sender_reaped',is.null(sender),'drained',drained,'arithmetic',evidence$arithmetic,'\n')
   cat('EVIDENCE',destination,'\n')
   print(rows[c('purpose','status')])
   return(1L)
  }
 }
 clean()
 destination<-hunt_output(paste0('native-options-ack-',mode,'-inconclusive.rds'))
 saveRDS(list(mode=mode,iteration=i,caught=caught,expected=expected,actual=actual,sender_reaped=is.null(sender)),destination)
 cat('INCONCLUSIVE mode',mode,'iterations',i,'caught',caught,'sender_reaped',is.null(sender),'\n')
 2L
}
status<-NULL
tryCatch(suspendInterrupts({status<-run()}),interrupt=function(cnd){cat('OUTSIDE-WINDOW interrupt\n');status<<-3L})
quit(status=status,save='no')
