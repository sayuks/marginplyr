pkgload::load_all(quiet=TRUE)
input <- dbplyr::tbl_lazy(data.frame(v=1))
expected <- list(rlib_warning_verbosity='default', lifecycle_verbosity='default')
options(c(list(marginplyr.audit_sql=TRUE),expected))
exit_status <- NULL
stress <- function() {
 pid <- Sys.getpid()
 sender <- parallel::mcparallel({
  Sys.sleep(0.15)
  end <- Sys.time()+20
  while(Sys.time()<end) {tools::pskill(pid,2L);Sys.sleep(runif(1L,0.005,0.015))}
  NULL
 },silent=TRUE,mc.set.seed=FALSE)
 stop_sender <- function() {
  if(!is.null(sender)) {tools::pskill(sender$pid,9L); suppressWarnings(parallel::mccollect(sender,wait=TRUE));sender<<-NULL}
 }
 on.exit(stop_sender(),add=TRUE)
 end <- Sys.time()+20
 caught <- 0L
 leaked <- FALSE
 for(i in seq_len(20000L)) {
  if(Sys.time()>=end) break
  outcome <- tryCatch(allowInterrupts(summarize_with_margins(input, answer=sum(v,na.rm=TRUE))),interrupt=identity)
  caught <- caught + inherits(outcome,'interrupt')
  observed <- setNames(lapply(names(expected),getOption),names(expected))
  if(!identical(observed,expected)) {leaked <- TRUE;break}
 }
 stop_sender()
 cat('ITERATIONS',i,'INTERRUPTS',caught,'OPTIONS_LEAK',leaked,'\n')
 print(observed)
 print(last_sent_queries(details=TRUE))
 if(leaked) return(1L)
 2L
}
tryCatch(suspendInterrupts({exit_status<-stress()}),interrupt=function(cnd){cat('DEFERRED interrupt\n');if(is.null(exit_status))exit_status<<-2L})
quit(status=exit_status)
