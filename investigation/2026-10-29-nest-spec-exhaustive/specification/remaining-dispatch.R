pkgload::load_all('.', quiet=TRUE)
values <- list(matrix=matrix(1L),array=array(1L,c(1L,1L,1L)),integer=1L,numeric=1.2,logical=TRUE,character='x',raw=as.raw(1L),complex=1+1i,list=list(1L))
functions <- c('names','length','rep','rep.int','rep_len','as.character','as.integer','as.numeric','as.vector','is.na','is.character','is.numeric','is.logical','is.integer','order','setequal','unname','match','seq_along','nchar','nzchar','unlist','[','[[','$','c','+')
observations <- list()
for(fn in c(functions,'format','unique','sort')) for(cl in names(values)) {
  count <- 0L
  method <- function(...) { count <<- count + 1L; 'a' }
  method_name <- paste(fn,cl,sep='.')
  stopifnot(!exists(method_name,envir=.GlobalEnv,inherits=FALSE))
  assign(method_name,method,envir=.GlobalEnv)
  x<-values[[cl]]
  code<-switch(fn,setequal=call(fn,x,x),match=call(fn,x,x),'['=call(fn,x,1L),'[['=call(fn,x,1L),'$'=call(fn,list(a=x),quote(a)),rep.int=call(fn,x,1L),rep_len=call(fn,x,1L),call(fn,x))
  result<-suppressWarnings(tryCatch(eval(code),error=identity))
  rm(list=method_name,envir=.GlobalEnv)
  observations[[length(observations)+1L]]<-data.frame(operation=fn,implicit_class=cl,dispatches=count,error=inherits(result,'error'))
}
result<-do.call(rbind,observations)
remaining<-result[result$operation %in% functions,]
controls<-result[!result$operation %in% functions,]
stopifnot(nrow(remaining)==243L,all(remaining$dispatches==0L),nrow(controls)==27L,all(controls$dispatches==1L))
cat('remaining whitelist probes=',nrow(remaining),' dispatched=0; ordinary generic calibration probes=',nrow(controls),' dispatched=',sum(controls$dispatches),'\n',sep='')
cat('Direct-call refusals among remaining probes=',sum(remaining$error),' (these were dispatch observations, not successful selections).\n',sep='')
