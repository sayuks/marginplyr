pkgload::load_all('.', quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
input <- data.frame(a='x')
root <- dtplyr::lazy_dt(input)
derived <- mutate(root, added=paste0(a,'!'))
read_types <- logical()
pattern <- NULL
methods <- c('length.stringr_regex','length.stringr_fixed','length.stringr_coll','length.stringr_pattern','length.unrelated_pattern')
old <- mget(methods,envir=.GlobalEnv,inherits=FALSE,ifnotfound=list(NULL))
restore <- function() for(nm in methods) {
 if(is.null(old[[nm]])) {if(exists(nm,envir=.GlobalEnv,inherits=FALSE))rm(list=nm,envir=.GlobalEnv)} else assign(nm,old[[nm]],envir=.GlobalEnv)
}
probe <- function(expr) {
 read_types <<- logical()
 answer <- tryCatch(eval(expr,envir=.GlobalEnv),error=identity)
 list(error=inherits(answer,'error'),class=if(inherits(answer,'error'))class(answer) else class(answer),value=if(inherits(answer,'error'))conditionMessage(answer) else answer,reads=read_types)
}
run <- function() {
 on.exit(restore())
 unrelated<-0L
 assign('length.unrelated_pattern',function(x) {unrelated<<-unrelated+1L;stop('unrelated method ran')},envir=.GlobalEnv)
 plain <- list()
 patterns <- list(regex=stringr::regex('a'),fixed=stringr::fixed('a'),coll=stringr::coll('a'))
 results<-list(); k<-0L
 for(kind in names(patterns)) {
  pattern <<- patterns[[kind]]
  p <- probe(quote(inspect_grouping(derived,.grouping=grouping_set(matches(pattern)),.format='list')))
  stopifnot(!p$error,identical(p$value$included,list(c('a','added'))))
  plain[[kind]]<-p
 }
 stopifnot(unrelated==0L)
 dispatch <- function(x) {
  typed<-is.character(tidyselect::peek_data()$a)
  read_types <<- c(read_types,typed)
  if(typed)1L else 2L
 }
 for(kind in names(patterns))for(level in c('exact','parent')) {
  pattern <<- patterns[[kind]]
  method_name<-if(level=='parent')'length.stringr_pattern' else paste0('length.',class(pattern)[[1]])
  assign(method_name,dispatch,envir=.GlobalEnv)
  direct <- probe(quote(tidyselect::eval_select(rlang::quo(matches(pattern)),input)))
  wrapped<-probe(quote(inspect_grouping(input,.grouping=grouping_set(matches(identity(pattern))),.format='list')))
  stopifnot(!direct$error,identical(names(direct$value),'a'),length(direct$reads)>0L,all(direct$reads),!wrapped$error,identical(wrapped$value$included,list('a')),all(wrapped$reads))
  for(surface in c('inspect','expand','fixed','dt_root','dt_derived')) {
   call<-switch(surface,
    inspect=quote(inspect_grouping(input,.grouping=grouping_set(matches(pattern)),.format='list')),
    expand=quote(expand_with_margins(input,.grouping=grouping_set(matches(pattern)))),
    fixed=quote(inspect_grouping(input,.by=matches(pattern),.format='list')),
    dt_root=quote(inspect_grouping(root,.grouping=grouping_set(matches(pattern)),.format='list')),
    dt_derived=quote(inspect_grouping(derived,.grouping=grouping_set(matches(pattern)),.format='list')))
   actual<-probe(call)
   desired <- if(surface == "dt_derived") {
    actual$error && "marginplyr_error" %in% actual$class &&
     grepl("Can't safely determine column metadata", actual$value, fixed=TRUE)
   } else !actual$error && length(actual$reads)>0L && all(actual$reads)
   k<-k+1L; results[[k]]<-data.frame(kind,level,surface,error=actual$error,typed_reads=all(actual$reads),read_count=length(actual$reads),desired=desired,message=if(actual$error)actual$value else '')
   if(!("--assert" %in% commandArgs(TRUE))) stopifnot(actual$error,length(actual$reads)>0L,!any(actual$reads),grepl('patterns must be length 1',actual$value,fixed=TRUE))
  }
  rm(list=method_name,envir=.GlobalEnv)
 }
 r<-bind_rows(results);print(r,row.names=FALSE);cat('Independent dispatch comparisons:',nrow(r),'misclassified; ordinary/wrapped controls: 12 passed; unrelated method calls:',unrelated,'\n')
 if('--assert' %in% commandArgs(TRUE))stopifnot(all(r$desired))
 invisible(r)
}
run()
