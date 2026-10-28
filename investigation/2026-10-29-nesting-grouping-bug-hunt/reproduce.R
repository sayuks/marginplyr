pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x<-tibble(g=c('a','b','a'),v=1:3)
verbs<-list(nest=nest_with_margins,nest_by=nest_by_with_margins,expand=expand_with_margins,summarize=summarize_with_margins)
run<-function(verb,input,id) {
 if(identical(verb,summarize_with_margins)) verb(input,n=n(),.grouping=rollup(g),.id=id,.sort='last') else verb(input,.grouping=rollup(g),.id=id,.sort='last')
}
force_result<-function(z) if(inherits(z,'dtplyr_step')) tibble::as_tibble(z,.name_repair='minimal') else z
failures<-list()
for(id in c('...','..1','..2','..0','..01')) {
 upstream<-tibble::as_tibble(mutate(dtplyr::lazy_dt(x),!!id:=1L),.name_repair='minimal')
 stopifnot(identical(upstream[[id]],rep(1L,nrow(x))))
 cat('PASS ordinary dtplyr mutation/collection:',id,'\n')
 for(name in names(verbs)) {
  v<-verbs[[name]]
  local<-force_result(run(v,x,id));safe_dt<-force_result(run(v,dtplyr::lazy_dt(x),'set'))
  stopifnot(id %in% names(local),'set' %in% names(safe_dt))
  phase<-'construction'; captured_calls<-NULL
  e<-tryCatch(withCallingHandlers({result<-run(v,dtplyr::lazy_dt(x),id);phase<-'collection';force_result(result);NULL},error=function(cnd) { captured_calls<<-sys.calls() }),error=identity)
  if(!is.null(e)) {
   calls<-vapply(captured_calls,function(call)paste(deparse(call),collapse=' '),character(1))
   stage<-if(any(grepl('get_col_names',calls,fixed=TRUE))) 'nest_expanded_margins()/get_col_names()' else if(any(grepl('reorder_margin_columns',calls,fixed=TRUE))) 'finalize_margin_operation()/reorder_margin_columns()' else if(identical(phase,'collection')) 'dtplyr::dt_eval()/data.table execution' else 'unknown'
   failures[[length(failures)+1L]]<-list(verb=name,id=id,error_class=class(e)[1],message=conditionMessage(e))
   cat('FAIL',name,id,'phase=',phase,'stage=',stage,'class=',class(e)[1],':',gsub('\n',' / ',conditionMessage(e)),'\n')
  } else cat('PASS Margin result',name,id,'\n')
 }
}
cat('Failures:',length(failures),'of',length(verbs)*5L,'cases\n')
if('--assert' %in% commandArgs(TRUE) && length(failures)>0L) stop('BUG REPRODUCED: dtplyr Margin output identifiers are rejected internally.')
