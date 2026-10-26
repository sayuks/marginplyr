# Run from the checkout root in a fresh R process.
# Rscript simulator-deep.R <off|on> <fresh-output.rds>
main<-function(args){
 stopifnot(length(args)==2L,args[[1]]%in%c('on','off'),!file.exists(args[[2]]))
 pkgload::load_all(quiet=TRUE);audited<-identical(args[[1]],'on');options(marginplyr.audit_sql=audited)
 script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[[1L]])
 case_sets<-dget(file.path(dirname(script),'cases.R'));cases<-c(case_sets$cases,case_sets$extra);weird<-case_sets$weird
 sims<-sort(grep('^simulate_',getNamespaceExports('dbplyr'),value=TRUE));results<-list()
 capture<-function(expr){conditions<-list();value<-withCallingHandlers(tryCatch(force(expr),error=identity),condition=function(cnd){if(inherits(cnd,c('warning','message','error')))conditions[[length(conditions)+1L]]<<-list(class=class(cnd),message=conditionMessage(cnd));if(inherits(cnd,'warning')&&!is.null(findRestart('muffleWarning')))invokeRestart('muffleWarning');if(inherits(cnd,'message')&&!is.null(findRestart('muffleMessage')))invokeRestart('muffleMessage')});list(value=value,conditions=conditions)}
 simplify<-function(value)if(inherits(value,'condition'))list(class=class(value),message=conditionMessage(value))else if(inherits(value,'tbl_lazy'))list(class=class(value),vars=dplyr::tbl_vars(value))else value
 for(sim in sims){
  con<-getExportedValue('dbplyr',sim)();base<-dbplyr::tbl_lazy(data.frame(g='a',h='b',v=1,w=1L,flag=TRUE,date=as.Date('2020-01-01')),con=con)
  odd<-dbplyr::tbl_lazy(setNames(data.frame('a','b',1,2),c('TRUE','FALSE','.env','..marginplyr_order_1')),con=con)
  verdicts<-getFromNamespace('share_dialect_verdicts','marginplyr');rm(list=ls(verdicts),envir=verdicts)
  for(kind in c('base','weird'))for(key in names(if(kind=='base')cases else weird)){
   S<-if(kind=='base')base else odd;expr<-(if(kind=='base')cases else weird)[[key]];set.seed(880);out<-capture(eval(expr));rec<-if(audited)last_sent_queries(details=TRUE)
   later<-if(inherits(out$value,'tbl_lazy'))capture(as.character(dbplyr::sql_render(out$value)))else NULL
   if(audited)stopifnot(identical(rec,last_sent_queries(details=TRUE)))
   results[[paste(sim,kind,key,sep='/')]]<-list(value=simplify(out$value),conditions=out$conditions,render=if(!is.null(later))list(value=simplify(later$value),conditions=later$conditions),record=rec,seed=.Random.seed)
  }
 }
 saveRDS(results,args[[2]]);cat(length(sims),'simulators;',length(results),'cases complete\n')
}
main(commandArgs(TRUE))
