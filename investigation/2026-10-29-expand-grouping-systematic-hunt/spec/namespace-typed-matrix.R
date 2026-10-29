args <- commandArgs(TRUE)
pkgload::load_all(if(length(args)) args[[1L]] else '.',quiet=TRUE)
output_dir <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = file.path(tempdir(), 'marginplyr-expand-hunt-spec'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- tibble::tibble(region=c('east','west'),amount=1:2)
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
wrap<-function(x,n){for(i in seq_len(n))x<-rlang::call2('(',x);x}
scenarios<-list()
for(op in c('::',':::'))for(helper in c('all_of','any_of','starts_with','ends_with','contains','matches','num_range','everything','last_col','one_of')) {
 scenarios[[paste(op,helper,sep='-')]]<-list(kind='leaf',nsop=op,name=helper,expr=rlang::call2(rlang::call2(op,quote(tidyselect),rlang::sym(helper)),'amount'))
}
for(op in c('::',':::'))for(fun in c('names','as.character','length','paste','tolower','unname')) {
 arg<-rlang::call2(rlang::call2(op,quote(base),rlang::sym(fun)),'amount')
 scenarios[[paste(op,'arg',fun,sep='-')]]<-list(kind='argument',nsop=op,name=fun,expr=rlang::call2('all_of',arg))
}
observations<-list()
for(scenario in names(scenarios))for(mode in c('direct','opaque'))for(depth in 0:1)for(backend in c('local','dtplyr'))for(position in c('by',constructors,'overlap')){
 s<-scenarios[[scenario]]
 env<-rlang::env(globalenv(),calls=0L)
 env$typed<-function(...){env$calls<-env$calls+1L;names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]}
 operator<-function(package,name){
  pkg<-as.character(substitute(package));nm<-as.character(substitute(name))
  if((s$kind=='leaf'&&pkg=='tidyselect'&&nm==s$name)||(s$kind=='argument'&&pkg=='base'&&nm==s$name))return(env$typed)
  getExportedValue(pkg,nm)
 }
 env[[s$nsop]]<-operator
 expression<-if(mode=='opaque')rlang::call2('identity',s$expr) else s$expr
 q<-rlang::new_quosure(wrap(expression,depth),env)
 stopifnot(identical(names(tidyselect::eval_select(q,data)),'region'))
 input<-if(backend=='local')data else dtplyr::lazy_dt(data)
 env$calls<-0L
 actual<-tryCatch({
  if(position=='by')inspect_grouping(input,.by=!!q,.format='list')$fixed
  else if(position=='overlap')expand_with_margins(input,.by=!!q,.grouping=rollup(region),.margin_label=NULL)
  else{
   spec<-eval(rlang::call2(position,q))
   inspect_grouping(input,.grouping=spec,.format='list')$included
  }
 },error=identity)
 expected<-if(position=='by')list('region') else if(position %in% c('rollup','cube'))list('region',character()) else list('region')
 match<-if(position=='overlap')inherits(actual,'marginplyr_error')&&grepl('both',conditionMessage(actual),fixed=TRUE) else identical(actual,expected)
 observations[[length(observations)+1L]]<-data.frame(mode,scenario,kind=s$kind,depth,backend,position,calls=env$calls,match,condition=if(inherits(actual,'error'))class(actual)[[1L]] else '',stringsAsFactors=FALSE)
}
out<-do.call(rbind,observations)
write.csv(out,file.path(output_dir,'namespace-typed-matrix.csv'),row.names=FALSE)
print(with(out,table(mode,backend,position,match)))
print(with(out,table(mode,kind,match)))
cat('observations=',nrow(out),'\n',sep='')
stopifnot(all(out$match[out$mode=='opaque']))
