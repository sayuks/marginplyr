args <- commandArgs(TRUE)
pkgload::load_all(if(length(args))args[[1L]]else ".",quiet=TRUE)
special <- c(".data",".env",".rows",".SD",".SDall",".xSD",".iSD",".N","T","..marginplyr_nest_1")
shapes <- list(identity=function(x)x,empty=function(x)x[0L])
calls <- comparisons <- refused <- 0L
for(role in c("fixed","dimension","payload"))for(name in special)for(shape in names(shapes)) {
  x<-data.table::data.table(fixed=c("b","a",NA,"a"),dimension=factor(c("x",NA,"y","x"),exclude=NULL),payload=1:4,
    packed=I(list(list(x=1L),NULL,quote(x+y),NA)),row_id=1:4)
  data.table::setnames(x,switch(role,fixed="fixed",dimension="dimension",payload="payload"),name)
  x<-shapes[[shape]](x)
  fixed<-if(role=="fixed")name else "fixed"
  dimension<-if(role=="dimension")name else "dimension"
  before<-serialize(x,NULL,version=3)
  reference<-tibble::as_tibble(x,.name_repair="minimal")
  for(keep in c(FALSE,TRUE))for(sort in c("none","first","last"))for(verb in c("nest_with_margins","nest_by_with_margins"))for(outputs in list(c("data","set"),c("..1","..01"),c(".rows",".SDall"))) {
    key<-outputs[[1L]];id<-outputs[[2L]]
    spec<-rlang::inject(rollup(dplyr::all_of(!!dimension)))
    invoke<-function(data)rlang::inject((get(verb))(data,.by=dplyr::all_of(!!fixed),.grouping=spec,.margin_label=NULL,.keep=keep,.sort=sort,.id=id,.key=key))
    got<-tryCatch(invoke(x),error=identity);want<-tryCatch(invoke(reference),error=identity)
    calls<-calls+2L
    stopifnot(identical(before,serialize(x,NULL,version=3)))
    if(inherits(got,"error")||inherits(want,"error")) {
      expected_refusal <- key %in% c(fixed,dimension) || id %in% names(x) ||
        (identical(verb,"nest_by_with_margins") && ".rows" %in% c(fixed,dimension))
      if(!expected_refusal){
        cat("UNEXPECTED",role,name,shape,keep,sort,verb,key,id,"\n")
        print(got);print(want)
      }
      stopifnot(expected_refusal,inherits(got,"marginplyr_error"),inherits(want,"marginplyr_error"),
        identical(conditionMessage(got),conditionMessage(want)))
      refused<-refused+1L
    }else{
      comparisons<-comparisons+1L
      stopifnot(identical(as.list(dplyr::ungroup(got)),as.list(dplyr::ungroup(want))))
    }
  }
}
cat(calls,"public calls;",comparisons,"local data.table/tibble pairs;",refused,"documented key/id collision or .rows rowwise refusal pairs; PASS: values matched and every serialized source stayed identical\n")
