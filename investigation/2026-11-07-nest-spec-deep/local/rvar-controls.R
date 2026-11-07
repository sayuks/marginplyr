Sys.setenv(TESTTHAT_PARALLEL='false');pkgload::load_all('.',quiet=TRUE)
x<-tibble::tibble(g=posterior::rvar(matrix(c(1,1,2,2),nrow=2)),rid=1:2)
before<-serialize(x,NULL)
for(fun in c('vctrs_group','native_nest','margin_nest','margin_nest_keep','margin_fixed','margin_payload')) {
 y<-tryCatch(switch(fun,vctrs_group=vctrs::vec_group_loc(x['g']),native_nest=tidyr::nest(x,.by=g),margin_nest=nest_with_margins(x,.grouping=grouping_set(g)),margin_nest_keep=nest_with_margins(x,.grouping=grouping_set(g),.keep=TRUE),margin_fixed=nest_with_margins(x,.by=g),margin_payload=nest_with_margins(x)),error=identity)
 cat(fun,':',if(inherits(y,'error'))conditionMessage(y)else paste('OK rows',nrow(y)),'serialized unchanged',identical(before,serialize(x,NULL)),'\n')
}
