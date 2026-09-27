# Throwaway probe: baseline checkout, then patched R directory.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
pkgload::load_all(args[[1L]], quiet=TRUE)
library(dplyr)
baseline <- marginplyr::summarize_with_margins
ns <- asNamespace('marginplyr')
patch_env <- new.env(parent=ns)
for (f in c('grouping-context.R','grouping-adapter-union.R','summarize_with_margins.R')) {
  sys.source(file.path(args[[2L]],f), envir=patch_env)
}
variant <- patch_env$summarize_with_margins
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
on.exit(DBI::dbDisconnect(con))
inputs <- list(empty=tibble(g=character(),v=double()), full=tibble(g=c('a','b'),v=c(2,3)))
run_case <- function(fun, x, which, sort='none', id=NULL) {
  k <- 0L
  bump <- function() { k <<- k+1L; k }
  q <- switch(which,
    direct = fun(x, bit=grouping_bit(g), ident=grouping_id(), plain=0L, .grouping=grouping_set(g), .sort=sort, .id=id),
    mixed = fun(x, across(v,list(bit=~grouping_bit(g),id=~grouping_id(), n=~sum(.x,na.rm=TRUE),math=~grouping_id()+1L,text=~as.character(grouping_id())), .names='{.col}_{.fn}'), .grouping=grouping_set(g), .sort=sort, .id=id),
    dynamic = fun(x, across(v,list(h=~grouping_id(),n=~sum(.x,na.rm=TRUE)), .names='{.col}_{.fn}_{bump()}'), .grouping=grouping_set(g), .sort=sort, .id=id),
    braced = fun(x, across(v,list(h=~{(grouping_id())},n=function(x) { (grouping_bit(g)) })), .grouping=grouping_set(g), .sort=sort, .id=id),
    enclosing = fun(x, good=local(identical(grouping_id(),0L)), type=local(typeof(grouping_id())), math=grouping_id()+1L, text=as.character(grouping_id()), .grouping=grouping_set(g), .sort=sort, .id=id),
    share = fun(x,total=sum(v,na.rm=TRUE),p=share_of_total(total),h=grouping_id(),.by=g,.grouping=grouping_set(),.sort=sort,.id=id,.check_share_source=FALSE),
    multi = fun(x,bit=grouping_bit(g),ident=grouping_id(),across(v,list(h=~grouping_id(),n=~sum(.x,na.rm=TRUE))),.by=g,.grouping=grouping_sets(grouping_set(),grouping_set()),.duplicates='keep',.sort=sort,.id=id)
  )
  sql <- as.character(dbplyr::sql_render(q))
  stopifnot(!grepl('identity|marginplyr_grouping_output',sql))
  list(out=collect(q), finite=collect(q,n=0), computed=collect(compute(q)), k=k, declared=attr(q,'marginplyr_declared_types'), sql=sql)
}
observations <- list()
for (input in names(inputs)) {
  x <- copy_to(con,inputs[[input]],paste0('x_',input))
  for (which in c('direct','mixed','dynamic','braced','enclosing','share','multi')) {
    for (sort in c('none','last')) for (id in list(NULL,'set')) {
      name <- paste(input,which,sort,if(is.null(id))'noid' else id,sep='/')
      old <- tryCatch(run_case(baseline,x,which,sort,id),error=function(e)e)
      new <- tryCatch(run_case(variant,x,which,sort,id),error=function(e)e)
      observations[[name]] <- list(old=old,new=new)
      if (inherits(old,'error') || inherits(new,'error')) {
        cat(name,'ERROR old:',if(inherits(old,'error'))conditionMessage(old)else'none','NEW:',if(inherits(new,'error'))conditionMessage(new)else'none','\n')
      } else {
        cat(name,'k',old$k,new$k,'declared',paste(names(new$declared),new$declared,collapse=','),'values',isTRUE(all.equal(as.data.frame(old$out),as.data.frame(new$out),check.attributes=FALSE)),'classes',class(old$out)[1],class(new$out)[1],'types',paste(vapply(new$out,typeof,character(1)),collapse=','),'computed',paste(vapply(new$computed,typeof,character(1)),collapse=','),'\n')
      }
    }
  }
}
