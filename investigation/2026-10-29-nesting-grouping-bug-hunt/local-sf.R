pkgload::load_all(quiet=TRUE)
library(dplyr)
sfc<-sf::st_sfc(lapply(1:3,function(i) sf::st_point(c(i,4-i))),crs=4326)
sfc2<-sf::st_sfc(lapply(1:3,function(i) sf::st_point(c(2*i,5-i))),crs=4326)
fail<-list();count<-0L
normalize<-function(x,key)list(outer=as.list(ungroup(x)[setdiff(names(x),key)]),cells=lapply(x[[key]],as.list))
for(n in c(0L,3L))for(nm in c('geometry','.data','.env','.rows','..marginplyr_nest_1','data'))for(multi in c(FALSE,TRUE))for(active_second in c(FALSE,TRUE)) {
 if(active_second && !multi) next
 input<-sf::st_sf(g=c('b','a','b'),rid=1:3,geometry=sfc)
 names(input)[names(input)=='geometry']<-nm
 sf::st_geometry(input)<-nm
 if(multi)input$other_shape<-sfc2
 if(active_second)sf::st_geometry(input)<-'other_shape'
 input<-input[seq_len(n),]
 control<-as_tibble(input)
 for(role in c('none','fixed_g','dim_g','fixed_geom','dim_geom','both_fixed','composite_dim'))for(keep in c(FALSE,TRUE))for(sort in c('none','last','first'))for(verbname in c('nest_with_margins','nest_by_with_margins'))for(key in c('payload',nm)) {
  bycols<-switch(role,fixed_g='g',fixed_geom=nm,both_fixed=c('g',nm),character())
  dimcols<-switch(role,dim_g='g',dim_geom=nm,composite_dim=c('g',nm),character())
  if(key %in% c(bycols,dimcols))next
  if(verbname=='nest_by_with_margins'&&'.rows'%in%c(bycols,dimcols))next
  before<-serialize(input,NULL)
  run<-function(x)get(verbname)(x,.by=all_of(bycols),.grouping=if(length(dimcols))rollup(all_of(dimcols)),.margin_label=NULL,.keep=keep,.sort=sort,.key=key,.id='set')
  result<-tryCatch(run(input),error=function(e)e)
  expected<-tryCatch(run(control),error=function(e)e)
  count<-count+1L;tag<-paste(n,nm,multi,active_second,role,keep,sort,verbname,key)
  if(inherits(result,'error') || inherits(expected,'error')) {
   if(inherits(result,'error') != inherits(expected,'error') || !identical(conditionMessage(result),conditionMessage(expected)))fail[[tag]]<-list(actual=if(inherits(result,'error'))conditionMessage(result) else 'success',expected=if(inherits(expected,'error'))conditionMessage(expected)else'success')
  } else if(!identical(normalize(result,key),normalize(expected,key)))fail[[tag]]<-list(actual=result,expected=expected)
  if(!identical(before,serialize(input,NULL)))fail[[paste(tag,'mutated')]]<-TRUE
 }
}
cat('checked',count,'failures',length(fail),'\n');print(fail)
stopifnot(length(fail) == 0L)
cat('PASS: 4176 checked cases\n')
