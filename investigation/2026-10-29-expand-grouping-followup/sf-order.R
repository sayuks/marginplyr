# Evidence only. Run against the working tree; --assert exits one on the audited baseline.
args <- commandArgs(trailingOnly=TRUE)
root <- Sys.getenv('MARGINPLYR_HUNT_ROOT', '.')
pkgload::load_all(root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
base <- sf::st_sf(
 k=c('q','p'),g=c('b','a'),rid=1:2,
 geometry=sf::st_sfc(sf::st_point(c(0,0)),sf::st_point(c(1,1)))
)
cat('Minimal reproduction\n')
y<-expand_with_margins(base,.by=geometry,.grouping=rollup(g),.margin_label=NULL,.id='sid')
cat('Expected: geometry,g,sid,k,rid\nObserved:',paste(names(y),collapse=','),'\n')
cat('Rows:',nrow(y),'\n')
# Public column names, cardinality, row copies and geometry values are inspected separately.
# The class/object-attribute caveat in ADR 0016 is not treated as a preservation promise.
cases<-0L;bad_order<-0L;value_fail<-0L;source_fail<-0L;ordinary_success<-0L;alias_success<-0L;sf_upstream<-0L
for(n in c(0L,2L))for(position in c('first','middle','last'))for(id in list(NULL,'sid','..1'))for(sort in c('none','first','last'))for(shape in c('fixed_only','fixed_rollup','dimension_only','fixed_composite')){
 x<-base[seq_len(n),]
 fields<-switch(position,first=c('geometry','k','g','rid'),middle=c('k','geometry','g','rid'),last=c('k','g','rid','geometry'))
 # Raw list rearrangement leaves geometry name metadata valid and controls the input order.
 x<-x[,fields,drop=FALSE]
 # select.sf forces active geometry last, so explicit reconstruction sets the wanted order.
 x<-structure(unclass(x)[fields],class=class(x),row.names=.set_row_names(n),sf_column='geometry',agr=attr(x,'agr'))
 snap<-serialize(x,NULL)
 by<-if(shape=='dimension_only')'k' else 'geometry'
 spec<-switch(shape,fixed_only=NULL,fixed_rollup=rollup(g),dimension_only=grouping_set(geometry),fixed_composite=rollup(grouping_set(k,g)))
 dims<-switch(shape,fixed_only=character(),fixed_rollup='g',dimension_only='geometry',fixed_composite=c('k','g'))
 if(shape=='dimension_only')by<-'k'
 run<-function(z)expand_with_margins(z,.by=all_of(by),.grouping=spec,.margin_label=NULL,.id=id,.sort=sort)
 actual<-run(x);control<-run(as_tibble(x))
 expected_names<-c(by,dims,id,setdiff(names(x),c(by,dims)))
 cases<-cases+1L
 good<-identical(names(actual),expected_names)
 if(!good)bad_order<-bad_order+1L
 if(identical(id,'..1'))alias_success<-alias_success+as.integer(good) else ordinary_success<-ordinary_success+as.integer(good)
 # The backend method's own reordering is a cause control, not the source of the promise.
 upstream<-dplyr::select(x,all_of(c(by,dims)),everything())
 if(!identical(names(upstream),c(by,dims,setdiff(names(x),c(by,dims)))))sf_upstream<-sf_upstream+1L
 actual_list<-as.list(actual);control_list<-as.list(control)
 values_ok<-all(vapply(names(actual),function(col){
  a<-actual_list[[col]];b<-control_list[[col]]
  if(inherits(a,'sfc')){a<-as.character(a);b<-as.character(b)}
  isTRUE(all.equal(a,b,check.attributes=FALSE))
 },logical(1)))
 if(!values_ok||nrow(actual)!=nrow(control))value_fail<-value_fail+1L
 if(!identical(serialize(x,NULL),snap))source_fail<-source_fail+1L
 cat('CASE',cases,'rows',n,'input',position,'id',if(is.null(id))'<NULL>'else id,'sort',sort,'shape',shape,'order',good,'values',values_ok,'actual',paste(names(actual),collapse=','),'\n')
}
cat('TOTAL',cases,'ORDER_FAILURES',bad_order,'VALUE_FAILURES',value_fail,'SOURCE_MUTATIONS',source_fail,'ORDINARY_ID_ORDER_PASSES',ordinary_success,'DOTDOT_ID_ORDER_PASSES',alias_success,'UPSTREAM_ORDER_DIFFERENCES',sf_upstream,'\n')
if('--assert'%in%args&&bad_order>0L)quit(status=1L)
stopifnot(value_fail==0L,source_fail==0L)
