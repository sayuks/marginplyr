suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
options(lifecycle_verbosity = 'quiet', width = 120)
phase <- 'setup'
inspection_warnings <- 0L
globalCallingHandlers(warning = function(cnd) {
 cat('Warning phase:', phase, conditionMessage(cnd), '\n')
 if (endsWith(phase, 'inspection')) inspection_warnings <<- inspection_warnings + 1L
})
observe <- function(expr) tryCatch(force(expr),error=identity)
checks<-0L;failures<-list();inspections<-0L;refusals<-0L
record<-function(label,truth,detail=NULL){checks<<-checks+1L;if(!isTRUE(truth))failures[[label]]<<-detail}
preds<-list(integer=is.integer,double=is.double,numeric=is.numeric,character=is.character,logical=is.logical,factor=is.factor,list=is.list,object=is.object)
callbacks<-0L
row_filter<-function(x){callbacks<<-callbacks+1L;x>0L}
row_type<-function(x){callbacks<<-callbacks+1L;if(length(x))'actual'else 0L}
base_ops<-list(
 root=identity,
 unsafe_scalar=function(x)dplyr::mutate(x,a='fixed'),
 unsafe_callback=function(x)dplyr::mutate(x,a=row_type(a)),
 unsafe_repeated=function(x)dplyr::mutate(x,a=b,a=row_type(a)),
 copy=function(x)dplyr::mutate(x,copy=a),
 tag=function(x)dplyr::mutate(x,tag='fixed'),
 numeric=function(x)dplyr::mutate(x,a=a/2L,b=-b,a=a+1L),
 constants=function(x)dplyr::mutate(x,number=1/2L,tag='fixed',added_flag=TRUE),
 aliases=function(x)dplyr::mutate(x,one=1L,two=one,three=two+b,one=NULL),
 select=function(x)dplyr::select(x,z,b,a),
 rename=function(x)dplyr::rename(x,renamed=a),
 arrange=function(x)dplyr::arrange(x,dplyr::desc(a)),
 filter=function(x)dplyr::filter(x,row_filter(a)),
 filter_select=function(x)dplyr::select(dplyr::filter(x,row_filter(a)),a,z),
 copy_filter=function(x)dplyr::filter(dplyr::mutate(x,copy=a+1L),row_filter(a)),
 cached=function(x)dplyr::compute(dplyr::mutate(x,copy=a+1L)),
 cache_rename=function(x)dplyr::rename(dplyr::compute(dplyr::mutate(x,copy=a+1L)),renamed=copy),
 cached_join=function(x){l<-dplyr::compute(dplyr::mutate(x,t=a+1L));r<-dplyr::compute(dplyr::mutate(x,t='fixed'));dplyr::inner_join(l,r,by='a')},
 cached_union=function(x){l<-dplyr::compute(dplyr::mutate(x,t=a+1L));dplyr::union_all(l,l)},
 cached_rename_union=function(x){l<-dplyr::compute(dplyr::rename(x,renamed=a));dplyr::union_all(l,l)},
 cached_semi=function(x){l<-dplyr::compute(dplyr::rename(x,renamed=a));dplyr::semi_join(l,l,by='renamed')}
)
for(rows in c(0L,1L,5L))for(immutable in c(TRUE,FALSE))for(op_name in names(base_ops)) {
 source<-data.table::data.table(a=seq_len(rows),b=seq_len(rows)/2,z=rep(c('a','b'),length.out=rows),flag=rep(TRUE,rows))
 before<-data.table::copy(source)
 operation<-base_ops[[op_name]]
 phase<-paste(rows,immutable,op_name,'construct');graph<-operation(dtplyr::lazy_dt(source,immutable=immutable))
 reference_graph<-operation(dtplyr::lazy_dt(data.table::copy(before)))
 phase<-paste(rows,immutable,op_name,'oracle');reference<-observe(dplyr::collect(reference_graph))
 if(inherits(reference,'error')){record(paste(rows,immutable,op_name,'oracle'),FALSE,conditionMessage(reference));next}
 for(pred_name in names(preds))for(role in c('by','set')) {
  pred<-preds[[pred_name]]
  expected<-names(tidyselect::eval_select(rlang::new_quosure(rlang::expr(dplyr::where(!!pred)),environment()),reference))
  callbacks<-0L;phase<-paste(rows,immutable,op_name,pred_name,role,'inspection')
  result<-observe(if(role=='by')rlang::inject(inspect_grouping(graph,.by=dplyr::where(!!pred),.format='list'))else rlang::inject(inspect_grouping(graph,.grouping=grouping_set(dplyr::where(!!pred)),.format='list')))
  inspections<-inspections+1L
  label<-paste(rows,immutable,op_name,pred_name,role,sep='/')
  got<-if(inherits(result,'error'))conditionMessage(result)else if(role=='by')result$fixed[[1L]]else result$included[[1L]]
  if(startsWith(op_name,'unsafe_')) {
   refusals<-refusals+1L
   record(paste(label,'refusal'),inherits(result,'marginplyr_error') && grepl("Can't safely determine column metadata",got,fixed=TRUE),got)
  } else record(paste(label,'selection'),identical(got,expected),list(expected=expected,actual=got))
  record(paste(label,'rows-unread'),callbacks==0L,callbacks)
  record(paste(label,'source-unchanged'),identical(source,before),list(source=source,before=before))
 }
}
record('no-new-inspection-warnings', inspection_warnings == 0L, inspection_warnings)
cat('Metadata inspections:',inspections,'expected refusals:',refusals,'assertions:',checks,'failures:',length(failures),'\n')
if(length(failures)){print(head(failures,20L));quit(status=1L)}
