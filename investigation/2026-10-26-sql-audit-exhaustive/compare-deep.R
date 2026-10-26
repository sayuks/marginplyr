# Replays saved evidence without a database.
# Rscript compare-deep.R <capture-directory> [fresh-summary.rds]
# Row multisets are compared; this does not establish Margin order or tied-key order.
# PostgreSQL normalization is restricted to generated savepoint names and consecutive
# forwarding copies of savepoint control statements seen at the two DBI generics.
args<-commandArgs(TRUE);stopifnot(length(args)%in%c(1L,2L));root<-normalizePath(args[[1L]],mustWork=TRUE);if(length(args)==2L&&file.exists(args[[2L]]))stop('Output already exists')
normalize_sql<-function(sql)gsub('marginplyr_share_[^\" ]+', 'marginplyr_share_<generated>', sql)
control_trace<-function(sql){if(length(sql)<2L)return(sql);control<-grepl('^(SAVEPOINT|ROLLBACK TO SAVEPOINT|RELEASE SAVEPOINT) ',sql);sql[!c(FALSE,control[-1L]&control[-length(control)]&sql[-1L]==sql[-length(sql)])]}
canonical<-function(x){if(is.data.frame(x)){x<-x[do.call(order,c(as.list(x),list(na.last=TRUE))),,drop=FALSE];rownames(x)<-NULL};x}
total<-0L; failures<-list(); order_only<-character(); coverage<-list()
for(backend in c('sqlite','duckdb','postgres')){
 off<-readRDS(file.path(root,paste0(backend,'-off.rds')));on<-readRDS(file.path(root,paste0(backend,'-on.rds')))
 stopifnot(identical(names(off),names(on)),length(off)==if(backend=='postgres')103L else 102L)
 for(key in names(off)){
  a<-off[[key]];b<-on[[key]];label<-paste(backend,key,sep='/');if(backend=='postgres'){a$sent<-normalize_sql(a$sent);b$sent<-normalize_sql(b$sent)}
  for (field in c('sent', 'seed', 'value', 'conditions', 'render')) {
    if (!identical(a[[field]], b[[field]])) {
      failures[[paste(label, field, sep = '/')]] <- list(off = a[[field]], on = b[[field]])
    }
  }
  if(!identical(a$collected,b$collected)){
   aa<-a$collected;bb<-b$collected;aa$value<-canonical(aa$value);bb$value<-canonical(bb$value)
   if(!identical(aa,bb))failures[[paste(label,'collected',sep='/')]]<-list(off=a$collected,on=b$collected)else order_only<-c(order_only,label)
  }
  rec<-b$record; internal<-rec[rec$purpose!='result',,drop=FALSE]
  expected<-internal$sql;actual<-b$sent;if(backend=='postgres'){expected<-normalize_sql(expected);actual<-control_trace(actual)}
  if(!identical(expected,actual))failures[[paste(label,'coverage',sep='/')]]<-list(record=internal$sql,sent=b$sent)
  if(!is.null(b$render)&&is.character(b$render$value)&&any(rec$purpose=='result')&&!identical(rec$sql[rec$purpose=='result'],b$render$value))failures[[paste(label,'result',sep='/')]]<-list(record=rec$sql[rec$purpose=='result'],render=b$render$value)
  coverage[[label]]<-table(factor(rec$purpose,levels=c('selection_proxy','observed_label_collision','share_dialect','share_dialect_control','share_dialect_transaction','result')))
  total<-total+1L
 }
}
cat('pairs:',total,'\n');cat('failures:',length(failures),'\n');print(names(failures));cat('order-only:',length(order_only),'\n');print(order_only);cat('audit rows:\n');print(Reduce('+',coverage))
if(length(args)==2L)saveRDS(list(pairs=total,failures=failures,order_only=order_only,coverage=coverage),args[[2L]])
sim_off <- readRDS(file.path(root, 'sim-off.rds'))
sim_on <- readRDS(file.path(root, 'sim-on.rds'))
stopifnot(identical(names(sim_off), names(sim_on)), length(sim_off) == 528L)
sim_fail <- character()
for (key in names(sim_off)) {
  for (field in c('value', 'conditions', 'render', 'seed')) {
    if (!identical(sim_off[[key]][[field]], sim_on[[key]][[field]])) {
      sim_fail <- c(sim_fail, paste(key, field, sep = '/'))
    }
  }
}
cat('Simulator pairs:', length(sim_off), 'failures:', length(sim_fail), '\n')
print(sim_fail)
if (length(sim_fail)) stop('Simulator differential failure')
if(length(failures))stop('Differential failure')
