suppressPackageStartupMessages({pkgload::load_all('.',quiet=TRUE);library(dplyr);library(dtplyr)})
set.seed(301030L)
cases<-list();status<-character();details<-rep('',1600L)
rhs<-list(quote(a),quote(b),quote(c),quote(1L),quote(1),quote('text'),NULL,quote(a+1L),quote(a/b),quote(-a),quote(a+b))
for(i in 1:1600) {
 n<-sample(c(0L,1L,3L),1L)
 b<-switch(as.character(i%%6L),'0'=letters[seq_len(n)],'1'=as.numeric(seq_len(n)),'2'=seq_len(n),'3'=factor(letters[seq_len(n)],levels=letters[1:4]),'4'=as.Date('2026-01-01')+seq_len(n),'5'=rep(TRUE,n))
 d<-tibble::tibble(a=seq_len(n),b=b,c=seq_len(n)+1L)
 count<-sample(1:5,1)
 updates<-lapply(sample(seq_along(rhs),count,replace=TRUE),function(k)rhs[[k]])
 names(updates)<-sample(c('a','b','c','derived','copy'),count,replace=TRUE)
 root<-lazy_dt(d)
 s<-tryCatch(rlang::eval_tidy(rlang::expr(mutate(root,!!!updates))),error=identity)
 if(inherits(s,'error')) {status[i]<-'query_error';next}
 got<-tryCatch(suppressWarnings(collect(s)),error=identity)
 if(inherits(got,'error')) {status[i]<-'source_error';next}
 results<-lapply(c('integer','double','character','logical','complex','list'),function(kind) {
  pred<-local({k<-kind;function(x)identical(typeof(x),k)})
  actual<-tryCatch(inspect_grouping(s,.by=where(pred),.format='list')$fixed[[1]],error=identity)
  if(inherits(actual,'error'))return(if(inherits(actual,'marginplyr_error'))'refused'else 'external')
  exp<-names(got)[vapply(got,pred,logical(1))]
  if(identical(actual,exp))'pass'else paste('MISMATCH',kind,paste(actual,collapse=','),paste(exp,collapse=','))
 })
 bad<-grep('MISMATCH|external',results)
 status[i]<-if(length(bad))paste(results[bad],collapse=';')else if(all(results=='refused'))'refused'else 'pass'
 if(length(bad)) {details[i]<-paste(deparse(rlang::expr(mutate(root,!!!updates))),collapse=' ');cat(i,status[i],details[i],'\n')}
}
print(table(status));write.csv(data.frame(case=seq_along(status),status=status,details=ifelse(is.na(details),'',details)), file.path(Sys.getenv('MARGINPLYR_HUNT_OUTPUT',tempdir()),'dtplyr-mutate-results.csv'),row.names=FALSE)
