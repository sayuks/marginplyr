out <- 'review/independent'
rows <- list()
check <- function(name, pass, detail='') {
 rows[[length(rows)+1L]] <<- data.frame(case=name,pass=isTRUE(pass),detail=detail)
 cat(if(isTRUE(pass))'PASS' else 'FAIL',name,detail,'\n')
}
for(explicit in c('implicit','explicit'))for(shape in c('sorted','unsorted'))for(action in c('collect','compute')) {
 read<-function(audit)readRDS(file.path(out,paste0(paste('655',audit,explicit,shape,action,sep='-'),'.rds')))
 on<-read('on');off<-read('off');label<-paste(explicit,shape,action)
 if(explicit=='implicit') {
  check(paste(label,'expected-warning-failure'),is.character(off$result$error)&&grepl('Missing values are always removed',off$result$error,fixed=TRUE),off$result$error)
  check(paste(label,'same-error'),identical(on$result$error,off$result$error))
 } else {
  check(paste(label,'no-error'),is.null(off$result$error)&&is.null(on$result$error))
  check(paste(label,'same-values'),identical(on$result$value,off$result$value))
 }
 check(paste(label,'record'),identical(on$record$purpose,'result')&&!anyNA(on$record$sql)&&grepl('SELECT',on$record$sql,fixed=TRUE))
 check(paste(label,'verbosity-restored'),on$verbosity_restored&&off$verbosity_restored)
 check(paste(label,'source-preserved'),on$source_preserved&&off$source_preserved)
}
write.csv(do.call(rbind,rows),file.path(out,'655-results.csv'),row.names=FALSE)
stopifnot(all(vapply(rows,function(x)x$pass,logical(1))))
