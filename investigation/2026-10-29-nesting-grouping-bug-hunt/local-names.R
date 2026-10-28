pkgload::load_all(quiet=TRUE)
ks <- c('.by','.groups','...','..1','.data','.env','.rows','T','F','n','list','as_tibble','rowwise','data','set','\n','a b','a"b','a`b','NA','Inf', 'x\ty', '.keep', '.key', '{.key}')
fail<-list()
for (n in c(0L,3L)) for (k in ks) for (id in list(NULL,'..1','.env','.data','.groups')) for (keep in c(FALSE,TRUE)) for(verb in c('nest_with_margins','nest_by_with_margins')) {
 if(identical(k,id)) next
 input<-data.frame(g=rep('a',n),x=seq_len(n))
 msg<-paste(n,k,paste(id,collapse=','),keep,verb)
 tryCatch({
   out<-get(verb)(input,.key=k,.id=id,.keep=keep)
   stopifnot(identical(names(out),c(id,k)))
   if(n || verb=='nest_by_with_margins')stopifnot(identical(as.list(out[[k]][[1]]),as.list(input)))
 },error=function(e) { fail[[msg]] <<- conditionMessage(e) })
}
print(fail)
stopifnot(length(fail) == 0L)
cat('PASS: 968 checked cases\n')
