# Run from the repository root; exploratory evidence for 2026-10-28.
wt <- '.'
pkgload::load_all(wt, quiet=TRUE)
canon <- function(x, nms, id, key) {
  x <- dplyr::ungroup(x)
  names(x)[names(x) == key] <- 'CELL'
  names(x)[names(x) == id] <- 'ID'
  for (j in seq_along(nms)) names(x)[names(x)==nms[[j]]] <- c('g','h','p')[[j]]
  x$CELL <- lapply(x$CELL,function(z){for(j in seq_along(nms)) names(z)[names(z)==nms[[j]]] <- c('g','h','p')[[j]]; as.list(z)})
  as.list(x)
}
callit <- function(d,nms,id,key,keep,sort,verb) {
  rlang::inject(verb(d, .by = dplyr::all_of(!!nms[[1]]), .grouping=rollup(dplyr::all_of(!!nms[[2]])), .id=id,.key=key,.keep=keep,.sort=sort))
}
source <- tibble::tibble(g=c('y','x','x',NA),h=c('b','a','b','a'),p=1:4)
nms <- c('.rows','.data','.env','...', '..1','.by','.key','.keep','.sort','.id','list','data','T','F','NA','NULL','Inf','NaN','function','x y','x\ny','x\ty','x`y','x{y}','x\\y','é','a b','a.b','..marginplyr_nest_1','_margin_set_id')
errors <- list(); counter<-0L
for(v in list(nest_with_margins,nest_by_with_margins)) for(keep in c(FALSE,TRUE)) for(s in c('none','first','last')) {
 base<-canon(callit(source,c('g','h','p'),'SET','DATA',keep,s,v),c('g','h','p'),'SET','DATA')
 for(role in 1:5) for(n in nms){
  counter<-counter+1L
  if(identical(v,nest_by_with_margins)&&n=='.rows'&&role%in%c(1,2,4))next
  names_now<-c('g','h','p');id<-'SET';key<-'DATA'
  if(role<=3)names_now[[role]]<-n else if(role==4)id<-n else key<-n
  d<-source;names(d)<-names_now
  result<-tryCatch(canon(callit(d,names_now,id,key,keep,s,v),names_now,id,key),error=identity)
  if(inherits(result,'error')||!identical(result,base)){
   rec<-list(verb=deparse(substitute(v)),by=inherits(v, 'x'),keep=keep,sort=s,role=role,name=n,result=result)
   errors[[length(errors)+1L]]<-rec
   cat('BAD',if(identical(v,nest_by_with_margins))'by' else 'nest',keep,s,role,encodeString(n),if(inherits(result,'error'))conditionMessage(result)else paste(capture.output(str(result)),collapse=' '),'\n')
  }
 }
}
cat('TOTAL',counter,'BAD',length(errors),'\n');saveRDS(errors,'/tmp/marginplyr-4306-local-name-errors.rds')
