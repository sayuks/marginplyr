# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
inner<-tibble::tibble(a=c(NA,1,NA,1,2),b=c(1,NA,NA,1,2))
for(kind in c('packed','matrix','array')){
 d<-tibble::tibble(g=switch(kind,packed=inner,matrix=as.matrix(inner),array=array(as.matrix(inner),c(5,1,2))),v=seq_len(5))
 for(role in c('by','dimension'))for(s in c('first','last')){
   r<-rlang::inject(nest_with_margins(d,.by=!!(if(role=='by')rlang::sym('g')else NULL),.grouping=!!(if(role=='dimension')rollup(g)else NULL),.margin_label=NULL,.sort=s,.id='SET',.keep=TRUE))
   cat(kind,role,s,'rows',vapply(r$data,function(z)paste(z$v,collapse=','),''),'\n')
 }
}
# Zero-width packed and array grouping dimensions retain source cardinality.
for(g in list(tibble::tibble(.rows=4L),matrix(numeric(),4L,0L),array(numeric(),c(4L,0L,2L)))){
  x<-tibble::tibble(g=g,payload=1:4)
  for(keep in c(FALSE,TRUE))for(v in list(nest_with_margins,nest_by_with_margins)){
    r<-v(x,.grouping=rollup(g),.margin_label=NULL,.keep=keep,.sort='last',.id='set')
    stopifnot(identical(vapply(r$data,nrow,integer(1)),c(4L,4L)))
    if(keep)for(cell in r$data)stopifnot(identical(dim(cell$g),dim(g)))
  }
}
cat('Zero-width structural grouping checks: 12 passed\n')
