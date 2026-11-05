pkgload::load_all(quiet=TRUE)
keys <- list(packed0=tibble::tibble(.rows=3), array0=array(integer(),c(3,0,1)),matrix0=matrix(integer(),3,0),
   asis=I(c(1,1,2)), names=c(a=1,b=2,c=3), nested_empty=tibble::tibble(a=tibble::tibble(.rows=3)),
   nested_factor=tibble::tibble(a=structure(c(1L,2L,NA_integer_),levels=c('a',NA_character_),class='factor')),
   POSIXlt=as.POSIXlt(as.Date('2020-01-01')+0:2),
   expression=expression(x,x,y), nested=list(data.frame(x=1),data.frame(x=1),data.frame(x=2)))
count <- 0L
for (g in keys) for (verb in list(nest_with_margins,nest_by_with_margins)) {
  x <- tibble::tibble(g=g,row_id=1:3)
  before <- serialize(x,NULL)
  reference <- dplyr::nest_by(x,g,.keep=TRUE)
  groups <- lapply(reference$data, `[[`, 'row_id')
  for (keep in c(FALSE,TRUE)) for (sort in c('none','first','last')) {
    z <- verb(x,.grouping=rollup(g),.margin_label=NULL,.keep=keep,.id='set',.sort=sort)
    details <- lapply(z$data[z$set==1L], `[[`, 'row_id')
    stopifnot(setequal(details,groups),identical(z$data[[which(z$set==2L)]]$row_id,1:3),
              all(vapply(z$data,function(cell) identical(names(cell),c('g'[keep],'row_id')),logical(1))),
              identical(serialize(x,NULL),before))
    count <- count+1L
  }
}
cat(count,'native source-membership comparisons matched.\n')
