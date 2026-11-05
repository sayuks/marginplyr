args <- commandArgs(TRUE)
pkgload::load_all(if(length(args))args[[1]]else'.',quiet=TRUE)
out <- if(length(args)>1)args[[2]]else tempdir()
p <- sf::st_sfc(sf::st_point(c(0,0)),sf::st_point(c(0,0)),sf::st_point(c(1,1)),sf::st_point(c(NA_real_,NA_real_)),crs=4326)
d <- sf::st_sf(g=c('a','b','a','b'),rid=1:4,geo=p)
before<-serialize(d,NULL)
rows<-list()
for(empty in c(FALSE,TRUE))for(verb in c('nest_with_margins','nest_by_with_margins'))for(keep in c(FALSE,TRUE))for(role in c('dimension','fixed','payload')) {
 input<-if(empty)d[0,]else d
 params<-list(.data=input,.keep=keep)
 if(role=='dimension')params$.grouping<-grouping_set(geo)
 if(role=='fixed')params$.by<-quote(geo)
 if(role=='payload')params$.grouping<-rollup(g)
 z<-tryCatch(do.call(get(verb),params),error=identity)
 rows[[length(rows)+1]]<-data.frame(empty,verb,keep,role,status=if(inherits(z,'error'))'error'else'match',message=if(inherits(z,'error'))conditionMessage(z)else'')
}
# The corresponding native mutation also leaves an invalid sf object.
native<-dplyr::mutate(d,geo=as.character(geo))
native_geometry<-tryCatch(sf::st_geometry(native),error=identity)
print(native_geometry)
stopifnot(inherits(native_geometry,'error'))
# Geometry payload is representable without the sf container's invariant.
plain<-nest_with_margins(tibble::as_tibble(d),.grouping=rollup(geo),.keep=TRUE)
stopifnot(nrow(plain)==4L)
stopifnot(identical(before,serialize(d,NULL)))
r<-do.call(rbind,rows);write.csv(r,file.path(out,'sf-disposition.csv'),row.names=FALSE);print(with(r,table(role,status)))
cat('sf',as.character(packageVersion('sf')),'; source unchanged\n')
