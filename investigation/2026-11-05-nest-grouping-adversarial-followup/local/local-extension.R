args <- commandArgs(TRUE)
pkgload::load_all(if(length(args))args[[1]]else'.',quiet=TRUE)
out <- if(length(args)>1) args[[2]] else tempdir()
format.hunt_record <- function(x,...) paste0(vctrs::field(x,'a'),':',vctrs::field(x,'b'))
as.character.hunt_record <- function(x,...)format(x)
family <- list(
 labelled=haven::labelled(c(1,2,1,NA_real_),c(one=1,two=2)),
 labelled_spss=haven::labelled_spss(c(1,9,1,NA_real_),na_values=9),
 tagged_na=c(1,haven::tagged_na('a'),haven::tagged_na('b'),NA_real_),
 blob=blob::blob(as.raw(1:2),as.raw(1:2),as.raw(2:3),NULL),
 record=vctrs::new_rcrd(list(a=c(1L,1L,2L,NA_integer_),b=c('a','a','b',NA)),class='hunt_record'),
 list_of=vctrs::list_of(1:2,1:2,2:3,integer()),
 units=units::set_units(c(1,2,1,NA_real_),'m')
)
rows<-list()
for(fam in names(family))for(empty in c(FALSE,TRUE))for(role in c('dimension','fixed','payload'))for(keep in c(FALSE,TRUE))for(verb in c('nest_with_margins','nest_by_with_margins'))for(sort in c('none','last','first')){
 d<-tibble::tibble(g=c('x','y','x','y'),k=family[[fam]],rid=1:4)
 if(empty)d<-vctrs::vec_slice(d,integer())
 before<-serialize(d,NULL)
 spec<-if(role=='dimension')rollup(k)else rollup(g)
 args<-list(.data=d,.grouping=spec,.margin_label=NULL,.id='sid',.keep=keep,.sort=sort)
 if(role=='fixed')args$.by<-quote(k)
 z<-tryCatch(do.call(get(verb),args),error=identity)
 expected<-character()
 for(sid in 1:2){keys<-c(if(role=='fixed')'k',if(sid==1)if(role=='dimension')'k'else'g');groups<-if(length(keys))vctrs::vec_group_loc(d[keys])$loc else if(nrow(d))list(seq_len(nrow(d)))else list();for(idx in groups)expected<-c(expected,paste(sid,paste(idx,collapse=','),sep=':'))}
 actual<-if(inherits(z,'error'))character()else vapply(seq_len(nrow(z)),function(i)paste(z$sid[[i]],paste(z$data[[i]]$rid,collapse=','),sep=':'),character(1))
 status<-if(inherits(z,'error'))'error'else if(!identical(sort(expected),sort(actual)))'membership'else'match'
 # The source projections compare geometry-free values through vctrs equality,
 # so arbitrary attributes stay outside the ADR 0016 promise boundary.
 if(!inherits(z,'error'))for(i in seq_len(nrow(z)))for(nm in names(z$data[[i]])){
  source<-vctrs::vec_slice(d[[nm]],z$data[[i]]$rid)
  if(!all(vctrs::vec_equal(source,z$data[[i]][[nm]],na_equal=TRUE)))status<-'projection'
 }
 if(!identical(before,serialize(d,NULL)))status<-'mutation'
 rows[[length(rows)+1]]<-data.frame(family=fam,empty,role,keep,verb,sort,status,message=if(inherits(z,'error'))conditionMessage(z)else'')
}
r<-do.call(rbind,rows);write.csv(r,file.path(out,'extension-results.csv'),row.names=FALSE);print(with(r,table(family,status)));print(unique(r[r$status!='match',c('family','empty','role','status','message')]),row.names=FALSE)
stopifnot(all(r$status=='match'))
