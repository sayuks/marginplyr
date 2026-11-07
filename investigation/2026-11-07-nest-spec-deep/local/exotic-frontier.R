Sys.setenv(TESTTHAT_PARALLEL='false')
a <- commandArgs(TRUE)
display <- '--display' %in% a
a <- setdiff(a,'--display')
root <- normalizePath(if(length(a)) a[[1]] else '.')
out <- if(length(a)>1) a[[2]] else tempdir()
pkgload::load_all(root,quiet=TRUE)
i <- c(2,1,NA,1,2,3)
family <- list(
 hms=hms::as_hms(i), units=units::set_units(i,'m'), integer64=bit64::as.integer64(c('9007199254740992','9007199254740993',NA,'9007199254740993','9007199254740992','5')),
 labelled=haven::labelled(i,labels=c(one=1,two=2)), labelled_spss=haven::labelled_spss(i,na_values=3,labels=c(one=1,two=2)),
 tagged_na=c(haven::tagged_na('a'),haven::tagged_na('b'),NA,1,1,2),
 yearmon=zoo::as.yearmon(c('2020-02','2020-01',NA,'2020-01','2020-02','2020-03')),
 yearqtr=zoo::as.yearqtr(c('2020 Q2','2020 Q1',NA,'2020 Q1','2020 Q2','2020 Q3')),
 posixlt=I(as.POSIXlt(as.POSIXct(i,origin='1970-01-01',tz='Asia/Tokyo'))),
 blob=blob::blob(as.raw(2),as.raw(1),NULL,as.raw(1),as.raw(2),as.raw(3)),
 surv=survival::Surv(c(2,1,NA,1,2,3),c(0,1,1,1,0,1)),
 distribution=distributional::dist_normal(c(2,1,NA,1,2,3),rep(1,6)),
 sfc=sf::st_sfc(lapply(i,function(z)sf::st_point(c(z,z))),crs=4326),
 rvar=posterior::rvar(matrix(rep(i,4),nrow=4,byrow=TRUE))
)
results <- list()
sets <- list(c('g','h'),'g','h',character())
for(type in names(family)) for(empty in c(FALSE,TRUE)) {
 input <- tibble::tibble(f=c('y','x',NA,'x','y','x'),g=family[[type]],h=c('b','a','a',NA,'b','a'),rid=1:6,p=family[[type]])
 if(empty) input <- vctrs::vec_slice(input,integer())
 before <- serialize(input,NULL)
 for(verb in c('nest_with_margins','nest_by_with_margins')) for(keep in c(FALSE,TRUE)) for(srt in c('none','first','last')) {
  z <- tryCatch(get(verb)(input,.by=f,.grouping=cube(g,h),.margin_label=if(display)'Total'else NULL,.id='sid',.keep=keep,.sort=srt),error=identity)
  status <- 'match'; message <- ''
  if(inherits(z,'error')) {status <- 'error';message <- conditionMessage(z)} else {
   expect <- list()
   for(si in seq_along(sets)) {
    groups <- vctrs::vec_group_loc(input[c('f',sets[[si]])])$loc
    for(rows in groups) expect[[length(expect)+1]] <- paste(si,paste(input$rid[rows],collapse=','),sep=':')
   }
   actual <- vapply(seq_len(nrow(z)),function(k)paste(z$sid[k],paste(z$data[[k]]$rid,collapse=','),sep=':'),character(1))
   if(!identical(sort(actual),sort(as.character(unlist(expect))))) {status <- 'membership';message <- paste(actual,collapse=';')}
   if(status=='match') for(k in seq_len(nrow(z))) {
    rows <- z$data[[k]]$rid
    cols <- c(if(keep)c('f','g','h'),'rid','p')
    wanted <- vctrs::vec_slice(input[cols],rows)
    good <- tryCatch(identical(names(z$data[[k]]),cols) && all(vapply(cols,function(col)all(vctrs::vec_equal(z$data[[k]][[col]],wanted[[col]],na_equal=TRUE)),logical(1))),error=function(e)FALSE)
    if(!good){status <- 'payload';message <- paste('row',k);break}
   }
   if(status=='match' && srt!='none') {
    key <- tibble::as_tibble(dplyr::ungroup(z)[c('f','g','h','sid')])
    key$.mf <- vctrs::vec_detect_missing(key$f)
    key$.mg <- vctrs::vec_detect_missing(key$g)
    key$.mh <- vctrs::vec_detect_missing(key$h)
    key$.bg <- as.integer(!vapply(key$sid,function(si)'g'%in%sets[[si]],logical(1)))
    key$.bh <- as.integer(!vapply(key$sid,function(si)'h'%in%sets[[si]],logical(1)))
    if(srt=='first') {key$.bg <- -key$.bg;key$.bh <- -key$.bh}
    key$.row <- seq_len(nrow(key))
    wanted <- dplyr::arrange(key,.mf,f,.bg,.mg,g,.bh,.mh,h,sid)$.row
    if(!identical(wanted,seq_len(nrow(z)))) {status <- 'order';message <- paste(wanted,collapse=',')}
   }
  }
  serialized_unchanged <- identical(before,serialize(input,NULL))
  results[[length(results)+1]] <- data.frame(type,empty,verb,keep,sort=srt,status,message,serialized_unchanged)
 }
}
r <- do.call(rbind,results)
write.csv(r,file.path(out,if(display)'display-frontier.csv'else'exotic-frontier.csv'),row.names=FALSE)
print(table(r$type,r$status));print(unique(r[r$status!='match',c('type','empty','status','message')]),row.names=FALSE)
expected_refusal <- if(display)r$type%in%c('surv','posixlt')else r$type %in% c('surv','sfc','rvar') | (r$type=='posixlt' & !r$empty)
stopifnot(nrow(r)==336L,all(r$status==ifelse(expected_refusal,'error','match')),
          all(r$serialized_unchanged[r$type!='rvar']))
cat('All 336 calls matched their independently classified contract or native boundary.\n')
print(sessionInfo())
