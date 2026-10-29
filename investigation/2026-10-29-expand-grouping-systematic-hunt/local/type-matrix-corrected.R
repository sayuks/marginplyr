pkgload::load_all(quiet = TRUE)
types <- list(chr=c('b',NA,'a'),int=c(2L,NA_integer_,1L),dbl=c(2,NaN,NA_real_),log=c(TRUE,NA,FALSE),date=as.Date(c('2026-01-01',NA,'2025-01-01')),posixct=as.POSIXct(c('2026-01-01',NA,'2025-01-01'),tz='Asia/Tokyo'),difftime=as.difftime(c(2,NA,1),units='hours'),list=list(2L,NULL,1L),list_of=vctrs::list_of(2L,NULL,1L),packed=tibble::tibble(a=c(2L,NA,1L),b=c('x',NA,'y')),matrix=matrix(c(2L,NA,1L,4L,NA,3L),ncol=2),array=array(c(2L,NA,1L,4L,NA,3L),dim=c(3L,1L,2L)),nested=tibble::tibble(a=tibble::tibble(b=c(2L,NA,1L)),z=c(3,NA,1)))
types$factor <- factor(c('b',NA,'a'),levels=c('unused','b','a'))
types$factor_na <- factor(c('b',NA,'a'),levels=c('unused','b',NA,'a'),exclude=NULL)
types$ordered <- ordered(c('b',NA,'a'),levels=c('unused','b','a'))
types$hms <- hms::hms(c(2,NA,1))
types$int64 <- bit64::as.integer64(c('9223372036854775807',NA,'9007199254740993'))
types$units <- units::set_units(c(2,NA,1),'m')
types$blob <- blob::blob(as.raw(2),NULL,as.raw(1))
{types$duration <- lubridate::as.duration(c(2,NA,1)); types$period <- lubridate::seconds(c(2,NA,1))}
failures<-list(); runs<-0L
for(type in names(types)) for(label in c('NULL','NA','string')) for(n in c(0L,1L,3L)) for(role in c('dimension','fixed','payload')) for(sort in c('none','first','last')) {
 vals <- vctrs::vec_slice(types[[type]],seq_len(n))
 dat <- tibble::tibble(g=c('b',NA,'a')[seq_len(n)],rowid=seq_len(n)); dat[[switch(role,dimension='g',fixed='f',payload='v')]] <- vals
 lab <- switch(label,'NULL'=NULL,'NA'=NA_character_,string='All')
 expected_reject <- role=='dimension' && ((label=='string' && (is.data.frame(vals)||length(dim(vals))>1L)) || (label=='NA' && type=='factor_na'))
 by<-if(role=='fixed') 'f' else character()
 actual <- tryCatch(expand_with_margins(dat,.by=dplyr::all_of(by),.grouping=rollup(g),.margin_label=lab,.id='sid',.sort=sort),error=identity)
 runs<-runs+1L; key<-paste(type,label,n,role,sort,sep='/')
 if(inherits(actual,'error')) {if(!expected_reject) failures[[key]]<-paste(class(actual)[1],conditionMessage(actual));next}
 if(expected_reject) {failures[[key]]<-'expected refusal';next}
 stopifnot(nrow(actual)==n*2L)
 for(sid in 1:2) {
  got<-actual[actual$sid==sid,,drop=FALSE]; got<-got[order(got$rowid),,drop=FALSE]
  want<-dat
  if(!is.null(lab)&&!is.na(lab)) want$g <- if(sid==2L)rep(lab,n) else as.character(dat$g) else if(sid==2L) want$g<-vctrs::vec_init(dat$g,n)
  if(is.factor(dat$g) && !is.null(lab) && !is.na(lab)) {
   lev<-levels(dat$g); if(!is.null(lab)&&!is.na(lab))lev<-c(lev,lab)
   want$g<-factor(want$g,levels=lev,ordered=is.ordered(dat$g),exclude=NULL)
  }
  for(col in names(want)) {
   if(!identical(got[[col]],want[[col]])) {
    failures[[paste0(key,'/sid',sid,'/',col)]]<-all.equal(got[[col]],want[[col]])
   }
  }
 }
}
cat('type cells',runs,'signals',length(failures),'\n'); print(failures)
stopifnot(length(failures) == 3L, all(grepl('^hms/string/0/dimension/', names(failures))))
