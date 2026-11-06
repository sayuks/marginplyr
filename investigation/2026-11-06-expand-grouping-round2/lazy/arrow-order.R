Sys.setenv(TESTTHAT_PARALLEL='false')
output <- dirname(normalizePath(sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])))
pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
library(arrow,warn.conflicts=FALSE)
cap<-function(x)tryCatch(force(x),error=function(e)e)
fixtures<-list(text=Array$create(c('z','a',NA,'b')),integer=Array$create(c(9L,-2L,NA,3L)),double=Array$create(c(9.5,-2.5,NA,3.5)),date=Array$create(as.Date(c('2026-01-09','2026-01-01',NA,'2026-01-03'))),timestamp=Array$create(as.POSIXct(c('2026-01-09','2026-01-01',NA,'2026-01-03'),tz='UTC')),int64=Array$create(c('9007199254740995','-9007199254740995',NA,'9007199254740994'))$cast(int64()),decimal=Array$create(c('99999999999999.12','-99999999999999.12',NA,'99999999999999.11'))$cast(decimal128(22,2)),dictionary=DictionaryArray$create(Array$create(c(0L,1L,NA,2L)),Array$create(c('z','a','b','unused'))))
families<-list(rollup=list(spec=rollup(g,h),sets=list(c('g','h'),'g',character())),composite=list(spec=rollup(grouping_set(g,h)),sets=list(c('g','h'),character())),cube=list(spec=cube(g,h),sets=list(c('g','h'),'g','h',character())),repeated=list(spec=grouping_sets(grouping_set(),grouping_set(g),grouping_set(),grouping_set(g,h)),sets=list(character(),'g',character(),c('g','h'))))
res<-list();ii<-0L;tmp<-tempfile('arrow-order');dir.create(tmp)
for(type in names(fixtures)){
 t<-Table$create(g=fixtures[[type]],h=c('b','a',NA,'c'),f=c(NA,'x','x','a'),rid=1:4)
 path<-file.path(tmp,paste0(type,'.arrow'));write_ipc_file(t,path);ds<-open_dataset(path,format='ipc')
 for(shape in c('table','batch','dataset','uniondataset')) {
  src<-switch(shape,table=t,batch=as_record_batch(t),dataset=ds,uniondataset=open_dataset(list(ds,ds)))
  for(fam in names(families))for(sort in c('first','last')) {
   sets<-families[[fam]]$sets
   expected<-bind_rows(lapply(seq_along(sets),function(i){data.frame(f=c(NA,'x','x','a'),g_rank=if('g'%in%sets[[i]])c(3L,1L,NA,2L)else NA_integer_,h_rank=if('h'%in%sets[[i]])c(2L,1L,NA,3L)else NA_integer_,g_bit=as.integer(!'g'%in%sets[[i]]),h_bit=as.integer(!'h'%in%sets[[i]]),sid=i,rid=1:4)}))
   if(shape=='uniondataset')expected<-bind_rows(expected,expected)
   bit<-if(sort=='last')1L else -1L
   o<-order(is.na(expected$f),expected$f,bit*expected$g_bit,is.na(expected$g_rank),expected$g_rank,bit*expected$h_bit,is.na(expected$h_rank),expected$h_rank,expected$sid,na.last=TRUE)
   # A total occurrence can have ties, so compare the declared key sequence.
   key<-function(df)paste(df$f,df$g_bit,df$g_rank,df$h_bit,df$h_rank,df$sid,sep='|')
   expected_keys<-key(expected[o,])
   q<-cap(expand_with_margins(src,.by=f,.grouping=families[[fam]]$spec,.duplicates='keep',.id='sid',.margin_label=NULL,.sort=sort))
   for(route in c('collect','compute','reader')) {
    a<-cap(if(inherits(q,'error'))q else switch(route,collect=collect(q),compute=collect(compute(q)),reader=collect(as_arrow_table(as_record_batch_reader(q)))))
    if(!inherits(a,'error')){
      gbit<-vapply(a$sid,function(i)as.integer(!'g'%in%sets[[i]]),integer(1));hbit<-vapply(a$sid,function(i)as.integer(!'h'%in%sets[[i]]),integer(1))
      observed<-data.frame(f=a$f,g_bit=gbit,h_bit=hbit,g_rank=ifelse(gbit==1L,NA_integer_,c(3L,1L,NA,2L)[a$rid]),h_rank=ifelse(hbit==1L,NA_integer_,c(2L,1L,NA,3L)[a$rid]),sid=a$sid)
      same<-identical(key(observed),expected_keys)
    }else same<-FALSE
    ii<-ii+1L;res[[ii]]<-data.frame(type,shape,fam,sort,route,status=if(inherits(a,'error'))'error' else if(same)'match' else 'mismatch',error=if(inherits(a,'error'))conditionMessage(a)else'')
   }
  }
 }
}
r<-bind_rows(res);write.csv(r,file.path(output,'arrow-order.csv'),row.names=FALSE);print(count(r,status));print(filter(r,status!='match'),row.names=FALSE);unlink(tmp,recursive=TRUE);stopifnot(all(r$status=='match'))
