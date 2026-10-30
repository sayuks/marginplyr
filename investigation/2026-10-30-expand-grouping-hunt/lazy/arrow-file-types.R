pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
clock <- structure(c(2,1,NA),class=c('hms','difftime'),units='secs')
cases <- list(
 null=list(arrow::null(),c(NA,NA,NA)), bool=list(arrow::boolean(),c(TRUE,FALSE,NA)),
 int8=list(arrow::int8(),c(2L,1L,NA)), int16=list(arrow::int16(),c(2L,1L,NA)), int32=list(arrow::int32(),c(2L,1L,NA)), int64=list(arrow::int64(),c(2L,1L,NA)),
 uint8=list(arrow::uint8(),c(2L,1L,NA)), uint16=list(arrow::uint16(),c(2L,1L,NA)), uint32=list(arrow::uint32(),c(2L,1L,NA)), uint64=list(arrow::uint64(),c(2L,1L,NA)),
 float16=list(arrow::float16(),c(2,1,NA)), float32=list(arrow::float32(),c(2,1,NA)), float64=list(arrow::float64(),c(2,1,NA)), nan=list(arrow::float64(),c(NaN,Inf,NA)),
 decimal128=list(arrow::decimal128(18,4),c(2.25,1.125,NA)), decimal256=list(arrow::decimal256(50,4),c(2.25,1.125,NA)),
 utf8=list(arrow::utf8(),c('b','a',NA)),large_utf8=list(arrow::large_utf8(),c('b','a',NA)),
 date32=list(arrow::date32(),as.Date(c('2026-01-02','2026-01-01',NA))), date64=list(arrow::date64(),as.Date(c('2026-01-02','2026-01-01',NA))),
 timestamp_s=list(arrow::timestamp('s','UTC'),as.POSIXct(c('2026-01-02','2026-01-01',NA),tz='UTC')),
 timestamp_ms=list(arrow::timestamp('ms','Asia/Tokyo'),as.POSIXct(c('2026-01-02','2026-01-01',NA),tz='Asia/Tokyo')),
 timestamp_us=list(arrow::timestamp('us','America/New_York'),as.POSIXct(c('2026-01-02','2026-01-01',NA),tz='America/New_York')),
 timestamp_ns=list(arrow::timestamp('ns'),as.POSIXct(c('2026-01-02','2026-01-01',NA),tz='UTC')),
 time32_s=list(arrow::time32('s'),clock),time32_ms=list(arrow::time32('ms'),clock),time64_us=list(arrow::time64('us'),clock),time64_ns=list(arrow::time64('ns'),clock),
 duration_s=list(arrow::duration('s'),as.difftime(c(2,1,NA),units='secs')), duration_ns=list(arrow::duration('ns'),as.difftime(c(2,1,NA),units='secs')),
 binary=list(arrow::binary(),list(as.raw(2),as.raw(1),NULL)),large_binary=list(arrow::large_binary(),list(as.raw(2),as.raw(1),NULL)),fixed_binary=list(arrow::fixed_size_binary(2),list(as.raw(c(2,3)),as.raw(c(1,2)),NULL)),
 list=list(arrow::list_of(arrow::int16()),list(c(2L,3L),1L,NULL)),large_list=list(arrow::large_list_of(arrow::int16()),list(c(2L,3L),1L,NULL)),fixed_list=list(arrow::fixed_size_list_of(arrow::int16(),2),list(c(2L,3L),c(1L,2L),NULL)),
 struct=list(arrow::struct(x=arrow::int16(),y=arrow::utf8()),data.frame(x=c(2L,1L,NA),y=c('b','a',NA))),
 nested_struct=list(arrow::struct(x=arrow::list_of(arrow::int32())),data.frame(x=I(list(2:3,1L,NULL))))
)
for (vt in c('int32','float64','utf8')) {
 vals <- switch(vt,int32=c(2L,1L,NA),float64=c(2.5,1.25,NA),utf8=c('b','a',NA))
 for (ord in c(FALSE,TRUE)) cases[[paste('dict',vt,ord,sep='_')]] <- list(arrow::dictionary(arrow::int8(),do.call(getExportedValue('arrow',vt),list()),ordered=ord),vals)
}
cases$extension <- list(NULL,vctrs::new_vctr(c(2L,1L,NA),class='hunt_vctr'))
cases$map <- list(arrow::map_of(arrow::utf8(),arrow::int32()),list(data.frame(key=c('b','a'),value=1:2),data.frame(key='x',value=3L),NULL))
results <- list(); ix <- 0L
canon <- function(x) { x <- as.data.frame(x); x <- x[order(x$sid,x$row_id),,drop=FALSE]; rownames(x)<-NULL; x }
for (name in names(cases)) {
 cs <- cases[[name]]
 fixture <- tryCatch({
  if(is.null(cs[[1L]])) arrow::vctrs_extension_array(cs[[2L]]) else if(inherits(cs[[1L]],'DictionaryType')) {
   arrow::call_function('dictionary_encode',arrow::Array$create(cs[[2L]]))$cast(cs[[1L]])
  } else if(identical(name,'float16')) {
   arrow::Array$create(cs[[2L]])$cast(cs[[1L]])
  } else arrow::Array$create(cs[[2L]],type=cs[[1L]])
 },error=identity)
 if(inherits(fixture,'error')) { cat('FIXTURE',name,conditionMessage(fixture),'\n'); next }
 for (empty in c(FALSE,TRUE)) {
  g <- if(empty)fixture$Slice(0L,0L)else fixture
  tab <- arrow::Table$create(g=g,row_id=if(empty)integer()else 1:3)
  scratch <- tempfile('arrow-files-'); dir.create(scratch)
  shapes <- list()
  for (format in c('parquet','ipc')) {
   path <- file.path(scratch, paste0('facts.',format))
   fs <- tryCatch({
    if(format=='parquet') arrow::write_parquet(tab,path) else arrow::write_feather(tab,path)
    arrow::open_dataset(path,format=format)
   },error=identity)
   if(inherits(fs,'error')) cat('FILE_FIXTURE',name,empty,format,conditionMessage(fs),'\n') else shapes[[format]] <- fs
  }
  for(shape in names(shapes)) for(lab in c('null','na','text')) for(sort in c('none','first','last')) {
   src <- shapes[[shape]]; source_type <- arrow::schema(src)$GetFieldByName('g')$type; label <- switch(lab,null=NULL,na=NA_character_,text='Total')
   # Direct Arrow-native null conversion is independent of package R proxies.
   val <- tryCatch(arrow::Scalar$create(NA,type=arrow::null())$cast(source_type),error=identity)
   control <- tryCatch({
    inc <- if(lab=='text')mutate(src,g=as.character(g),sid=1L)else mutate(src,sid=1L)
    om <- if(lab=='text')mutate(src,g='Total',sid=2L)else if(inherits(val,'error'))stop(val)else mutate(src,g=val,sid=2L)
    ans <- union_all(inc,om)
    if(sort!='none') {
     if(inherits(source_type,'DictionaryType')) {
      # Arrow sorts dictionaries only after decoding the values.
      ty <- source_type$value_type
      ans <- mutate(ans,private=arrow::cast(g,ty))
      ans <- if(sort=='last')arrange(ans,sid,is.na(private),private)else arrange(ans,desc(sid),is.na(private),private)
      ans <- select(ans,-private)
     } else ans <- if(sort=='last')arrange(ans,sid,is.na(g),g)else arrange(ans,desc(sid),is.na(g),g)
    }
    list(data=collect(ans),schema=arrow::schema(ans))
   },error=identity)
   actual <- tryCatch({ q<-expand_with_margins(src,.grouping=rollup(g),.margin_label=label,.id='sid',.sort=sort); list(data=collect(q),schema=arrow::schema(q))},error=identity)
   status <- if(inherits(control,'error')) { if(inherits(actual,'error'))'both_error'else 'control_error_only' } else if(inherits(actual,'error'))'package_error' else {
    a<-canon(actual$data); b<-canon(control$data); a<-a[names(b)]
    if(!isTRUE(all.equal(a,b,check.attributes=TRUE)))'value_diff'else if(!actual$schema$GetFieldByName('g')$type$Equals(control$schema$GetFieldByName('g')$type))'type_diff'else 'pass'
   }
   ix<-ix+1L
   results[[ix]]<-data.frame(name=name,empty=empty,shape=shape,label=lab,sort=sort,status=status,
    actual_error=if(inherits(actual,'error'))conditionMessage(actual)else'',control_error=if(inherits(control,'error'))conditionMessage(control)else'')
   if(status %in% c('package_error','value_diff','type_diff')) cat('LEAD',name,empty,shape,lab,sort,status,if(inherits(actual,'error'))conditionMessage(actual)else'','\n')
  }
 }
}
unlink(scratch,recursive=TRUE)
res<-do.call(rbind,results);write.csv(res,file.path(Sys.getenv('MARGINPLYR_HUNT_OUTPUT',tempdir()),'arrow-file-types.csv'),row.names=FALSE);print(table(res$status));print(table(res$name,res$status))
