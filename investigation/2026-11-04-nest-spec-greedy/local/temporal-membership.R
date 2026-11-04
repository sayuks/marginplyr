# Run from the investigated checkout root; no package implementation is changed.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all('.', quiet = TRUE)
base <- c(1,1,2,NA,2,1)
fixtures <- list(
 duration = lubridate::dseconds(base),
 period = lubridate::seconds(base),
 interval = lubridate::interval(as.Date('2020-01-01')+base, as.Date('2020-01-03')+base),
 yearmon = zoo::as.yearmon(2020+base/12),
 yearqtr = zoo::as.yearqtr(2020+base/4),
 distribution = distributional::dist_normal(base,1),
 distribution_sample = distributional::dist_sample(lapply(base,function(i) if(is.na(i)) numeric() else i+(1:3))),
 integer64 = bit64::as.integer64(c('9007199254740993','9007199254740993','9007199254740994',NA,'9007199254740994','9007199254740993')),
 hms = hms::hms(base),
 difftime = as.difftime(base,units='secs'),
 Date = as.Date('2020-01-01')+base,
 POSIXct = as.POSIXct('2020-01-01',tz='Pacific/Chatham')+base
)
plans <- list(
 rollup = list(spec=rollup(g,h),sets=list(c('g','h'),'g',character())),
 cube = list(spec=cube(g,h),sets=list(c('g','h'),'g','h',character())),
 irregular = list(spec=grouping_sets(grouping_set(h), grouping_set(g), grouping_set(), grouping_set(g,h)),sets=list('h','g',character(),c('g','h')))
)
rows <- list(); index <- 0L; projection_count <- 0L
signature <- function(groups) sort(vapply(groups,function(x)paste(x,collapse=':'),character(1)))
for(nm in names(fixtures)) for(empty in c(FALSE,TRUE)) for(role in c('dimension','fixed')) for(src in c('tibble','frame','data.table','grouped')) for(pn in names(plans)) for(labelled in c(FALSE,TRUE)) for(keep in c(FALSE,TRUE)) for(vn in c('nest','nest_by')) {
 index <- index+1L
 id <- paste(nm,empty,role,src,pn,labelled,keep,vn,sep='/')
 d <- tibble::tibble(g=fixtures[[nm]],h=c('x','x','x','y',NA,'y'),row=seq_along(base),payload=lapply(seq_along(base),function(i)list(id=i,tag=NA_character_)))
 if(empty)d<-d[0,]
 original<-d
 if(src=='frame')d<-as.data.frame(d)
 if(src=='data.table')d<-data.table::as.data.table(d)
 # A separate implicit fixed key exercises grouping metadata without changing g.
 if(src=='grouped')d<-dplyr::group_by(d,dplyr::across(tidyselect::all_of(if(role=='fixed')c('h','g') else 'h')))
 fixed<-if(role=='fixed')'g' else character()
 if(src=='grouped')fixed<-unique(c('h',fixed))
 plan <- if(role=='fixed')list(spec=rollup(h),sets=list('h',character())) else plans[[pn]]
 if(src=='grouped')plan<-list(spec=rollup(g),sets=list('g',character()))
 # Fixed columns cannot also be dimensions.
 if(role=='fixed' && src=='grouped')plan<-list(spec=grouping_set(),sets=list(character()))
 verb <- if(vn=='nest')nest_with_margins else nest_by_with_margins
 before<-serialize(d,NULL)
 call <- rlang::expr(verb(d,.grouping=plan$spec,.margin_label=if(labelled)'Total' else NULL,.id='set',.key='cell',.keep=keep,.sort=c('none','first','last')[(index%%3)+1L]))
 if(src!='grouped')call$.by<-rlang::expr(tidyselect::all_of(fixed))
 y<-tryCatch(eval(call),error=function(e)e)
 if(inherits(y,'error'))stop(id,': ',conditionMessage(y))
 stopifnot(identical(serialize(d,NULL),before))
 stopifnot(if(vn=='nest')!inherits(y,c('grouped_df','rowwise_df')) else inherits(y,'rowwise_df'))
 if(vn=='nest_by')stopifnot(identical(dplyr::group_vars(y),setdiff(names(y),'cell')))
 for(s in seq_along(plan$sets)) {
  keys<-unique(c(fixed,plan$sets[[s]]))
  expected<-if(!nrow(original))list() else if(length(keys))vctrs::vec_group_loc(original[keys])$loc else list(seq_len(nrow(original)))
  cells<-y$cell[y$set==s]
  observed<-lapply(cells,function(z)z$row)
  if(!identical(signature(expected),signature(observed)))stop(id,': membership')
  for(cell in cells) {
   expected_names<-if(keep)unique(c(fixed,unique(unlist(plan$sets)),names(original))) else setdiff(names(original),unique(c(fixed,unlist(plan$sets))))
   stopifnot(identical(names(cell),expected_names))
   for(col in names(cell)) {
    want<-vctrs::vec_slice(original[[col]],cell$row)
    same<-identical(cell[[col]],want)||isTRUE(all.equal(cell[[col]],want,check.attributes=FALSE))
    if(!same)stop(id,': projection ',col)
    projection_count<-projection_count+1L
   }
  }
 }
 rows[[index]]<-data.frame(case=id,outer_rows=nrow(y),status='matched')
}
result<-do.call(rbind,rows)
write.csv(result,Sys.getenv('HUNT_CSV',file.path(tempdir(),'temporal-membership.csv')),row.names=FALSE)
cat('Calls:',nrow(result),'\nCell column projections:',projection_count,'\n')
print(table(sub('/.*','',result$case)))
print(sessionInfo())
