pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(arrow))
options(warn=1)
chk <- function(query, local, dimension, fixed, sets, label, sort) {
  spec <- grouping_sets(!!!lapply(sets, function(x) grouping_set(all_of(x))))
  query <- expand_with_margins(query, .grouping=spec, .by=all_of(fixed),
                             .margin_label=label, .sort=sort, .duplicates='keep', .id='set_id')
  stopifnot(inherits(query,'arrow_dplyr_query'))
  actual <- as.data.frame(collect(query))
  expected <- lapply(seq_along(sets), function(i) {
    arm <- local
    if (!(dimension %in% sets[[i]])) arm[[dimension]] <- vctrs::vec_init(arm[[dimension]], nrow(arm))
    arm[['set_id']] <- rep.int(i,nrow(arm))
    arm
  })
  expected <- as.data.frame(dplyr::bind_rows(expected))
  stopifnot(nrow(actual)==nrow(expected), setequal(names(actual),names(expected)))
  rownames(actual) <- rownames(expected) <- NULL
  sort_keys <- c('set_id','source_id')
  actual_rows <- order(actual$set_id,actual$source_id)
  expected_rows <- order(expected$set_id,expected$source_id)
  for (column in names(expected)) {
    if (!identical(actual[[column]][actual_rows],expected[[column]][expected_rows])) {
      return(paste0('mismatch ',column,' classes ',
                    paste(class(actual[[column]]),collapse='/'),' != ',paste(class(expected[[column]]),collapse='/'),
                    ' / ', paste(capture.output(str(actual[[column]])),collapse=' '),
                    ' vs ',paste(capture.output(str(expected[[column]])),collapse=' ')))
    }
  }
  'ok'
}
output <- list()
base_values <- list(
  chr=c('z','a',NA,'a'), int=c(2L,1L,NA,1L), bool=c(TRUE,FALSE,NA,FALSE),
  dbl=c(2.3, 1.1,NA,1.1), nan=c(NaN,2.4,NA,1.3),
  date=as.Date(c('2020-01-05','2020-01-02',NA,'2020-01-02')),
  ts=as.POSIXct(c('2020-01-05','2020-01-02',NA,'2020-01-02'),tz='Asia/Tokyo'),
  time=hms::hms(seconds=c(22,13,NA,13)),
  int64=bit64::as.integer64(c('9007199254740995','9007199254740993',NA,'9007199254740993')),
  fac=factor(c('z','a',NA,'a'),levels=c('z','x','a')),
  ord=ordered(c('z','a',NA,'a'),levels=c('z','x','a'))
)
for (type in names(base_values)) {
  df <- tibble(value=base_values[[type]], fixed=c('y', 'x', NA, 'x'), source_id=1:4)
  tbl <- Table$create(df)
  bch <- RecordBatch$create(df)
  remote_forms <- list(Table=tbl, RecordBatch=bch,
                arrange=arrange(tbl, desc(source_id + 3L)),
                limit=head(arrange(tbl, desc(source_id)),2L),
                filter=filter(tbl, source_id%%2L==0L),
                grouped=group_by(tbl, fixed),
                select=select(tbl, value, fixed, source_id),
                join=left_join(tbl,select(Table$create(df),source_id,xtra=fixed),by='source_id'),
                union=union_all(filter(tbl,source_id%%2L==0L),filter(tbl,source_id%%2L==1L)),
                aggregate=summarise(group_by(tbl, value,fixed,source_id),count=n(),.groups='drop'))
  for (shape in names(remote_forms)) {
    remote <- remote_forms[[shape]]
    local <- tryCatch(ungroup(collect(remote)),error=function(e) NULL)
    if (is.null(local)) next
    for (empty in c(FALSE,TRUE)) {
      src <- if (empty) filter(remote, source_id < 0L) else remote
      src_local <- if (empty) local[0L,] else local
      for (settype in c('detail','empty_first','repeat')) {
        sets <- switch(settype,detail=list('value'), empty_first=list(character(),'value'),
                       "repeat"=list('value',character(),'value'))
        for (sort in c('none','last','first')) {
          ans <- tryCatch(chk(src,src_local,'value','fixed',sets,NULL,sort),error=function(e) conditionMessage(e))
          output[[length(output)+1L]] <- data.frame(type,shape,empty,settype,sort,ans)
        }
      }
    }
  }
}
ans <- dplyr::bind_rows(output)
print(table(ans$ans),max=20)
