pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
base_data <- data.frame(g=c('a','a','b'),x=1:3,y=c(5,6,7))
forms <- c(
 across='(function(cols) dplyr::across(dplyr::all_of(cols), sum))("x")',
 pick='(function(cols) sum(unlist(dplyr::pick(dplyr::all_of(cols)))))("x")',
 if_any='(function(cols) sum(dplyr::if_any(dplyr::all_of(cols), ~.x>4)))("x")',
 if_all='(function(cols) sum(dplyr::if_all(dplyr::all_of(cols), ~.x>4)))("x")')
capture <- function(call) tryCatch(suppressWarnings(eval(call)),error=function(e)e)
checks <- violations <- control_pass <- 0L
for (kind in c('data.frame','tibble','data.table')) {
 data <- switch(kind, data.frame=base_data,tibble=tibble::as_tibble(base_data),data.table=data.table::as.data.table(base_data))
 for (group in c('fixed','rollup')) for (share in if (group == 'fixed') c('none','total') else c('none','total','parent')) for (outer in c('absent','x','y')) for (helper in names(forms)) {
  if (exists('cols',inherits=FALSE)) rm(cols)
  if (outer!='absent') cols <- outer
  form <- forms[[helper]]
  extra <- if(share=='none') '' else paste0(', total=sum(x), fraction=share_of_',share,'(total)')
  arguments <- if(group=='fixed') ', .by=g' else ', .grouping=rollup(g), .margin_label=NULL'
  actual <- capture(parse(text=paste0('summarize_with_margins(data,out=',form,extra,arguments,')'))[[1]])
  detail <- capture(parse(text=paste0('dplyr::summarise(data,out=',form,', .by=g)'))[[1]])
  stopifnot(!inherits(detail,'error'))
  expected <- tibble::as_tibble(detail)
  if(group=='rollup') {
   grand <- capture(parse(text=paste0('dplyr::summarise(data,out=',form,')'))[[1]])
   stopifnot(!inherits(grand,'error'))
   grand$g <- NA_character_; grand <- tibble::as_tibble(grand)[c('g','out')]
   expected <- dplyr::bind_rows(expected, grand)
  }
  if(inherits(actual,'error')) {
   equal <- FALSE
   observed <- conditionMessage(actual)
  } else {
   actual <- tibble::as_tibble(actual)[c('g','out')]
   equal <- isTRUE(all.equal(expected,actual,check.attributes=TRUE))
   observed <- paste('nrow',nrow(actual),'out_names',paste(names(actual$out),collapse=','))
  }
  checks <- checks+1L
  if(outer=='x') {
   stopifnot(equal)
   control_pass <- control_pass+1L
  } else {
   stopifnot(!equal)
   violations <- violations+1L
  }
  cat(kind,group,share,outer,helper,'MATCH',equal,observed,'\n')
 }
}
cat('CASES',checks,'CONFIRMED_DIVERGENCES',violations,'MATCHING_BINDING_CONTROLS',control_pass,'\n')
