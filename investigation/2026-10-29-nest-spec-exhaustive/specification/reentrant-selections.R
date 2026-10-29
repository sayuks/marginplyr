pkgload::load_all('.', quiet = TRUE)
outer_data <- tibble::tibble(region=c('east','west'), amount=c(1L,1L), row=1:2)
inner_data <- tibble::tibble(inner=1:2, text=c('a','b'))
backends <- list(local=identity, dtplyr=dtplyr::lazy_dt)
consumers <- c('fixed','grouping_set','grouping_sets','rollup','cube','grouping_spec','nest_with_margins','nest_by_with_margins')
inner_actions <- c('inspect','nest','rowwise')
entry_modes <- c('helper','predicate')
fail_modes <- c(FALSE,TRUE)
checks <- calls <- nested_calls <- 0L
for(backend in names(backends)) for(inner_action in inner_actions) for(fail_inner in fail_modes) for(entry in entry_modes) for(consumer in consumers) {
 source <- backends[[backend]](outer_data)
 before_options <- options('lifecycle_verbosity','warn','rlang_backtrace_on_error')
 callback_calls <- 0L
 inner <- function() {
   nested_calls <<- nested_calls + 1L
   if(fail_inner) {
     got <- tryCatch(switch(inner_action,
       inspect=inspect_grouping(backends[[backend]](inner_data),.grouping=rollup(absent)),
       nest=nest_with_margins(backends[[backend]](inner_data),.by=absent),
       rowwise=nest_by_with_margins(backends[[backend]](inner_data),.by=absent)
     ),error=identity)
     stopifnot(inherits(got,'error'))
   } else if(inner_action=='inspect') {
     result <- inspect_grouping(backends[[backend]](inner_data),.by=tidyselect::where(is.character),.grouping=rollup(inner))
     stopifnot(identical(result$fixed,c('(text)','(text)')))
   } else if(inner_action=='nest') {
     result <- dplyr::collect(nest_with_margins(backends[[backend]](inner_data),.grouping=rollup(inner)))
     stopifnot(sum(lengths(lapply(result$data,function(x)x$text)))==4L)
   } else {
     result <- nest_by_with_margins(backends[[backend]](inner_data),.by=text)
     stopifnot(inherits(result,'rowwise_df'),nrow(result)==2L)
   }
 }
 select_region <- function() {
   callback_calls <<- callback_calls + 1L
   before_data <- tidyselect::peek_data(); before_vars <- tidyselect::peek_vars()
   inner()
   stopifnot(identical(tidyselect::peek_data(),before_data), identical(tidyselect::peek_vars(),before_vars))
   checks <<- checks+2L
   names(before_data)[vapply(before_data,is.character,logical(1))]
 }
 predicate <- function(x) {
   callback_calls <<- callback_calls + 1L
   before_data <- tidyselect::peek_data(); before_vars <- tidyselect::peek_vars()
   inner()
   stopifnot(identical(tidyselect::peek_data(),before_data), identical(tidyselect::peek_vars(),before_vars))
   checks <<- checks+2L
   is.character(x)
 }
 selection <- if(entry=='helper') quote(tidyselect::all_of(select_region())) else quote(tidyselect::where(predicate))
 apply_selection <- function(selection) {
   if(consumer=='fixed') return(inspect_grouping(source,.by=!!selection))
   if(consumer %in% c('nest_with_margins','nest_by_with_margins')) {
     return(dplyr::collect(rlang::eval_tidy(rlang::call2(consumer,source,.by=selection))))
   }
   spec<-rlang::eval_tidy(rlang::call2(consumer,selection))
   inspect_grouping(source,.grouping=spec)
 }
 actual<-apply_selection(selection)
 expected<-apply_selection(quote(tidyselect::all_of('region')))
 stopifnot(identical(actual,expected), identical(do.call(options, as.list(names(before_options))),before_options))
 expected_calls<-if(entry=='helper') 1L else ncol(outer_data)
 stopifnot(callback_calls==expected_calls)
 calls<-calls+1L
}
cat('public comparisons=',calls,' callback calls=',nested_calls,' context restoration assertions=',checks,'; every plan/cell matched all_of(region), callback budgets and options restored\n',sep='')
