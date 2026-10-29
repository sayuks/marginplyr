pkgload::load_all(quiet=TRUE)
run <- function(qualified) {
  seen <- 0L
  data <- data.frame(region=c('a','b'), amount=1:2)
  choose <- function(...) {seen <<- seen+1L;if(seen==1L)'region' else 'amount'}
  if(qualified) {
    `::` <- function(package, name) choose
    spec <- grouping_sets(marginplyr::rollup())
  } else {
    rollup <- choose
    spec <- grouping_sets(rollup())
  }
  result <- inspect_grouping(data,.grouping=spec,.format='list')
  list(included=result$included, reads=seen)
}
actual<-run(TRUE); control<-run(FALSE)
print(actual);print(control)
stopifnot(identical(control$included,list('region')),identical(control$reads,1L))
cat('independent_capture_match:',identical(actual,control),'\n')
if('--assert' %in% commandArgs(TRUE)) stopifnot(identical(actual,control))
