args <- commandArgs(TRUE)
assert <- '--assert' %in% args
args <- setdiff(args,'--assert')
pkgload::load_all(if (length(args)) args[[1]] else '.',quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL='false')
# The method only changes logical negation. The stored scalar remains TRUE,
# isTRUE() accepts it, and direct if() reads it as true.
`!.label_check_flag` <- function(x) TRUE
flag <- structure(TRUE,class='label_check_flag')
input <- tibble::tibble(g='Total',v=1L)
stopifnot(isTRUE(flag), if(flag) TRUE else FALSE)
verbs <- list(
  nest_with_margins=function(flag) nest_with_margins(input,.grouping=rollup(g),.check_margin_label=flag),
  nest_by_with_margins=function(flag) nest_by_with_margins(input,.grouping=rollup(g),.check_margin_label=flag),
  expand_with_margins=function(flag) expand_with_margins(input,.grouping=rollup(g),.check_margin_label=flag),
  summarize_with_margins=function(flag) summarize_with_margins(input,n=dplyr::n(),.grouping=rollup(g),.check_margin_label=flag)
)
rows <- list()
for (verb in names(verbs)) {
  plain <- tryCatch(verbs[[verb]](TRUE),error=identity)
  stored <- tryCatch(verbs[[verb]](flag),error=identity)
  unchecked <- verbs[[verb]](FALSE)
  stopifnot(inherits(plain,'marginplyr_error'))
  if(!assert) stopifnot(!inherits(stored,'error'),nrow(stored)==2L,identical(stored,unchecked))
  rows[[verb]]<-data.frame(verb,plain_error=inherits(plain,'marginplyr_error'),classed_error=inherits(stored,'error'),classed_rows=if(inherits(stored,'error'))NA_integer_ else nrow(stored))
  if(assert) stopifnot(inherits(stored,'marginplyr_error'))
}
print(do.call(rbind,rows),row.names=FALSE)
if(assert) quit(save="no",status=0L)
# A FALSE value with the opposite negation is rejected during isFALSE(), so
# the reverse direction is not an accepted collision-read witness.
`!.label_check_flag` <- function(x) FALSE
false_flag <- structure(FALSE,class='label_check_flag')
stopifnot(!isFALSE(false_flag))
false_outcome <- tryCatch(verbs[[1]](false_flag),error=identity)
stopifnot(inherits(false_outcome,'marginplyr_error'),grepl('logical scalar',conditionMessage(false_outcome),fixed=TRUE))
# .keep uses if(.keep), and an accepted stored TRUE retains the key.
`!.label_check_flag` <- function(x) TRUE
keep <- nest_with_margins(tibble::tibble(g='a',v=1L),.grouping=rollup(g),.keep=flag)
stopifnot(all(vapply(keep$data,function(cell)'g'%in%names(cell),logical(1))))
# Names and dimensions without custom logical operators preserve the choice.
for(scalar in list(c(named=TRUE),matrix(TRUE),I(TRUE))) {
  outcome<-tryCatch(verbs[[1]](scalar),error=identity)
  stopifnot(inherits(outcome,'marginplyr_error'),grepl('already present',conditionMessage(outcome),fixed=TRUE))
}
cat('FALSE dispatch rejection, .keep, and ordinary attribute controls passed.\n')
