# Investigated 2026-10-29 against 59d55a7867ffc0255f75be32a233d9d475bcb7fb.
# Run with Rscript <script> [repository-root].
args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
repo <- if (length(positional)) positional[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
roots <- list(
  ordinary=tibble(g=c(1L,2L,3L),a=c('x','y','z'),value=c(0L,1L,NA_integer_)),
  special=tibble(.SD=c(1L,2L,3L),a=c('x','y','z'),value=c(0L,1L,NA_integer_)),
  char=tibble(g=c('a','b','c'),a=c(TRUE,FALSE,NA),value=c(1,2,NA)),
  double=tibble(g=c(1,2,3),a=c('x','y','z'),value=c(0L,1L,NA_integer_))
)
ops <- list(root=function(x)x, filter=function(x)filter(x, FALSE),
  rename=function(x)rename(x,renamed=a), reorder=function(x)relocate(x,value),
  mutate=function(x)mutate(x, new=value+0.5),
  mutate_copy=function(x)mutate(x,value=a),
  mutate_sequence=function(x)mutate(x,value=value+0.5,a=value),
  group=function(x)group_by(x,a), arrange=function(x)arrange(x,desc(value)))
selectors <- list(numeric=expr(where(is.numeric)),integer=expr(where(is.integer)),
 double=expr(where(is.double)),character=expr(where(is.character)),logical=expr(where(is.logical)),
 factor=expr(where(is.factor)))
count <- 0L; refused<-0L; skipped<-0L; failures<-list()
check <- function(step, label) {
 real <- tryCatch(collect(ungroup(step)),error=identity)
 if(inherits(real,'error') || !all(group_vars(step) %in% names(real))) {skipped<<-skipped+1L;return(invisible(NULL))}
 for(sn in names(selectors)){
  expr<-selectors[[sn]]
  actual<-tryCatch(rlang::eval_tidy(expr(inspect_grouping(!!step,.grouping=grouping_set(!!expr),.format='list'))),error=identity)
  expected<-tryCatch(rlang::eval_tidy(expr(inspect_grouping(!!real,.grouping=grouping_set(!!expr),.format='list'))),error=identity)
  if(inherits(actual,'marginplyr_error')&&grepl("Can't safely determine column metadata",conditionMessage(actual),fixed=TRUE)){refused<<-refused+1L;next}
  if(length(group_vars(step))){expected<-tryCatch(rlang::eval_tidy(expr(inspect_grouping(!!real,.by=all_of(group_vars(step)),.grouping=grouping_set(!!expr),.format='list'))),error=identity)}
  count<<-count+1L
  ok<-if(inherits(actual,'error')||inherits(expected,'error')) inherits(actual,'error')&&inherits(expected,'error')&&identical(class(actual),class(expected)) else identical(actual,expected)
  if(!ok) failures[[paste(label,sn)]]<<-list(actual=actual,expected=expected)
 }
}
for(rn in names(roots))for(on in names(ops)){
 root <- dtplyr::lazy_dt(roots[[rn]])
 step <- ops[[on]](root)
 check(step,paste(rn,on))
 for(post in c('filter','rename','reorder','group','arrange')) check(ops[[post]](step),paste(rn,on,post))
}
for(on in names(ops))for(pn in names(ops)){
 l<-ops[[on]](dtplyr::lazy_dt(roots$ordinary));r<-ops[[pn]](dtplyr::lazy_dt(roots$ordinary))
 for(jn in c('inner_join','left_join','right_join','full_join','semi_join','anti_join','union_all','union','intersect','setdiff')) {
  join<-get(jn,asNamespace('dplyr'))
  step<-tryCatch(if(grepl('join',jn)) join(l,r,by='g') else join(l,r),error=identity)
  if(inherits(step,'error')){skipped<-skipped+1L;next}
  check(step,paste(on,pn,jn))
 }
}
cat('CHECKED',count,'REFUSED',refused,'SKIPPED',skipped,'FAILURES',length(failures),'\n')
if(length(failures)) print(utils::head(failures, 5L))
stopifnot(length(failures)==0L)
