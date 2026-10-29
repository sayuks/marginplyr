repo <- '.'
pkgload::load_all(repo, quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
set.seed(20261029)
check <- function(d, dots, i) {
  e <- environment()
  exp <- tryCatch(suppressWarnings(rlang::eval_tidy(rlang::expr(dplyr::summarise(d, !!!dots, .by = g)), env = e)), error=identity)
  got <- tryCatch(suppressWarnings(rlang::eval_tidy(rlang::expr(summarize_with_margins(d, !!!dots, .grouping = grouping_set(g), .margin_label = NULL)), env=e)), error=identity)
  if (inherits(exp,'error') && inherits(got,'error')) return(invisible(TRUE))
  if (!isTRUE(all.equal(exp,got,check.attributes=TRUE))) {
    cat('FAIL CASE', i, '\n'); print(dots); print(exp); print(got)
    saveRDS(list(d=d,dots=dots,expected=exp,actual=got),sprintf('/tmp/marginplyr-summary-fail-%s.rds',i)); return(FALSE)
  }
  TRUE
}
frames <- list(
 tibble::tibble(g=c('a','a','b'),x=1:3,y=c(4,NA,5),z=c('q','r','s')),
 tibble::tibble(g=character(),x=integer(),y=double(),z=character()),
 tibble::tibble(g=c(NA_character_,'b'),x=1:2,y=3:4,z=c('q','r'))
)
exprs <- list(
 rlang::exprs(n=dplyr::n(), m=sum(x)),
 rlang::exprs(x=sum(x), twice=x*2),
 rlang::exprs(x=NULL, count=ncol(dplyr::pick(dplyr::everything()))),
 rlang::exprs(tibble::tibble(lo=min(x),hi=max(x)), delta=hi-lo),
 rlang::exprs(tibble::tibble(lo=sum(x)),dplyr::across(dplyr::where(is.numeric),sum,.names='sum_{.col}')),
 rlang::exprs(dplyr::across(c(x,y),list(lo=min,hi=max))),
 rlang::exprs(dplyr::across(c(x,y),~tibble::tibble(lo=min(.x),hi=max(.x)),.unpack=TRUE)),
 rlang::exprs(dplyr::across(c(x,y),~tibble::tibble(lo=min(.x),hi=max(.x)))),
 rlang::exprs(tibble::tibble(x=sum(x)),dplyr::pick(x)),
 rlang::exprs(val=list(dplyr::pick(x,y))),
 rlang::exprs(val=list(x), n=length(val[[1]])),
 rlang::exprs(mat=matrix(c(min(x),max(x)),nrow=1)),
 rlang::exprs(mat=I(matrix(c(min(x),max(x)),nrow=1))),
 rlang::exprs(v=integer()),
 rlang::exprs(dplyr::pick(dplyr::where(is.logical))),
 rlang::exprs(dplyr::across(.cols=c(x,y),.fns=NULL)),
 rlang::exprs(dplyr::across(.cols=c(x,y),.fns=~NULL)),
 rlang::exprs(x=as.character(sum(x)),val=paste(names(dplyr::pick(dplyr::where(is.character))),collapse=',')),
 rlang::exprs(nrow(dplyr::pick(x))),
 rlang::exprs(identity(dplyr::pick(x))),
 rlang::exprs(I(dplyr::pick(x))),
 rlang::exprs(structure(dplyr::pick(x),class=c('special','tbl_df','tbl','data.frame'))),
 rlang::exprs(cbind(dplyr::pick(x),zz=sum(x))),
 rlang::exprs(unclass(dplyr::pick(x))),
 rlang::exprs({dplyr::pick(x)}),
 rlang::exprs((dplyr::pick(x))),
 rlang::exprs(foo=dplyr::pick(x)),
 rlang::exprs(foo={dplyr::pick(x)}),
 rlang::exprs({x <- sum(x); x}),
 rlang::exprs(x=length(dplyr::pick(x)[[1]]), y=sum(x)),
 rlang::exprs(dplyr::across(x,~sum(.x)),x=0L,dplyr::across(x,~sum(.x),.names='after_{.col}'))
)
fails <- 0L; n <- 0L
for(d in frames) for(dots in exprs) {n<-n+1L; if(!check(d,dots,n)) fails<-fails+1L}
cat('SUMMARY',n,'cases;',fails,'mismatches\n')
cat(R.version.string,'\n'); print(sapply(c('marginplyr','dplyr','rlang','vctrs','tibble'),packageVersion))
