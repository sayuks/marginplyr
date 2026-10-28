args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
d <- tibble(g=c('a','a','b'),x=c(1,2,30),y=c(2,5,9))
run <- function(dots, data=d) {
  cap <- function(e) tryCatch(withCallingHandlers(e,warning=function(c) invokeRestart('muffleWarning')),error=identity)
  expected <- cap(rlang::eval_tidy(rlang::expr(dplyr::summarise(!!data,!!!dots,.by=g))))
  actual <- cap(rlang::eval_tidy(rlang::expr(summarize_with_margins(!!data,!!!dots,.grouping=grouping_set(g),.margin_label=NULL))))
  if(inherits(expected,'error') && inherits(actual,'error')) return(invisible(NULL))
  if(!isTRUE(all.equal(actual,expected,check.attributes=TRUE))) {
    cat('\nCASE ',paste(vapply(dots,rlang::as_label,character(1)),collapse=' | '),'\n')
    cat('EXPECTED\n');print(expected);cat('ACTUAL\n');print(actual)
  }
}
cases <- list(
  rlang::quos(n=n()),
  rlang::quos(s=sum(x), across(c(x,y), ~sum(.x)), z=s+1),
  rlang::quos(x=sum(x), across(c(x,y), ~sum(.x))),
  rlang::quos(across(c(x,y),list(sum=sum,mean=mean))),
  rlang::quos(across(c(x,y), ~tibble(a=min(.x),b=max(.x)), .unpack=TRUE)),
  rlang::quos(tibble(x=sum(x),y=sum(y)), z=x+y),
  rlang::quos((across(c(x,y), function(v) sum(v)+n()))),
  rlang::quos((across(c(x,y), (function(n) function(v) sum(v)+n)(n())))),
  rlang::quos({f <- function(v) sum(v); across(c(x,y),f)}),
  rlang::quos(local({f<-function(v) sum(v); across(c(x,y),f)})),
  rlang::quos(eval(quote(across(c(x,y),sum)))),
  rlang::quos(eval(parse(text='across(c(x,y),sum)'))),
  rlang::quos(pick(x)),
  rlang::quos(z=sum(pick(x,y))),
  rlang::quos(if_any(c(x,y),~.x>1)),
  rlang::quos(z={h<-function() across(c(x,y),sum);h()}),
  rlang::quos((function() across(c(x,y),sum))()),
  rlang::quos((function(q=pick(x,y)) sum(q))()),
  rlang::quos((function() dplyr::summarise(tibble(a=1:3),across(a,sum)))()),
  rlang::quos(z=if(TRUE) across(c(x,y),sum) else 1),
  rlang::quos(z=identity(across(c(x,y),sum))),
  rlang::quos(across(c(x,y),sum,.names='{.col}_{n()}')),
  rlang::quos(across(c(x,y),sum,.names='{.col}_{sum(x)}')),
  rlang::quos(across(c(x,y),list(function(v)sum(v)),.names='{.col}_{.fn}'))
)
invisible(lapply(cases,run))
cat('CASES',length(cases),'\n')

cases2 <- list(
  list(rlang::new_quosure(rlang::call2('pick',rlang::missing_arg(),rlang::missing_arg()),environment())),rlang::quos(across()),rlang::quos(if_any()),rlang::quos(if_all()),
  rlang::quos(x=NULL,across(everything(),length)),
  rlang::quos(x=NULL,pick(everything())),
  rlang::set_names(rlang::quos(sum(x),z+1),c('z','z')),
  rlang::quos(z=sum(x), across(z, sum)),
  rlang::quos(z=data.frame(a=sum(x)),across(z,~.x$a)),
  rlang::quos(z=matrix(sum(x),1),w=length(z)),
  rlang::quos({z<-sum(x);tibble(z=z)},a=z+1),
  rlang::quos(local({z<-sum(x);tibble(z=z)}),a=z+1),
  rlang::quos((function(n)function(x)sum(x)+n)(n())(x)),
  rlang::quos(across(c(x,y), (function(n)function(x)sum(x)+n)(n()))),
  rlang::quos(a=stats::setNames(sum(x),'hi')),
  rlang::quos(a=structure(sum(x),foo='bar')),
  rlang::quos(a=list(sum(x))),
  rlang::quos(z=dplyr::summarise(.data=tibble(a=1:3),across(a,sum))),
  rlang::quos(z=dplyr::summarise(tibble(a=1:3) %>% mutate(a=a+1),across(a,sum))),
  rlang::quos(z=(tibble(a=1:3) %>% dplyr::summarise(across(a,sum)))),
  rlang::quos(z=(tibble(a=1:3) |> dplyr::summarise(across(a,sum))))
)
for(data in list(d,d[0,],as.data.frame(d),as.data.frame(d[0,]))) for(dots in cases2) run(dots,data)
cat('MORE CASES',length(cases2)*4,'\n')
