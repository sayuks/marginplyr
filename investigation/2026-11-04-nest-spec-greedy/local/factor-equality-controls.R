Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.', quiet=TRUE)
f <- function(x) structure(as.integer(factor(x, levels=c('A','a',NA_character_),exclude=NULL)), levels=c('A','a',NA_character_),class=c('casefold_factor','factor'))
registerS3method('vec_proxy_equal','casefold_factor', function(x,...) tolower(levels(x)[unclass(x)]),envir=asNamespace('vctrs'))
registerS3method('vec_ptype2','casefold_factor.casefold_factor',function(x,y,...)vctrs::vec_slice(x,integer()),envir=asNamespace('vctrs'))
registerS3method('vec_cast','casefold_factor.casefold_factor',function(x,to,...)x,envir=asNamespace('vctrs'))
x <- tibble::tibble(f=f(c('A','a')),g=c('x','x'),row=1:2)
print(dplyr::summarise(x, rows=list(row),.by=f))
for (v in list(nest_with_margins,nest_by_with_margins)) for (plan in list(NULL,grouping_set(g),rollup(g))) {
 y<-v(x,.by=f,.grouping=plan,.keep=TRUE,.id='set')
 print(y);print(lapply(y$data,function(z)z$row))
}
