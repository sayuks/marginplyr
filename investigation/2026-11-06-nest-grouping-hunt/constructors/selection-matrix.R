Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet=TRUE)
suppressWarnings({
d <- tibble::tibble(a=c("x","y"), b=1:2, z=c(TRUE,FALSE), dot1=c(3,4), `a b`=as.Date(c("2020-01-01","2020-01-02")), .name_repair="minimal")
source <- list(local=d,dtplyr=dtplyr::lazy_dt(d))
base_env <- rlang::env(sel=c("a","z"), ix=c(2L,1L), chosen="b", a="b", dot1="z")
expressions <- list(quote(a),"a",1L,-1L,NULL,quote(c(a,b)),quote(c("a","z")),quote(all_of(sel)),quote(.env$sel),quote(all_of(.env$sel)),quote(.env$ix),quote(a:b),quote(!a),quote(where(is.character)),quote(tidyselect::peek_vars()[1:2]),quote(names(tidyselect::peek_data())),quote(base::`(`(a)),quote((a)),quote(base::identity(a)),quote((function(x)x)(chosen)),quote(all_of(c(sel,chosen))),quote(dot1),quote(`a b`))
wrap <- list(plain=identity,syntactic=function(x)rlang::call2("(",x),qualified=function(x)rlang::call2(quote(base::`(`),x),private=function(x)rlang::call2(quote(base:::`(`),x),identity=function(x)rlang::call2(quote(base::identity),x),double=function(x)rlang::call2(quote(base::`(`),rlang::call2("(",x)))
rows <- list()
for(e in seq_along(expressions)) for(w in names(wrap)) {
 q<-rlang::new_quosure(wrap[[w]](expressions[[e]]),base_env)
 expected_select<-tryCatch(names(suppressWarnings(tidyselect::eval_select(q,d[0,,drop=FALSE]))),error=identity)
 for(ct in c("grouping_set","grouping_sets","rollup","cube","grouping_spec")) for(be in names(source)){
  actual<-tryCatch(suppressWarnings(inspect_grouping(source[[be]],.grouping=eval(rlang::call2(ct,q)),.format="list",.duplicates="keep")),error=identity)
  expected<-if(inherits(expected_select,"error")) expected_select else tryCatch(inspect_grouping(source[[be]],.grouping=eval(rlang::call2(ct,rlang::expr(all_of(!!expected_select)))),.format="list",.duplicates="keep"),error=identity)
  equal<-if(inherits(expected,"error")) inherits(actual,"error") && identical(class(actual), class(expected)) && identical(conditionMessage(actual), conditionMessage(expected)) else identical(actual,expected)
  rows[[length(rows)+1L]]<-data.frame(expression=e,wrapper=w,constructor=ct,backend=be,reference_outcome=if(inherits(expected,"error")) "refusal" else "result",actual_outcome=if(inherits(actual,"error")) "refusal" else "result",status=if(equal)"match" else "DIFFERENCE",detail=if(inherits(actual,"error")) conditionMessage(actual) else paste(actual$included,collapse="|"))
 }
}
result<-do.call(rbind,rows)
write.csv(result,file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()), "selection-results.csv"),row.names=FALSE)
print(table(result$status)); print(result[result$status!="match",],row.names=FALSE)
})
