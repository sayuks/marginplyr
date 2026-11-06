Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
d <- data.frame(a=c("x","y"),b=c(1L,1L),value=11:12)
rows <- list()
for (ct in c("grouping_set","grouping_sets","rollup","cube","grouping_spec")) for (backend in c("local","dtplyr")) for (op in c("::",":::")) for (style in c("bound","constructor","injected","embraced","numbered")) {
 reads <- 0L
 e <- rlang::env(s=grouping_set(a))
 e[[op]] <- function(package,name) { reads <<- reads+1L; function(x) "b" }
 head <- rlang::call2(op, quote(base), quote(`(`))
 wrap <- function(x) rlang::call2(head,x)
 q <- rlang::new_quosure(wrap(if(style=="constructor") quote(grouping_set(a)) else quote(s)),e)
 expected_selection <- names(tidyselect::eval_select(q,d[0,,drop=FALSE]))
 stopifnot(identical(expected_selection,"b"), identical(reads,1L))
 if(style %in% c("bound","constructor")) spec <- eval(rlang::call2(ct,rlang::quo_get_expr(q)),e)
 if(style=="injected") spec <- eval(rlang::call2(ct,q),e)
 if(style=="embraced") {
   fn <- eval(rlang::call2("function", pairlist(x=rlang::missing_arg()),rlang::call2(ct,wrap(quote({{ x }})))),e)
   spec <- eval(rlang::call2(fn,quote(s)),e)
 }
 if(style=="numbered") {
   fn <- eval(rlang::call2("function", pairlist(...=rlang::missing_arg()),rlang::call2(ct,wrap(quote(..1)))),e)
   spec <- eval(rlang::call2(fn,quote(s)),e)
 }
 input <- if(backend=="local") d else dtplyr::lazy_dt(d)
 for(consumer in c("inspect","nest","nest_by")) {
   consume <- switch(consumer,inspect=function(s) inspect_grouping(input,.grouping=s,.format="list"),nest=function(s) nest_with_margins(input,.grouping=s),nest_by=function(s) nest_by_with_margins(input,.grouping=s))
   reads<-0L
   actual<-tryCatch(consume(spec),error=identity)
   observed_reads<-reads
   reference<-consume(eval(rlang::call2(ct,rlang::expr(all_of(!!expected_selection)))))
   if (!inherits(actual, "error") && consumer != "inspect") { actual <- dplyr::collect(actual); reference <- dplyr::collect(reference) }; equal<-identical(actual,reference)
   rows[[length(rows)+1L]]<-data.frame(constructor=ct,backend=backend,operator=op,style=style,consumer=consumer,lookup_reads=observed_reads,status=if(equal)"match" else if(inherits(actual,"error")) "false_refusal" else "wrong_result",detail=if(inherits(actual,"error")) conditionMessage(actual) else "")
 }
}
result<-do.call(rbind,rows)
write.csv(result,file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()), "qualified-spec-results.csv"),row.names=FALSE)
print(with(result,table(operator,status)))
print(with(result,table(operator,lookup_reads)))
print(with(result,table(constructor,status)))
