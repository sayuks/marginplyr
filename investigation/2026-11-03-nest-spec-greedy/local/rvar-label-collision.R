pkgload::load_all(quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
out <- "investigation/2026-11-03-nest-spec-greedy/local"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
run <- function(verb,source,spec,label="Total",check=TRUE) {
  if(verb=="summarize_with_margins") {
    summarize_with_margins(source,n=dplyr::n(),.grouping=spec,.margin_label=label,
      .check_margin_label=check,.id="set")
  } else get(verb)(source,.grouping=spec,.margin_label=label,
    .check_margin_label=check,.id="set")
}
input <- tibble::tibble(g=posterior::rvar(matrix(1,nrow=4L,ncol=1L)),row_id=1L)
display <- as.character(input$g)
cat("rvar dimension:",dim(input$g),"display class:",class(display),"display dim:",dim(display),"\n")
print(display)
stopifnot(identical(tidyr::nest(input,.by=g)$data[[1]]$row_id,1L))
stopifnot(identical(vctrs::vec_group_loc(input$g)$loc,list(1L)))
minimal <- lapply(c("nest_with_margins","nest_by_with_margins"),function(verb) {
  result<-tryCatch(run(verb,input,grouping_set(g)),error=identity)
  cat(verb,":",if(inherits(result,"error")) conditionMessage(result) else "succeeded","\n")
  result
})
if("--assert" %in% commandArgs(trailingOnly=TRUE)) {
  stopifnot(all(vapply(minimal,function(z) !inherits(z,"error") && nrow(z)==1L && identical(z$data[[1]]$row_id,1L),logical(1))))
  quit(status=0L)
}
for(verb in c("nest_with_margins","nest_by_with_margins")) {
  unchecked<-run(verb,input,grouping_set(g),check=FALSE)
  missing<-run(verb,input,grouping_set(g),label=NULL)
  fixed<-get(verb)(input,.by=g)
  for(control in list(unchecked,missing,fixed)) stopifnot(nrow(control)==1L,identical(control$data[[1]]$row_id,1L))
}
raw_predicate<-tryCatch(dplyr::if_else(display=="Total",1L,0L,missing=0L),error=identity)
stopifnot(inherits(raw_predicate,"error"))
cat("isolated predicate:",conditionMessage(raw_predicate),"\n")
stopifnot(identical(sum(as.vector(display)=="Total"),0L))
stopifnot(identical(sum(as.vector(display)==display[[1]]),1L))
base_input<-tibble::tibble(g=array(1L,1L),row_id=1L)
stopifnot(nrow(nest_with_margins(base_input,.grouping=grouping_set(g)))==1L)
rows<-list(); k<-0L
for(n in c(1L,3L)) for(draws in 4L) {
  input<-tibble::tibble(g=posterior::rvar(matrix(rep(seq_len(n),each=draws),nrow=draws,ncol=n)),row_id=seq_len(n))
  for(source_kind in c("tibble","frame")) {
    source<-switch(source_kind,tibble=input,frame=as.data.frame(input),data.table=data.table::as.data.table(input))
    native<-tidyr::nest(tibble::as_tibble(source),.by=g)
    stopifnot(nrow(native)==n)
    for(plan in c("single","rollup")) for(label_kind in c("absent","present")) for(check in c(FALSE,TRUE)) {
      spec<-if(plan=="single") grouping_set(g) else rollup(g)
      label<-if(label_kind=="absent" || n==0L) "Total" else as.character(input$g)[[1]]
      for(verb in c("nest_with_margins","nest_by_with_margins")) {
        value<-tryCatch(run(verb,source,spec,label,check),error=identity)
        k<-k+1L
        verdict<-if(inherits(value,"marginplyr_error")) "package_refusal" else if(inherits(value,"error")) "external_error" else "success"
        rows[[k]]<-data.frame(n,draws,source_kind,plan,label_kind,check,verb,verdict,
          detail=if(inherits(value,"error")) conditionMessage(value) else "")
      }
    }
  }
}
ledger<-do.call(rbind,rows)
write.csv(ledger,file.path(out,"rvar-label-collision.csv"),row.names=FALSE)
print(table(ledger$verdict));print(with(ledger,table(n,check,verdict)))
print(sessionInfo())
