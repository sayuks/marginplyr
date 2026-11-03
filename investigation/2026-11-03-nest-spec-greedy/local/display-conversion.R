pkgload::load_all(quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
out <- "investigation/2026-11-03-nest-spec-greedy/local"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
call_verb <- function(verb, source, spec, check=TRUE, label="Total") {
  if (verb == "summarize_with_margins") {
    summarize_with_margins(source, n=dplyr::n(), .grouping=spec,
      .check_margin_label=check, .margin_label=label, .id="set")
  } else get(verb)(source, .grouping=spec, .check_margin_label=check,
    .margin_label=label, .id="set")
}
minimal <- tibble::tibble(g=hms::hms(numeric()))
cat("empty hms as.character type:",typeof(as.character(minimal$g)),"\n")
stopifnot(nrow(dplyr::summarise(minimal,n=dplyr::n(),.by="g"))==0L)
stopifnot(nrow(dplyr::nest_by(minimal,g))==0L)
minimal_results <- lapply(c("nest_with_margins","nest_by_with_margins"),function(verb) {
  result <- tryCatch(call_verb(verb,minimal,rollup(g)),error=identity)
  cat(verb,":",if(inherits(result,"error")) paste(class(result)[1],conditionMessage(result)) else "succeeded","\n")
  result
})
if("--assert" %in% commandArgs(trailingOnly=TRUE)) {
  stopifnot(all(vapply(minimal_results,function(x) !inherits(x,"error") && nrow(x)==0L && is.character(x$g),logical(1))))
  quit(status=0L)
}
types <- list(
  hms=hms::hms(c(1,2)), difftime=as.difftime(c(1,2),units="secs"),
  date=as.Date(c("2026-01-01","2026-01-02")),
  posixct=as.POSIXct(c("2026-01-01","2026-01-02"),tz="UTC"),
  posixlt=as.POSIXlt(c("2026-01-01","2026-01-02"),tz="UTC"),
  units=units::set_units(c(1,2),"m"),
  integer64=bit64::as.integer64(c(1,2)),
  versions=package_version(c("1.0","2.0")), character=c("a","b")
)
type_rows <- lapply(names(types),function(nm) data.frame(kind=nm,
  empty_type=typeof(as.character(vctrs::vec_slice(types[[nm]],integer()))),
  populated_type=typeof(as.character(types[[nm]]))))
write.csv(do.call(rbind,type_rows),file.path(out,"empty-display-types.csv"),row.names=FALSE)
print(do.call(rbind,type_rows),row.names=FALSE)
rows<-list(); k<-0L
for(kind in c("hms","difftime","character")) for(empty in c(FALSE,TRUE)) {
  input<-tibble::tibble(g=types[[kind]],row_id=1:2)
  if(empty) input<-input[integer(),]
  for(source_kind in c("tibble","frame","data.table","dtplyr")) {
    source<-switch(source_kind,tibble=input,frame=as.data.frame(input),
      data.table=data.table::as.data.table(input),dtplyr=dtplyr::lazy_dt(input))
    for(plan in c("single","rollup","cube")) for(check in c(FALSE,TRUE)) {
      spec<-switch(plan,single=grouping_set(g),rollup=rollup(g),cube=cube(g))
      for(verb in c("nest_with_margins","nest_by_with_margins","expand_with_margins","summarize_with_margins")) {
        value<-tryCatch(dplyr::collect(call_verb(verb,source,spec,check)),error=identity)
        k<-k+1L
        verdict<-if(inherits(value,"error")) "error" else if(!is.character(value$g)) "wrong_display_type" else "match"
        rows[[k]]<-data.frame(kind,empty,source_kind,plan,check,verb,verdict,
          result_rows=if(inherits(value,"error")) NA_integer_ else nrow(value),
          detail=if(inherits(value,"error")) paste(class(value)[1],conditionMessage(value)) else typeof(value$g))
      }
    }
  }
}
ledger<-do.call(rbind,rows)
write.csv(ledger,file.path(out,"display-conversion.csv"),row.names=FALSE)
print(table(ledger$verdict)); print(with(ledger,table(kind,empty,verdict)))
cat("Control NULL-label empty rollup:\n")
for(verb in c("nest_with_margins","nest_by_with_margins")) {
  result<-call_verb(verb,minimal,rollup(g),label=NULL)
  stopifnot(nrow(result)==0L,inherits(result$g,"hms"))
  cat(verb,"0 rows with typed hms key\n")
}
print(sessionInfo())
stopifnot(all(vapply(minimal_results,inherits,logical(1),"vctrs_error_ptype2")))
