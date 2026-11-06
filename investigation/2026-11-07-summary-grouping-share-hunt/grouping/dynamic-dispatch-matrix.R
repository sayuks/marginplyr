Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
contract <- "--assert" %in% commandArgs(trailingOnly=TRUE)
controls_only <- "--controls-only" %in% commandArgs(trailingOnly=TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", script_dir)
source_data <- tibble::tibble(a=c("east","west"), b=c(1L,1L),z=c(2,3))
seen <- character()
makeActiveBinding("hunt_dynamic_length", function() {
 kind <- typeof(tidyselect::peek_data()$a)
 seen <<- c(seen,kind)
 if(kind=="character") 1L else 2L
},globalenv())
cleanup <- function() {
 if(exists("length.stringr_pattern",globalenv(),inherits=FALSE)) rm("length.stringr_pattern",envir=globalenv())
}
method <- as.vector(as.vector(list(x=NULL,as.vector("hunt_dynamic_length","symbol")),"pairlist"),"function")
rows <- list()
for(modifier in c("fixed","regex","coll")) for(input_kind in c("tibble","data.frame","data.table","dtplyr")) for(position in c("fixed","grouping_set","grouping_sets","rollup","cube","grouping_spec")) for(consumer in c("inspect","summary")) {
 cleanup()
 data <- switch(input_kind,tibble=source_data,data.frame=as.data.frame(source_data),data.table=data.table::as.data.table(source_data),dtplyr=dtplyr::lazy_dt(source_data,immutable=TRUE))
 original <- serialize(data,NULL)
 pattern <- rlang::call2(modifier,"a",.ns="stringr")
 selection <- rlang::expr(matches(!!pattern,vars={
  "length.stringr_pattern" <<- as.vector(as.vector(list(x=NULL,as.vector("hunt_dynamic_length","symbol")),"pairlist"),"function")
  tidyselect::peek_vars()
 }))
 typed_selection <- selection
 typed_selection$vars <- rlang::expr(base::identity(!!selection$vars))
 cleanup()
 direct <- names(tidyselect::eval_select(selection,source_data))
 stopifnot(identical(direct,"a"))
 run <- function(sel,preinstalled=FALSE) {
  cleanup()
  if(preinstalled) assign("length.stringr_pattern",method,globalenv())
  seen <<- character()
  spec <- if(position=="fixed") marginplyr::grouping_set(b) else rlang::eval_tidy(rlang::call2(getExportedValue("marginplyr",position),sel))
  by <- if(position=="fixed") rlang::new_quosure(sel,globalenv()) else rlang::quo(b)
  result <- tryCatch(if(consumer=="inspect") marginplyr::inspect_grouping(data,.by=!!by,.grouping=spec,.format="list") else tibble::as_tibble(marginplyr::summarize_with_margins(data,n=dplyr::n(),.by=!!by,.grouping=spec,.margin_label=NULL,.id="set")),error=identity)
  kinds <- paste(seen,collapse=",")
  cleanup()
  list(value=result,kinds=kinds)
 }
 actual <- if(controls_only) NULL else run(selection)
 typed <- run(typed_selection)
 installed <- run(selection,TRUE)
 literal <- run(quote(all_of("a")))
 stopifnot(!inherits(typed$value,"error"),!inherits(installed$value,"error"),!inherits(literal$value,"error"),identical(typed$value,literal$value),identical(installed$value,literal$value),identical(serialize(data,NULL),original))
 if(!controls_only) {
  if(contract) stopifnot(!inherits(actual$value,"error"),identical(actual$value,literal$value)) else stopifnot(inherits(actual$value,"error"),grepl("patterns must be length 1",conditionMessage(actual$value),fixed=TRUE),identical(actual$kinds,"integer"))
 }
 rows[[length(rows)+1L]] <- data.frame(modifier,input_kind,position,consumer,actual_types=if(controls_only) NA_character_ else actual$kinds,typed_types=typed$kinds,preinstalled_types=installed$kinds,false_refusal=if(controls_only) NA else inherits(actual$value,"error"))
}
cleanup()
results <- do.call(rbind,rows)
write.csv(results,file.path(output,if(controls_only)"dynamic-dispatch-controls.csv" else "dynamic-dispatch-matrix.csv"),row.names=FALSE)
cat("Completed",nrow(results),"probes; typed/preinstalled/literal references matched\n")
if(!controls_only) cat("False refusals:",sum(results$false_refusal),"\n")
for(pkg in c("dplyr","tidyselect","stringr","vctrs","rlang","dtplyr","data.table","pkgload")) cat(pkg,as.character(packageVersion(pkg)),"\n")
cat(R.version.string,"\n",R.version$platform,"\n")
