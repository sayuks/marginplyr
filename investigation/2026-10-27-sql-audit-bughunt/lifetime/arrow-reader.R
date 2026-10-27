args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if(dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if(!dir.create(output_dir,recursive=TRUE)) stop("Could not create output directory.")
pkgload::load_all('.',quiet=TRUE)
input <- data.frame(g=c('a','b'),v=c(1,2))
verbs <- list(summarize=summarize_with_margins,summarise=summarise_with_margins,expand=expand_with_margins,nest=nest_with_margins,nest_by=nest_by_with_margins,inspect=inspect_grouping)
run <- function(verb,vname,mode,query,audited) {
  options(marginplyr.audit_sql=audited)
  reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  data <- if(query) dplyr::select(reader,dplyr::everything()) else reader
  value <- tryCatch(switch(mode,
    default=verb(data),
    grouping=verb(data,.grouping=rollup(g)),
    typed=verb(data,.grouping=rollup(dplyr::where(is.character))),
    invalid=verb(data,.duplicates='bad')
  ),error=function(cnd) list(classes=class(cnd),message=conditionMessage(cnd)))
  if(audited) {
    stopifnot(nrow(last_sent_queries())==0L,identical(last_sent_queries()$sql,character()),identical(last_sent_queries(details=TRUE),dplyr::tibble(purpose=character(),sql=character(),source=character(),status=character(),conditions=list())))
  } else stopifnot(inherits(tryCatch(last_sent_queries(),error=identity),'marginplyr_error'))
  # Neither the accepted inspection nor a rejected construction may read batches.
  remaining <- as.data.frame(dplyr::collect(arrow::as_arrow_table(reader)))
  stopifnot(identical(remaining,input))
  list(value=value,remaining=remaining)
}
comparisons <- 0L
for(vname in names(verbs)) for(query in c(FALSE,TRUE)) for(mode in c('default','grouping','typed','invalid')) {
  off <- run(verbs[[vname]],vname,mode,query,FALSE)
  on <- run(verbs[[vname]],vname,mode,query,TRUE)
  stopifnot(identical(off,on))
  comparisons <- comparisons+1L
}
cat('RecordBatchReader audit ON/OFF comparisons passed:',comparisons,'\n')
saveRDS(list(cases=comparisons,session=sessionInfo()),file.path(output_dir,'summary.rds'))
writeLines(capture.output(sessionInfo()),file.path(output_dir,'session-info.txt'))
