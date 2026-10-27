args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if(dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if(!dir.create(output_dir,recursive=TRUE)) stop("Could not create output directory.")
pkgload::load_all('.',quiet=TRUE)
input <- data.frame(g=c('a','b'),v=c(1,2))
dataset_dir <- tempfile('audit_dataset_parent_')
arrow::write_dataset(input,dataset_dir,format='parquet')
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
remote <- dplyr::copy_to(con,input,'sql_parent_for_nonsql')
constructors <- list(
  table=function() arrow::arrow_table(input),batch=function() arrow::RecordBatch$create(input),dataset=function() arrow::open_dataset(dataset_dir),
  table_query=function() dplyr::select(arrow::arrow_table(input),dplyr::everything()),batch_query=function() dplyr::select(arrow::RecordBatch$create(input),dplyr::everything()),dataset_query=function() dplyr::select(arrow::open_dataset(dataset_dir),dplyr::everything()),
  immutable=function() dtplyr::lazy_dt(data.table::as.data.table(input),immutable=TRUE),mutable=function() dtplyr::lazy_dt(data.table::as.data.table(input),immutable=FALSE),data_table=function() data.table::as.data.table(input)
)
verbs <- list(summarize_with_margins,summarise_with_margins,expand_with_margins,nest_with_margins,nest_by_with_margins,inspect_grouping)
read <- function() tryCatch(last_sent_queries(details=TRUE),error=function(cnd) list(classes=class(cnd),message=conditionMessage(cnd)))
cases <- 0L
for(bname in names(constructors)) for(verb in verbs) for(parent_audited in c(FALSE,TRUE)) for(child_audited in c(FALSE,TRUE)) {
  options(marginplyr.audit_sql=parent_audited)
  summarize_with_margins(remote,total=sum(v),.grouping=rollup(g))
  expected <- read()
  options(marginplyr.audit_sql=parent_audited)
  saw_parent_row <- FALSE
  outer_factory <- function() {
    before <- read()
    if(parent_audited && nrow(before)>0L) {
      stopifnot(identical(before$purpose,'selection_proxy'),identical(before$status,'returned'))
      saw_parent_row <<- TRUE
    }
    options(marginplyr.audit_sql=child_audited)
    inner_factory <- function() {
      inside <- read()
      if(child_audited) stopifnot(nrow(inside)==0L,identical(inside$sql,character()))
      else stopifnot('marginplyr_error'%in%inside$classes)
      rollup(g)
    }
    tryCatch(verb(constructors[[bname]](),.grouping=inner_factory()),error=function(cnd) invisible(NULL))
    stopifnot(identical(read(),before))
    'g'
  }
  summarize_with_margins(remote,total=sum(v),.grouping=rollup(dplyr::all_of(outer_factory())))
  if(parent_audited) stopifnot(saw_parent_row)
  stopifnot(identical(read(),expected),identical(getOption('marginplyr.audit_sql'),child_audited))
  cases <- cases+1L
}
DBI::dbDisconnect(con,shutdown=TRUE)
unlink(dataset_dir,recursive=TRUE)
cat('SQL-parent/nonSQL-child reentry passed:',cases,'cases\n')
saveRDS(list(cases=cases,session=sessionInfo()),file.path(output_dir,'summary.rds'))
writeLines(capture.output(sessionInfo()),file.path(output_dir,'session-info.txt'))
