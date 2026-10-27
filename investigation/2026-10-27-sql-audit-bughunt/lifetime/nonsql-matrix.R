args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if(dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if(!dir.create(output_dir,recursive=TRUE)) stop("Could not create output directory.")
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
source_data <- data.frame(g=c('a','a','b'),h=c('x','y','x'),v=c(1,2,3))
dataset_dir <- tempfile('audit_dataset_')
arrow::write_dataset(source_data,dataset_dir,format='parquet')
con <- DBI::dbConnect(RSQLite::SQLite(),':memory:')
remote <- dplyr::copy_to(con,source_data,'nonsql_nested_sql')
constructors <- list(
  arrow_table=function() arrow::arrow_table(source_data),
  arrow_batch=function() arrow::RecordBatch$create(source_data),
  arrow_dataset=function() arrow::open_dataset(dataset_dir),
  arrow_table_query=function() dplyr::filter(arrow::arrow_table(source_data),v>0),
  arrow_batch_query=function() dplyr::filter(arrow::RecordBatch$create(source_data),v>0),
  arrow_dataset_query=function() dplyr::filter(arrow::open_dataset(dataset_dir),v>0),
  dtplyr_immutable=function() dtplyr::lazy_dt(data.table::as.data.table(source_data),immutable=TRUE),
  dtplyr_mutable=function() dtplyr::lazy_dt(data.table::as.data.table(source_data),immutable=FALSE),
  data_table=function() data.table::as.data.table(source_data)
)
verbs <- list(summarize=summarize_with_margins,summarise=summarise_with_margins,expand=expand_with_margins,nest=nest_with_margins,nest_by=nest_by_with_margins,inspect=inspect_grouping)
condition_summary <- function(cnd) list(classes=class(cnd),message=conditionMessage(cnd))
record <- function() tryCatch(last_sent_queries(details=TRUE),error=condition_summary)
assert_record <- function(audited) {
  if(audited) {
    plain <- last_sent_queries(); detailed <- last_sent_queries(details=TRUE)
    stopifnot(identical(names(plain),c('purpose','sql')),identical(names(detailed),c('purpose','sql','source','status','conditions')),nrow(detailed)==0L,identical(detailed$purpose,character()),identical(detailed$sql,character()),identical(detailed$source,character()),identical(detailed$status,character()),identical(detailed$conditions,list()),identical(last_sent_queries(FALSE,TRUE),detailed),identical(last_sent_queries(FALSE),plain))
  } else {
    caught <- tryCatch(last_sent_queries(),error=identity)
    stopifnot(inherits(caught,'marginplyr_error'),grepl('marginplyr.audit_sql',conditionMessage(caught),fixed=TRUE))
  }
}
normalize <- function(value) {
  if(inherits(value,c('arrow_dplyr_query','Table','RecordBatch','Dataset','dtplyr_step'))) value <- dplyr::collect(value)
  if(is.data.frame(value)) {
    value <- as.data.frame(value)
    for(name in names(value)) {
      if(is.list(value[[name]])) value[[name]] <- lapply(value[[name]],normalize)
    }
    if(nrow(value)>1L) {
      keys <- vapply(seq_len(nrow(value)),function(i) paste(format(serialize(as.list(value[i,,drop=FALSE]),NULL)),collapse=""),character(1))
      value <- value[order(keys),,drop=FALSE]
    }
    rownames(value) <- NULL
  }
  value
}
run <- function(constructor,verb,vname,mode,audited) {
  options(marginplyr.audit_sql=audited)
  input <- constructor()
  source_before <- normalize(input)
  warnings <- messages <- list()
  nested_calls <- 0L
  factory <- function() {
    before <- record()
    options(marginplyr.audit_sql=!audited)
    summarize_with_margins(remote,total=sum(v),.grouping=rollup(g),.check_margin_label=TRUE)
    stopifnot(identical(record(),before))
    options(marginplyr.audit_sql=audited)
    nested_calls <<- nested_calls+1L
    rollup(g)
  }
  select_g <- function() {
    factory()
    'g'
  }
  options_list <- switch(mode,
    default=list(),
    rollup=list(.grouping=rlang::quo(rollup(g))),
    typed=list(.grouping=rlang::quo(rollup(dplyr::where(is.character)))),
    invalid_option=list(.duplicates='bogus'),
    missing_column=list(.grouping=rlang::quo(rollup(no_such_column))),
    observed_labels=list(.grouping=rlang::quo(rollup(g)),.check_margin_label=TRUE),
    reentry=list(.grouping=rlang::quo(factory())),
    typed_reentry=list(.grouping=rlang::quo(rollup(dplyr::all_of(select_g()))))
  )
  if(vname=='inspect') options_list$.check_margin_label <- NULL
  if(vname %in% c('summarize','summarise')) options_list$total <- rlang::quo(sum(v,na.rm=TRUE))
  call <- rlang::expr(verb(!!input,!!!options_list))
  # Inject captured expressions; do not hand quosure objects to constructor parameters.
  for(i in seq_along(call)) {
    if(rlang::is_quosure(call[[i]])) call[[i]] <- rlang::quo_get_expr(call[[i]])
  }
  value <- tryCatch(withCallingHandlers(eval(call),
    warning=function(cnd) { warnings[[length(warnings)+1L]] <<- condition_summary(cnd); invokeRestart('muffleWarning') },
    message=function(cnd) { messages[[length(messages)+1L]] <<- condition_summary(cnd); invokeRestart('muffleMessage') }),error=condition_summary)
  assert_record(audited)
  held <- record()
  if(!is.list(value) || is.data.frame(value) || inherits(value,c('arrow_dplyr_query','dtplyr_step'))) {
    value <- tryCatch(withCallingHandlers(normalize(value),warning=function(cnd) {warnings[[length(warnings)+1L]] <<- condition_summary(cnd);invokeRestart('muffleWarning')},message=function(cnd) {messages[[length(messages)+1L]] <<- condition_summary(cnd);invokeRestart('muffleMessage')}),error=condition_summary)
  }
  stopifnot(identical(record(),held),isTRUE(all.equal(normalize(input),source_before)))
  # A subsequent SQL call must acquire its own complete record.
  options(marginplyr.audit_sql=TRUE)
  sql_result <- expand_with_margins(remote,.grouping=rollup(g))
  sql_record <- last_sent_queries(details=TRUE)
  stopifnot(identical(sql_record$purpose,'result'),identical(sql_record$status,'prepared'),identical(sql_record$sql,as.character(dbplyr::sql_render(sql_result))))
  list(value=value,warnings=warnings,messages=messages,nested_calls=nested_calls)
}
comparisons <- 0L
outcomes <- list()
for(bname in names(constructors)) for(vname in names(verbs)) for(mode in c('default','rollup','typed','invalid_option','missing_column','observed_labels','reentry','typed_reentry')) {
  off <- run(constructors[[bname]],verbs[[vname]],vname,mode,FALSE)
  on <- run(constructors[[bname]],verbs[[vname]],vname,mode,TRUE)
  if(!isTRUE(all.equal(on,off))) {
    cat('MISMATCH',bname,vname,mode,'\n'); print(all.equal(on,off)); dput(off);dput(on);stop('audit changed behavior')
  }
  comparisons <- comparisons+1L
  outcomes[[comparisons]] <- list(backend=bname,verb=vname,mode=mode,value=on$value,nested_calls=on$nested_calls)
}
saveRDS(outcomes,file.path(output_dir,'outcomes.rds'))
DBI::dbDisconnect(con)
unlink(dataset_dir,recursive=TRUE)
cat('Non-SQL audit ON/OFF comparisons passed:',comparisons,'\n')
saveRDS(list(cases=comparisons,session=sessionInfo()),file.path(output_dir,'summary.rds'))
writeLines(capture.output(sessionInfo()),file.path(output_dir,'session-info.txt'))
