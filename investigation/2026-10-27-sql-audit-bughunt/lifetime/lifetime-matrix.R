args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if(dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if(!dir.create(output_dir,recursive=TRUE)) stop("Could not create output directory.")
pkgload::load_all('.', quiet=TRUE)
local <- data.frame(g=c('a','b'),v=c(1,3))
sqlite <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
duck <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE), dbdir=':memory:')
inputs <- list(local=local, sqlite=dplyr::copy_to(sqlite,local,'scope_sqlite'), duckdb=dplyr::copy_to(duck,local,'scope_duckdb'))
read <- function() tryCatch(last_sent_queries(details=TRUE),error=function(cnd) class(cnd))
verbs <- list(summarize=summarize_with_margins, summarise=summarise_with_margins, expand=expand_with_margins, nest=nest_with_margins, nest_by=nest_by_with_margins, inspect=inspect_grouping)
checks <- 0L
for(vname in names(verbs)) for(bname in names(inputs)) for(audited in c(FALSE,TRUE)) {
  if(bname!='local' && vname %in% c('nest','nest_by')) next
  verb <- verbs[[vname]]; data <- inputs[[bname]]
  options(marginplyr.audit_sql=audited)
  baseline <- verb(data,.grouping=rollup(g))
  expected <- read()
  callback_count <- 0L
  factory <- function() {
    callback_count <<- callback_count+1L
    before <- read()
    # Recursive SQL/local/SQL ownership, option values deliberately not restored.
    options(marginplyr.audit_sql=!audited)
    inspect_grouping(inputs$sqlite,.grouping={
      inner_before <- read()
      options(marginplyr.audit_sql=audited)
      summarize_with_margins(inputs$local,total={
        local_before <- read()
        options(marginplyr.audit_sql=!audited)
        inspect_grouping(inputs$duckdb,.grouping=rollup(g))
        stopifnot(identical(read(),local_before))
        sum(v)
      },.grouping=rollup(g))
      stopifnot(identical(read(),inner_before))
      # A failed .data promise must not manufacture a new child record.
      failure <- tryCatch(inspect_grouping(stop('data promise failed')),error=identity)
      stopifnot(inherits(failure,'error'),identical(read(),inner_before))
      rollup(g)
    })
    stopifnot(identical(read(),before))
    rollup(g)
  }
  options(marginplyr.audit_sql=audited)
  actual <- verb(data,.grouping=factory())
  stopifnot(callback_count==1L,identical(read(),expected))
  if(audited) {
    held <- last_sent_queries(details=TRUE)
    altered <- held
    altered$sql[] <- 'caller mutation'
    if(nrow(altered)) {
      altered$conditions[[1L]] <- dplyr::tibble(classes=list('caller'),message='caller',phase='caller',handling='caller')
    }
    stopifnot(identical(last_sent_queries(details=TRUE),held))
    inspect_grouping(local)
    stopifnot(identical(held,expected))
  }
  checks <- checks+1L
}
# At top level, a failing outer .data promise retains the inner tracked record.
for(input in inputs) {
  options(marginplyr.audit_sql=TRUE)
  inner <- NULL
  caught <- tryCatch(inspect_grouping({
    expand_with_margins(input,.grouping=rollup(g)); inner <- read(); stop('input fail after inner')
  }),error=identity)
  stopifnot(inherits(caught,'error'),identical(read(),inner))
  checks <- checks+1L
}
DBI::dbDisconnect(sqlite)
DBI::dbDisconnect(duck,shutdown=TRUE)
cat('Lifetime matrix passed:',checks,'cases\n')
saveRDS(list(cases=checks,session=sessionInfo()),file.path(output_dir,'summary.rds'))
writeLines(capture.output(sessionInfo()),file.path(output_dir,'session-info.txt'))
