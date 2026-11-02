# Dated label-validation and helper-ownership controls, not a package test.
Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(quiet=TRUE)
assert_correct <- "--assert" %in% commandArgs(trailingOnly=TRUE)
cat("SHA:",system2("git",c("rev-parse","HEAD"),stdout=TRUE),"\n")
observe <- function(x) tryCatch(force(x),error=identity)
records <- list()
for(helper in c("as.character","sum")) for(verb in c("nest","nest_by","expand","summarize")) {
  root <- new.env(parent=globalenv())
  root$source <- data.table::data.table(g="Total",row_id=1L)
  before <- serialize(root$source,NULL)
  lazy <- evalq(dtplyr::lazy_dt(source),root)
  run <- function(x) switch(verb,
    nest=nest_with_margins(x,.grouping=rollup(g),.check_margin_label=TRUE),
    nest_by=nest_by_with_margins(x,.grouping=rollup(g),.check_margin_label=TRUE),
    expand=expand_with_margins(x,.grouping=rollup(g),.check_margin_label=TRUE),
    summarize=summarize_with_margins(x,count=dplyr::n(),.grouping=rollup(g),.check_margin_label=TRUE))
  normal <- observe(run(lazy))
  stopifnot(inherits(normal,"marginplyr_error"),grepl("already present",conditionMessage(normal)))
  reads <- 0L
  root[[helper]] <- if(helper=="as.character") function(x,...) {reads <<- reads+1L; rep("MASK",base::length(x))} else function(...) {reads <<- reads+1L; 0L}
  native <- dplyr::collect(lazy)
  grouped <- dplyr::collect(dplyr::summarise(dplyr::group_by(lazy,g),n=dplyr::n()))
  stopifnot(identical(native$g,"Total"),identical(grouped$n,1L),reads==0L)
  actual <- observe(run(lazy)); collision_reads <- reads
  cat(helper,verb,": masked validation reads",collision_reads,"; result",paste(class(actual),collapse="/"),"\n")
  stopifnot(identical(serialize(root$source,NULL),before))
  # Explicit user expressions retain their own ordinary lexical lookup.
  explicit <- if(helper=="as.character") {
    dplyr::collect(dplyr::mutate(lazy,cast=as.character(g)))$cast
  } else dplyr::collect(dplyr::summarise(lazy,total=sum(row_id)))$total
  stopifnot(reads>collision_reads,identical(explicit,if(helper=="as.character") "MASK" else 0L))
  records[[length(records)+1L]] <- data.frame(helper,verb,collision_reads,refused=inherits(actual,"marginplyr_error"),stringsAsFactors=FALSE)
}
results <- do.call(rbind,records)
utils::write.csv(results,"investigation/2026-11-02-nest-grouping-hunt/backend/collision-helper-results.csv",row.names=FALSE)
print(results,row.names=FALSE)
if(assert_correct) stopifnot(all(results$refused))
