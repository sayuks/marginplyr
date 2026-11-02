# Dated finite probe of generated Margin-label helper ownership.
Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(quiet=TRUE)
out <- "investigation/2026-11-02-nest-grouping-hunt/backend"
cat("SHA:",system2("git",c("rev-parse","HEAD"),stdout=TRUE),"\n")
canonical <- function(x) {
  x <- tibble::as_tibble(x)
  x$data <- lapply(x$data,tibble::as_tibble)
  x
}
records <- list()
for(helper in c("as.character","rep","length")) for(type in c("character","integer","factor")) for(size in c(0L,1L,3L)) for(keep in c(FALSE,TRUE)) for(labelled in c(FALSE,TRUE)) for(roll in c(FALSE,TRUE)) for(verb in c("nest","nest_by")) {
  keys <- switch(type,character=c("a","b","a"),integer=c(1L,2L,1L),factor=factor(c("a","b","a"),levels=c("b","a","unused")))
  input <- tibble::tibble(g=keys[seq_len(size)],row_id=seq_len(size))
  root <- new.env(parent=globalenv()); root$source <- data.table::as.data.table(input)
  before <- serialize(root$source,NULL); lazy <- evalq(dtplyr::lazy_dt(source),root)
  run <- function(x) {
    fn <- if(verb=="nest") nest_with_margins else nest_by_with_margins
    fn(x,.grouping=if(roll) rollup(g) else grouping_set(g),.margin_label=if(labelled) "Total" else NULL,.keep=keep,.id="set",.sort="last")
  }
  expected <- canonical(run(input))
  baseline <- canonical(dplyr::collect(run(lazy)))
  stopifnot(isTRUE(all.equal(baseline,expected)))
  reads <- 0L
  root[[helper]] <- switch(helper,
    as.character=function(x,...) {reads <<- reads+1L; rep("MASK",base::length(x))},
    rep=function(x,...) {reads <<- reads+1L; base::rep("MASK",...)},
    length=function(x) {reads <<- reads+1L; 0L})
  warnings <- character()
  actual <- tryCatch(withCallingHandlers(canonical(dplyr::collect(run(lazy))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
  status <- if(inherits(actual,"error")) "error" else if(isTRUE(all.equal(actual,expected))) "match" else "mismatch"
  if(inherits(actual,"error")) message <- conditionMessage(actual) else if(status=="match") message <- "" else message <- paste(all.equal(actual,expected),collapse="; ")
  stopifnot(identical(serialize(root$source,NULL),before))
  records[[length(records)+1L]] <- data.frame(helper,type,size,keep,labelled,roll,verb,status,reads,warnings=length(warnings),message,stringsAsFactors=FALSE)
}
results <- do.call(rbind,records)
utils::write.csv(results,file.path(out,"label-helper-results.csv"),row.names=FALSE)
print(with(results,table(helper,status))); cat("Total:",nrow(results),"\n")
print(aggregate(reads~helper+status,results,sum)); print(aggregate(warnings~helper+status,results,sum))
stopifnot(any(results$status=="mismatch"))
