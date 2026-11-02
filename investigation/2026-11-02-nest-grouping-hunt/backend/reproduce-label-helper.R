# Dated public-seam reproducer. --assert requires the intended result.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet=TRUE)
args <- commandArgs(trailingOnly=TRUE)
assert_correct <- "--assert" %in% args
helper <- if (length(setdiff(args,"--assert"))) setdiff(args,"--assert")[[1L]] else "as.character"
stopifnot(helper %in% c("as.character","rep","length"))
cat("SHA:",system2("git",c("rev-parse","HEAD"),stdout=TRUE),"\n")
root <- new.env(parent=globalenv())
root$source <- data.table::data.table(g="a",row_id=1L)
before <- serialize(root$source,NULL)
source <- evalq(dtplyr::lazy_dt(source),root)
spec <- if(helper=="as.character") grouping_set(g) else rollup(g)
query <- nest_with_margins(source,.grouping=spec,.id="set",.keep=TRUE)
cat("Unmasked lazy query:\n"); dplyr::show_query(query)
reads <- 0L
root[[helper]] <- switch(helper,
  as.character=function(x,...) {reads <<- reads+1L; rep("MASK",base::length(x))},
  rep=function(x,...) {reads <<- reads+1L; base::rep("MASK",...)},
  length=function(x) {reads <<- reads+1L; 0L})
# These controls put no helper expression into the source's evaluation graph.
stopifnot(identical(dplyr::collect(source)$g,"a"))
counted <- dplyr::collect(dplyr::summarise(dplyr::group_by(source,g),n=dplyr::n()))
stopifnot(identical(counted$g,"a"),identical(counted$n,1L))
upstream <- dplyr::collect(tidyr::nest(dplyr::group_by(source,g),data=row_id))
stopifnot(identical(upstream$g,"a"),identical(upstream$data[[1L]]$row_id,1L),reads==0L)
expected <- if(helper=="as.character") "a" else c("a","Total")
actual <- tryCatch(suppressWarnings(dplyr::collect(query)),error=identity)
cat("Masked previously built lazy result; helper reads:",reads,"\n"); print(actual)
stopifnot(identical(serialize(root$source,NULL),before))
root[[helper]] <- get(helper,envir=baseenv())
restored <- dplyr::collect(query)
stopifnot(identical(restored$g,expected))
cat("Restoring only the helper binding made the same query's keys:",paste(restored$g,collapse=", "),"\n")
if(assert_correct) stopifnot(!inherits(actual,"error"),identical(actual$g,expected))
