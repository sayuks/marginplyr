# Child records are process-local; fork inherits the pre-fork snapshot.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if (dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if (!dir.create(output_dir, recursive = TRUE)) stop("Could not create output directory.")
repo <- normalizePath(".", mustWork = TRUE)
pkgload::load_all(repo, quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
check_n <- 0L
check <- function(x) { check_n <<- check_n + 1L; stopifnot(x) }
remote <- dbplyr::lazy_frame(g='a',v=1,con=dbplyr::simulate_postgres())
invisible(summarize_with_margins(remote,total=sum(v),.grouping=rollup(g)))
parent <- last_sent_queries(details=TRUE)
parent_raw <- serialize(parent,NULL)
worker <- function(repo, fresh=TRUE) {
  if (fresh) pkgload::load_all(repo, quiet=TRUE)
  options(marginplyr.audit_sql=TRUE)
  before <- tryCatch(marginplyr::last_sent_queries(details=TRUE), error=function(e) list(classes=class(e), message=conditionMessage(e)))
  remote <- dbplyr::lazy_frame(g='x', v=3, con=dbplyr::simulate_postgres())
  invisible(marginplyr::summarize_with_margins(remote,total=sum(v),.grouping=marginplyr::rollup(g)))
  sql <- marginplyr::last_sent_queries(details=TRUE)
  invisible(marginplyr::inspect_grouping(data.frame(g='x'),.grouping=marginplyr::rollup(g)))
  empty <- marginplyr::last_sent_queries(details=TRUE)
  list(before=before, sql=sql, empty=empty, pid=Sys.getpid())
}
process_kinds <- c('fork', 'callr', 'PSOCK')
observations <- list()
for (kind in process_kinds) {
  workers <- switch(kind,
    fork=parallel::mclapply(1:2,function(i) worker(repo,FALSE),mc.cores=2L),
    callr=lapply(1:2,function(i) callr::r(worker,args=list(repo=repo),libpath=.libPaths(),spinner=FALSE)),
    PSOCK={cl <- parallel::makePSOCKcluster(2L); ans <- tryCatch(parallel::clusterCall(cl,worker,repo),finally=parallel::stopCluster(cl)); ans})
  for(w in workers) {
    check(w$pid != Sys.getpid())
    check(nrow(w$sql)==1L && w$sql$purpose=='result' && w$sql$status=='prepared')
    check(nrow(w$empty)==0L && identical(names(w$empty),names(parent)))
    if (kind=='fork') {
      check(identical(w$before,parent))
    } else {
      check(identical(w$before$classes[[1L]], 'marginplyr_error'))
      check(grepl('No tracked call has run in this session.', w$before$message, fixed=TRUE))
    }
  }
  check(identical(serialize(last_sent_queries(details=TRUE),NULL),parent_raw))
  observations[[kind]] <- workers
  cat('PASS process boundary:',kind,'; 2 child records; parent unchanged\n')
}
cat('PASS total assertions:', check_n, '\n')
saveRDS(list(assertions = check_n, process_kinds = process_kinds,
             workers_per_kind = 2L, observations = observations,
             session = sessionInfo()),
        file.path(output_dir, 'summary.rds'))
writeLines(capture.output(sessionInfo()), file.path(output_dir, 'session-info.txt'))
