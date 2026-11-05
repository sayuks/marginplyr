# Replay four desired-behaviour failures against an isolated immutable source.
repo <- normalizePath('.')
sha <- '0403c9150394cbae21b59f34c75791d0873b8451'
evidence <- file.path(repo,'investigation/2026-11-05-summary-grouping-share-greedy')
workspace <- tempfile('marginplyr-independent-')
dir.create(workspace)
source_dir <- file.path(workspace,'source')
dir.create(source_dir)
archive <- file.path(workspace,'source.tar')
stopifnot(system2('git',c('archive','--format=tar',paste0('--output=',shQuote(archive)),sha))==0L)
utils::untar(archive,exdir=source_dir)
cases <- c('summary/glue-namespace.R','spec/pattern-dispatch.R','share/accessor-matrix.R','share/namespace-matrix.R')
rows <- list()
for (target in cases) {
 result <- processx::run(file.path(R.home('bin'),'Rscript'),c(file.path(evidence,target),'--assert'),wd=source_dir,error_on_status=FALSE,timeout=120000)
 log <- file.path(evidence,'coordinator',paste0(gsub('/','-',sub('[.]R$','',target)),'.log'))
 writeLines(c(paste('SHA:',sha),paste('Command: Rscript',target,'--assert'),paste('Exit:',result$status),result$stdout,result$stderr),log)
 stopifnot(result$status==1L,grepl('is not TRUE',result$stderr,fixed=TRUE))
 rows[[length(rows)+1L]] <- data.frame(script=target,status=result$status,signal='desired-behaviour-assertion-failed',sha)
 cat(target,': confirmed desired-behaviour failure (exit 1)\n')
}
write.csv(do.call(rbind,rows),file.path(evidence,'coordinator/replay-results.csv'),row.names=FALSE)
unlink(workspace,recursive=TRUE)
