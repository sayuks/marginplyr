args<-commandArgs(TRUE); Sys.setenv(NOT_CRAN='true')
r<-testthat::test_local(args[[1]],filter='^(sqlite|margin-order)',reporter='summary',stop_on_failure=FALSE)
f<-as.data.frame(r);write.csv(f[c('file','test','nb','failed','error','warning','skipped')],'review/B-direct/regressions.csv',row.names=FALSE)
cat('TESTS',nrow(f),'ASSERTIONS',sum(f$nb),'FAILURES',sum(f$failed),'ERRORS',sum(f$error),'SKIPPED',sum(f$skipped),'\n')
