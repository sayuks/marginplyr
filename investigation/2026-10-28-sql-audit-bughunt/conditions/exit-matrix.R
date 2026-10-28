args <- commandArgs(TRUE)
main <- function(root = ".") {
  pkgload::load_all(root, quiet = TRUE)
  library(testthat)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "exit_condition_source")
  original_collect <- dplyr::collect
  check <- function() summarize_with_margins(remote,total = sum(.data$v),.grouping=rollup(dplyr::all_of("g")),.check_margin_label=TRUE)
  leaf <- structure(list(message="resumable driver interrupt", call=NULL),class=c("fixture_interrupt","interrupt","condition"))
  err <- errorCondition("terminal driver error",class="fixture_error")
  run <- function(audit,phase,ending,signal) {
    options(marginplyr.audit_sql=audit)
    seen <- list(); cleaned <- character(); hit <- FALSE; outcome <- NULL
    recover <- function() withRestarts({ if (signal == "native") rlang::interrupt() else signalCondition(leaf) }, resume_driver=function() NULL)
    finish <- function() switch(ending,returned=NULL,escaped=invokeRestart("leave_driver","escaped"),errored=stop(err),interrupted=stop(leaf))
    testthat::local_mocked_bindings(collect=function(x,...) {
      on.exit({ cleaned <<- c(cleaned,"first") }, add=TRUE)
      if (phase == "body") {
        recover()
        finish()
      } else {
        on.exit({recover();finish()},add=TRUE)
      }
      on.exit({ cleaned <<- c(cleaned,"last") },add=TRUE)
      original_collect(x,...)
    },.package="dplyr")
    value <- tryCatch(withRestarts(withCallingHandlers(check(),interrupt=function(cnd) {
      seen[[length(seen)+1L]] <<- cnd
      if (!hit) {hit <<- TRUE; invokeRestart("resume_driver")}
    }),leave_driver=identity),error=identity,interrupt=identity)
    out <- list(class=class(value), message=if(inherits(value,"condition"))conditionMessage(value) else if (is.character(value)) value else "query",seen=lapply(seen,class),cleaned=cleaned)
    if (audit) {
      rows <- last_sent_queries(FALSE,TRUE)
      expected <- ending
      if (ending == "escaped") expected <- "unknown"
      if (!identical(rows$status,expected)) stop(paste(phase,ending,signal,"status",rows$status,"expected",expected))
    }
    out
  }
  cases <- expand.grid(phase=c("body","onexit"),ending=c("returned","escaped","errored","interrupted"),signal=c("bare","native"),stringsAsFactors=FALSE)
  for (i in seq_len(nrow(cases))) {
    a <- cases[i,]
    off <- run(FALSE,a$phase,a$ending,a$signal)
    on <- run(TRUE,a$phase,a$ending,a$signal)
    if(!identical(off,on)) {print(a); print(off);print(on);stop("off/on mismatch")}
  }
  cat("PASS:",nrow(cases),"cleanup/resumption pairs\n")
}
main(if (length(args)) args[[1L]] else ".")
