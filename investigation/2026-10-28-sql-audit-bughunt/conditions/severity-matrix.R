args <- commandArgs(TRUE)
main <- function(root = ".") {
  pkgload::load_all(root, quiet = TRUE)
  library(testthat)
  diagnostics_path <- tempfile("severity-diagnostics-", fileext = ".log")
  diagnostics <- file(diagnostics_path, open = "wt")
  completed <- FALSE
  sink(diagnostics, type = "message")
  on.exit({
    sink(type = "message")
    close(diagnostics)
    if (!completed) cat(readLines(diagnostics_path, warn = FALSE), sep = "\n", file = stderr())
    unlink(diagnostics_path)
  }, add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(con,data.frame(g="a",v=1),"severity_source")
  perms <- function(xs) {
    if (!length(xs)) return(list(character()))
    unlist(lapply(seq_along(xs), function(i) lapply(perms(xs[-i]),function(p)c(xs[i],p))),recursive=FALSE)
  }
  severities <- c("error","warning","interrupt","message")
  classes <- unlist(lapply(seq_along(severities),function(n) unlist(lapply(combn(severities,n,simplify=FALSE),perms),recursive=FALSE)),recursive=FALSE)
  cases <- expand.grid(class=seq_along(classes),form=c("bare","warning","stop"),warn=c(-1L,0L,1L,2L),handler=c("observe","muffle","restart"),stringsAsFactors=FALSE)
  check <- function() summarize_with_margins(remote,total=sum(.data$v),.grouping=rollup(dplyr::all_of("g")),.check_margin_label=TRUE)
  run <- function(audit,cls,form,warn,handling) {
    options(marginplyr.audit_sql=audit,warn=warn)
    leaf <- structure(list(message="compound driver severity",call=NULL),class=c(cls,"condition"))
    seen <- list();cleanup <- character()
    testthat::local_mocked_bindings(collect=function(...) {
      on.exit({ cleanup <<- c(cleanup,"collect exit") })
      withRestarts({
        if (form == "bare") signalCondition(leaf) else if (form == "warning") warning(leaf) else stop(leaf)
      },resume_driver=function()NULL)
      data.frame(g=0)
    },.package="dplyr")
    value <- tryCatch(withRestarts(withCallingHandlers(check(),condition=function(cnd) {
      seen[[length(seen)+1L]] <<- list(class=class(cnd),message=conditionMessage(cnd))
      if (handling == "muffle" && inherits(cnd,"warning") && !is.null(findRestart("muffleWarning"))) invokeRestart("muffleWarning")
      if (handling == "restart" && !is.null(findRestart("resume_driver"))) invokeRestart("resume_driver")
      if (form == "stop" && !any(c("error","interrupt") %in% cls)) invokeRestart("leave_driver",cnd)
    }),leave_driver=identity),error=identity,interrupt=identity)
    out <- list(class=class(value),message=if(inherits(value,"condition"))conditionMessage(value) else "query",seen=seen,cleanup=cleanup)
    if (audit) {
      rows <- last_sent_queries(FALSE,TRUE)
      if (nrow(rows) != 1L) stop("Internal row count mismatch")
      summaries <- rows$conditions[[1L]]
      observed_leaf <- sum(vapply(summaries$classes,identical,logical(1),class(leaf)))
      expected_leaf <- as.integer(any(c("warning","error","interrupt") %in% cls))
      if (observed_leaf != expected_leaf) stop("Condition-summary leaf count mismatch")
      if (nrow(summaries) && any(summaries$phase != "invocation" | summaries$handling != "propagated")) stop("Condition-summary boundary mismatch")
      if (inherits(value,"tbl_sql") && !identical(rows$status,"returned")) stop("Returned mismatch")
      if(inherits(value,"condition") &&
         xor(inherits(value,"error"),inherits(value,"interrupt"))) {
        expected <- if(inherits(value,"error"))"errored" else if(inherits(value,"interrupt"))"interrupted" else NA_character_
        if(!is.na(expected) && !identical(rows$status,expected)) stop(paste("Terminal mismatch",paste(cls,collapse=","),form,handling,expected,rows$status))
      }
    }
    out
  }
  for(i in seq_len(nrow(cases))) {
    x<-cases[i,]
    off<-run(FALSE,classes[[x$class]],x$form,x$warn,x$handler)
    on<-run(TRUE,classes[[x$class]],x$form,x$warn,x$handler)
    if(!identical(off,on)) {print(x);print(classes[[x$class]]);print(off);print(on);stop("off/on mismatch")}
    if(i %% 256L == 0L)cat("completed",i,"of",nrow(cases),"\n")
  }
  completed <- TRUE
  cat("PASS:",nrow(cases),"severity pairs across",length(classes),"ordered severity sets\n")
}
main(if (length(args)) args[[1L]] else ".")
