Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."),quiet=TRUE)
invisible(loadNamespace("data.table"))
main <- function() {
  native <- utils::getS3method("head","data.table")
  restore <- function() registerS3method("head","data.table",native,envir=asNamespace("utils"))
  on.exit(restore())
  data <- data.table::data.table(region=c("north","south"),value=4:5)
  step <- dtplyr::lazy_dt(data)
  pristine <- serialize(data,NULL)
  invoked <- integer()
  changed_schema <- function(x,n=6L,...) {
    invoked <<- c(invoked,nrow(x))
    zero <- native(x,n=n,...)
    if (nrow(zero)==0L) data.table::set(zero,j="region",value=integer())
    zero
  }
  registerS3method("head","data.table",changed_schema,envir=asNamespace("utils"))
  stopifnot(identical(dplyr::collect(step)$region,c("north","south")),length(invoked)==0L)
  name_plan <- inspect_grouping(step,.grouping=grouping_set(region),.format="list")
  stopifnot(identical(name_plan$included,list("region")),length(invoked)==0L)
  typed <- tryCatch(inspect_grouping(step,.grouping=grouping_set(where(is.character)),.format="list"),error=identity)
  if ("--assert" %in% commandArgs(TRUE)) {
    faithful <- !inherits(typed,"condition") && identical(typed$included,list("region"))
    safe_refusal <- inherits(typed,"marginplyr_error")
    protected <- !any(invoked > 0L) && identical(serialize(data,NULL),pristine)
    cat("Faithful schema or safe refusal:",faithful || safe_refusal,"protected source:",protected,"\n")
    stopifnot((faithful || safe_refusal) && protected)
    return(invisible(NULL))
  }
  cat("Typed plan:",paste(typed$included[[1L]],collapse=","),"head inputs:",paste(invoked,collapse=","),"\n")
  stopifnot(identical(typed$included,list(character())),identical(data.table::copy(data)$region,c("north","south")))
  for (f in list(nest_with_margins,nest_by_with_margins)) {
    actual <- dplyr::collect(f(step,.grouping=grouping_set(where(is.character))))
    cat("Nest cells:",nrow(actual),"expected: 2\n")
    stopifnot(nrow(actual)==1L)
  }
  stopifnot(identical(serialize(data,NULL),pristine))
  restore()
  control <- inspect_grouping(step,.grouping=grouping_set(where(is.character)),.format="list")
  stopifnot(identical(control$included,list("region")))
  cat("Restored registered method: intended region recovered.\n")
  mutation <- function(x,n=6L,...) {
    if (nrow(x)>0L) data.table::set(x,j="region",value=rep("written",nrow(x)))
    native(x,n=n,...)
  }
  registerS3method("head","data.table",mutation,envir=asNamespace("utils"))
  inspect_grouping(step,.grouping=grouping_set(where(is.character)))
  cat("Original caller values after inspection:",paste(data$region,collapse=","),"\n")
  stopifnot(identical(data$region,c("written","written")))
  restore()
  stopifnot(identical(utils::getS3method("head","data.table"),native))
}
main()
