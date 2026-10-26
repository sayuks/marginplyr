# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
repo_root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(repo_root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(a=c('x','y'),b=c(1,1))
promise <- quote(if(is.numeric(tidyselect::peek_data()$a)) 'a' else 'b')
for(n in c('inspect_fixed','inspect_dimension','nest_fixed','nest_by_fixed','nest_dimension','nest_by_dimension')) {
  f <- switch(n,
    inspect_fixed=function(a) inspect_grouping(x,.by=base::c(a)),
    inspect_dimension=function(a) inspect_grouping(x,.grouping=grouping_set(base::c(a))),
    nest_fixed=function(a) nest_with_margins(x,.by=base::c(a)),
    nest_by_fixed=function(a) nest_by_with_margins(x,.by=base::c(a)),
    nest_dimension=function(a) nest_with_margins(x,.grouping=grouping_set(base::c(a)),.margin_label=NULL),
    nest_by_dimension=function(a) nest_by_with_margins(x,.grouping=grouping_set(base::c(a)),.margin_label=NULL)
  )
  result <- eval(bquote(f(.(promise))))
  cat(n, ':\n'); print(result)
  if (startsWith(n, 'inspect')) {
    column <- if (n == 'inspect_fixed') 'fixed' else 'included'
    stopifnot(identical(result[[column]], '(a)'))
  } else {
    result <- dplyr::collect(result)
    stopifnot(nrow(result) == 2L)
  }
}
cat('backend inspection, fresh promise per call\n')
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
sources<-list(local=x,dtplyr=dtplyr::lazy_dt(x),arrow=arrow::Table$create(x),duckdb=dplyr::copy_to(con,x,'x',temporary=TRUE))
for(n in names(sources)) {
  dat<-sources[[n]]
  f<-function(a) inspect_grouping(dat,.by=base::c(a))$fixed
  observed <- eval(bquote(f(.(promise))))
  stopifnot(identical(observed, '(a)'))
  cat(n, ':', observed, '\n')
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('active-binding early duplicate control\n')
makeActiveBinding('a',function() if(is.numeric(tidyselect::peek_data()$a)) 'a' else 'b',globalenv())
print(inspect_grouping(x,.grouping=grouping_sets(tidyselect::all_of(a),a)))
err<-tryCatch(inspect_grouping(x,.grouping=grouping_sets(base::c(a),a)),error=identity)
cat(class(err)[[1L]],':',conditionMessage(err),'\n')

stopifnot(inherits(err, 'marginplyr_error'))
cat('Observed all public-route and duplicate-refusal symptoms: PASS\n')

# A fresh default promise per call also reaches every constructor consumer.
input <- tibble::tibble(cols = c("x", "y"), key = c(1, 1))
numeric_columns <- function() {
  data <- tidyselect::peek_data()
  names(data)[vapply(data, is.numeric, logical(1))]
}
constructor_plan <- function(data, constructor, cols = numeric_columns()) {
  expr <- rlang::call2(constructor, quote(base::c(cols)))
  specification <- rlang::eval_bare(expr, environment())
  inspect_grouping(data, .grouping = specification,
                   .duplicates = "drop", .format = "list")
}
constructor_controls <- 0L
for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
  for (data in list(input, dtplyr::lazy_dt(input))) {
    plan <- constructor_plan(data, constructor)
    stopifnot(identical(unique(unlist(plan$included, use.names = FALSE)), c("cols", "key")))
    constructor_controls <- constructor_controls + 1L
  }
}
cat("Observed default-promise misselection in all constructor consumers:",
    constructor_controls, "PASS\n")
