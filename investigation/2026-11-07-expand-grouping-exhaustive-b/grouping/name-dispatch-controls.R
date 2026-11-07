args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
x <- data.frame(a=c("A","B"), b=1:2)
class(x) <- c("selector_frame", "data.frame")
source_before <- serialize(x,NULL)
busy <- FALSE
names.selector_frame <- function(x) {
  ordinary <- attr(x, "names")
  if (busy) return(ordinary)
  busy <<- TRUE
  on.exit({ busy <<- FALSE })
  data <- tryCatch(tidyselect::peek_data(), error=function(e) NULL)
  if (!is.null(data) && is.character(data[[1L]])) rev(ordinary) else ordinary
}
expr <- quote(names(tidyselect::peek_data())[1L])
expected <- tidyselect::eval_select(expr,x)
stopifnot(identical(expected,c(b=2L)), identical(names(x),c("a","b")))
stopifnot(identical(dplyr::group_vars(dplyr::group_by(x,a)),"a"))
force_typed <- inspect_grouping(x,.by=a,.grouping=grouping_set(identity(names(tidyselect::peek_data())[1L])),.format="list")
stopifnot(identical(force_typed$fixed,list("a")), identical(force_typed$included,list("b")))
for (consume in c("inspect", "expand", "summary", "nest", "nest_by")) {
  run <- switch(consume,
    inspect=function(g) inspect_grouping(x,.by=a,.grouping=g,.format="list"),
    expand=function(g) expand_with_margins(x,.by=a,.grouping=g),
    summary=function(g) summarize_with_margins(x,n=dplyr::n(),.by=a,.grouping=g),
    nest=function(g) nest_with_margins(x,.by=a,.grouping=g),
    nest_by=function(g) nest_by_with_margins(x,.by=a,.grouping=g))
  literal <- run(grouping_set(b))
  typed <- run(grouping_set(identity(names(tidyselect::peek_data())[1L])))
  actual <- tryCatch(run(grouping_set(names(tidyselect::peek_data())[1L])),error=identity)
  stopifnot(identical(literal,typed), inherits(actual,"marginplyr_error"), grepl("both",conditionMessage(actual),fixed=TRUE))
  cat(consume,": correct literal/typed controls; false overlap ->",gsub("\n"," | ",conditionMessage(actual)),"\n")
}
stopifnot(identical(source_before,serialize(x,NULL)))
rm(names.selector_frame)
# Removing the method makes the selector choose a, so the original overlap
# refusal is required. The method is load-bearing, not a setup fixture artifact.
stopifnot(identical(tidyselect::eval_select(expr,x),c(a=1L)))
cat("All five consumers reproduced; source unchanged; removed-method control selected a.\n")
