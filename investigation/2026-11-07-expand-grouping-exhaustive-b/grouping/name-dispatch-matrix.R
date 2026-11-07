args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
x <- data.frame(a = c("A", "B"), b = 1:2)
class(x) <- c("selector_frame", "data.frame")
source_before <- serialize(x, NULL)
busy <- FALSE
typed_calls <- 0L
names.selector_frame <- function(x) {
  ordinary <- attr(x, "names")
  if (busy) return(ordinary)
  busy <<- TRUE
  on.exit({ busy <<- FALSE })
  data <- tryCatch(tidyselect::peek_data(), error = function(e) NULL)
  if (!is.null(data)) typed_calls <<- typed_calls + 1L
  if (!is.null(data) && is.character(data[[1L]])) rev(ordinary) else ordinary
}
name_alias <- base::names
spellings <- c(
  "names(tidyselect::peek_data())[1L]",
  "base::names(tidyselect::peek_data())[1L]",
  "(names)(tidyselect::peek_data())[1L]",
  "name_alias(tidyselect::peek_data())[1L]",
  "names(tidyselect::peek_data())[[1L]]",
  "all_of(names(tidyselect::peek_data())[1L])",
  "c(names(tidyselect::peek_data())[1L])",
  "names(tidyselect::peek_data())[1L:1L]"
)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
records <- list()
for (spelling in spellings) {
  expr <- rlang::parse_expr(spelling)
  typed_calls <- 0L
  direct <- tidyselect::eval_select(expr, x)
  stopifnot(identical(direct, c(b=2L)), typed_calls > 0L)
  for (position in c(constructors, ".by")) {
    for (consumer in c("inspect", "expand")) {
      run <- function(opaque = FALSE, literal = FALSE) {
        selection <- if (literal) quote(b) else if(opaque) rlang::call2("identity", expr) else expr
        by <- if (position == ".by") selection else quote(a)
        spec_expr <- if (position == ".by") quote(grouping_set(a)) else rlang::call2(position, rlang::new_quosure(selection, globalenv()))
        spec <- eval(spec_expr, globalenv())
        fun <- if(consumer=="inspect") inspect_grouping else expand_with_margins
        rlang::eval_tidy(rlang::new_quosure(rlang::call2(fun, x, .by=by, .grouping=spec), globalenv()))
      }
      typed_calls <- 0L
      literal <- run(literal=TRUE)
      literal_calls <- typed_calls
      typed_calls <- 0L
      typed <- run(opaque=TRUE)
      control_calls <- typed_calls
      typed_calls <- 0L
      actual <- tryCatch(run(), error=identity)
      actual_calls <- typed_calls
      stopifnot(identical(literal, typed), inherits(actual,"marginplyr_error"),
                grepl("both",conditionMessage(actual),fixed=TRUE),
                literal_calls == 0L, control_calls > 0L, actual_calls == 0L,
                identical(source_before,serialize(x,NULL)))
      records[[length(records)+1L]] <- data.frame(spelling, position, consumer,
        literal_calls, control_calls, actual_calls, false_overlap=TRUE,
        source_unchanged=TRUE)
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, if (length(args) > 1L) args[[2L]] else file.path(tempdir(), "name-dispatch-matrix.csv"), row.names=FALSE)
print(table(results$consumer, results$false_overlap))
cat("All",nrow(results),"false-overlap cases matched literal/opaque controls; typed callbacks skipped only on failing path.\n")
rm(names.selector_frame)
stopifnot(identical(tidyselect::eval_select(rlang::parse_expr(spellings[[1]]),x),c(a=1L)))
print(sessionInfo())
