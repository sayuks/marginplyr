args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
x <- data.frame(a = c("A", "B"), b = c(1L, 1L))
class(x) <- c("selector_frame", "data.frame")
busy <- FALSE
names.selector_frame <- function(x) {
  ordinary <- attr(x, "names")
  if (busy) return(ordinary)
  busy <<- TRUE
  on.exit(busy <<- FALSE)
  data <- tryCatch(tidyselect::peek_data(), error=function(e) NULL)
  if (!is.null(data) && is.character(data[[1L]])) rev(ordinary) else ordinary
}
stopifnot(identical(tidyselect::eval_select(quote(names(tidyselect::peek_data())[1L]),x),c(b=2L)))
actual <- inspect_grouping(x,.by=names(tidyselect::peek_data())[1L],.format="list")
expected <- inspect_grouping(x,.by=identity(names(tidyselect::peek_data())[1L]),.format="list")
stopifnot(identical(actual$fixed,list("a")),identical(expected$fixed,list("b")))
wrong <- summarize_with_margins(x,n=dplyr::n(),.by=names(tidyselect::peek_data())[1L])
right <- summarize_with_margins(x,n=dplyr::n(),.by=identity(names(tidyselect::peek_data())[1L]))
stopifnot(identical(wrong$n,c(1L,1L)),identical(right$n,2L))
print(actual$fixed); print(expected$fixed); print(wrong); print(right)
for (nest in list(nest_with_margins,nest_by_with_margins)) {
  wrong <- nest(x,.by=names(tidyselect::peek_data())[1L])
  right <- nest(x,.by=identity(names(tidyselect::peek_data())[1L]))
  stopifnot(nrow(wrong)==2L, nrow(right)==1L)
}
cat("Fixed-key selection silently changed summary and both nested grains.\n")
