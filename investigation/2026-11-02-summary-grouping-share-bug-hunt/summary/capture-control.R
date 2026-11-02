# Isolate promise capture from Margin planning and private grouping keys.
data <- data.frame(x = 1:3)
capture_argument <- function(cols) rlang::enquo(cols)
probe <- function(...) {
  stopifnot(identical(..1, "x"))
  dots <- list(ellipsis = capture_argument(...), dot1 = capture_argument(..1))
  for (name in names(dots)) {
    captured <- dots[[name]]
    cat(name, "captured expression:", deparse(rlang::quo_get_expr(captured)), "\n")
    result <- tidyselect::eval_select(captured, data)
    stopifnot(identical(result, c(x = 1L)))
    print(result)
  }
  for (expr in list(quote(...), quote(..1))) {
    cat("Quoted expression:", deparse(expr), "\n")
    print(tryCatch(tidyselect::eval_select(expr, data, env = environment()),
                   error = conditionMessage))
  }
}
probe("x")
