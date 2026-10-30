pkgload::load_all('.', quiet=TRUE)
d <- tibble::tibble(a=1L,b='x')
f <- function() {
  mask_reads <- 0L
  all_of <- function(...) {
    mask_reads <<- mask_reads + 1L
    data <- tidyselect::peek_data()
    names(data)[vapply(data,is.character,logical(1))]
  }
  invisible(tryCatch(all_of(),error=identity))
  stopifnot(mask_reads==1L)
  mask_reads <- 0L
  exprs <- list(quote(all_of('b')),quote((all_of)('b')),
                quote(((all_of))('b')),quote(dplyr::all_of('b')),
                quote((dplyr::all_of)('b')))
  checks <- 0L
  for (expr in exprs) {
    q <- rlang::new_quosure(expr,environment())
    control <- names(tidyselect::eval_select(q,d))
    fixed <- rlang::inject(inspect_grouping(d,.by=!!q,.format='list'))
    variable <- rlang::inject(inspect_grouping(d,.grouping=grouping_set(!!q),.format='list'))
    stopifnot(identical(control,'b'),identical(fixed$fixed,list('b')),
              identical(variable$included,list('b')))
    checks <- checks+3L
  }
  stopifnot(mask_reads==0L)
  cat('Helper-head comparisons=',checks,'; all selected b; caller masking function read zero times after a one-read calibration.\n',sep='')
}
f()
