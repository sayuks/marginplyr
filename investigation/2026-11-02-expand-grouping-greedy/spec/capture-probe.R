Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
x <- tibble::tibble(a = 1L, b = 'x', c = TRUE)
record <- function(label, expr, env) {
  cat('\nCASE ', label, '\n', sep = '')
  cat(paste(rlang::expr_deparse(expr), collapse = '\n'), '\n')
  y <- tryCatch(eval(expr, env), error = identity)
  if (inherits(y, 'error')) cat(class(y)[[1]], conditionMessage(y), '\n') else print(y)
  invisible(y)
}
for (kind in c('name', 'call', 'qualified-call')) {
  e <- rlang::env(globalenv(), reads = 0L)
  e$grouping_set <- function(...) {
    e$reads <- e$reads + 1L
    if (e$reads == 1L) 'a' else 'b'
  }
  head <- switch(kind, name = quote(grouping_set), call = quote((grouping_set)), `qualified-call` = quote(marginplyr::grouping_set))
  if (kind == 'qualified-call') e[['::']] <- function(...) e$grouping_set
  qhead <- rlang::new_quosure(head, e)
  expr <- rlang::expr(inspect_grouping(x, .grouping = grouping_sets((!!qhead)(a)), .format = 'list'))
  result <- record(kind, expr, globalenv())
  stopifnot(identical(result$included, list('a')), identical(e$reads, 1L))
  cat('reads:', e$reads, '\n')
}

for (n in c(0L, 1L, 30L, 31L, 32L, 40L)) {
  columns <- if (n) paste0('d', seq_len(n)) else character()
  input <- tibble::as_tibble(stats::setNames(rep(list('x'), n), columns), .rows = 1L)
  input$fixed <- 'partition'
  spec <- if (n) rollup(all_of(columns)) else grouping_set()
  plan <- inspect_grouping(input, .by = fixed, .grouping = spec, .format = 'list')
  stopifnot(identical(plan$set_id, seq_len(n + 1L)))
  stopifnot(all(lengths(plan$grouping_bits) == n))
  expected <- if (n > 31L) rep(NA_integer_, n + 1L) else as.integer(2^seq.int(0L, n) - 1)
  stopifnot(identical(plan$grouping_id, expected))
  actual <- tryCatch(summarize_with_margins(input, id = grouping_id(), .by = fixed, .grouping = spec, .id = 'set', .label = NULL), error = identity)
  if (n > 31L) {
    stopifnot(inherits(actual, 'marginplyr_error'), grepl('31', conditionMessage(actual)))
  } else {
    actual <- actual[order(actual$set), ]
    stopifnot(identical(actual$id, expected))
  }
  cat('identifier dimensions:', n, 'passed\n')
}
cat('capture and identifier boundary assertions passed\n')
