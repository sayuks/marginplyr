args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all(quiet = TRUE)
data <- data.frame(a = c('u', 'u'), b = c('x', 'y'), value = c(1, 3))
constructors <- c('grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')
failures <- character()
check <- function(ok, text) {
  if (!isTRUE(ok)) failures <<- c(failures, text)
}
probe <- function(parent, nested, actual_spec = FALSE, ordinary = FALSE,
                  qualified = FALSE, summarize = FALSE) {
  reads <- 0L
  env <- rlang::env(globalenv())
  shadow <- function(...) {
    reads <<- reads + 1L
    if (actual_spec) return(marginplyr::grouping_set(a))
    if (reads == 1L) 'a' else 'b'
  }
  env[[nested]] <- shadow
  env$ordinary <- shadow
  child <- if (qualified) quote(tidyselect::all_of('a')) else {
    rlang::call2(if (ordinary) 'ordinary' else nested)
  }
  spec <- eval(rlang::call2(parent, child, .ns = 'marginplyr'), env)
  if (summarize) {
    result <- marginplyr::summarize_with_margins(
      data, total = sum(value), .grouping = spec, .margin_label = NULL
    )
  } else {
    result <- marginplyr::inspect_grouping(data, .grouping = spec)
  }
  list(reads = reads, result = result)
}
for (parent in constructors) {
  ordinary <- probe(parent, 'rollup', ordinary = TRUE)
  stopifnot(ordinary$reads == 1L, ordinary$result$included[[1L]] == '(a)')
  qualified <- probe(parent, 'rollup', qualified = TRUE)
  stopifnot(qualified$reads == 0L, qualified$result$included[[1L]] == '(a)')
  for (nested in constructors) {
    if (parent != 'grouping_set') {
      actual <- probe(parent, nested, actual_spec = TRUE)
      stopifnot(actual$reads == 1L, actual$result$included[[1L]] == '(a)')
    }
    observed <- probe(parent, nested)
    summary <- probe(parent, nested, summarize = TRUE)
    label <- paste(parent, nested, sep = '/')
    cat(label, ': inspect reads=', observed$reads,
        ' included=', observed$result$included[[1L]],
        '; summarize reads=', summary$reads,
        ' names=', paste(names(summary$result), collapse = ','),
        ' rows=', nrow(summary$result), '\n', sep = '')
    check(observed$reads == 1L, paste(label, 'inspection reads selection once'))
    check(observed$result$included[[1L]] == '(a)',
          paste(label, 'inspection retains first selection'))
    check(summary$reads == 1L, paste(label, 'summary reads selection once'))
    check(identical(names(summary$result), c('a', 'total')),
          paste(label, 'summary groups the first selection'))
    expected_rows <- if (parent %in% c('rollup', 'cube')) 2L else 1L
    check(nrow(summary$result) == expected_rows,
          paste(label, 'summary produces correct grouping grain'))
  }
}
cat('Normal controls passed. Desired-behavior failures:', length(failures), '\n')
if (length(failures)) stop(paste(failures, collapse = '\n'), call. = FALSE)
