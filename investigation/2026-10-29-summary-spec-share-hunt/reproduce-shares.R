# Reproduce at the investigation commit; no namespace or repository mutation.
args <- commandArgs(TRUE)
root <- if (any(grepl('^--root=', args))) sub('^--root=', '', args[grepl('^--root=', args)][1L]) else '.'
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
mode <- if ('--assert' %in% args) 'assert' else 'report'
findings <- list()

# F1: A share's internal dtplyr grouping cannot read a fixed key called `.`.
data <- tibble(. = c('f', 'f'), g = c('a', 'b'), x = c(1, 3))
source <- dtplyr::lazy_dt(data)
expected <- c(0.25, 0.75, 1)
base <- collect(summarise(source, value = sum(x), .by = all_of('.')))
stopifnot(identical(base$value, 4))
plain <- collect(summarize_with_margins(source, value = sum(x), .by = all_of('.'), .grouping = rollup(g)))
stopifnot(identical(plain$value, c(1, 3, 4)))
for (kind in c('parent', 'total')) {
  request <- rlang::call2(paste0('share_of_', kind), quote(value))
  local <- summarize_with_margins(data, value = sum(x), share = !!request, .by = all_of('.'), .grouping = rollup(g))
  stopifnot(identical(local$share, expected))
  actual <- tryCatch(summarize_with_margins(source, value = sum(x), share = !!request, .by = all_of('.'), .grouping = rollup(g)), error = identity)
  cat('F1', kind, ':', if (inherits(actual, 'error')) conditionMessage(actual) else paste(collect(actual)$share, collapse = ','), '\n')
  findings[[paste0('F1_', kind)]] <- list(expected = expected, actual = actual)
}

# F2: One generated across output incorrectly inherits other outputs' aliases.
data <- tibble(g = c('a', 'b'), x = c(1, 3))
for (kind in c('parent', 'total')) {
  for (form in c('columns', 'functions')) {
    ordinary <- if (form == 'columns') {
      rlang::expr(across(c(x, prior), sum, .names = 'tot_{.col}'))
    } else {
      rlang::expr(across(x, list(plain = sum, alias = ~sum(.x) + prior), .names = '{.col}_{.fn}'))
    }
    source_name <- if (form == 'columns') 'tot_x' else 'x_plain'
    request <- rlang::call2(paste0('share_of_', kind), rlang::sym(source_name))
    plain <- summarize_with_margins(data, prior = sum(x), !!ordinary, .grouping = rollup(g))
    stopifnot(identical(plain[[source_name]], c(1, 3, 4)))
    actual <- tryCatch(summarize_with_margins(data, prior = sum(x), !!ordinary, share = !!request, .grouping = rollup(g)), error = identity)
    cat('F2', kind, form, ':', if (inherits(actual, 'error')) conditionMessage(actual) else paste(actual$share, collapse = ','), '\n')
    findings[[paste0('F2_', kind, '_', form)]] <- list(expected = expected, actual = actual)
  }
}
control <- summarize_with_margins(data, prior = sum(x), across(x, sum, .names = 'tot_{.col}'), across(prior, sum, .names = 'tot_{.col}'), share = share_of_total(tot_x), .grouping = rollup(g))
stopifnot(identical(control$share, expected))
if (mode == 'assert') {
 selected <- names(findings)
 if ('--only=F2' %in% args) selected <- selected[startsWith(selected, 'F2')]
 if ('--only=F1' %in% args) selected <- selected[startsWith(selected, 'F1')]
 for (name in selected) {
   actual <- findings[[name]]$actual
   if (inherits(actual, 'error')) stop(name, ' violates the expected successful call: ', conditionMessage(actual), call. = FALSE)
   stopifnot(isTRUE(all.equal(collect(actual)$share, findings[[name]]$expected)))
 }
}
