pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (nm in c('.SD', '.N', '.I', '.BY', '.GRP', '.NGRP', 'T', 'F', 'key', 'check.names', 'keep.rownames', 'stringsAsFactors', 'g', '.rows')) for (key in c('data', '.SD', '.rows')) for (id in list(NULL, 'set')) {
  x <- tibble(g = character(), v = integer())
  names(x)[1] <- nm
  local <- nest_by_with_margins(x, .key = key, .id = id)
  actual <- nest_by_with_margins(dtplyr::lazy_dt(x), .key = key, .id = id)
  stopifnot(nrow(actual) == 1L, identical(names(actual), names(local)), identical(lapply(actual[[key]], as.list), lapply(local[[key]], as.list)))
}
cat('PASS 84 empty reconstruction cases\n')
