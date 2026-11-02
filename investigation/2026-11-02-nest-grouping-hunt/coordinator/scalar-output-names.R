# Dated scalar-name metamorphic probe; no package test or implementation.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
forms <- function(x) list(x, c(alias = x), I(x), matrix(x, 1L, 1L), array(x, c(1L, 1L, 1L)))
count <- 0L
for (source in c('local', 'dtplyr')) {
  for (size in c(0L, 3L)) {
    input <- tibble(g = c('b', 'a', 'b')[seq_len(size)], value = seq_len(size))
    q <- if (source == 'dtplyr') dtplyr::lazy_dt(input) else input
    for (keys in c(FALSE, TRUE)) {
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        for (key in forms('cells')) {
          for (id in forms('set')) {
            result <- collect(verb(q, .grouping = if (keys) rollup(g), .key = key, .id = id))
            plain <- collect(verb(q, .grouping = if (keys) rollup(g), .key = 'cells', .id = 'set'))
            stopifnot(identical(result, plain))
            count <- count + 1L
          }
        }
      }
    }
  }
}
cat(count, 'attributed-scalar output name comparisons matched plain-string controls\n')
