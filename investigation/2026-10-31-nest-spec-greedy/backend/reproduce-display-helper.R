# Dated 2026-10-31; run from the audited checkout root.
# --assert requires correct public keys and intentionally fails on the baseline.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
assert_correct <- '--assert' %in% commandArgs(trailingOnly = TRUE)
source <- data.table::data.table(g = c('a', 'b'), row_id = 1:2)
before <- serialize(source, NULL)
root_env <- new.env(parent = globalenv())
root_env$source <- source
input <- evalq(dtplyr::lazy_dt(source), root_env)
stopifnot(identical(input$env, root_env))

# Construct before binding the function: this distinguishes late query lookup
# from capture, Grouping-plan expansion, and changes to source values.
lazy <- nest_with_margins(input, .grouping = grouping_set(g))
stopifnot(identical(collect(lazy)$g, c('a', 'b')))
reads <- 0L
root_env$min <- function(...) {
  reads <<- reads + 1L
  0L
}
source_control <- collect(input)
ordinary_control <- collect(summarise(input, n = n(), .by = g))
tidyr_control <- collect(tidyr::nest(input, data = row_id))
fixed_control <- collect(nest_with_margins(input, .by = g))
local_control <- nest_with_margins(source_control, .grouping = grouping_set(g))
stopifnot(identical(source_control$g, c('a', 'b')),
          identical(ordinary_control$g, c('a', 'b')),
          identical(tidyr_control$g, c('a', 'b')),
          identical(fixed_control$g, c('a', 'b')),
          identical(local_control$g, c('a', 'b')), reads == 0L)
wrong <- collect(lazy)
wrong_by <- nest_by_with_margins(input, .grouping = grouping_set(g))
cat('Root-local min binding; neither caller supplied a summary expression.\n')
cat('Lazy result keys: '); dput(wrong$g)
cat('Rowwise result keys: '); dput(wrong_by$g)
cat('min binding reads: ', reads, '\n', sep = '')
if (!assert_correct) stopifnot(
  identical(wrong$g, c(NA_character_, NA_character_)),
  identical(wrong_by$g, c(NA_character_, NA_character_)), reads == 4L)
stopifnot(identical(vapply(wrong$data, function(x) x$row_id[[1L]], integer(1)), 1:2),
          identical(vapply(wrong_by$data, function(x) x$row_id[[1L]], integer(1)), 1:2),
          identical(serialize(source, NULL), before))

# The exact same constructed query recovers when its root environment gives
# min the base binding. No caller source or captured specification changes.
root_env$min <- base::min
recovered <- collect(lazy)
stopifnot(identical(recovered$g, c('a', 'b')),
          identical(serialize(source, NULL), before))
cat('Rebinding min to base::min restored keys on the same lazy result.\n')
cat('Source, ordinary grouped dtplyr, tidyr nesting, fixed-key, and local controls passed.\n')
if (assert_correct) stopifnot(identical(wrong$g, c('a', 'b')),
                            identical(wrong_by$g, c('a', 'b')))
