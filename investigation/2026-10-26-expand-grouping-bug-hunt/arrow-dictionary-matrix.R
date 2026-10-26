# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages({library(dplyr); library(arrow)})
assert_mode <- '--assert' %in% commandArgs(trailingOnly = TRUE)
values <- list(integer = c(1L, 2L), double = c(1.25, 2.5), logical = c(TRUE, FALSE), date = as.Date(c('2026-01-01', '2026-01-02')), timestamp = as.POSIXct(c('2026-01-01', '2026-01-02'), tz = 'UTC'), text = c('x', 'y'))
failures <- list(); total <- 0L; controls <- 0L
for (kind in names(values)) {
  encoded <- call_function('dictionary_encode', Array$create(values[[kind]]))
  source <- Table$create(g = encoded, payload = 1:2)
  collected <- suppressWarnings(collect(source))
  stopifnot(nrow(collected) == 2L)
  for (shape in c('table', 'empty', 'dataset')) {
    input <- switch(shape, table = source, empty = head(source, 0L), dataset = InMemoryDataset$create(source))
    rows <- if (shape == 'empty') 0L else 2L
    name_plan <- suppressWarnings(inspect_grouping(input, .grouping = rollup(g), .format = 'list'))
    typed_plan <- suppressWarnings(inspect_grouping(input, .grouping = rollup(where(is.factor)), .format = 'list'))
    stopifnot(identical(name_plan, typed_plan))
    fixed <- suppressWarnings(collect(expand_with_margins(input, .by = g, .grouping = rollup(payload), .margin_label = NULL, .id = 'sid')))
    payload <- suppressWarnings(collect(expand_with_margins(input, .grouping = rollup(payload), .margin_label = NULL, .id = 'sid')))
    stopifnot(nrow(fixed) == 2L * rows, nrow(payload) == 2L * rows)
    # Scalar$create preserves an existing dictionary Array's physical type.
    # The schema proxy used by marginplyr instead represents it as an R factor.
    missing <- Scalar$create(call_function('dictionary_encode', Array$create(values[[kind]][NA_integer_])))
    ordinary_missing <- suppressWarnings(collect(union_all(input, mutate(input, g = !!missing))))
    ordinary_text <- suppressWarnings(collect(union_all(mutate(input, g = as.character(g)), mutate(input, g = 'Total'))))
    stopifnot(nrow(ordinary_missing) == rows * 2L, nrow(ordinary_text) == rows * 2L)
    if (rows > 0L) stopifnot(sum(is.na(ordinary_missing$g)) == rows)
    controls <- controls + 5L
    for (plan in c('set', 'rollup')) for (label in list(NULL, 'Total')) {
      specification <- if (plan == 'set') grouping_set(g) else rollup(g)
      got <- suppressWarnings(tryCatch(collect(expand_with_margins(input, .grouping = specification, .margin_label = label, .id = 'sid')), error = identity))
      total <- total + 1L
      if (inherits(got, 'error')) {
        failures[[length(failures) + 1L]] <- list(kind = kind, shape = shape, plan = plan, label = label, message = conditionMessage(got))
      } else {
        stopifnot(nrow(got) == rows * if (plan == 'set') 1L else 2L)
      }
    }
  }
}
cat('dimension cases:', total, '; passing controls:', controls, '; dimension failures:', length(failures), '\n')
print(as.data.frame(table(vapply(failures, `[[`, character(1), 'kind'))))
if (length(failures)) cat('first failure:', failures[[1L]]$message, '\n')

if (assert_mode) stopifnot(length(failures) == 0L)
