args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
d <- tibble(g = c('a', 'a', 'b'), x = c(1, 2, 30), y = c(2, 5, 9))
inner <- tibble(a = c(1, 1, 2), b = 1:3)
expressions <- alist(
  summarise(inner, z = sum(b), .by = a),
  summarise(inner, z = ncol(pick(everything())), .by = a),
  summarise(inner, z = cur_group_id(), .by = a),
  mutate(inner, z = cur_group_id(), .by = a),
  filter(inner, if_any(everything(), ~ .x > 1)),
  arrange(inner, desc(b)),
  group_by(inner, z = cur_group_id()),
  rowwise(inner, a),
  distinct(inner, a),
  select(inner, where(is.numeric)),
  rename_with(inner, toupper),
  relocate(inner, b),
  slice(inner, 1),
  slice_head(inner, n = 1),
  slice_tail(inner, n = 1),
  slice_min(inner, b, n = 1),
  slice_max(inner, b, n = 1),
  count(inner, a),
  add_count(inner, a),
  tally(inner),
  add_tally(inner),
  inner %>% group_by(a) %>% summarise(z = ncol(pick(everything()))),
  inner |> group_by(a) |> summarise(z = cur_group_id()),
  summarise(tibble(z = sum(pick(everything()))), out = z),
  mutate(tibble(z = sum(pick(everything()))), out = z)
)
cap <- function(e) tryCatch(withCallingHandlers(e, warning = function(c) invokeRestart('muffleWarning')), error = identity)
failures <- 0L
for (e in expressions) {
  dot <- rlang::new_quosure(rlang::call2('list', e), environment())
  expected <- cap(rlang::eval_tidy(rlang::expr(summarise(!!d, result = !!dot, .by = g))))
  actual <- cap(rlang::eval_tidy(rlang::expr(summarize_with_margins(!!d, result = !!dot, .grouping = grouping_set(g), .margin_label = NULL))))
  # dplyr itself expands an inner pick() against the outer mask. ADR 0019
  # assigns these two selections to the nested operation; use that contract.
  if (deparse1(e) %in% c(
    'summarise(inner, z = ncol(pick(everything())), .by = a)',
    'inner %>% group_by(a) %>% summarise(z = ncol(pick(everything())))'
  )) {
    nested <- summarise(inner, z = ncol(pick(everything())), .by = a)
    expected <- tibble(g = c('a', 'b'), result = list(nested, nested))
  }
  if (inherits(expected, 'error') && inherits(actual, 'error')) next
  if (!isTRUE(all.equal(expected, actual))) {
    failures <- failures + 1L
    cat('FAIL', deparse1(e), '\n'); print(expected); print(actual)
  }
}
cat('NESTED MASK CASES', length(expressions), 'FAILURES', failures, '\n')
if (failures) stop('Differential mismatch', call. = FALSE)
