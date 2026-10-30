# Dated investigation of immutable dtplyr row-selection and cache graphs.
# Run from the investigated repository root.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart('muffleWarning')
  }), error = identity)
  list(value = value, warnings = warnings)
}
normalize <- function(result) {
  if (inherits(result, 'dtplyr_step')) result <- collect(result)
  result <- ungroup(result)
  rows <- lapply(seq_len(nrow(result)), function(i) {
    cell <- as_tibble(result$data[[i]], .name_repair = 'minimal')
    if (ncol(cell)) cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
    list(key = as.list(result[i, setdiff(names(result), 'data'), drop = FALSE]),
         columns = names(cell), rows = nrow(cell), values = as.list(cell))
  })
  sort(vapply(rows, function(x) paste(as.character(serialize(x, NULL)), collapse = ''), character(1)))
}
ops <- list(
  distinct_all = function(x) distinct(x),
  distinct_keys = function(x) distinct(x, g, .keep_all = TRUE),
  head = function(x) slice_head(x, n = 3L),
  tail = function(x) slice_tail(x, n = 3L),
  min = function(x) slice_min(x, v, n = 2L, with_ties = TRUE),
  max = function(x) slice_max(x, v, n = 2L, with_ties = FALSE),
  positive = function(x) filter(x, v > 0L),
  indexed = function(x) slice(x, 1L, 3L),
  ranked = function(x) filter(x, min_rank(v) <= 2L),
  grouped_head = function(x) ungroup(slice_head(group_by(x, g), n = 1L)),
  grouped_distinct = function(x) ungroup(distinct(group_by(x, g), .keep_all = TRUE)),
  duplicated_filter = function(x) filter(x, !duplicated(g)),
  lag_filter = function(x) filter(x, v > lag(v, default = 0L))
)
posts <- list(
  plain = identity,
  cache = function(x) compute(x),
  rename_cache = function(x) rename(compute(x), original = aux),
  double_cache = function(x) arrange(compute(relocate(compute(x), aux)), desc(v)),
  grouped_cache = function(x) group_by(compute(x), g),
  self_union = function(x) {z <- compute(x); union_all(z, z)},
  self_join = function(x) {z <- compute(x); inner_join(z, z, by = c('g', 'v'), suffix = c('', '_right'))},
  shared_chain = function(x) {z <- compute(x); a <- compute(rename(z, copy = aux)); union_all(arrange(a, v), arrange(a, desc(v)))}
)
counts <- c(graphs = 0L, upstream_errors = 0L, upstream_warnings = 0L,
            typed_matches = 0L, typed_refusals = 0L, typed_mismatches = 0L,
            nest_matches = 0L, nest_refusals = 0L, nest_mismatches = 0L,
            source_mutations = 0L, unexpected_warnings = 0L)
results <- list()
for (size in c(0L, 1L, 7L)) for (kind in c('ordinary', 'special', 'factor')) {
  source <- data.table::data.table(g = rep(c('a', 'b', NA_character_, 'a', 'b', 'a', 'a'), length.out = size),
                                  v = rep(c(1L, -2L, 3L, 1L, 0L, 5L, NA_integer_), length.out = size),
                                  aux = seq_len(size))
  if (kind == 'special') data.table::setnames(source, 'aux', '.SD')
  if (kind == 'factor') source$g <- addNA(factor(source$g, levels = c('z', 'b', 'a')))
  original <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  for (op_name in names(ops)) for (post_name in names(posts)) {
    label <- paste(size, kind, op_name, post_name, sep = '/')
    built <- observe(posts[[post_name]](ops[[op_name]](root)))
    if (inherits(built$value, 'error')) next
    q <- built$value
    counts[['graphs']] <- counts[['graphs']] + 1L
    control <- observe(collect(q))
    if (inherits(control$value, 'error')) {
      counts[['upstream_errors']] <- counts[['upstream_errors']] + 1L
      next
    }
    counts[['upstream_warnings']] <- counts[['upstream_warnings']] + length(control$warnings)
    local_source <- control$value
    typed <- observe(inspect_grouping(q, .by = where(is.character), .format = 'list'))
    expected <- observe(inspect_grouping(local_source, .by = where(is.character), .format = 'list'))
    typed_status <- if (inherits(typed$value, 'marginplyr_error')) 'refusals' else if (identical(typed$value, expected$value)) 'matches' else 'mismatches'
    counts[[paste0('typed_', typed_status)]] <- counts[[paste0('typed_', typed_status)]] + 1L
    if (typed_status == 'mismatches') {cat('TYPED DIFF', label, '\n'); print(typed$value); print(expected$value)}
    if (length(typed$warnings)) {counts[['unexpected_warnings']] <- counts[['unexpected_warnings']] + length(typed$warnings); cat('TYPED WARN', label, paste(typed$warnings, collapse = '\n'), '\n')}
    for (verb_name in c('nest', 'nest_by')) for (keep in c(FALSE, TRUE)) {
      fun <- if (verb_name == 'nest') nest_with_margins else nest_by_with_margins
      run <- function(x) normalize(fun(x, .grouping = rollup(g), .keep = keep, .margin_label = NULL, .id = 'set', .sort = 'last'))
      got <- observe(run(q))
      want <- observe(run(local_source))
      status <- if (inherits(got$value, 'marginplyr_error')) 'refusals' else if (identical(got$value, want$value)) 'matches' else 'mismatches'
      counts[[paste0('nest_', status)]] <- counts[[paste0('nest_', status)]] + 1L
      if (status == 'mismatches') {cat('NEST DIFF', label, verb_name, keep, '\n'); if (inherits(got$value, 'error')) cat(conditionMessage(got$value), '\n') else print(all.equal(got$value, want$value))}
      if (length(got$warnings)) {counts[['unexpected_warnings']] <- counts[['unexpected_warnings']] + length(got$warnings); cat('NEST WARN', label, verb_name, keep, paste(got$warnings, collapse = '\n'), '\n')}
      results[[length(results) + 1L]] <- data.frame(label, verb = verb_name, keep, status)
    }
    if (!identical(serialize(source, NULL), original)) {counts[['source_mutations']] <- counts[['source_mutations']] + 1L; cat('SOURCE MUTATION', label, '\n')}
  }
}
print(counts)
write.csv(bind_rows(results), 'investigation/2026-10-30-nest-spec-greedy/backend/row-selection-results.csv', row.names = FALSE)
stopifnot(counts[['typed_mismatches']] == 0L, counts[['nest_mismatches']] == 0L,
          counts[['source_mutations']] == 0L, counts[['unexpected_warnings']] == 0L)
