# Dated exploration: run from the investigated checkout root.
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(102934)
counts <- c(graphs = 0L, backend_control_refused = 0L, inspection_refused = 0L,
            comparisons = 0L, equal = 0L, mismatches = 0L, premature_reads = 0L,
            source_mutations = 0L)
read_events <- integer()
probe <- function(value) {
  read_events <<- c(read_events, length(value))
  value > 0L
}
normalize <- function(result) {
  if (inherits(result, 'dtplyr_step')) result <- collect(result)
  result <- ungroup(result)
  rows <- lapply(seq_len(nrow(result)), function(i) {
    cell <- as_tibble(result$data[[i]], .name_repair = 'minimal')
    if (ncol(cell)) cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
    list(keys = as.list(result[i, setdiff(names(result), 'data'), drop = FALSE]),
         names = names(cell), rows = nrow(cell), cells = as.list(cell))
  })
  sort(vapply(rows, function(x) paste(as.character(serialize(x, NULL)), collapse = ''), character(1)))
}
observe <- function(expr) tryCatch(force(expr), error = identity)
post <- list(identity = identity, filter = function(x) filter(x, probe(v)),
             arrange = function(x) arrange(x, desc(v), g),
             rename = function(x) rename(x, payload = extra),
             relocate = function(x) relocate(x, v),
             group = function(x) group_by(x, g),
             cache = function(x) compute(x),
             copy = function(x) data.table::copy(x))
# Derive graphs from immutable roots; repeated references stress independent
# branch ownership. The explicitly collected input supplies the nesting oracle.
for (size in c(0L, 1L, 5L)) {
  for (kind in c('ordinary', 'special', 'factor')) {
    source <- data.table::data.table(
      g = rep(c('x', 'y', NA_character_, 'x', 'y'), length.out = size),
      v = rep(c(1L, -2L, 3L, 4L, 0L), length.out = size),
      extra = seq_len(size)
    )
    if (kind == 'special') data.table::setnames(source, 'extra', '.SD')
    if (kind == 'factor') source$g <- addNA(factor(source$g, levels = c('z', 'y', 'x')))
    before <- as.list(data.table::copy(source))
    root <- dtplyr::lazy_dt(source, immutable = TRUE)
    base <- list(root = root, filtered = filter(root, probe(v)),
                 ordered = arrange(root, desc(v)), cached = compute(root),
                 cached_filter = compute(filter(root, probe(v))),
                 arithmetic = mutate(root, w = v + 1L),
                 copied = mutate(root, other_g = g))
    for (left_name in names(base)) {
      left <- base[[left_name]]
      graph <- list(plain = left)
      for (right_name in c('root', 'filtered', 'cached', 'cached_filter')) {
        right <- base[[right_name]]
        # Differing schemas would exercise external set compatibility rather
        # than a nesting boundary, so constrain set operations to equal names.
        for (operation in c('left_join', 'inner_join', 'full_join', 'semi_join', 'anti_join')) {
          graph[[paste(operation, right_name, sep = '-')]] <- observe(
            get(operation, asNamespace('dplyr'))(left, right, by = c('g', 'v'), suffix = c('', '_right'))
          )
        }
        if (identical(names(left), names(right))) {
          graph[[paste('union_all', right_name, sep = '-')]] <- observe(union_all(left, right))
          graph[[paste('intersect', right_name, sep = '-')]] <- observe(intersect(left, right))
          graph[[paste('setdiff', right_name, sep = '-')]] <- observe(setdiff(left, right))
        }
      }
      # Sample independently after expanding a multi-input graph; include a
      # second cache before a downstream operation and another repeated union.
      for (graph_name in names(graph)) {
        q0 <- graph[[graph_name]]
        if (inherits(q0, 'error')) next
        for (post_name in c('identity', 'filter', 'arrange', 'relocate', 'group', 'cache')) {
          q <- observe(post[[post_name]](q0))
          if (inherits(q, 'error')) next
          if ((counts[['graphs']] %% 3L) == 0L) q <- observe(arrange(compute(q), v))
          if (inherits(q, 'error')) next
          counts[['graphs']] <- counts[['graphs']] + 1L
          label <- paste(size, kind, left_name, graph_name, post_name)
          expected_input <- observe(collect(q))
          if (inherits(expected_input, 'error')) {
            counts[['backend_control_refused']] <- counts[['backend_control_refused']] + 1L
            next
          }
          if (!identical(as.list(source), before)) {
            counts[['source_mutations']] <- counts[['source_mutations']] + 1L
            cat('UPSTREAM SOURCE MUTATION', label, '\n')
            next
          }
          read_events <- integer()
          inspection <- observe(inspect_grouping(q, .grouping = rollup(where(is.character))))
          typed_expected <- observe(inspect_grouping(expected_input, .grouping = rollup(where(is.character))))
          if (length(read_events)) {
            counts[['premature_reads']] <- counts[['premature_reads']] + 1L
            cat('INSPECTION READ', label, paste(read_events, collapse = ','), '\n')
          }
          if (inherits(inspection, 'error')) {
            counts[['inspection_refused']] <- counts[['inspection_refused']] + 1L
            if (!inherits(inspection, 'marginplyr_error')) cat('UNTYPED INSPECTION', label, conditionMessage(inspection), '\n')
            next
          }
          if (!identical(inspection, typed_expected)) {
            counts[['mismatches']] <- counts[['mismatches']] + 1L
            cat('INSPECTION DIFF', label, '\n'); print(inspection); print(typed_expected)
          }
          # One sampled variant per graph keeps this finite while covering
          # every verb, keep policy, sort and fixed-key shape over the run.
          index <- counts[['graphs']]
          verb <- if (index %% 2L) nest_with_margins else nest_by_with_margins
          keep <- (index %% 4L) < 2L
          sort <- c('none', 'first', 'last')[[1L + index %% 3L]]
          run <- function(z) verb(z, .grouping = rollup(g), .keep = keep,
                                  .margin_label = NULL, .id = 'set', .sort = sort)
          expected <- observe(normalize(run(expected_input)))
          read_events <- integer()
          actual <- observe(run(q))
          if (identical(verb, nest_with_margins) && length(read_events)) {
            counts[['premature_reads']] <- counts[['premature_reads']] + 1L
            cat('NEST CONSTRUCTION READ', label, paste(read_events, collapse = ','), '\n')
          }
          actual <- observe(normalize(actual))
          counts[['comparisons']] <- counts[['comparisons']] + 1L
          if (identical(actual, expected)) counts[['equal']] <- counts[['equal']] + 1L else {
            counts[['mismatches']] <- counts[['mismatches']] + 1L
            cat('NEST DIFF', label, '\n')
            if (inherits(actual, 'error')) cat(conditionMessage(actual), '\n')
            else print(all.equal(actual, expected))
          }
          if (!identical(as.list(source), before)) {
            counts[['source_mutations']] <- counts[['source_mutations']] + 1L
            cat('MARGIN SOURCE MUTATION', label, '\n')
          }
        }
      }
    }
  }
}
print(counts)
