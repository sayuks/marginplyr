# Independent finite Grouping-family oracle; run from the repository root.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
set.seed(2026110202)
columns <- c('a', 'b,=c', '日本語', '.data')
leaf <- function(cols) list(kind = 'selection', columns = cols)
node <- function(kind, args = list()) list(kind = kind, args = args)
random_leaf <- function(nonempty = FALSE) {
  n <- sample.int(length(columns) + as.integer(!nonempty), 1L) - as.integer(!nonempty)
  leaf(sample(columns, n))
}
random_node <- function(depth) {
  if (depth == 0L) return(random_leaf())
  kind <- sample(c('set', 'sets', 'rollup', 'cube', 'product'), 1L)
  n <- sample.int(4L, 1L) - 1L
  if (kind %in% c('sets', 'rollup', 'cube')) n <- max(1L, n)
  args <- lapply(seq_len(n), function(i) {
    if (kind == 'set') return(random_leaf())
    if (kind %in% c('rollup', 'cube')) {
      if (sample(c(TRUE, FALSE), 1L)) return(random_leaf(TRUE))
      return(node('set', list(random_leaf(TRUE))))
    }
    if (sample(c(TRUE, FALSE), 1L)) random_leaf() else random_node(depth - 1L)
  })
  node(kind, args)
}
family <- function(tree) {
  if (tree$kind == 'selection') return(list(tree$columns))
  families <- lapply(tree$args, family)
  if (tree$kind == 'set') return(list(unique(unlist(families, use.names = FALSE))))
  if (tree$kind == 'sets') return(unlist(families, recursive = FALSE))
  if (tree$kind == 'product') {
    acc <- list(character())
    for (f in families) acc <- unlist(lapply(acc, function(a) lapply(f, function(b) unique(c(a, b)))), recursive = FALSE)
    return(acc)
  }
  units <- unlist(lapply(seq_along(tree$args), function(i) {
    if (tree$args[[i]]$kind == 'selection') lapply(tree$args[[i]]$columns, function(x) x) else families[[i]]
  }), recursive = FALSE)
  if (tree$kind == 'rollup') return(lapply(rev(seq.int(0L, length(units))), function(n) unique(unlist(units[seq_len(n)], use.names = FALSE))))
  indices <- lapply(seq_len(2^length(units)) - 1L, function(mask) which(as.logical(intToBits(mask)[seq_along(units)])))
  # SQL cube order: cardinality descending, lexicographic subset positions.
  keys <- vapply(indices, function(i) paste(sprintf('%02d', i), collapse = ','), character(1))
  indices <- indices[order(-lengths(indices), keys)]
  lapply(indices, function(i) unique(unlist(units[i], use.names = FALSE)))
}
syntax <- function(tree) {
  if (tree$kind == 'selection') return(rlang::call2('all_of', tree$columns))
  constructor <- switch(tree$kind, set = 'grouping_set', sets = 'grouping_sets', rollup = 'rollup', cube = 'cube', product = 'grouping_spec')
  rlang::call2(constructor, !!!lapply(tree$args, syntax))
}
normalize <- function(sets, policy) {
  sets <- lapply(sets, function(s) if (is.null(s)) character() else s)
  dimensions <- unique(unlist(sets, use.names = FALSE))
  if (is.null(dimensions)) dimensions <- character()
  included <- lapply(sets, function(s) dimensions[dimensions %in% s])
  duplicate <- duplicated(vapply(included, function(s) paste(as.integer(dimensions %in% s), collapse = ''), character(1)))
  if (policy == 'error' && any(duplicate)) return('duplicate')
  if (policy == 'drop') included <- included[!duplicate]
  bits <- lapply(included, function(s) if (!length(dimensions)) integer() else stats::setNames(as.integer(!dimensions %in% s), dimensions))
  ids <- vapply(bits, function(b) as.integer(sum(b * 2^rev(seq_along(b) - 1L))), integer(1))
  list(included = included, omitted = lapply(included, function(s) dimensions[!dimensions %in% s]), grouping_bits = bits, grouping_id = ids)
}
fixtures <- list(node('set'), node('product'))
while (length(fixtures) < 500L) {
  t <- random_node(3L)
  if (t$kind == 'selection') t <- node('set', list(t))
  if (length(family(t)) <= 64L) fixtures[[length(fixtures) + 1L]] <- t
}
data <- tibble::as_tibble(stats::setNames(rep(list('v'), 4L), columns))
rows <- list()
for (i in seq_along(fixtures)) {
  tree <- fixtures[[i]]
  for (fixed in c(FALSE, TRUE)) {
    input <- if (fixed) dplyr::mutate(data, fixed = 'partition') else data
    by <- if (fixed) quote(fixed) else NULL
    for (policy in c('error', 'drop', 'keep')) {
      expected <- normalize(family(tree), policy)
      call <- rlang::call2('inspect_grouping', quote(input), .by = by, .grouping = syntax(tree), .duplicates = policy, .format = 'list')
      actual <- tryCatch(eval(call), error = identity)
      if (identical(expected, 'duplicate')) {
        status <- if (inherits(actual, 'marginplyr_error') && grepl('Duplicate grouping sets', conditionMessage(actual))) 'expected-duplicate-refusal' else 'candidate-duplicate'
      } else if (inherits(actual, 'error')) status <- 'candidate-error' else {
        expected$fixed <- rep(list(if (fixed) 'fixed' else character()), length(expected$included))
        expected$set_id <- seq_along(expected$included)
        status <- if (all(vapply(names(expected), function(name) identical(actual[[name]], expected[[name]]), logical(1)))) 'match' else 'candidate-values'
      }
      rows[[length(rows) + 1L]] <- data.frame(fixture = i, fixed = fixed, policy = policy, status = status,
        source = paste(rlang::expr_deparse(syntax(tree)), collapse = '\\n'),
        observed = if (inherits(actual, 'error')) conditionMessage(actual) else paste(actual$grouping_id, collapse = ','))
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, 'investigation/2026-11-02-expand-grouping-greedy/spec/family-results.csv', row.names = FALSE)
print(table(results$status))
print(utils::head(results[grepl('candidate', results$status), ], 12L), row.names = FALSE)
cat('Executed plan comparisons:', nrow(results), '\n')
stopifnot(!any(grepl('candidate', results$status)))
