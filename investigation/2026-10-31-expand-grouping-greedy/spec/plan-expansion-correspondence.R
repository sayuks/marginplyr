# Dated investigation harness; run from the repository root.
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
columns <- c('a', '.data', 'x,=y', '日本語')
subsets <- unlist(lapply(0:4, function(n) combn(columns, n, simplify = FALSE)), recursive = FALSE)
selection <- function(cols) rlang::call2('all_of', cols)
capture <- function(expr) tryCatch(expr, error = identity)
checks <- refusals <- 0L
for (left in subsets) for (right in subsets) {
  raw <- list(left, right)
  dimensions <- unique(c(left, right))
  raw <- lapply(raw, function(set) dimensions[dimensions %in% set])
  specification <- rlang::eval_tidy(rlang::call2('grouping_sets', selection(left), selection(right)))
  for (policy in c('error', 'drop', 'keep')) for (size in c(0L, 2L)) {
    data <- tibble::as_tibble(stats::setNames(rep(list(seq_len(size)), 4L), columns), .name_repair = 'minimal')
    data$f <- rep(c(1L, 2L), length.out = size)
    data$source_row <- seq_len(size)
    for (fixed_mode in c('explicit', 'grouped')) {
      input <- if (fixed_mode == 'grouped') dplyr::group_by(data, f) else data
      by <- if (fixed_mode == 'explicit') rlang::quo(f) else rlang::quo(NULL)
      plan <- capture(inspect_grouping(input, .by = !!by, .grouping = specification, .duplicates = policy, .format = 'list'))
      result <- capture(expand_with_margins(input, .by = !!by, .grouping = specification, .duplicates = policy,
        .margin_label = NULL, .id = 'set_occurrence', .sort = 'none'))
      if (policy == 'error' && anyDuplicated(raw)) {
        if (!inherits(plan, 'marginplyr_error') || !inherits(result, 'marginplyr_error')) print(list(left=left,right=right,policy=policy,size=size,fixed=fixed_mode,plan=plan,result=result))
        stopifnot(inherits(plan, 'marginplyr_error'), inherits(result, 'marginplyr_error'))
        refusals <- refusals + 1L
        next
      }
      expected <- if (policy == 'drop') unique(raw) else raw
      if (inherits(plan, 'error') || inherits(result, 'error')) {
        print(list(left=left, right=right, policy=policy, fixed=fixed_mode, size=size, plan=plan, result=result))
        quit(status=1L)
      }
      stopifnot(identical(plan$included, expected), identical(plan$fixed, rep(list('f'), length(expected))),
        identical(plan$set_id, seq_along(expected)), nrow(result) == size * length(expected))
      ordered <- dplyr::arrange(result, set_occurrence, source_row)
      stopifnot(identical(ordered$set_occurrence, rep(seq_along(expected), each=size)),
        identical(ordered$source_row, rep(seq_len(size), length(expected))),
        identical(ordered$f, rep(data$f, length(expected))))
      for (column in columns) {
        values <- unlist(lapply(expected, function(set) if (column %in% dimensions && !column %in% set) rep(NA_integer_, size) else data[[column]]))
        if (!length(values)) values <- integer()
        stopifnot(identical(ordered[[column]], values))
      }
      expected_bits <- lapply(expected, function(set) if (length(dimensions)) stats::setNames(as.integer(!dimensions %in% set), dimensions) else integer())
      expected_ids <- vapply(expected_bits, function(bits) as.integer(sum(bits * 2^rev(seq_along(bits)-1L))), integer(1))
      stopifnot(identical(plan$grouping_bits, expected_bits), identical(plan$grouping_id, expected_ids))
      checks <- checks + 1L
    }
  }
}
cat('Successful independent plan/value/identity comparisons:', checks, '\n')
cat('Required duplicate-policy refusal pairs:', refusals, '\n')
cat('Total inspection/expansion call pairs:', checks + refusals, '\n')
print(sessionInfo())
