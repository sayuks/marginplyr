# Independent denominator matching with typed and compound keys, 2026-10-29.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(291043)
keys <- list(
  numeric = c(0, -0, NA_real_, NaN, 1, 1 + 1e-14),
  logical = c(TRUE, FALSE, NA, TRUE, FALSE, NA),
  integer = c(0L, -1L, NA_integer_, 1L, 0L, 2L),
  character = c('a', 'A', NA, 'a\nb', '', 'a'),
  factor = factor(c('a', 'b', NA, 'a', 'b', 'a'), levels = c('a', 'b', 'unused')),
  ordered = ordered(c('a', 'b', NA, 'a', 'b', 'a'), levels = c('a', 'b', 'unused')),
  factor_na_level = factor(c('a', NA, 'a', NA, 'b', 'b'), exclude = NULL),
  Date = as.Date(c(0, 1, NA, 0, 1, 2), origin = '1970-01-01'),
  POSIXct = as.POSIXct(c(0, 1e-6, NA, 0, 1e-6, 2), origin = '1970-01-01', tz = 'Asia/Tokyo'),
  difftime = as.difftime(c(0, 1, NA, 0, 1, 2), units = 'hours'),
  integer64 = bit64::as.integer64(c('9007199254740992', '9007199254740993', NA, '9007199254740992', '9007199254740993', '0')),
  complex = c(0i, 1 + 1i, NA_complex_, 0i, 1 + 1i, 2i),
  list = list(1, 'a', NULL, 1, 'a', c(1, 2)),
  list_raw = list(as.raw(1), as.raw(2), NULL, as.raw(1), as.raw(2), raw()),
  matrix = matrix(c(1, 2, NA, 1, 2, 3, 0, 0, NA, 0, 0, 1), ncol = 2),
  frame = data.frame(a = c(1, 2, NA, 1, 2, 3), b = c('a', 'b', NA, 'a', 'b', 'c'))
)
plans <- list(rollup(g, h), rollup(c(g, h)), rollup(g, g, h))
counts <- c(attempts = 0L, results = 0L, ratios = 0L, ordinary_refusals = 0L,
            share_refusals = 0L, mismatches = 0L)
signals <- list()
check <- function(result, plan, output, source, parent) {
  stopifnot(is.double(result[[output]]))
  for (row in seq_len(nrow(result))) {
    occurrence <- result$set[[row]]
    included <- plan$included[[occurrence]]
    targets <- if (parent) which(seq_len(nrow(plan)) > occurrence &
                                  lengths(plan$included) < length(included)) else
      which(lengths(plan$included) == 0L)
    target <- if (!length(targets) || (!parent && occurrence %in% targets)) NA_integer_ else targets[[1L]]
    expected <- 1
    if (!is.na(target)) {
      matches <- result$set == target
      for (name in c(plan$fixed[[target]], plan$included[[target]])) {
        matches <- matches & vctrs::vec_equal(result[[name]],
          vctrs::vec_slice(result[[name]], row), na_equal = TRUE)
      }
      denominator <- result[[source]][matches]
      stopifnot(length(denominator) == 1L)
      numerator <- result[[source]][[row]]
      expected <- if (is.na(numerator) || is.na(denominator) || denominator == 0) NA_real_ else
        as.double(numerator) / as.double(denominator)
    }
    stopifnot(isTRUE(all.equal(result[[output]][[row]], expected, tolerance = 1e-12)))
    counts[['ratios']] <<- counts[['ratios']] + 1L
  }
}
for (key_name in names(keys)) {
  d <- tibble(g = keys[[key_name]], h = c('x', NA, 'x', 'y', 'y', NA),
              fixed = keys[[key_name]], x = sample(c(-3, -1, 0, 1, 4, NA_real_)))
  for (backend in c('local', 'dtplyr')) for (p in seq_along(plans)) for (fixed in c(FALSE, TRUE)) {
    counts[['attempts']] <- counts[['attempts']] + 1L
    by <- if (fixed) quote(fixed) else NULL
    input <- if (backend == 'local') d else dtplyr::lazy_dt(d)
    plan <- inspect_grouping(d, .grouping = plans[[p]], .by = !!by,
                            .duplicates = 'keep', .format = 'list')
    arguments <- list(.data = input, .grouping = plans[[p]], .by = by)
    plain <- tryCatch(collect(summarize_with_margins(input, value = sum(x, na.rm = TRUE),
      .grouping = plans[[p]], .by = !!by, .duplicates = 'keep', .margin_label = NULL,
      .id = 'set')), error = identity)
    if (inherits(plain, 'error')) {
      counts[['ordinary_refusals']] <- counts[['ordinary_refusals']] + 1L
      cat(key_name, backend, p, fixed, 'ORDINARY_REFUSAL', class(plain)[[1L]], conditionMessage(plain), '\n')
      next
    }
    result <- tryCatch(collect(summarize_with_margins(input,
      value = sum(x, na.rm = TRUE), count = n(),
      parent = share_of_parent(value), total = share_of_total(value),
      parent_count = share_of_parent(count), total_count = share_of_total(count),
      .grouping = plans[[p]], .by = !!by, .duplicates = 'keep',
      .margin_label = NULL, .id = 'set')), error = identity)
    if (inherits(result, 'error')) {
      counts[['share_refusals']] <- counts[['share_refusals']] + 1L
      cat(key_name, backend, p, fixed, 'SHARE_REFUSAL', class(result)[[1L]], conditionMessage(result), '\n')
      signals[[paste(key_name, backend, p, fixed)]] <- result
      next
    }
    problem <- tryCatch({
      for (source in c('value', 'count')) for (parent in c(FALSE, TRUE)) {
        output <- paste0(if (parent) 'parent' else 'total', if (source == 'count') '_count' else '')
        check(result, plan, output, source, parent)
      }
      NULL
    }, error = identity)
    if (inherits(problem, 'error')) {
      counts[['mismatches']] <- counts[['mismatches']] + 1L
      cat(key_name, backend, p, fixed, 'ORACLE_MISMATCH', conditionMessage(problem), '\n')
      print(result)
      signals[[paste(key_name, backend, p, fixed)]] <- problem
    } else counts[['results']] <- counts[['results']] + 1L
  }
}
print(counts)
cat('Signals:', length(signals), '\n')
if (length(signals)) quit(status = 1L)
