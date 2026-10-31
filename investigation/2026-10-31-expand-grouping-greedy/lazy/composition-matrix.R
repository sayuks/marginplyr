# Dated backend-composition differential; run from the repository root.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
out <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', '/tmp')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(expr, error = identity)
canon <- function(x) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(col) if (is.factor(col)) as.character(col) else col)
  if (nrow(x) > 0L) x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
shapes <- list(
  root = identity,
  filtered = function(x) filter(x, v > 0L),
  empty = function(x) filter(x, rid < 0L),
  selected = function(x) select(x, v, rid, h, g, b),
  renamed = function(x) rename(x, oldg = g) |> rename(g = oldg),
  relocated = function(x) relocate(x, v),
  ordered = function(x) arrange(x, desc(v), g),
  limited = function(x) arrange(x, v) |> head(2L),
  distinct = function(x) distinct(x),
  union = function(x) union_all(x, filter(x, v > 1L)),
  intersection = function(x) intersect(x, filter(x, v > 1L)),
  difference = function(x) setdiff(x, filter(x, v > 1L)),
  self_join = function(x) left_join(x, select(x, rid), by = 'rid'),
  semi_join = function(x) semi_join(x, filter(x, v > 1L), by = 'rid'),
  anti_join = function(x) anti_join(x, filter(x, v > 1L), by = 'rid'),
  full_join = function(x) full_join(x, filter(x, v > 1L), by = c('g', 'h', 'b', 'rid', 'v')),
  collapsed = function(x) collapse(x),
  computed = function(x) compute(x),
  computed_rename = function(x) compute(x) |> rename(oldg = g) |> rename(g = oldg),
  nested_cache = function(x) compute(x) |> rename(oldg = g) |> compute() |> rename(g = oldg),
  expanded = function(x) expand_with_margins(x, .grouping = rollup(h), .margin_label = NULL, .id = 'prev_id'),
  computed_expanded = function(x) expand_with_margins(x, .grouping = rollup(h), .margin_label = NULL, .id = 'prev_id') |> compute()
)
selection <- Sys.getenv('MARGINPLYR_HUNT_SHAPES')
if (nzchar(selection)) shapes <- shapes[strsplit(selection, ',', fixed = TRUE)[[1L]]]
plans <- list(included = grouping_set(g), rollup = rollup(g, h), cube = cube(g, h),
              omitted_first = grouping_sets(grouping_set(), grouping_set(g)),
              duplicate = grouping_sets(grouping_set(g), grouping_set(), grouping_set(g)))
rows <- list(); k <- 0L
for (n in c(0L, 1L, 4L)) {
  dat <- tibble(g = c('b','a',NA,'b')[seq_len(n)], h = c(2L,1L,NA,1L)[seq_len(n)],
                b = c('x','y','x',NA)[seq_len(n)], rid = seq_len(n), v = seq_len(n))
  for (backend in c('dtplyr', 'arrow')) {
    source <- if (backend == 'dtplyr') dtplyr::lazy_dt(data.table::as.data.table(dat)) else arrow::Table$create(dat)
    for (shape in names(shapes)) {
      q <- capture(shapes[[shape]](source))
      src <- if (inherits(q, 'error')) q else capture(collect(q))
      for (plan in names(plans)) for (label in c('null','text')) for (sort in c('none','first','last')) {
        lab <- if (label == 'null') NULL else 'Total'
        actual <- if (inherits(src, 'error')) src else capture(collect(expand_with_margins(q,
          .by = b, .grouping = plans[[plan]], .margin_label = lab, .check_margin_label = FALSE,
          .duplicates = 'keep', .id = 'sid', .sort = sort)))
        expected <- if (inherits(src, 'error')) src else capture(expand_with_margins(src,
          .by = b, .grouping = plans[[plan]], .margin_label = lab, .check_margin_label = FALSE,
          .duplicates = 'keep', .id = 'sid', .sort = sort))
        status <- if (inherits(src, 'error')) 'source_error' else if (inherits(expected, 'error')) {
          if (inherits(actual, 'error')) 'both_error' else 'control_error'
        } else if (inherits(actual, 'error')) {
          if (grepl('safely determine column metadata', conditionMessage(actual), fixed = TRUE)) 'metadata_refusal' else 'package_error'
        } else if (!isTRUE(all.equal(canon(actual)[names(expected)], canon(expected)))) 'value_diff' else {
          keys <- c('b','g',if (plan %in% c('cube','rollup')) 'h','sid')
          if (sort != 'none' && !isTRUE(all.equal(as.data.frame(actual[keys]), as.data.frame(expected[keys]), check.attributes = FALSE))) 'order_diff' else 'pass'
        }
        k <- k + 1L
        rows[[k]] <- data.frame(n = n, backend = backend, shape = shape, plan = plan,
          label = label, sort = sort, status = status,
          actual_error = if (inherits(actual, 'error')) conditionMessage(actual) else '',
          control_error = if (inherits(expected, 'error')) conditionMessage(expected) else '')
        if (status %in% c('value_diff','order_diff','package_error')) cat('LEAD', n, backend, shape, plan, label, sort, status, if (inherits(actual, 'error')) conditionMessage(actual) else '', '\n')
      }
    }
  }
}
res <- bind_rows(rows)
write.csv(res, file.path(out, 'composition-results.csv'), row.names = FALSE)
print(table(res$backend, res$status))
