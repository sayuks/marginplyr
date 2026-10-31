pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con_sql <- DBI::dbConnect(RSQLite::SQLite())
con_duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
source <- tibble(g = c('a', 'a', 'b', NA_character_), x = c(1, 2, 4, 8), z = c(4, 3, 2, 1))
make <- list(local = identity, dtplyr = dtplyr::lazy_dt, arrow = arrow::arrow_table,
             sqlite = function(d) copy_to(con_sql, d, name = 'input', overwrite = TRUE),
             duckdb = function(d) copy_to(con_duck, d, name = 'input', overwrite = TRUE))
transformers <- list(
  identity = identity,
  filter = function(d) filter(d, x > 2),
  empty = function(d) filter(d, x > 20),
  mutate_key = function(d) mutate(d, g = if_else(x > 3, 'q', g)),
  mutate_measure = function(d) mutate(d, x = x + z),
  rename_roundtrip = function(d) rename(rename(d, other = g), g = other),
  arrange = function(d) arrange(d, desc(x)),
  arrange_expression = function(d) arrange(d, desc(x + z)),
  distinct = function(d) distinct(d, g, x, z),
  union_all = function(d) union_all(d, filter(d, x > 3)),
  union = function(d) union(d, filter(d, x > 3)),
  intersect = function(d) intersect(d, filter(d, x > 3)),
  setdiff = function(d) setdiff(d, filter(d, x > 3)),
  left_join = function(d) left_join(d, select(filter(d, x > 2), g, z2 = z), by = 'g'),
  right_join = function(d) right_join(d, select(filter(d, x > 2), g, z2 = z), by = 'g'),
  full_join = function(d) full_join(d, select(filter(d, x > 2), g, z2 = z), by = 'g'),
  inner_join = function(d) inner_join(d, select(filter(d, x > 2), g, z2 = z), by = 'g'),
  semi_join = function(d) semi_join(d, select(filter(d, x > 2), g), by = 'g'),
  anti_join = function(d) anti_join(d, select(filter(d, x > 2), g), by = 'g'),
  grouped_mutate = function(d) ungroup(mutate(group_by(d, g), z = sum(x))),
  summarize = function(d) summarize(d, x = sum(x), z = sum(z), .by = g),
  filter_window = function(d) filter(d, x > mean(x)),
  mutate_window = function(d) mutate(d, x = x + mean(z)))
capture <- function(expr) tryCatch(suppressWarnings(expr), error = identity)
as_local <- function(d) if (is.data.frame(d)) as_tibble(d) else collect(d)
normalize <- function(d) {
  d <- as.data.frame(d)
  d <- d[order(d$id, is.na(d$g), d$g), c('g', 'id', 'total', 'count')]
  rownames(d) <- NULL
  d
}
counts <- c(matched = 0L, metadata_refusals = 0L, documented_empty = 0L,
            ordinary_sort_failures = 0L, input_refusals = 0L, unexpected = 0L)
for (backend in names(make)) {
  input <- make[[backend]](source)
  for (transform in names(transformers)) {
    d <- capture(transformers[[transform]](input))
    if (inherits(d, 'error')) {
      stopifnot(backend == 'arrow', transform %in% c('intersect', 'setdiff'))
      counts[['input_refusals']] <- counts[['input_refusals']] + 1L
      cat(backend, transform, 'ordinary input construction refused\n')
      next
    }
    expected <- capture({
      detail <- as_local(summarize(d, total = sum(x, na.rm = TRUE), count = n(), .by = g))
      detail$id <- 1L
      grand <- as_local(summarize(d, total = sum(x, na.rm = TRUE), count = n()))
      grand$g <- NA_character_
      grand$id <- 2L
      normalize(bind_rows(detail, grand))
    })
    actual <- capture(normalize(as_local(summarize_with_margins(
      d, total = sum(x, na.rm = TRUE), count = n(),
      .grouping = rollup(g), .margin_label = NULL, .id = 'id'))))
    if (inherits(actual, 'marginplyr_error') && backend == 'dtplyr' &&
        grepl("Can't safely determine column metadata", conditionMessage(actual), fixed = TRUE)) {
      stopifnot(!inherits(expected, 'error'))
      counts[['metadata_refusals']] <- counts[['metadata_refusals']] + 1L
      cat(backend, transform, 'documented metadata refusal\n')
    } else if (inherits(expected, 'error') && !inherits(actual, 'error') &&
               backend %in% c('arrow', 'duckdb') &&
               transform %in% c('arrange', 'arrange_expression')) {
      stopifnot(nrow(actual) == 4L, identical(actual$total, c(3, 4, 8, 15)))
      counts[['ordinary_sort_failures']] <- counts[['ordinary_sort_failures']] + 1L
      cat(backend, transform, 'ordinary summary retained an invalid sort key; Margin call succeeded\n')
    } else if (backend == 'dtplyr' && transform == 'empty') {
      stopifnot(!inherits(expected, 'error'), !inherits(actual, 'error'),
                nrow(expected) == 0L, nrow(actual) == 1L,
                actual$id == 2L, actual$total == 0, actual$count == 0)
      counts[['documented_empty']] <- counts[['documented_empty']] + 1L
      cat(backend, transform, 'Margin grand-total row retained as documented\n')
    } else if (inherits(actual, 'error') || inherits(expected, 'error')) {
      counts[['unexpected']] <- counts[['unexpected']] + 1L
      cat('UNEXPECTED', backend, transform, '\n')
      print(expected)
      print(actual)
    } else if (isTRUE(all.equal(actual, expected, check.attributes = FALSE))) {
      counts[['matched']] <- counts[['matched']] + 1L
    } else {
      counts[['unexpected']] <- counts[['unexpected']] + 1L
      cat('UNEXPECTED', backend, transform, '\n')
      print(expected)
      print(actual)
    }
  }
}
print(counts)
DBI::dbDisconnect(con_sql)
DBI::dbDisconnect(con_duck, shutdown = TRUE)
stopifnot(counts[['unexpected']] == 0L, sum(counts) == 115L)
