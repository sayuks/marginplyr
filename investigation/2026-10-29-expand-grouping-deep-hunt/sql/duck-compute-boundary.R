# Dated investigation evidence, not a package regression test.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
on.exit(DBI::dbDisconnect(con), add = TRUE)
source <- copy_to(con, tibble(g = c('a', 'b'), v = c(1L, 2L), .env = c('x', 'y')), 'facts')

cat('FOCUSED GROUPING WITNESS\n')
expanded <- expand_with_margins(source, .grouping = rollup(g), .margin_label = NULL, .id = 'sid')
grouped <- group_by(expanded, g)
materialized <- compute(grouped, analyze = FALSE)
print(group_vars(grouped))
print(group_vars(materialized))
print(collect(summarise(grouped, total = sum(v, na.rm = TRUE))))
print(collect(summarise(materialized, total = sum(v, na.rm = TRUE))))
stopifnot(identical(group_vars(grouped), 'g'), length(group_vars(materialized)) == 0L)

cat('GROUPING AND WINDOW-ORDER MATRIX\n')
meta_count <- 0L
for (role in c('payload', 'dimension', 'fixed', 'identifier')) {
  for (empty in c(FALSE, TRUE)) {
    for (temporary in c(FALSE, TRUE)) {
      input <- if (role == 'identifier') select(source, g, v) else source
      if (empty) input <- filter(input, FALSE)
      result <- switch(role,
        payload = expand_with_margins(input, .grouping = rollup(g), .margin_label = NULL, .id = 'sid'),
        dimension = expand_with_margins(input, .grouping = rollup(all_of('.env')), .margin_label = NULL, .id = 'sid'),
        fixed = expand_with_margins(input, .by = all_of('.env'), .grouping = rollup(g), .margin_label = NULL, .id = 'sid'),
        identifier = expand_with_margins(input, .grouping = rollup(g), .margin_label = NULL, .id = '.env'))
      derived <- arrange(group_by(result, g), v)
      name <- paste0('metadata_', role, '_', empty, '_', temporary)
      out <- compute(derived, name = name, temporary = temporary, analyze = FALSE)
      groups_before <- group_vars(derived)
      order_before <- vapply(dbplyr::op_sort(derived), rlang::expr_text, character(1))
      stopifnot(identical(groups_before, 'g'), identical(order_before, 'v'),
        length(group_vars(out)) == 0L, length(dbplyr::op_sort(out)) == 0L,
        nrow(collect(out)) == (if (empty) 0L else 4L))
      meta_count <- meta_count + 1L
      cat(role, empty, temporary, 'groups g -> empty; order v -> empty\n')
    }
  }
}
ordinary <- arrange(group_by(select(source, g, v), g), v)
ordinary_computed <- compute(ordinary, analyze = FALSE)
stopifnot(identical(group_vars(ordinary_computed), 'g'),
  identical(vapply(dbplyr::op_sort(ordinary_computed), rlang::expr_text, character(1)), 'v'))
cat('METADATA_CASES', meta_count, 'ordinary control retained g and v\n')

cat('INVALID INDEX DESTINATION MATRIX\n')
index_count <- 0L
for (method in c('ordinary', 'margin')) {
  for (index_arg in c('indexes', 'unique_indexes')) {
    for (temporary in c(FALSE, TRUE)) {
      for (overwrite in c(FALSE, TRUE)) {
        name <- paste0('index_', method, '_', index_arg, '_', temporary, '_', overwrite)
        if (overwrite) {
          DBI::dbExecute(con, paste0('CREATE ', if (temporary) 'TEMP ' else '', 'TABLE ', name, ' AS SELECT 777 AS sentinel'))
        }
        input <- if (method == 'ordinary') select(source, g, v) else expanded
        args <- list(x = input, name = name, temporary = temporary, overwrite = overwrite, analyze = FALSE)
        args[[index_arg]] <- list('missing')
        error <- tryCatch(do.call(compute, args), error = identity)
        exists <- DBI::dbExistsTable(con, name)
        state <- if (exists) DBI::dbGetQuery(con, paste0('SELECT * FROM ', name)) else NULL
        stopifnot(inherits(error, 'error'))
        if (method == 'ordinary') {
          stopifnot(if (overwrite) identical(state, data.frame(sentinel = 777L)) else !exists)
        } else {
          stopifnot(exists, nrow(state) == 4L, identical(names(state), c('g', 'sid', 'v', '.env')))
        }
        index_count <- index_count + 1L
        cat(method, index_arg, temporary, overwrite, 'table_exists', exists,
          'columns', paste(names(state), collapse = ','), '\n')
        if (method == 'margin' && overwrite && temporary && index_arg == 'indexes') {
          cat('CAUSAL ERROR:', conditionMessage(error), '\n')
        }
      }
    }
  }
}
cat('INDEX_CASES', index_count, '\n')
cat('Both deviations reproduced. This script requires the baseline bugs; it does not assert fixes.\n')
})
