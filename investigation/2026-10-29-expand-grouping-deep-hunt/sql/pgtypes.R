# Dated investigation evidence; requires the sibling fixture and scratch DB.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
script <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), 'postgres-fixtures.R'))
local({
  con <- postgres_fixture_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  passed <- 0L
  shared <- 0L
  fail <- list()
  for (nm in names(postgres_types)) {
    query <- paste0('SELECT ', postgres_types[[nm]], ' AS g, 1 AS rid UNION ALL SELECT NULL, 2')
    input <- tbl(con, sql(query))
    base <- tryCatch(collect(input), error = identity)
    if (inherits(base, 'error')) {
      cat('FIXTURE', nm, conditionMessage(base), '\n')
      next
    }
    for (shape in c('plain', 'filter', 'limit', 'window', 'empty')) {
      source <- switch(shape,
        plain = input, filter = filter(input, rid == 1L),
        limit = head(arrange(input, rid), 1),
        window = mutate(arrange(input, rid), v = row_number()),
        empty = filter(input, FALSE))
      source_rows <- collect(source)
      for (si in 1:3) for (sort in c('none', 'last', 'first')) {
        spec <- list(grouping_set(g), rollup(g), grouping_sets(grouping_set(), grouping_set(g)))[[si]]
        key <- paste(nm, shape, si, sort)
        result <- tryCatch(expand_with_margins(source, .grouping = spec,
          .margin_label = NULL, .id = 'sid', .sort = sort), error = identity)
        actual <- tryCatch(collect(result), error = identity)
        source_type <- DBI::dbGetQuery(con, paste0(
          'SELECT pg_typeof(g)::text AS ty FROM (', query, ') q LIMIT 1'))$ty
        if (inherits(actual, 'error')) {
          control <- tryCatch(collect(arrange(union_all(source,
            mutate(source, g = sql(paste0('CAST(NULL AS ', source_type, ')')))),
            is.na(g), g)), error = identity)
          if (inherits(control, 'error')) {
            shared <- shared + 1L
            cat('SHARED', key, conditionMessage(actual), '\n')
          } else {
            fail[[key]] <- actual
            cat('FAIL', key, conditionMessage(actual), '\n')
          }
          next
        }
        copies <- if (si == 1L) 1L else 2L
        valid <- nrow(actual) == nrow(source_rows) * copies &&
          identical(names(actual), c('g', 'sid', setdiff(names(source_rows), 'g'))) &&
          (nrow(actual) == 0L || all(table(actual$rid) == copies))
        if (valid) {
          passed <- passed + 1L
        } else {
          fail[[key]] <- actual
          cat('FAIL', key, 'ROWS/NAMES\n')
        }
      }
    }
  }
  cat('PASS', passed, 'SHARED', shared, 'FAIL', length(fail), '\n')
  if (length(fail)) stop('Unexpected PostgreSQL expansion discrepancy; inspect printed cases.')
})
