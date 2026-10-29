# Dated investigation evidence; requires the sibling fixture and scratch DB.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
script <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), 'postgres-fixtures.R'))
local({
  con <- postgres_fixture_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  type_of <- function(query) {
    DBI::dbGetQuery(con, paste0('SELECT pg_typeof((SELECT g FROM (',
      query, ') q LIMIT 1))::text AS ty'))$ty
  }
  passed <- 0L
  for (nm in names(postgres_types)) {
    query <- paste0('SELECT ', postgres_types[[nm]], ' AS g, 1 AS rid UNION ALL SELECT NULL, 2')
    input <- tbl(con, sql(query))
    expected <- type_of(query)
    for (si in 1:3) for (empty in c(FALSE, TRUE)) {
      source <- if (empty) filter(input, FALSE) else input
      spec <- list(grouping_set(g), rollup(g), grouping_sets(grouping_set(), grouping_set(g)))[[si]]
      result <- expand_with_margins(source, .grouping = spec,
        .margin_label = NULL, .id = 'sid')
      actual <- type_of(as.character(dbplyr::sql_render(result)))
      stopifnot(identical(expected, actual))
      passed <- passed + 1L
    }
  }
  cat('PHYSICAL_TYPE_MATCHES', passed, '\n')
})
