Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', unset = '.'), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  src <- copy_to(con, tibble(key = c(2L, 1L), value = c(29L, 19L)),
                 'independent_schema', temporary = TRUE)
  ordinary <- collect(collapse(arrange(src, key)))
  stopifnot(identical(names(ordinary), c('key', 'value')),
            identical(ordinary$key, c(1L, 2L)))
  x <- expand_with_margins(src, .grouping = grouping_set(key),
    .margin_label = NULL, .sort = 'first', .id = 'occurrence')
  direct <- collect(x)
  derived <- collapse(x)
  collapsed <- collect(derived)
  materialized <- compute(x, name = 'independent_materialized')
  materialized_collapsed <- collect(collapse(materialized))
  stopifnot(identical(names(direct), c('key', 'occurrence', 'value')),
            identical(colnames(derived), names(direct)),
            identical(names(materialized_collapsed), names(direct)),
            identical(as_tibble(arrange(collapsed[names(direct)], key)),
                      as_tibble(arrange(direct, key))))
  cat('Collapse public names:', names(direct), '\n')
  cat('Collapse collected names:', names(collapsed), '\n')
  cat('Ordinary/source and explicit-materialization schema controls passed.\n')
  if ('--assert-collapse' %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(identical(names(collapsed), names(direct)))
  }

  bool_src <- copy_to(con, tibble(group = c('b', 'a'), FaLsE = c(29L, 19L)),
                      'independent_boolean', temporary = TRUE)
  opts <- dbplyr::sql_options(cte = TRUE)
  ordinary_cte <- collect(filter(bool_src, TRUE), sql_options = opts)
  stopifnot(identical(names(ordinary_cte), c('group', 'FaLsE')),
            identical(ordinary_cte$FaLsE, c(29L, 19L)))
  y <- expand_with_margins(bool_src, .grouping = rollup(group), .sort = 'none')
  direct_y <- collect(y)
  materialized_y <- collect(filter(compute(y, name = 'independent_bool_materialized'), TRUE),
                            sql_options = opts)
  stopifnot(identical(names(direct_y), c('group', 'FaLsE')),
            identical(names(materialized_y), names(direct_y)),
            identical(sort(materialized_y$FaLsE), sort(direct_y$FaLsE)))
  raw <- DBI::dbGetQuery(con, dbplyr::sql_render(y, sql_options = opts))
  cte_y <- tryCatch(collect(y, sql_options = opts), error = identity)
  cat('CTE expected public names:', names(direct_y), '\n')
  cat('CTE raw query names:', names(raw), '\n')
  cat('CTE collect class:', class(cte_y)[[1L]], '\n')
  if (inherits(cte_y, 'error')) cat('CTE collect message:', conditionMessage(cte_y), '\n')
  cat('Ordinary/source and explicit-materialization CTE controls passed.\n')
  if ('--assert-cte' %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(!inherits(cte_y, 'error'), identical(names(cte_y), names(direct_y)))
  }
})
