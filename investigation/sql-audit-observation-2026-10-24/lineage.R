suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
on.exit(DBI::dbDisconnect(con))
DBI::dbWriteTable(con, 'sales', data.frame(region = c('Tokyo','Osaka'), revenue = c(1,2)))
sales <- dplyr::tbl(con, 'sales')
q <- summarize_with_margins(sales, revenue = sum(revenue, na.rm = TRUE), .grouping = rollup(region))
cat('original class:', paste(class(q), collapse=','), '\n')
cat('original attributes:', paste(names(attributes(q)), collapse=','), '\n')
cat('original list fields:', paste(names(unclass(q)), collapse=','), '\n')
cat('query attributes:', paste(names(attributes(q$lazy_query)), collapse=','), '\n')
attr(q, 'audit_probe_origin') <- 'A'
other <- sales
q2 <- q
attr(q2, 'audit_probe_origin') <- 'B'
report <- function(label, x) {
 cat(label, 'class=', paste(class(x), collapse=','), ' origin=', paste(attr(x,'audit_probe_origin'),collapse=','), '\n', sep='')
 invisible(x)
}
report('filter ', dplyr::filter(q, revenue > 0))
report('select ', dplyr::select(q, revenue))
report('left_join q-left ', dplyr::left_join(q, other, by='region'))
report('left_join q-right ', dplyr::left_join(other, q, by='region'))
report('left_join A-and-B ', dplyr::left_join(q, q2, by='region'))
report('collect ', dplyr::collect(q))
report('pull ', dplyr::pull(q,revenue))
computed <- report('compute ', dplyr::compute(q, name='materialized_q'))
report('read table again ', dplyr::tbl(con,'materialized_q'))
report('collapse ', dplyr::collapse(q))
cat('computed query fields:', paste(names(computed$lazy_query), collapse=','), '\n')
cat('computed query source:', as.character(dbplyr::remote_name(computed)), '\n')
plan <- inspect_grouping(sales, .grouping=rollup(region))
report('inspect_grouping ', plan)
DBI::dbDisconnect(con)
