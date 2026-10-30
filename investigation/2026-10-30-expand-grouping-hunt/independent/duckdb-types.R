pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ':memory:', array = 'matrix')
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TYPE mood AS ENUM ('sad','ok','happy')")
  expressions <- c(map = "MAP {'key': 3}", union = "union_value(a := 42)",
    uuid = "'550e8400-e29b-41d4-a716-446655440000'::UUID", json = "'{\"a\": 1}'::JSON",
    interval = "INTERVAL '3 months 2 days 1 minute'", enum = "'ok'::mood", bit = "'1011'::BIT",
    bignum = "'99999999999999999999999999999999999999999999999'::BIGNUM",
    variant = "42::VARIANT", array = '[1,2,3]::INTEGER[3]', map_empty = 'MAP()')
  physical <- function(query) DBI::dbGetQuery(con, paste0('DESCRIBE (', as.character(dbplyr::sql_render(query)), ')'))$column_type
  for (nm in names(expressions)) {
    raw <- tryCatch(tbl(con, sql(paste0('SELECT ', expressions[[nm]], ' AS g, 1 AS rid UNION ALL SELECT NULL, 2'))),error=identity)
    if (inherits(raw,'error')) { cat(nm, 'FIXTURE',conditionMessage(raw),'\n'); next }
    for(empty in c(FALSE,TRUE)) for(order in c('none','first','last')) {
      source <- if(empty) filter(raw,FALSE) else raw
      ordinary <- tryCatch(union_all(mutate(source,sid=1L),mutate(source,g=sql('NULL'),sid=2L)),error=identity)
      actual <- tryCatch(expand_with_margins(source,.grouping=rollup(g),.margin_label=NULL,.id='sid',.sort=order),error=identity)
      get <- function(x) if(inherits(x,'error')) paste('ERROR',conditionMessage(x)) else tryCatch(paste(physical(x),collapse=','),error=function(e) paste('ERROR',conditionMessage(e)))
      cat(nm, empty, order, 'source:', get(source),'ordinary:',get(ordinary),'actual:',get(actual),'\n')
      if(!inherits(actual,'error')) {
        result <- tryCatch(collect(actual),error=identity)
        if(inherits(result,'error')) cat('COLLECT',conditionMessage(result),'\n')
      }
    }
  }
})
