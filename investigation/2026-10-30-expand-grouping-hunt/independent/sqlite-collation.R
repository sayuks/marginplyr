pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
 con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
 on.exit(DBI::dbDisconnect(con))
 DBI::dbExecute(con,'CREATE TABLE d (g TEXT COLLATE NOCASE, f TEXT COLLATE NOCASE, rid INTEGER)')
 DBI::dbExecute(con,"INSERT INTO d VALUES ('a','x',1),('B','X',2),('A','z',3),('b','Z',4)")
 input <- tbl(con,'d')
 print(collect(arrange(input,g)))
 for (sort_mode in c('first','last')) for (label in list(NULL,'Total')) {
  ans <- expand_with_margins(input,.grouping=rollup(g),.margin_label=label,.sort=sort_mode,.id='sid')
  print(collect(ans)); print(dbplyr::sql_render(ans))
 }
})
