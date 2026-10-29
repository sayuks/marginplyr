pkgload::load_all(quiet=TRUE); library(dplyr)
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:'); DBI::dbExecute(con,'CREATE TABLE t (g BLOB, i INTEGER)'); DBI::dbExecute(con,"INSERT INTO t VALUES (x'4142',1), (NULL,2)"); x<-tbl(con,'t')
for(empty in c(FALSE,TRUE)) {xx<-if(empty)filter(x,FALSE) else x;cat('EMPTY=',empty,'\n');str(collect(xx));str(collect(compute(xx,analyze=FALSE)))
for(spec in list(grouping_set(g),rollup(g),grouping_sets(grouping_set(),grouping_set(g)))) for(sort in c('none','first','last')) {cat('SPEC=',format(spec),' SORT=',sort,'\n');z<-expand_with_margins(xx,.grouping=spec,.margin_label=NULL,.sort=sort,.id='sid');str(collect(z));str(collect(compute(z,analyze=FALSE))); print(dbplyr::sql_render(z))}}
DBI::dbDisconnect(con)
