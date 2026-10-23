# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages(pkgload::load_all('.'))
suppressPackageStartupMessages(library(dplyr))
# Use a disposable PostgreSQL server configured through PGHOST/PGPORT/PGUSER/PGDATABASE.
con <- DBI::dbConnect(RPostgres::Postgres())
source <- copy_to(con,tibble(g=1L),'null_red',temporary=TRUE)
query <- expand_with_margins(source,.grouping=rollup(g),.margin_label=NULL,.id='set')
cat(as.character(dbplyr::sql_render(query)),'\n')
result <- tryCatch(collect(query),error=identity)
DBI::dbDisconnect(con)
cat('outcome:',if(inherits(result,'error'))conditionMessage(result) else paste(result$g,collapse=','),'\n')
stopifnot('Typed missing integer dimension with .id must be collectable' = !inherits(result,'error'))
result <- arrange(result, .data$set)
stopifnot(identical(result$g,c(1L,NA_integer_)),identical(result$set,1:2))
