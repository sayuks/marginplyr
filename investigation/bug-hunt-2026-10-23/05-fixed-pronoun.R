# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages(pkgload::load_all('.'))
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
source <- copy_to(con,tibble(.env='fixed',g=c('x','y')),'input')
result <- tryCatch(collect(summarize_with_margins(
  source,n=n(),p=share_of_total(n),.by=all_of('.env'),
  .grouping=rollup(g),.margin_label=NULL,.check_share_source=FALSE
)),error=identity)
DBI::dbDisconnect(con)
cat('outcome:',if(inherits(result,'error'))conditionMessage(result) else paste(result$p,collapse=','),'\n')
stopifnot('A valid fixed key named .env must work with Total shares' = !inherits(result,'error'))
stopifnot(identical(sort(result$p),c(0.5,0.5,1)))
