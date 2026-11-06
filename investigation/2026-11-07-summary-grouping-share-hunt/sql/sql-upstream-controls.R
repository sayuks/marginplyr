Sys.setenv(TESTTHAT_PARALLEL="false")
library(dplyr)
for(kind in c("sqlite","duckdb")) {
  con <- if(kind=="sqlite") DBI::dbConnect(RSQLite::SQLite(),":memory:") else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),config=list(threads="1"))
  d <- data.frame(x=1:2)
  names(d) <- "..1"
  DBI::dbWriteTable(con,"source",d)
  src <- tbl(con,"source")
  calls <- alist(collect(src),collect(summarize(src,n=n(),.by=all_of("..1"))),
    collect(summarize(src,!!!setNames(list(rlang::expr(n())),"..1"))))
  for(call in calls) {
    value <- tryCatch(eval(call),error=identity)
    stopifnot(inherits(value,"error"),grepl("Names can't be of the form",conditionMessage(value),fixed=TRUE))
    cat(kind,"ordinary collection/grouping/output rejects ..1\n")
  }
  if(kind=="sqlite") DBI::dbDisconnect(con) else DBI::dbDisconnect(con,shutdown=TRUE)
}
