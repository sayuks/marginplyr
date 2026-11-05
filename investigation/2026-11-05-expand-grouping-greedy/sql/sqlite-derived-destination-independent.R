pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
probe <- function(transform = identity, dest = "FACTS", existing=FALSE, qualified=FALSE, boolean=TRUE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con,"facts",data.frame(k=c("z","x","y"),v=1:3))
  if (existing) DBI::dbExecute(con,"CREATE TEMP TABLE facts AS SELECT * FROM main.facts")
  input <- tbl(con,if (qualified) DBI::Id(schema="main",table="facts") else "facts")
  result <- expand_with_margins(input,.grouping=rollup(k),.id=if(boolean)"FALSE" else "sid",.sort="last",.margin_label=NULL)
  result <- transform(result)
  expected <- collect(result)
  got <- tryCatch(compute(result,name=dest,temporary=TRUE,overwrite=existing,analyze=FALSE),error=identity)
  list(expected=nrow(expected),actual=if(inherits(got,"error")) conditionMessage(got) else nrow(collect(got)),
       persistent=nrow(DBI::dbReadTable(con,DBI::Id(schema="main",table="facts"))),
       temporary=if(DBI::dbExistsTable(con,DBI::Id(schema="temp",table="facts")))nrow(DBI::dbReadTable(con,DBI::Id(schema="temp",table="facts"))))
}
transforms <- list(direct=identity,mutate=function(x)mutate(x,v=v+100L),select=function(x)select(x,v,k,all_of("FALSE")),arrange=function(x)arrange(x,desc(v)),head=function(x)head(x,4L))
for (nm in names(transforms)) {
  cat(nm,"\n")
  print(probe(transforms[[nm]]))
  print(probe(transforms[[nm]],existing=TRUE))
}
cat("qualified source\n"); print(probe(transforms$mutate,qualified=TRUE))
cat("different destination\n"); print(probe(transforms$mutate,dest="saved"))
cat("ordinary identifier\n"); print(probe(transforms$mutate,boolean=FALSE))
