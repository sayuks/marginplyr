Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(commandArgs(trailingOnly = TRUE)[[1L]], quiet = TRUE)
library(dplyr)
library(marginplyr)
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script_path))
cases <- c("TRUE", "false", ".env", ".data", "..1", ".BY", ".SD", "x y", "x`y", "x\ny", "a\"b", "rowid", "CURRENT_DATE", "..marginplyr_set_id")
run <- function(kind) {
  con <- if(kind=="sqlite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE), config=list(threads="1"))
  on.exit(if(kind=="sqlite") DBI::dbDisconnect(con) else DBI::dbDisconnect(con,shutdown=TRUE))
  rows <- list()
  for (nm in cases) for(role in c("fixed","dimension","source","share","id")) for(sort in c("none","last")) {
    nms <- c(fixed="f",dimension="g",source="n",share="s",id="set")
    nms[[role]] <- nm
    d <- data.frame(f=c(1L,1L,2L,2L),g=c("a","b","a","b"),x=c(1,3,2,6))
    names(d)[1:2] <- nms[c("fixed","dimension")]
    DBI::dbWriteTable(con,"source",d,overwrite=TRUE)
    val <- tryCatch({
      q <- summarize_with_margins(tbl(con,"source"),
        !!!setNames(list(rlang::expr(sum(x)),rlang::expr(share_of_total(!!rlang::sym(nms[["source"]])))),nms[c("source","share")]),
        .by=all_of(nms[["fixed"]]), .grouping=rollup(all_of(nms[["dimension"]])),
        .id=nms[["id"]],.sort=sort,.margin_label=NULL,.check_share_source=kind!="sqlite")
      v <- collect(q)
      stopifnot(identical(names(v),unname(nms[c("fixed","dimension","id","source","share")])))
      roots <- v[v[[nms[["id"]]]]==2L,]
      expected <- ifelse(v[[nms[["id"]]]]==2L,1,v[[nms[["source"]]]]/roots[[nms[["source"]]]][match(v[[nms[["fixed"]]]],roots[[nms[["fixed"]]]])])
      if(isTRUE(all.equal(v[[nms[["share"]]]],expected))) c("match","") else c("mismatch",paste(v[[nms[["share"]]]],collapse=","))
    }, error=function(e) c("error",conditionMessage(e)))
    rows[[length(rows)+1L]] <- data.frame(kind,name=nm,role,sort,verdict=val[[1L]],detail=val[[2L]])
  }
  do.call(rbind,rows)
}
ans <- rbind(run("sqlite"),run("duckdb"))
write.csv(ans,file.path(out,"names-results.csv"),row.names=FALSE)
print(with(ans,table(kind,verdict)))
if(any(ans$verdict!="match")) print(unique(ans[ans$verdict!="match",c("kind","name","role","detail")]))
stopifnot(all(ans$verdict[ans$name!="..1"]=="match"))
unsupported <- ans[ans$name=="..1",]
stopifnot(nrow(unsupported)==20L, all(unsupported$verdict=="error"),
  all(grepl("Names can't be of the form", unsupported$detail, fixed=TRUE)))
