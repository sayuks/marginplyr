Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(commandArgs(trailingOnly = TRUE)[[1L]], quiet = TRUE)
library(dplyr)
library(marginplyr)
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script_path))
run <- function(kind) {
  con <- if (kind == "sqlite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), config = list(threads = "1"))
  on.exit(if (kind == "sqlite") DBI::dbDisconnect(con) else DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "source", data.frame(f = c(1L,1L,2L,2L,NA,NA), g = c("a","b","a","b","a","b"), h = c(1L,2L,1L,2L,1L,2L), x = c(1,2,-3,5,0,NA)))
  measures <- if (kind == "sqlite") c("sum(x)", "random() * 1.0", "sum(x) * 1e300", "sum(x) * 1e-300", "1e999", "-1e999", "0.0", "NULL") else
    c("sum(x)", "random()", "sum(x) * 1e300", "sum(x) * 1e-300", "'Infinity'::DOUBLE", "'-Infinity'::DOUBLE", "'NaN'::DOUBLE", "NULL::DOUBLE", "sum(CAST(x AS DECIMAL(38,18)))")
  rows <- list()
  div <- function(x,y) if(is.na(x) || is.na(y) || y == 0) NA_real_ else as.double(x)/as.double(y)
  for (measure in measures) for (fixed in c(FALSE,TRUE)) for (duplicates in c("error","keep")) for (sort in c("none","first","last")) {
    z <- tryCatch({
      q <- summarize_with_margins(tbl(con,"source"), n = !!dbplyr::sql(measure),
        t = share_of_total(n), p = share_of_parent(n),
        .by = all_of(if (fixed) "f" else character()), .grouping = rollup(g,h), .id = "sid",
        .duplicates = duplicates, .sort = sort, .margin_label = NULL,
        .check_share_source = kind != "sqlite")
      sql <- as.character(dbplyr::sql_render(q))
      stopifnot(identical(grepl("GROUP BY GROUPING SETS",sql,fixed=TRUE), kind=="duckdb" && duplicates=="error"))
      v <- collect(q)
      mismatch <- FALSE
      same <- function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&a==b)
      for (i in seq_len(nrow(v))) {
        part <- if (fixed) same(v$f,v$f[[i]]) else rep(TRUE,nrow(v))
        j <- which(v$sid==3L & part)
        et <- if(v$sid[[i]]==3L) 1 else div(v$n[[i]],v$n[[j[[1L]]]])
        parent <- v$sid[[i]]+1L
        j <- which(v$sid==parent & part & if(parent==2L) same(v$g,v$g[[i]]) else TRUE)
        ep <- if(v$sid[[i]]==3L) 1 else div(v$n[[i]],v$n[[j[[1L]]]])
        if (!isTRUE(all.equal(c(v$t[[i]],v$p[[i]]),c(et,ep),tolerance=1e-12))) mismatch <- TRUE
      }
      if (mismatch) saveRDS(list(v=v,sql=sql), file.path(out,paste0(kind,"-mismatch-",length(rows)+1L,".rds")))
      c(if(mismatch) "mismatch" else "match", "")
    }, error=function(e) c("error", conditionMessage(e)))
    rows[[length(rows)+1L]] <- data.frame(kind,measure,fixed,duplicates,sort,verdict=z[[1L]],detail=z[[2L]])
  }
  do.call(rbind,rows)
}
ans <- rbind(run("sqlite"),run("duckdb"))
write.csv(ans,file.path(out,"sql-results.csv"),row.names=FALSE)
print(with(ans,table(kind,verdict)))
if (any(ans$verdict!="match")) print(unique(ans[ans$verdict!="match",c("kind","measure","verdict","detail")]))
stopifnot(all(ans$verdict=="match"))
