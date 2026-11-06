Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
library(marginplyr)
out <- commandArgs(trailingOnly = TRUE)
if (!length(out)) out <- tempdir()
dir.create(out[[1L]], recursive = TRUE, showWarnings = FALSE)
run <- function() {
  home <- tempfile("duckdb-hunt-")
  dir.create(home)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:",
    config = list(home_directory = home, threads = "1"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE source AS SELECT * FROM (VALUES ",
    "('A','x',1.0),('a','y',2.0),('B','x',4.0),('b','y',8.0),",
    "('é','x',16.0),('e','y',32.0),(NULL,'x',64.0)) t(k,c,v)"))
  keys <- c("k", "k COLLATE NOCASE", "k COLLATE NOACCENT",
            "k COLLATE NOCASE.NOACCENT")
  measures <- c("sum(v)", "min(v)", "max(v)", "avg(v)",
    "sum(v * 1e-300)", "sum(v * 1e300)",
    "sum(CAST(v AS DECIMAL(38,0)))",
    "sum(CAST(v * 1e-12 AS DECIMAL(38,18)))")
  rows <- list()
  record <- function(...) rows[[length(rows) + 1L]] <<- data.frame(...)
  for (key in keys) for (measure in measures) for (fixed in c(FALSE, TRUE)) {
    src <- tbl(con, dbplyr::sql(paste0(
      "SELECT ",key," AS k,c,v FROM source")))
    for (aggregation in c("auto", "portable")) for (sort in c("none","first","last")) {
      value <- tryCatch({
        q <- if (fixed) summarize_with_margins(src,
          n = !!dbplyr::sql(measure), t = share_of_total(n), p = share_of_parent(n),
          .grouping = rollup(c), .by = k, .id = "sid",
          .duplicates = if (aggregation == "auto") "error" else "keep",
          .sort = sort, .margin_label = NULL)
        else summarize_with_margins(src,
          n = !!dbplyr::sql(measure), t = share_of_total(n), p = share_of_parent(n),
          .grouping = rollup(k,c), .id = "sid",
          .duplicates = if (aggregation == "auto") "error" else "keep",
          .sort = sort, .margin_label = NULL)
        native <- grepl("GROUP BY GROUPING SETS", as.character(dbplyr::sql_render(q)), fixed = TRUE)
        stopifnot(identical(native, aggregation == "auto"))
        collect(q)
      }, error = identity)
      if (inherits(value,"error")) {
        record(key=key,measure=measure,fixed=fixed,aggregation=aggregation,sort=sort,
          verdict="error",detail=conditionMessage(value))
        next
      }
      # Ask the same database to compare retained spellings, preserving its
      # collation equivalence instead of assuming R string equality.
      same <- function(a,b) {
        if (is.na(a) || is.na(b)) return(is.na(a) && is.na(b))
        coll <- sub("^k", "", key)
        as.logical(DBI::dbGetQuery(con,paste0("SELECT ",DBI::dbQuoteString(con,a),
          coll," = ",DBI::dbQuoteString(con,b),coll))[[1L]])
      }
      ratio <- function(x,y) if (is.na(x)||is.na(y)||y==0) NA_real_ else as.double(x)/as.double(y)
      mismatch <- FALSE
      for (i in seq_len(nrow(value))) {
        root <- if (fixed) 2L else 3L
        match_fixed <- if (fixed) vapply(value$k,function(z) same(z,value$k[[i]]),TRUE) else rep(TRUE,nrow(value))
        totals <- which(value$sid==root & match_fixed)
        tp <- if (value$sid[[i]]==root) 1 else ratio(value$n[[i]],value$n[[totals[[1L]]]])
        parent <- value$sid[[i]]+1L
        parent_match <- match_fixed
        if (!fixed && parent==2L) parent_match <- vapply(value$k,function(z) same(z,value$k[[i]]),TRUE)
        pp <- if (value$sid[[i]]==root) 1 else {
          j <- which(value$sid==parent & parent_match)
          ratio(value$n[[i]], value$n[[j[[1L]]]])
        }
        if (!isTRUE(all.equal(c(value$t[[i]],value$p[[i]]),c(tp,pp),tolerance=1e-12))) mismatch <- TRUE
      }
      record(key=key,measure=measure,fixed=fixed,aggregation=aggregation,sort=sort,
        verdict=if(mismatch) "mismatch" else "match",detail="")
    }
  }
  ans <- do.call(rbind,rows)
  write.csv(ans,file.path(out[[1L]],"collation-numeric-results.csv"),row.names=FALSE)
  print(table(ans$verdict))
  if(any(ans$verdict!="match")) print(ans[ans$verdict!="match",])
  stopifnot(all(ans$verdict=="match"))
}
run()
