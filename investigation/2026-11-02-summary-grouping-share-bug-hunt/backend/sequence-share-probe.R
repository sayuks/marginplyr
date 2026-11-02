pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
run <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  t <- copy_to(con,tibble(g=c("a","a","b","b"),h=c("x","y","x","y"),x=1:4),"seq_input")
  for (dims in 1:2) for (kind in c("parent","total")) for (source in c("raw","aggregate")) for (adapter in c("native","portable")) {
    DBI::dbExecute(con,"CREATE OR REPLACE SEQUENCE s START 1")
    dots <- list(v=if (source=="raw") dbplyr::sql("nextval('s')") else rlang::expr(sum(x,na.rm=TRUE)+!!dbplyr::sql("nextval('s')")),
                 p=rlang::call2(if(kind=="parent") "share_of_parent" else "share_of_total",quote(v)))
    q <- summarize_with_margins(t,!!!dots,.grouping=if(dims==1) rollup(g) else rollup(g,h),
      .id="set",.check_share_source=FALSE,.duplicates=if(adapter=="native") "error" else "keep")
    v <- collect(q)
    root <- dims+1L
    exp <- vapply(seq_len(nrow(v)),function(i) {
      if(v$set[[i]]==root) return(1)
      ps <- if(kind=="parent") v$set[[i]]+1L else root
      j <- which(v$set==ps & (ps==root | v$g==v$g[[i]]))
      stopifnot(length(j)==1L)
      v$v[[i]]/v$v[[j]]
    },numeric(1))
    ok <- isTRUE(all.equal(v$p,exp))
    cat("dims",dims,"kind",kind,"source",source,"adapter",adapter,"match",ok,"\n")
    if(!ok) { print(v);print(exp) }
  }
}
run()
