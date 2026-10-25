# Recorded investigation: 2026-10-25, c62ecdb01d31ca5c2c9ab8baed0dda970dd844e5.
# Run in a fresh R process from the repository root. This is an investigation
# harness, not a package test or implementation. See manifest.txt for limits.

# The default intentionally fails its final equality assertion: an unmuffled
# child warning also reaches the enclosing operation. muffle-child is its control.
pkgload::load_all('.',quiet=TRUE)
library(dplyr)

main <- function() {
  muffle_child <- 'muffle-child' %in% commandArgs(TRUE)
  old <- options(marginplyr.audit_sql=TRUE)
  on.exit(options(old),add=TRUE)
  raw <- DBI::dbConnect(RSQLite::SQLite(),':memory:')
  on.exit(DBI::dbDisconnect(raw),add=TRUE)
  dialect <- dbplyr::dialect_sqlite()
  class(dialect) <- c('sql_dialect_audit_reentry',class(dialect))
  con <- dbplyr::with_dialect(raw,dialect)
  state <- new.env(parent=emptyenv())
  state$nested <- FALSE
  state$ready <- FALSE
  state$child_rows <- NULL
  state$outer_before <- NULL
  state$outer_after <- NULL
  state$delivered <- character()
  translator <- function(con) {
    base <- getS3method('sql_translation','sql_dialect_sqlite',envir=asNamespace('dbplyr'))(con)
    scalar <- dbplyr::sql_translator(.parent=base$scalar,
      audit_outer=function(x) {
        rows <- last_sent_queries(FALSE,TRUE)
        if(state$ready && !state$nested && nrow(rows) && tail(rows$status,1L)=='invoking') {
          state$nested <- TRUE
          state$outer_before <- rows
          if(muffle_child) {
            withCallingHandlers(
              summarize_with_margins(state$inner,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE),
              warning=function(cnd) invokeRestart('muffleWarning')
            )
          } else {
            summarize_with_margins(state$inner,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE)
          }
          state$outer_after <- last_sent_queries(FALSE,TRUE)
        }
        x
      },
      audit_child=function(x) {
        warning('child translator warning',call.=FALSE)
        state$child_rows <- last_sent_queries(FALSE,TRUE)
        x
      }
    )
    dbplyr::sql_variant(scalar,base$aggregate,base$window)
  }
  registerS3method('sql_translation','sql_dialect_audit_reentry',translator,envir=asNamespace('dbplyr'))
  outer <- dplyr::copy_to(con,data.frame(g='a',v=1),'parent_source')
  inner <- dplyr::copy_to(con,data.frame(g='b',v=2),'child_source')
  outer <- mutate(outer,g=audit_outer(g))
  state$inner <- mutate(inner,g=audit_child(g))
  state$ready <- TRUE
  query <- withCallingHandlers(summarize_with_margins(outer,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE),warning=function(cnd) {
    state$delivered <- c(state$delivered,conditionMessage(cnd))
    invokeRestart('muffleWarning')
  })
  cat('caller delivered warnings:\n');print(state$delivered)
  cat('parent before nested call:\n');print(state$outer_before$conditions[[1L]])
  cat('parent after nested call:\n');print(state$outer_after$conditions[[1L]])
  cat('last observed child rows:\n');print(state$child_rows)
  cat('final parent rows:\n');print(last_sent_queries(FALSE,TRUE))
  cat('collected parent result:\n');print(collect(query))
  stopifnot(identical(state$outer_before,state$outer_after))
}
main()
