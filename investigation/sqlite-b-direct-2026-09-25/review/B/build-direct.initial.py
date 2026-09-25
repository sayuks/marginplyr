from pathlib import Path
p=Path('/private/tmp/marginplyr-sqlite-design-20260925/variants/B-direct/R/sqlite-typed-order.R')
s=p.read_text(); a=s.index('# THROWAWAY DESIGN PROTOTYPE B:')
s=s[:a]+'''# THROWAWAY DESIGN PROTOTYPE B-direct: only one full result-row insertion.
# The ordered public SELECT does not need the compound anchor when its target
# already has the source and package-declared column types.
#' @exportS3Method dplyr::compute
#' @noRd
compute.marginplyr_sqlite_typed_result <- function(x, name = NULL,
                                                   temporary = TRUE,
                                                   overwrite = FALSE,
                                                   unique_indexes = list(),
                                                   indexes = list(),
                                                   analyze = TRUE, ...,
                                                   sql_options = NULL,
                                                   in_transaction = FALSE) {
  if (!sqlite_typed_result_direct(x)) return(NextMethod())
  stopifnot(is.logical(in_transaction), length(in_transaction) == 1L,
            !is.na(in_transaction))
  con <- x$con
  public <- attr(x, "marginplyr_public_columns")
  sorted <- length(attr(x, "marginplyr_order_keys")) > 0L
  rowid_alias <- setdiff(c("rowid", "oid", "_rowid_"), tolower(public))
  if (sorted && length(rowid_alias) == 0L) {
    abort_marginplyr(
      paste0("Can't materialize this SQLite Margin order: result columns shadow ",
             "all three rowid aliases (`rowid`, `oid`, `_rowid_`)."),
      call = rlang::caller_call()
    )
  }
  # A bare persistent name can resolve to temp for dbplyr overwrite and index
  # statements. This prototype refuses that already-unsafe case before writes.
  if (is.character(name) && !inherits(name, "sql") &&
      length(name) == 1L && !temporary &&
      DBI::dbExistsTable(con, DBI::Id(schema = "temp", table = name))) {
    abort_marginplyr("A temporary table shadows the persistent destination.")
  }
  insert_from <- dbplyr::sql_render(attr(x, "marginplyr_public_query"),
                                     sql_options = sql_options)
  savepoint <- basename(tempfile(pattern = "marginplyr_savepoint_"))
  DBI::dbBegin(con, name = savepoint)
  completed <- FALSE
  on.exit({if (!completed) DBI::dbRollback(con, name = savepoint)}, add = TRUE)
  result <- dplyr::compute(
    attr(x, "marginplyr_public_anchor"), name = name,
    temporary = temporary, overwrite = overwrite,
    unique_indexes = unique_indexes, indexes = indexes, analyze = FALSE,
    ..., sql_options = sql_options, in_transaction = FALSE
  )
  table_sql <- as.character(dbplyr::remote_table(result))
  base_name <- dbplyr::remote_name(result)
  if (identical(table_sql, as.character(DBI::dbQuoteIdentifier(con, base_name)))) {
    table_id <- DBI::Id(schema = if (temporary) "temp" else "main",
                        table = base_name)
    table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_id))
    result <- dplyr::tbl(con, table_id, vars = public)
  }
  columns <- paste(DBI::dbQuoteIdentifier(con, public), collapse = ", ")
  DBI::dbExecute(con, paste0("INSERT INTO ", table_sql, " (", columns,
                             ") ", insert_from))
  if (analyze) DBI::dbExecute(con, paste("ANALYZE", table_sql))
  DBI::dbCommit(con, name = savepoint)
  completed <- TRUE
  if (!sorted) return(result)
  dbplyr::window_order(
    dplyr::arrange(result, !!dbplyr::sql(rowid_alias[[1L]]))
  )
}
'''
p.write_text(s)
