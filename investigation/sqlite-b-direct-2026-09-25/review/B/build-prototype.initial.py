from pathlib import Path
root=Path('/private/tmp/marginplyr-sqlite-design-20260925/variants/B')
p=root/'R/sqlite-typed-order.R'
s=p.read_text()
s=s.replace('  if (nrow(out) == 0L) {\n    for (name in names(attr(x, "marginplyr_declared_types"))) {\n      type <- attr(x, "marginplyr_declared_types")[[name]]\n      out[[name]] <- vector(type, 0L)\n    }\n  }', '''  for (name in names(attr(x, "marginplyr_declared_types"))) {
    type <- attr(x, "marginplyr_declared_types")[[name]]
    if (all(is.na(out[[name]]))) {
      out[[name]] <- rep(vector(type, 1L)[NA_integer_], nrow(out))
    }
  }''')
a=s.index('# A zero-row projection declares')
s=s[:a]+'''# THROWAWAY DESIGN PROTOTYPE B: the typed temporary table owns the schema
# repair; dbplyr owns every statement targeting the user's destination.
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
  if (!sqlite_typed_result_direct(x)) {
    return(NextMethod())
  }
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
  savepoint <- basename(tempfile(pattern = "marginplyr_savepoint_"))
  stage_name <- basename(tempfile(pattern = "marginplyr_typed_"))
  DBI::dbBegin(con, name = savepoint)
  completed <- FALSE
  on.exit({
    if (!completed) DBI::dbRollback(con, name = savepoint)
  }, add = TRUE)
  stage <- dplyr::compute(
    attr(x, "marginplyr_public_anchor"), name = stage_name,
    temporary = TRUE, analyze = FALSE, in_transaction = FALSE,
    sql_options = sql_options
  )
  stage_id <- DBI::Id(schema = "temp", table = stage_name)
  stage_sql <- as.character(DBI::dbQuoteIdentifier(con, stage_id))
  columns <- paste(DBI::dbQuoteIdentifier(con, public), collapse = ", ")
  insert <- paste0(
    "INSERT INTO ", stage_sql, " (", columns, ") ",
    dbplyr::sql_render(attr(x, "marginplyr_public_query"),
                       sql_options = sql_options)
  )
  DBI::dbExecute(con, insert)
  # Give dbplyr a qualified source so a destination with the same base name
  # cannot redirect its SELECT. The constructor has all public vars already.
  stage <- dplyr::tbl(con, stage_id, vars = public)
  if (sorted) {
    stage <- dbplyr::window_order(
      dplyr::arrange(stage, !!dbplyr::sql(rowid_alias[[1L]]))
    )
  }
  result <- dplyr::compute(
    stage, name = name, temporary = temporary, overwrite = overwrite,
    unique_indexes = unique_indexes, indexes = indexes, analyze = analyze,
    ..., sql_options = sql_options, in_transaction = FALSE
  )
  DBI::dbRemoveTable(con, stage_id)
  DBI::dbCommit(con, name = savepoint)
  completed <- TRUE
  if (!sorted) return(result)
  dbplyr::window_order(
    dplyr::arrange(result, !!dbplyr::sql(rowid_alias[[1L]]))
  )
}
'''
p.write_text(s)
p=root/'R/expand_with_margins.R'
s=p.read_text().replace('    sort_id = sort_id\n  )\n}', '''    sort_id = sort_id,
    declared_types = if (sqlite_declared_type_result(operation) &&
                         !is.null(operation$set_id_name)) {
      stats::setNames("integer", operation$set_id_name)
    } else {
      character()
    }
  )
}''')
p.write_text(s)
