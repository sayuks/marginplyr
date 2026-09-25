from pathlib import Path
root=Path('/private/tmp/marginplyr-sqlite-design-20260925/variants/C')
p=root/'R/expand_with_margins.R'
s=p.read_text().replace('    sort_id = sort_id\n  )','    sort_id = sort_id,\n    declared_types = if (is.null(operation$set_id_name)) character() else stats::setNames("integer", operation$set_id_name)\n  )')
p.write_text(s)
p=root/'R/sqlite-typed-order.R';s=p.read_text()
s=s.replace('  if (nrow(out) == 0L) {','  if (TRUE) {').replace('      out[[name]] <- vector(type, 0L)','      out[[name]] <- switch(type, integer = as.integer(out[[name]]), double = as.double(out[[name]]), character = as.character(out[[name]]))')
s=s[:s.index('compute.marginplyr_sqlite_typed_result <- function')]+'''compute.marginplyr_sqlite_typed_result <- function(x, ..., in_transaction = FALSE) {
  if (!sqlite_typed_result_direct(x)) return(NextMethod())
  # THROWAWAY C: dbplyr owns the physical destination. A savepoint still owns
  # atomicity because removing it would expose caller state on index failure.
  con <- x$con
  token <- paste0("marginplyr_c_", basename(tempfile()))
  id <- as.character(DBI::dbQuoteIdentifier(con, token))
  DBI::dbExecute(con, paste("SAVEPOINT", id))
  committed <- FALSE
  on.exit(if (!committed) {
    DBI::dbExecute(con, paste("ROLLBACK TO", id))
    DBI::dbExecute(con, paste("RELEASE", id))
  }, add = TRUE)
  public_query <- attr(x, "marginplyr_public_query")
  result <- dplyr::compute(public_query, ..., in_transaction = FALSE)
  DBI::dbExecute(con, paste("RELEASE", id))
  committed <- TRUE
  plain <- result
  class(result) <- c("marginplyr_sqlite_typed_result", class(result))
  attr(result, "marginplyr_public_query") <- plain
  attr(result, "marginplyr_public_columns") <- attr(x, "marginplyr_public_columns")
  attr(result, "marginplyr_order_keys") <- character()
  attr(result, "marginplyr_declared_types") <- attr(x, "marginplyr_declared_types")
  attr(result, "marginplyr_original_query") <- result$lazy_query
  result
}
'''
p.write_text(s)
