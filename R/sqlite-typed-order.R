# A live SQLite connection needs the typed source-column anchor to remain the
# first arm of the outermost UNION ALL (ADR 0031). Simulators do not execute
# queries and retain the ordinary dbplyr result path.
sqlite_typed_order_needed <- function(operation, source_columns) {
  length(source_columns) > 0L &&
    margin_sorting(operation) &&
    identical(operation$backend$kind, "sql") &&
    inherits(dbplyr::remote_con(operation$data), "SQLiteConnection")
}

# `margin_order_terms()` includes `desc()` only for a Grouping bit. Negating
# that bit gives the same ascending order and can be projected as a SQL column.
sqlite_order_value <- function(term) {
  if (rlang::is_call(term, "desc", ns = "dplyr")) {
    return(rlang::expr(-(!!term[[2L]])))
  }
  term
}

# The public query is the ordinary dbplyr result. The companion UNION is what
# direct collection executes; it carries sort columns only so SQLite can order
# without putting an outer projection around the typed anchor (ADR 0031).
sqlite_typed_order_result <- function(operation, unsorted, ordered,
                                      execution, source_columns) {
  key_id <- if (length(operation$plan$set_ids) > 1L) {
    execution$sort_id
  } else {
    NULL
  }
  terms <- margin_order_terms(
    plan = operation$plan,
    sort = operation$sort,
    sort_id = key_id
  )
  public_columns <- as.character(dplyr::tbl_vars(ordered))
  key_names <- new_margin_internal_names(
    length(terms),
    used_names = tolower(c(operation$data_vars, dplyr::tbl_vars(unsorted))),
    prefix = "..marginplyr_order_"
  )
  keys <- stats::setNames(lapply(terms, sqlite_order_value), key_names)
  staged <- dplyr::mutate(unsorted, !!!keys)
  staged <- dplyr::select(
    staged, dplyr::all_of(public_columns), dplyr::all_of(key_names)
  )
  anchor <- sql_margin_type_anchor(
    operation$data, staged, source_columns = source_columns
  )
  typed_union <- combine_margin_branches(list(anchor, staged))
  public_anchor <- sql_margin_type_anchor(
    operation$data, ordered, source_columns = source_columns
  )

  class(ordered) <- c("marginplyr_sqlite_typed_order", class(ordered))
  attr(ordered, "marginplyr_typed_union") <- typed_union
  attr(ordered, "marginplyr_public_anchor") <- public_anchor
  attr(ordered, "marginplyr_order_keys") <- key_names
  attr(ordered, "marginplyr_public_columns") <- public_columns
  attr(ordered, "marginplyr_original_query") <- ordered$lazy_query
  ordered
}

# A downstream dplyr verb changes the public lazy query. From that point the
# object follows dbplyr as ADR 0018 specifies for tables derived from a Margin
# result; the companion query is only for the direct result.
sqlite_typed_order_direct <- function(x) {
  identical(x$lazy_query, attr(x, "marginplyr_original_query"))
}

# The true result query has projected sort columns. Showing it also keeps the
# Sent query record aligned with the SQL direct collection executes (ADR 0027).
#' @exportS3Method dbplyr::sql_render
#' @noRd
sql_render.marginplyr_sqlite_typed_order <- function(query, ...) {
  if (!sqlite_typed_order_direct(query)) {
    return(NextMethod())
  }
  union <- attr(query, "marginplyr_typed_union")
  keys <- attr(query, "marginplyr_order_keys")
  sql <- as.character(dbplyr::sql_render(union, ...))
  quoted <- as.character(DBI::dbQuoteIdentifier(query$con, keys))
  dbplyr::sql(paste0(sql, "\nORDER BY ", paste(quoted, collapse = ", ")))
}

# Direct collection drops the internal sort columns after the driver has read
# the anchored compound query. `db_collect()` preserves dbplyr's finite-n and
# incomplete-result behavior.
#' @exportS3Method dplyr::collect
#' @noRd
collect.marginplyr_sqlite_typed_order <- function(x, ..., n = Inf,
                                                    warn_incomplete = TRUE,
                                                    sql_options = NULL) {
  if (!sqlite_typed_order_direct(x)) {
    return(NextMethod())
  }
  if (identical(n, Inf)) {
    n <- -1L
  }
  sql <- dbplyr::sql_render(x, sql_options = sql_options)
  out <- dbplyr::db_collect(
    x$con, sql, n = n, warn_incomplete = warn_incomplete, ...
  )
  out[attr(x, "marginplyr_public_columns")]
}

# SQLite assigns rowids in INSERT SELECT order. The first table keeps the
# compound query and its sort columns. A zero-row source projection declares
# the requested table's public column types; INSERT copies the sorted rows.
#' @exportS3Method dplyr::compute
#' @noRd
compute.marginplyr_sqlite_typed_order <- function(x, name = NULL,
                                                    temporary = TRUE,
                                                    overwrite = FALSE,
                                                    unique_indexes = list(),
                                                    indexes = list(),
                                                    analyze = TRUE, ...,
                                                    sql_options = NULL) {
  if (!sqlite_typed_order_direct(x)) {
    return(NextMethod())
  }
  public <- attr(x, "marginplyr_public_columns")
  rowid_alias <- setdiff(
    c("rowid", "oid", "_rowid_"), tolower(public)
  )
  if (length(rowid_alias) == 0L) {
    abort_marginplyr(
      paste0(
        "Can't materialize this SQLite Margin order: result columns shadow ",
        "all three rowid aliases (`rowid`, `oid`, `_rowid_`)."
      ),
      call = rlang::caller_call()
    )
  }

  con <- x$con
  stage_name <- basename(tempfile(pattern = "marginplyr_order_"))
  on.exit(try(DBI::dbRemoveTable(con, stage_name), silent = TRUE), add = TRUE)
  dbplyr::db_compute(
    con, stage_name, dbplyr::sql_render(x, sql_options = sql_options),
    temporary = TRUE, analyze = FALSE
  )
  anchor <- attr(x, "marginplyr_public_anchor")
  quote <- function(names) {
    as.character(DBI::dbQuoteIdentifier(con, names))
  }
  result <- DBI::dbWithTransaction(con, {
    result <- dplyr::compute(
      anchor,
      name = name,
      temporary = temporary,
      overwrite = overwrite,
      unique_indexes = unique_indexes,
      indexes = indexes,
      analyze = FALSE,
      ...,
      sql_options = sql_options
    )
    table_name <- quote(dbplyr::remote_name(result))
    columns <- paste(quote(public), collapse = ", ")
    keys <- paste(quote(attr(x, "marginplyr_order_keys")), collapse = ", ")
    insert <- paste0(
      "INSERT INTO ", table_name, " (", columns, ") ",
      "SELECT ", columns, " FROM ", quote(stage_name), " ORDER BY ", keys
    )
    DBI::dbExecute(con, insert)
    if (analyze) {
      DBI::dbExecute(con, paste("ANALYZE", table_name))
    }
    result
  })
  dplyr::arrange(result, !!dbplyr::sql(rowid_alias[[1L]]))
}
