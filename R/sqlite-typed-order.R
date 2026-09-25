# Whether the prepared Margin operation uses a live SQLite connection.
# The caller has already selected the backend and prepared its Grouping plan.
live_sqlite_margin_result <- function(operation) {
  identical(operation$backend$kind, "sql") &&
    inherits(dbplyr::remote_con(operation$data), "SQLiteConnection")
}

# Driver types exist only on a live connection, not a dbplyr simulation.
sqlite_declared_type_result <- function(operation) {
  live_sqlite_margin_result(operation) &&
    !inherits(dbplyr::remote_con(operation$data), "TestConnection")
}

# Whether a live SQLite result can cover the union's source-column type anchor.
# The caller holds a prepared operation with its sort and identifier choices.
sqlite_final_anchor_path <- function(operation) {
  live_sqlite_margin_result(operation) &&
    (margin_sorting(operation) || !is.null(operation$set_id_name))
}

# Whether a live SQLite result needs its declared types at the result boundary.
# The caller supplies source columns and package-created output types.
sqlite_typed_result_needed <- function(operation, source_columns,
                                       declared_types) {
  live_sqlite_margin_result(operation) &&
    (length(declared_types) > 0L ||
       (length(source_columns) > 0L && margin_sorting(operation)))
}

# `margin_order_terms()` includes `desc()` only for a Grouping bit. Negating
# that bit gives the same ascending order and can be projected as a SQL column.
sqlite_order_value <- function(term) {
  if (rlang::is_call(term, "desc", ns = "dplyr")) {
    return(rlang::expr(-(!!term[[2L]])))
  }
  term
}

# Builds a public lazy result with a typed companion query (ADR 0031).
# The caller passes one prepared operation, its query before and after applying
# Margin order, the matching execution, and source columns in both queries.
sqlite_typed_result <- function(operation, unsorted, ordered,
                                execution, source_columns, declared_types) {
  terms <- if (margin_sorting(operation)) {
    margin_order_terms(
      plan = operation$plan,
      sort = operation$sort,
      sort_id = execution$sort_id
    )
  } else {
    list()
  }
  public_columns <- as.character(dplyr::tbl_vars(ordered))
  key_names <- new_margin_internal_names(
    length(terms),
    used_names = tolower(c(operation$data_vars, dplyr::tbl_vars(unsorted))),
    prefix = "..marginplyr_order_"
  )
  keys <- stats::setNames(lapply(terms, sqlite_order_value), key_names)
  staged <- if (length(keys) > 0L) {
    dplyr::mutate(unsorted, !!!keys)
  } else {
    unsorted
  }
  staged <- dplyr::select(
    staged, dplyr::all_of(public_columns), dplyr::all_of(key_names)
  )
  anchor <- sql_margin_type_anchor(
    operation$data, staged, source_columns = source_columns,
    declared_types = declared_types
  )
  typed_union <- combine_margin_branches(list(anchor, staged))
  public_anchor <- sql_margin_type_anchor(
    operation$data, ordered, source_columns = source_columns,
    declared_types = declared_types
  )

  public_query <- ordered
  class(ordered) <- c("marginplyr_sqlite_typed_result", class(ordered))
  attr(ordered, "marginplyr_typed_union") <- typed_union
  attr(ordered, "marginplyr_public_anchor") <- public_anchor
  attr(ordered, "marginplyr_order_keys") <- key_names
  attr(ordered, "marginplyr_public_columns") <- public_columns
  attr(ordered, "marginplyr_public_query") <- public_query
  attr(ordered, "marginplyr_declared_types") <- declared_types
  attr(ordered, "marginplyr_original_query") <- ordered$lazy_query
  ordered
}

# A downstream dplyr verb changes the public lazy query. From that point the
# object follows dbplyr as ADR 0018 specifies for tables derived from a Margin
# result; the companion query is only for the direct result.
sqlite_typed_result_direct <- function(x) {
  identical(x$lazy_query, attr(x, "marginplyr_original_query"))
}

# The true result query has projected sort columns. Showing it also keeps the
# Sent query record aligned with the SQL direct collection executes (ADR 0027).
#' @exportS3Method dbplyr::sql_render
#' @noRd
sql_render.marginplyr_sqlite_typed_result <- function(query, ...) {
  if (!sqlite_typed_result_direct(query)) {
    return(NextMethod())
  }
  union <- attr(query, "marginplyr_typed_union")
  keys <- attr(query, "marginplyr_order_keys")
  if (length(keys) == 0L) {
    return(dbplyr::sql_render(
      attr(query, "marginplyr_public_query"), ...
    ))
  }
  sql <- as.character(dbplyr::sql_render(union, ...))
  quoted <- as.character(DBI::dbQuoteIdentifier(query$con, keys))
  dbplyr::sql(paste0(sql, "\nORDER BY ", paste(quoted, collapse = ", ")))
}

# Direct sorted collection drops the internal sort columns after reading the
# anchored compound query. Unsorted collection keeps dbplyr's result query and
# its condition context. Both restore declared R types when no rows return.
#' @exportS3Method dplyr::collect
#' @noRd
collect.marginplyr_sqlite_typed_result <- function(x, ..., n = Inf,
                                                   warn_incomplete = TRUE,
                                                   sql_options = NULL) {
  if (!sqlite_typed_result_direct(x)) {
    return(NextMethod())
  }
  if (length(attr(x, "marginplyr_order_keys")) == 0L) {
    out <- dplyr::collect(
      attr(x, "marginplyr_public_query"), ..., n = n,
      warn_incomplete = warn_incomplete, sql_options = sql_options
    )
  } else {
    if (identical(n, Inf)) {
      n <- -1L
    }
    sql <- dbplyr::sql_render(x, sql_options = sql_options)
    out <- dbplyr::db_collect(
      x$con, sql, n = n, warn_incomplete = warn_incomplete, ...
    )
  }
  out <- out[attr(x, "marginplyr_public_columns")]
  if (nrow(out) == 0L) {
    for (name in names(attr(x, "marginplyr_declared_types"))) {
      type <- attr(x, "marginplyr_declared_types")[[name]]
      out[[name]] <- vector(type, 0L)
    }
  }
  out
}

# A zero-row projection declares the requested table's public column types;
# INSERT fills it with the requested result. Sorted results stage the compound
# query first, then insert in key order so SQLite rowids preserve Margin order.
#' @exportS3Method dplyr::compute
#' @noRd
compute.marginplyr_sqlite_typed_result <- function(x, name = NULL,
                                                   temporary = TRUE,
                                                   overwrite = FALSE,
                                                   unique_indexes = list(),
                                                   indexes = list(),
                                                   analyze = TRUE, ...,
                                                   sql_options = NULL) {
  if (!sqlite_typed_result_direct(x)) {
    return(NextMethod())
  }
  if (length(attr(x, "marginplyr_order_keys")) == 0L) {
    con <- x$con
    quote <- function(names) {
      as.character(DBI::dbQuoteIdentifier(con, names))
    }
    return(DBI::dbWithTransaction(con, {
      result <- dplyr::compute(
        attr(x, "marginplyr_public_anchor"), name = name,
        temporary = temporary, overwrite = overwrite,
        unique_indexes = unique_indexes, indexes = indexes,
        analyze = FALSE, ..., sql_options = sql_options
      )
      table_name <- quote(dbplyr::remote_name(result))
      columns <- paste(quote(attr(x, "marginplyr_public_columns")),
                       collapse = ", ")
      insert <- paste0(
        "INSERT INTO ", table_name, " (", columns, ") ",
        as.character(dbplyr::sql_render(
          attr(x, "marginplyr_public_query"), sql_options = sql_options
        ))
      )
      DBI::dbExecute(con, insert)
      if (analyze) {
        DBI::dbExecute(con, paste("ANALYZE", table_name))
      }
      result
    }))
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
