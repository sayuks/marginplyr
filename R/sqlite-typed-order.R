# Whether the prepared Margin operation uses a live SQLite connection.
# The caller has already selected the backend and prepared its Grouping plan.
live_sqlite_margin_result <- function(operation) {
  identical(operation$backend$kind, "sql") &&
    inherits(dbplyr::remote_con(operation$data), "SQLiteConnection")
}

# Whether a prepared result has a SQLite driver whose empty R types need repair.
# The caller holds a prepared SQL operation and has not executed its query.
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

# Render the query direct collection runs, keeping the Sent query record
# aligned with that SQL (ADR 0027).
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
# its condition context. Both restore declared R types for all-missing columns.
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
    } else {
      # Match dbplyr's head() validation before limiting the compound query.
      utils::head(attr(x, "marginplyr_public_query"), n = n)
      n <- trunc(n)
    }
    sql <- dbplyr::sql_render(x, sql_options = sql_options)
    if (n >= 0) {
      sql <- dbplyr::sql(paste0(
        as.character(sql),
        "\nLIMIT ", format(n, scientific = FALSE, trim = TRUE)
      ))
    }
    out <- dbplyr::db_collect(
      x$con, sql, n = n, warn_incomplete = warn_incomplete, ...
    )
  }
  out <- out[attr(x, "marginplyr_public_columns")]
  for (name in names(attr(x, "marginplyr_declared_types"))) {
    if (all(is.na(out[[name]]))) {
      type <- attr(x, "marginplyr_declared_types")[[name]]
      out[[name]] <- as.vector(out[[name]], mode = type)
    }
  }
  out
}

# Resolve an explicit destination before any write. A bare persistent name
# shadowed by temp, or a bare temporary overwrite that would find main, is
# unsafe on SQLite. The caller may omit the name for dbplyr to generate one.
sqlite_compute_destination <- function(con, name, temporary, overwrite) {
  if (is.null(name)) {
    return(NULL)
  }
  destination <- dbplyr::as_table_path(name, con)
  parts <- dbplyr::table_path_components(destination, con)[[1L]]
  if (length(parts) == 1L && (!temporary || overwrite)) {
    table <- parts[[1L]]
    temp_exists <- DBI::dbExistsTable(
      con, DBI::Id(schema = "temp", table = table)
    )
    if (!temporary && temp_exists) {
      abort_marginplyr(
        paste0(
          "A temporary table shadows the persistent destination; ",
          "use an explicit schema or another name."
        ),
        call = rlang::caller_call()
      )
    }
    if (temporary && overwrite && !temp_exists &&
          DBI::dbExistsTable(con, DBI::Id(schema = "main", table = table))) {
      abort_marginplyr(
        paste0(
          "A temporary overwrite would target a table in main; ",
          "use an explicit schema or another name."
        ),
        call = rlang::caller_call()
      )
    }
  }
  destination
}

# Apply materialization within one owned SQLite savepoint. DBI's named rollback
# also releases it; only the caller commits or rolls back an enclosing
# transaction.
sqlite_with_compute_savepoint <- function(con, destination, code) {
  savepoint <- basename(tempfile(pattern = "marginplyr_savepoint_"))
  DBI::dbBegin(con, name = savepoint)
  tryCatch({
    value <- code(destination)
    DBI::dbCommit(con, name = savepoint)
    value
  }, error = function(err) {
    tryCatch(
      DBI::dbRollback(con, name = savepoint),
      error = function(cleanup) {
        rlang::abort(
          paste0(
            "SQLite compute failed; its savepoint rollback failed: ",
            conditionMessage(cleanup)
          ),
          parent = err
        )
      }
    )
    stop(err)
  })
}

# Materialize the public query directly into the typed destination (ADR 0031).
# The caller holds the unmodified direct result; later dplyr verbs delegate.
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
  if (!rlang::is_bool(temporary)) {
    abort_marginplyr("`temporary` must be TRUE or FALSE.",
                     call = rlang::caller_call())
  }
  if (!rlang::is_bool(overwrite)) {
    abort_marginplyr("`overwrite` must be TRUE or FALSE.",
                     call = rlang::caller_call())
  }
  if (!rlang::is_bool(analyze)) {
    abort_marginplyr("`analyze` must be TRUE or FALSE.",
                     call = rlang::caller_call())
  }
  if (!rlang::is_bool(in_transaction)) {
    abort_marginplyr("`in_transaction` must be TRUE or FALSE.",
                     call = rlang::caller_call())
  }
  con <- x$con
  public <- attr(x, "marginplyr_public_columns")
  sorted <- length(attr(x, "marginplyr_order_keys")) > 0L
  rowid_alias <- setdiff(c("rowid", "oid", "_rowid_"), tolower(public))
  if (sorted && length(rowid_alias) == 0L) {
    abort_marginplyr(
      paste0(
        "Can't materialize this SQLite Margin order: result columns shadow ",
        "all three rowid aliases (`rowid`, `oid`, `_rowid_`)."
      ),
      call = rlang::caller_call()
    )
  }
  destination <- sqlite_compute_destination(con, name, temporary, overwrite)
  insert_from <- dbplyr::sql_render(
    attr(x, "marginplyr_public_query"), sql_options = sql_options
  )
  columns <- paste(DBI::dbQuoteIdentifier(con, public), collapse = ", ")
  sqlite_with_compute_savepoint(con, destination, function(destination) {
    anchor <- attr(x, "marginplyr_public_anchor")
    # Leave an absent sql_options argument absent: dbplyr's deprecated cte
    # option in ... is exclusive with a supplied sql_options, even NULL.
    if (is.null(sql_options)) {
      result <- dplyr::compute(
        anchor, name = destination, temporary = temporary,
        overwrite = overwrite, unique_indexes = unique_indexes,
        indexes = indexes, analyze = FALSE, ..., in_transaction = FALSE
      )
    } else {
      result <- dplyr::compute(
        anchor, name = destination, temporary = temporary,
        overwrite = overwrite, unique_indexes = unique_indexes,
        indexes = indexes, analyze = FALSE, ..., sql_options = sql_options,
        in_transaction = FALSE
      )
    }
    if (is.null(destination)) {
      destination <- dbplyr::as_table_path(dbplyr::remote_name(result), con)
    }
    parts <- dbplyr::table_path_components(destination, con)[[1L]]
    if (length(parts) == 1L) {
      destination <- dbplyr::as_table_path(
        DBI::Id(
          schema = if (temporary) "temp" else "main", table = parts[[1L]]
        ),
        con
      )
    }
    table_sql <- as.character(destination)
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", table_sql, " (", columns, ") ", as.character(insert_from)
    ))
    if (analyze) {
      DBI::dbExecute(con, paste("ANALYZE", table_sql))
    }
    result <- dplyr::tbl(con, destination, vars = public)
    if (sorted) {
      result <- dbplyr::window_order(
        dplyr::arrange(result, !!dbplyr::sql(rowid_alias[[1L]]))
      )
    }
    result
  })
}
