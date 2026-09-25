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
  if (nrow(out) == 0L) {
    for (name in names(attr(x, "marginplyr_declared_types"))) {
      type <- attr(x, "marginplyr_declared_types")[[name]]
      out[[name]] <- vector(type, 0L)
    }
  }
  out
}

# Direct materialization is temporarily refused at this boundary (issue #661).
# A downstream query delegates to dbplyr; the direct result still collects.
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
  abort_marginplyr(
    paste0(
      "Direct compute() of this SQLite Margin result is temporarily disabled ",
      "because materialization can write to a different table than requested. ",
      "Use collect() to retrieve the result until destination handling is fixed."
    ),
    call = rlang::caller_call()
  )
}
