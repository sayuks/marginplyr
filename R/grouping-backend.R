# Named because two readings need it and a second copy of the vector is what
# would drift: the kind below, and the branch summary's Absorbing-backend
# handler, which has only `.data` to decide from and must not answer for a
# warning some other backend raised (ADR 0025). `RecordBatchReader` is
# deliberately absent, as it is from the kind: no verb accepts one.
arrow_input_classes <- function() {
  c("arrow_dplyr_query", "Table", "RecordBatch", "Dataset")
}

grouping_backend <- function(.data) {
  is_local <- is.data.frame(.data)
  is_dtplyr <- inherits(.data, "dtplyr_step")
  is_arrow <- inherits(.data, arrow_input_classes())
  is_sql <- inherits(.data, "tbl_lazy") && !is_dtplyr && !is_arrow

  dialect <- if (is_sql) {
    dbplyr::sql_dialect(dbplyr::remote_con(.data))
  } else {
    NULL
  }
  is_duckdb <- inherits(
    dialect,
    c("duckdb_connection", "sql_dialect_duckdb")
  )
  is_postgres <- inherits(dialect, "sql_dialect_postgres")

  kind <- if (is_local) {
    "local"
  } else if (is_dtplyr) {
    "dtplyr"
  } else if (is_arrow) {
    "arrow"
  } else if (is_duckdb) {
    "duckdb"
  } else if (is_postgres) {
    "postgres"
  } else if (is_sql) {
    "sql"
  } else {
    "other"
  }

  check_backend_version(kind, call = rlang::caller_env())
  capabilities <- backend_capabilities(kind)

  structure(
    c(
      list(kind = kind, dialect = dialect),
      capabilities
    ),
    class = "marginplyr_backend"
  )
}

backend_capabilities <- function(kind) {
  capability_names <- c(
    "collect_selection_proxy",
    "can_read_schema",
    "can_restore_factors",
    "can_encode_factor_missing_values",
    "native_grouping_sets",
    "native_duplicate_sets",
    "records_window_order",
    "invents_row_on_column_add"
  )
  enabled <- list(
    local = c(
      "can_read_schema",
      "can_restore_factors",
      "can_encode_factor_missing_values"
    ),
    dtplyr = c(
      "collect_selection_proxy",
      "can_read_schema",
      "can_restore_factors",
      "can_encode_factor_missing_values",
      # `data.table` reads a table's row count from its first column, so a
      # column-less one is always empty; the consequence in the other
      # direction is what this names -- giving a zero-column table a column
      # materialises exactly one row (#184). No other kind does that: a local
      # frame holds rows without columns natively, an arrow table gives a
      # column-less one a column and stays empty, and a SQL table with no
      # columns cannot be constructed at all. `other` is granted nothing, so
      # an unrecognised backend keeps the ordinary attachment.
      "invents_row_on_column_add"
    ),
    arrow = "can_read_schema",
    duckdb = c(
      "collect_selection_proxy",
      "can_read_schema",
      "can_restore_factors",
      "native_grouping_sets",
      "native_duplicate_sets",
      "records_window_order"
    ),
    postgres = c("native_grouping_sets", "records_window_order"),
    sql = "records_window_order",
    other = character()
  )

  profile <- enabled[[kind]]
  if (is.null(profile)) {
    stop("Unknown marginplyr backend kind: ", kind, call. = FALSE)
  }
  stats::setNames(
    as.list(capability_names %in% profile),
    capability_names
  )
}

check_backend_version <- function(kind, call) {
  requirement <- switch(
    kind,
    arrow = list(package = "arrow", version = "13.0.0"),
    dtplyr = list(package = "dtplyr", version = "1.3.2"),
    NULL
  )
  if (is.null(requirement)) {
    return(invisible(NULL))
  }

  rlang::check_installed(
    requirement$package,
    version = requirement$version,
    compare = ">=",
    reason = paste0(
      "to use marginplyr with ",
      kind,
      " backends"
    ),
    call = call
  )
}

supports_grouping_sets <- function(.data,
                                   plan = NULL,
                                   backend = grouping_backend(.data)) {
  if (!backend$native_grouping_sets) {
    return(FALSE)
  }
  if (
    !is.null(plan) &&
      identical(plan$duplicates, "keep") &&
      !backend$native_duplicate_sets
  ) {
    return(FALSE)
  }
  TRUE
}
