grouping_backend <- function(.data) {
  is_local <- is.data.frame(.data)
  is_dtplyr <- inherits(.data, "dtplyr_step")
  is_arrow <- inherits(
    .data,
    c("arrow_dplyr_query", "ArrowTabular", "Dataset")
  )
  is_sql <- inherits(.data, "tbl_lazy") && !is_dtplyr && !is_arrow

  dialect <- if (is_sql) {
    tryCatch(
      dbplyr::sql_dialect(dbplyr::remote_con(.data)),
      error = function(cnd) NULL
    )
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

  structure(
    list(
      kind = kind,
      dialect = dialect,
      collect_selection_proxy =
        is_dtplyr || is_arrow || is_duckdb,
      can_read_schema =
        is_local || is_dtplyr || is_arrow || is_duckdb,
      can_restore_factors =
        is_local || is_dtplyr || is_duckdb,
      native_grouping_sets = is_duckdb || is_postgres,
      native_duplicate_sets = is_duckdb,
      is_duckdb = is_duckdb
    ),
    class = "marginplyr_backend"
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
