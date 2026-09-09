# Named because two readings need it and a second copy of the vector is what
# would drift: the kind below, and the branch summary's Absorbing-backend
# handler, which has only `.data` to decide from and must not answer for a
# warning some other backend raised (ADR 0025). A direct `RecordBatchReader` is
# deliberately absent: inspection normalizes one before this classifier, while
# each Margin verb applies its narrower reader-source rule.
arrow_input_classes <- function() {
  c("arrow_dplyr_query", "Table", "RecordBatch", "Dataset")
}

# Whether an Arrow input's source graph contains a RecordBatchReader. A caller
# holds either a direct Arrow object or the query produced by Arrow's dplyr
# methods. Queries keep their left source in `.data` and any right sources in
# the join and union nodes; walking all three is what prevents wrapping a
# one-shot reader in a query from bypassing the reusable-input refusal.
arrow_input_has_reader_source <- function(.data) {
  if (is.null(.data)) {
    return(FALSE)
  }
  if (inherits(.data, "RecordBatchReader")) {
    return(TRUE)
  }
  if (!inherits(.data, "arrow_dplyr_query")) {
    return(FALSE)
  }

  query <- unclass(.data)
  any(vapply(
    list(
      query[[".data"]],
      query[["join"]][["right_data"]],
      query[["union_all"]][["right_data"]]
    ),
    arrow_input_has_reader_source,
    logical(1L)
  ))
}

# `is_sql` is whether this input is a SQL backend. It is equivalent to
# `dialect` being non-`NULL`, but `dialect` is the field dispatch reads, and
# its nullity does not announce itself as the test (ADR 0027).
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
      list(kind = kind, dialect = dialect, is_sql = is_sql),
      capabilities
    ),
    class = "marginplyr_backend"
  )
}

# Whether this input is a Mutable step: a dtplyr step whose root was built with
# `immutable = FALSE`. `lazy_dt()` records that argument as `implicit_copy` on
# the `dtplyr_step_first` it returns, inverted -- `TRUE` is the caller waiving
# the copy.
#
# The root is what is read, and every derived step carries the field too. Its
# value there answers a different question: a `filter()` and a `select()` over
# one mutable root both carry `TRUE`, and only one of them destroys the
# caller's table, so no step below the root separates the destructive
# derivations from the safe ones (#451). Walking to the root asks the question
# that does separate them -- whether dtplyr was given permission to write to
# the caller's table at all.
#
# The walk stops where the parent stops being a step, rather than at
# `dtplyr_step_first` by class: the root's own `parent` holds the
# `data.table` it was built from, so a class test would be a second reading of
# the same boundary.
#
# Both fields are read with `[[`, whose character index is exact, and not with
# `$`, which matches a prefix on a list -- so a dtplyr that renamed
# `implicit_copy` to something starting with it would otherwise be read rather
# than let through. A field neither name finds answers `FALSE`, both being
# non-exported dtplyr internals; `test-grouping-backends.R` pins them, which is
# what reports such a release.
mutable_dtplyr_step <- function(.data) {
  if (!inherits(.data, "dtplyr_step")) {
    return(FALSE)
  }
  root <- .data
  while (inherits(root[["parent"]], "dtplyr_step")) {
    root <- root[["parent"]]
  }
  isTRUE(root[["implicit_copy"]])
}

# The refusal a Mutable step earns, raised before any branch is built and so
# before the caller's table can be written to. ADR 0029 records why the input
# is refused rather than copied, and how much wider than the damage the line is
# drawn.
abort_mutable_dtplyr_step <- function() {
  abort_marginplyr(c(
    "{.arg .data} comes from {.code dtplyr::lazy_dt(immutable = FALSE)}.",
    i = paste0(
      "A Margin verb builds one branch per grouping set from the same step, ",
      "and data.table writes each branch to your table by reference."
    ),
    i = "Rebuild the input with {.code dtplyr::lazy_dt(immutable = TRUE)}."
  ))
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
    "invents_row_on_column_add",
    "drops_na_factor_level_on_union",
    "refuses_dictionary_sort"
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
      "invents_row_on_column_add",
      # `data.table`'s rbind drops a declared NA factor level from the result
      # and turns the values that used it into missing codes, whatever the
      # branches held -- it does so for a list of one table. Every branch list
      # this kind produces is combined that way, so a dimension carrying such
      # a level has to cross the union as character and be rebuilt after it
      # (#408). No other kind is granted this: a local frame's `bind_rows()`
      # keeps the level, and the two that lose it -- duckdb drops it on the
      # way in and arrow refuses the column outright -- lose it before any
      # branch exists, which is outside what marginplyr promises (ADR 0016).
      "drops_na_factor_level_on_union"
    ),
    arrow = c(
      "can_read_schema",
      # Arrow holds a factor as a dictionary column, and its sort refuses one
      # -- `dplyr::arrange()` on a dictionary fails the same way with no
      # marginplyr in the pipeline (#452). ADR 0018's *Factor dimensions sort
      # by level* is authoritative for what the Margin order does about it and
      # for why no other kind is granted this.
      "refuses_dictionary_sort"
    ),
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
