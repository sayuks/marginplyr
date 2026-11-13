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

# Whether a live SQLite result needs its types at the result boundary.
# The caller supplies source columns and package-created output types.
sqlite_typed_result_needed <- function(operation, source_columns,
                                       declared_types) {
  live_sqlite_margin_result(operation) &&
    (length(declared_types) > 0L ||
       !is.null(sql_margin_source_anchor(operation$data)) ||
       length(source_columns) > 0L)
}

# `margin_order_terms()` includes `desc()` only for a Grouping bit. Negating
# that bit gives the same ascending order and can be projected as a SQL column.
sqlite_order_value <- function(term) {
  if (rlang::is_call(term, "desc", ns = "dplyr")) {
    return(rlang::expr(-(!!term[[2L]])))
  }
  term
}

# Render a direct companion with its source anchor and final aliases outside
# CTE lifting (ADR 0031). The caller supplies a prepared live SQLite query;
# keeping each outer projection also retains its source collation.
sqlite_direct_execution_sql <- function(result,
                                        con = dbplyr::remote_con(result),
                                        ..., sql_options = NULL) {
  if (!isTRUE(sql_options$cte)) {
    return(dbplyr::sql_render(
      result, con = con, ..., sql_options = sql_options
    ))
  }
  render <- function(op) {
    if (inherits(op, "lazy_union_query")) {
      return(dbplyr::sql_query_union(
        con, render(op$x),
        list(table = lapply(op$unions$table, function(table) {
          render(table$lazy_query)
        }), all = op$unions$all)
      ))
    }
    # The anchor must keep its source projection direct for RSQLite types.
    # Other arms keep their final projection over a separately rendered input.
    if (!inherits(op, "lazy_base_query") &&
          !inherits(op$x, "lazy_base_query") &&
          length(sqlite_boolean_columns(dbplyr::op_vars(op$x))) == 0L) {
      input <- dbplyr::sql_render(op$x, con = con, sql_options = sql_options)
      op$x <- dplyr::tbl(con, input, vars = dbplyr::op_vars(op$x))$lazy_query
    }
    built <- dbplyr::sql_build(op, con = con, sql_options = sql_options)
    dbplyr::sql_render(built, con = con, ..., sql_options = sql_options)
  }
  render(result$lazy_query)
}

# Builds a public lazy result with a typed companion query (ADR 0031).
# The caller passes one prepared operation, its query before and after applying
# Margin order, the matching execution, and source columns in both queries.
sqlite_typed_result <- function(operation, unsorted, ordered,
                                execution, source_columns, declared_types) {
  input_anchor <- sql_margin_source_anchor(operation$data)
  source_types <- attr(input_anchor, "marginplyr_declared_types") %||%
    character()
  carried_types <- source_types[intersect(source_columns, names(source_types))]
  # A new Margin label owns its column's type over the input's declaration.
  declared_types <- c(
    carried_types[setdiff(names(carried_types), names(declared_types))],
    declared_types
  )
  key_id <- if (length(operation$plan$set_ids) > 1L) {
    execution$sort_id
  } else {
    NULL
  }
  staged_id <- margin_staged_sort_identifier(operation, execution$sort_id)
  if (is.null(key_id) && !is.null(staged_id)) {
    unsorted <- drop_margin_staged_identifier(
      unsorted, staged_id, operation$backend
    )
  }
  terms <- if (margin_sorting(operation)) {
    margin_order_terms(
      plan = operation$plan,
      sort = operation$sort,
      sort_id = key_id
    )
  } else {
    list()
  }
  public_columns <- as.character(dplyr::tbl_vars(ordered))
  # Reuse public value columns; the remaining order terms need their own
  # projections in SQLite's compound SELECT.
  public_terms <- lapply(public_columns, margin_column_pronoun)
  key_names <- vapply(terms, function(term) {
    columns <- public_columns[vapply(public_terms, identical, logical(1), term)]
    if (length(columns) > 0L) columns[[1L]] else ""
  }, character(1))
  projected <- !nzchar(key_names)
  key_names[projected] <- new_margin_internal_names(
    sum(projected),
    used_names = c(operation$data_vars, dplyr::tbl_vars(unsorted)),
    prefix = "..marginplyr_order_",
    backend = operation$backend
  )
  keys <- stats::setNames(
    lapply(terms[projected], sqlite_order_value), key_names[projected]
  )
  staged <- if (length(keys) > 0L) {
    dplyr::mutate(unsorted, !!!keys)
  } else {
    unsorted
  }
  staged <- dplyr::select(
    staged, dplyr::all_of(public_columns), dplyr::all_of(key_names[projected])
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

  public_query <- if (length(key_names) == 0L && !is.null(input_anchor)) {
    typed_union
  } else {
    ordered
  }
  # A dropped source .env can remain in an intermediate projection.
  if (any(sql_name_needs_quoting(
    c(operation$data_vars, public_columns)
  ))) {
    ordered <- sql_quote_generated_columns(ordered)
    typed_union <- sql_quote_generated_columns(typed_union)
    public_anchor <- sql_quote_generated_columns(public_anchor)
    public_query <- sql_quote_generated_columns(public_query)
  }
  boolean_columns <- sqlite_boolean_columns(unique(c(
    operation$data_vars, public_columns
  )))
  boolean_aliases <- stats::setNames(new_margin_internal_names(
    length(boolean_columns),
    used_names = c(operation$data_vars, dplyr::tbl_vars(staged)),
    prefix = "..marginplyr_boolean_", backend = operation$backend
  ), boolean_columns)
  if (length(boolean_aliases) > 0L) {
    source <- attr(operation$data, "marginplyr_boolean_input_query") %||%
      operation$data$lazy_query
    typed_union <- sqlite_boolean_query(typed_union, source, boolean_aliases)
    public_anchor <- sqlite_boolean_query(
      public_anchor, source, boolean_aliases, public = FALSE
    )
    public_query <- sqlite_boolean_query(public_query, source, boolean_aliases)
  }
  if (length(boolean_aliases) > 0L) {
    ordered <- sqlite_boolean_public_query(ordered)
  }
  public_boolean_aliases <- boolean_aliases[
    intersect(public_columns, names(boolean_aliases))
  ]
  input_collect_errors <- attr(input_anchor, "marginplyr_collect_errors")
  driver_anchor <- !is.null(input_anchor) &&
    !identical(input_collect_errors, "dbplyr")
  collect_errors <- if (length(key_names) > 0L ||
                          length(public_boolean_aliases) > 0L ||
                          driver_anchor) {
    "driver"
  } else {
    "dbplyr"
  }
  # Source-only anchors retain delegated collection conditions when carried.
  # Declared types and identifier-safe boundaries keep direct conditions.
  anchor_collect_errors <- if (identical(collect_errors, "driver") ||
                                 length(declared_types) > 0L ||
                                 ".env" %in% public_columns) {
    "driver"
  } else {
    "dbplyr"
  }
  class(ordered) <- c("marginplyr_sqlite_typed_result", class(ordered))
  attr(ordered, "marginplyr_anchored_source") <-
    !is.null(input_anchor) || length(source_columns) > 0L
  attr(ordered, "marginplyr_collect_errors") <- collect_errors
  attr(ordered, "marginplyr_anchor_collect_errors") <- anchor_collect_errors
  attr(ordered, "marginplyr_boolean_aliases") <- public_boolean_aliases
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

# Collapse consumes the public lazy graph rather than the direct execution SQL.
# Ordinary dbplyr semantics apply after this composition boundary (ADR 0031).
#' @exportS3Method dplyr::collapse
#' @noRd
collapse.marginplyr_sqlite_typed_result <- function(x, ...) {
  class(x) <- setdiff(class(x), "marginplyr_sqlite_typed_result")
  aliases <- sqlite_boolean_query_aliases(x)
  if (length(aliases) == 0L) {
    return(dplyr::collapse(x, ...))
  }
  x <- sqlite_boolean_query(x, NULL, aliases, public = FALSE)
  result <- sqlite_boolean_projection(dplyr::collapse(x, ...), aliases)
  # Derived verbs keep final-render alias repair, without a direct companion.
  class(result) <- c("marginplyr_sqlite_typed_result", class(result))
  result
}

# Render direct execution SQL or repair aliases in a derived public query.
# Direct rendering keeps the Sent query record aligned with that SQL (ADR 0027).
#' @exportS3Method dbplyr::sql_render
#' @noRd
sql_render.marginplyr_sqlite_typed_result <- function(query, ...,
                                                      sql_options = NULL) {
  if (!sqlite_typed_result_direct(query)) {
    query <- sqlite_boolean_derived_query(query)
    return(NextMethod())
  }
  union <- attr(query, "marginplyr_typed_union")
  keys <- attr(query, "marginplyr_order_keys")
  if (length(keys) == 0L) {
    return(sqlite_direct_execution_sql(
      attr(query, "marginplyr_public_query"), ..., sql_options = sql_options
    ))
  }
  sql <- as.character(sqlite_direct_execution_sql(
    union, ..., sql_options = sql_options
  ))
  quoted <- as.character(DBI::dbQuoteIdentifier(query$con, keys))
  dbplyr::sql(paste0(sql, "\nORDER BY ", paste(quoted, collapse = ", ")))
}

# Answer the SQL options a `verb` method's `sql_options` and dbplyr's
# deprecated `cte` argument stand for, refusing both together as dbplyr's own
# method does. A supplied `cte` carries dbplyr 2.6.0's deprecation warning.
# The caller passes its own two formals, either possibly missing.
sqlite_deprecated_cte_options <- function(verb, sql_options, cte,
                                          frame = rlang::caller_env(),
                                          user_env = rlang::caller_env(2)) {
  rlang::check_exclusive(sql_options, cte, .require = FALSE, .frame = frame)
  if (missing(cte)) {
    return(sql_options)
  }
  lifecycle::deprecate_warn(
    "2.4.0", paste0(verb, "(cte)"),
    with = I(paste0(verb, "(sql_options = sql_options(cte = TRUE))")),
    env = asNamespace("dbplyr"), user_env = user_env
  )
  dbplyr::sql_options(cte = cte)
}

# Direct collection keeps a source anchor outermost and drops internal
# sort columns after reading. Boolean-word outputs also avoid a finite
# collection subquery. Other unsorted results delegate to dbplyr. All paths
# restore declared R types for all-missing columns.
#' @exportS3Method dplyr::collect
#' @noRd
collect.marginplyr_sqlite_typed_result <- function(x, ..., n = Inf,
                                                   warn_incomplete = TRUE,
                                                   sql_options = NULL, cte) {
  if (!sqlite_typed_result_direct(x)) {
    return(NextMethod())
  }
  sql_options <- sqlite_deprecated_cte_options("collect", sql_options, cte)
  boolean_output <- length(attr(x, "marginplyr_boolean_aliases")) > 0L
  anchor_only <- isTRUE(attr(x, "marginplyr_anchored_source")) &&
    length(attr(x, "marginplyr_order_keys")) == 0L && !boolean_output
  if (length(attr(x, "marginplyr_order_keys")) == 0L &&
        !boolean_output && !anchor_only) {
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
      if (!anchor_only) {
        n <- trunc(n)
      }
    }
    sql <- dbplyr::sql_render(x, sql_options = sql_options)
    if (n >= 0) {
      sql <- dbplyr::sql(paste0(
        as.character(sql),
        "\nLIMIT ", format(n, scientific = FALSE, trim = TRUE)
      ))
    }
    out <- withCallingHandlers(
      dbplyr::db_collect(
        x$con, sql, n = n, warn_incomplete = warn_incomplete, ...
      ),
      error = function(cnd) {
        # Keep dbplyr's wrapper and its original driver cause at this boundary.
        if (identical(attr(x, "marginplyr_collect_errors"), "dbplyr")) {
          cli::cli_abort("Failed to collect lazy table.", parent = cnd)
        }
      }
    )
  }
  out <- out[attr(x, "marginplyr_public_columns")]
  # `db_collect()` returns DBI's data frame; dbplyr's `collect()` returns a
  # tibble, which every path here matches.
  out <- tibble::as_tibble(out)
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

# Whether SQLite can bind a bare relation name without reading its rows.
# The caller supplies the unquoted name inside explicit compute.
sqlite_compute_name_resolves <- function(con, table, conservative = FALSE) {
  quoted <- DBI::dbQuoteIdentifier(con, table)
  tryCatch({
    # Preparing EXPLAIN also resolves eponymous virtual tables, whose names
    # need not appear in any SQLite schema catalog.
    DBI::dbGetQuery(con, paste("EXPLAIN SELECT 1 FROM", quoted), n = 1L)
    TRUE
  }, error = function(cnd) {
    # A view can resolve successfully but fail to prepare its own input.
    if (identical(conditionMessage(cnd), paste0("no such table: ", table))) {
      return(FALSE)
    }
    if (conservative) return(TRUE)
    stop(cnd)
  })
}

# Whether the SELECT's virtual reads can be ruled out for these sources.
# The caller supplies FROM operands and VOpen identities on this connection.
sqlite_virtual_sources_clear <- function(con, sources, virtual_reads) {
  for (source in sources) {
    probe <- tryCatch(
      DBI::dbGetQuery(con, paste("EXPLAIN SELECT * FROM", source)),
      error = function(cnd) NULL
    )
    # VOpen carries the virtual table object, shared by prepared statements.
    id <- probe$p4[probe$opcode == "VOpen"]
    if (length(id) != 1L || !grepl("^vtab:", id) || id %in% virtual_reads) {
      return(FALSE)
    }
  }
  TRUE
}

# Whether virtual-table access can be separated from the overwrite target.
# The caller holds the temporary b-tree roots and VOpen operands of the SELECT.
sqlite_temp_virtual_safe <- function(con, table, roots, virtual_reads) {
  tables <- DBI::dbGetQuery(con, "PRAGMA temp.table_list")
  if (!all(c("name", "type") %in% names(tables))) return(FALSE)
  catalog <- DBI::dbGetQuery(con, paste(
    "SELECT name, tbl_name, rootpage, sql FROM temp.sqlite_master",
    "WHERE type IN ('table', 'index')"
  ))
  table_key <- sql_ascii_lower(table)
  shadow <- sql_ascii_lower(tables$name[tables$type == "shadow"])
  # SQLite associates a shadow table with the name before its last underscore.
  owner <- sub("_[^_]*$", "", shadow)
  owned <- shadow[owner == table_key]
  shadow_roots <- catalog$rootpage[sql_ascii_lower(catalog$tbl_name) %in% owned]
  if (any(roots %in% shadow_roots)) {
    return(FALSE)
  }
  if (length(virtual_reads) == 0L) return(TRUE)
  virtual <- tables$name[tables$type == "virtual"]
  virtual_keys <- sql_ascii_lower(virtual)
  definition <- sql_ascii_lower(catalog$sql[match(virtual, catalog$name)])
  identifier <- paste0(
    "(?:\"(?:[^\"]|\"\")*\"|`(?:[^`]|``)*`|\\[[^]]*\\]|'(?:[^']|'')*'|[\\w$]+)"
  )
  declaration <- paste0(
    "^create virtual table\\s+", identifier, "\\s+using\\s+"
  )
  fts <- paste0(declaration, "fts[345]\\s*\\(")
  rtree <- paste0(declaration, "rtree(?:_i32)?\\s*\\(")
  # FTS5 also accepts prefixes of the content option. A column called
  # content has no assignment, and does not change where FTS reads its data.
  content_option <- "\\bc(?:o|on|ont|onte|onten|ontent)?\\s*="
  fts_args <- sub(fts, "", definition, perl = TRUE)
  private_fts <- grepl(fts, definition, perl = TRUE) &
    paste0(virtual_keys, "_content") %in% shadow &
    !grepl(content_option, fts_args, perl = TRUE)
  private <- private_fts | grepl(rtree, definition, perl = TRUE)
  candidates <- virtual[!private | virtual_keys == table_key |
                          virtual_keys %in% owner[shadow == table_key]]
  paths <- paste0("temp.", DBI::dbQuoteIdentifier(con, candidates),
                  recycle0 = TRUE)
  sqlite_virtual_sources_clear(con, paths, virtual_reads)
}

# Whether the SELECT avoids table and index PRAGMAs. Their arguments can
# resolve a temporary relation at execution without a temp Transaction opcode.
# The caller has already prepared the SELECT and holds its VOpen identities.
sqlite_pragma_sources_clear <- function(con, virtual_reads) {
  if (length(virtual_reads) == 0L) return(TRUE)
  modules <- DBI::dbGetQuery(con, "PRAGMA module_list")
  if (!"name" %in% names(modules)) return(FALSE)
  relation_pragma <- paste0(
    "^pragma_(table_(info|xinfo|list)|index_(info|xinfo|list)|",
    "foreign_key_(list|check)|(integrity|quick)_check)$"
  )
  pragmas <- modules$name[grepl(relation_pragma, sql_ascii_lower(modules$name))]
  if (length(pragmas) == 0L) return(TRUE)
  schemas <- DBI::dbGetQuery(con, "PRAGMA database_list")
  attached <- sql_ascii_lower(schemas$name)
  # SQLite falls back to eponymous modules after a table lookup in a missing
  # schema. Using that qualifier bypasses named tables without changing state.
  repeat {
    schema <- basename(tempfile("marginplyr_pragma_"))
    if (!schema %in% attached) break
  }
  paths <- paste0(DBI::dbQuoteIdentifier(con, schema), ".",
                  DBI::dbQuoteIdentifier(con, pragmas), "()")
  sqlite_virtual_sources_clear(con, paths, virtual_reads)
}

# Whether the rendered SELECT can be shown independent of the temporary target.
# The caller holds the existing table's catalog rows, including its indexes.
sqlite_temp_overwrite_safe <- function(con, catalog, insert_from) {
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN", insert_from))
  if (!all(c("opcode", "p1", "p2", "p3", "p4") %in% names(plan)) ||
        nrow(plan) == 0L) {
    return(FALSE)
  }
  # SQLite opens b-trees by root page in database 1 (temp). A covering index
  # can be opened in place of its table.
  opens <- plan$opcode %in% c("OpenRead", "ReopenIdx")
  roots <- plan$p2[opens & plan$p3 == 1L]
  if (any(roots %in% catalog$rootpage)) return(FALSE)
  uses_temp <- any(plan$opcode == "Transaction" & plan$p1 == 1L)
  virtual_reads <- unique(plan$p4[plan$opcode == "VOpen"])
  if (!all(grepl("^vtab:", virtual_reads)) ||
        !sqlite_pragma_sources_clear(con, virtual_reads)) {
    return(FALSE)
  }
  if (!uses_temp || (length(virtual_reads) == 0L &&
                       !any(catalog$rootpage == 0L))) {
    return(TRUE)
  }
  sqlite_temp_virtual_safe(con, catalog$tbl_name[[1L]], roots, virtual_reads)
}

# Admit a temporary destination and decide if its name must be published last.
# The caller owns a savepoint and supplies a resolved destination and SELECT.
sqlite_plan_temp_destination <- function(con, destination, temporary,
                                         overwrite, insert_from, call) {
  if (is.null(destination)) return(FALSE)
  parts <- dbplyr::table_path_components(destination, con)[[1L]]
  if (length(parts) == 1L) {
    if (!temporary) return(FALSE)
  } else if (!identical(tolower(parts[[1L]]), "temp")) {
    return(FALSE)
  }
  table <- dbplyr::table_path_name(destination, con)
  catalog <- DBI::dbGetQuery(con, paste(
    "SELECT type, tbl_name, rootpage FROM temp.sqlite_master",
    "WHERE tbl_name = ? COLLATE NOCASE AND type IN ('table', 'index', 'view')"
  ), params = list(table))
  if (overwrite && "table" %in% catalog$type &&
        !sqlite_temp_overwrite_safe(con, catalog, insert_from)) {
    abort_marginplyr(
      paste0(
        "Can't safely overwrite this SQLite temporary table while computing ",
        "the Margin result. Choose a different destination name."
      ),
      call = call
    )
  }
  nrow(catalog) == 0L && sqlite_compute_name_resolves(
    con, table, conservative = TRUE
  )
}

# Allocate a temp name that cannot replace an existing source binding.
# The caller owns the compute savepoint.
sqlite_new_compute_name <- function(con) {
  repeat {
    table <- basename(tempfile("marginplyr_result_"))
    if (!sqlite_compute_name_resolves(con, table)) {
      return(dbplyr::as_table_path(table, con))
    }
  }
}

# Check deferred indexes against the columns of the type anchor.
# The caller has already mapped requested columns to their private aliases.
sqlite_check_compute_indexes <- function(anchor, indexes, arg) {
  missing <- setdiff(unlist(indexes, use.names = FALSE), colnames(anchor))
  if (length(missing) > 0L) {
    rlang::abort(c(
      paste0("All columns specified through `", arg, "` must exist in `x`."),
      i = paste("Missing columns:", paste(missing, collapse = ", "))
    ))
  }
}

# Publish the filled temp destination and return its qualified path.
# The caller owns a savepoint and has checked the requested name is vacant
# in temp. Boolean columns still have their private aliases.
sqlite_publish_destination <- function(con, private, destination,
                                       legacy, unique_indexes, indexes) {
  table <- dbplyr::table_path_name(destination, con)
  # This table has no dependents. Avoid revalidating unrelated schema objects
  # and restore the connection setting even if the rename fails.
  suspendInterrupts(tryCatch({
    if (!legacy) DBI::dbExecute(con, "PRAGMA legacy_alter_table = ON")
    DBI::dbExecute(con, paste(
      "ALTER TABLE", as.character(private), "RENAME TO",
      DBI::dbQuoteIdentifier(con, table)
    ))
  }, finally = {
    if (!legacy) DBI::dbExecute(con, "PRAGMA legacy_alter_table = OFF")
  }))
  create_index <- function(index, unique) {
    tryCatch(
      DBI::dbExecute(con, dbplyr::sql_table_index(
        con, destination, index, unique = unique
      )),
      error = function(cnd) {
        rlang::abort(
          paste0("Can't create index on table `", table, "`."), parent = cnd
        )
      }
    )
  }
  for (index in unique_indexes) create_index(index, unique = TRUE)
  for (index in indexes) create_index(index, unique = FALSE)
  dbplyr::as_table_path(DBI::Id(schema = "temp", table = table), con)
}

# Restore Boolean output names after indexes have used the private aliases.
# The caller owns the destination and the materialization savepoint.
sqlite_restore_compute_names <- function(con, destination, aliases) {
  for (column in names(aliases)) {
    DBI::dbExecute(con, paste(
      "ALTER TABLE", as.character(destination), "RENAME COLUMN",
      DBI::dbQuoteIdentifier(con, aliases[[column]]), "TO",
      DBI::dbQuoteIdentifier(con, column)
    ))
  }
}

# Apply materialization within one owned SQLite savepoint (ADR 0031).
# The caller supplies a live connection and prepares the complete result in
# code; only the caller commits or rolls back an enclosing transaction.
sqlite_with_compute_savepoint <- function(con, destination, code) {
  savepoint <- basename(tempfile(pattern = "marginplyr_savepoint_"))
  released <- FALSE
  conditions <- new.env(parent = emptyenv())
  cleanup <- NULL
  step <- "rollback"
  # Acquisition can fail after acquiring ownership; retain that trigger and
  # attempt cleanup under the same protection as the ownership handoffs.
  value <- suspendInterrupts({
    result <- tryCatch({
      DBI::dbBegin(con, name = savepoint)
      value <- allowInterrupts(code(destination))
      DBI::dbCommit(con, name = savepoint)
      released <- TRUE
      value
    }, error = function(cnd) {
      conditions$error <- cnd
      NULL
    }, interrupt = function(cnd) {
      conditions$interrupt <- cnd
      NULL
    })
    coalesce_margin_interrupts({
      if (!released &&
            (!is.null(conditions$error) || !is.null(conditions$interrupt))) {
        tryCatch({
          DBI::dbExecute(con, paste(
            "ROLLBACK TO", DBI::dbQuoteIdentifier(con, savepoint)
          ))
          step <- "release"
          DBI::dbCommit(con, name = savepoint)
        }, error = function(cnd) {
          cleanup <<- cnd
        })
      }
      result
    }, conditions)
  })
  failure <- coalesce_margin_interrupts({
    if (!is.null(cleanup)) {
      rlang::error_cnd(message = paste0(
        "SQLite compute failed; its savepoint ", step, " failed: ",
        conditionMessage(cleanup)
      ), cleanup = cleanup)
    }
  }, conditions)
  # Diagnostic preparation can itself observe interruption. Attach causes only
  # after it finishes, so a cleanup failure keeps priority in that case too.
  if (!is.null(failure)) {
    failure$parent <- if (!is.null(conditions$interrupt)) {
      margin_interrupt_outcome(conditions)
    } else {
      conditions$error
    }
    stop(failure)
  }
  if (!is.null(conditions$interrupt)) {
    signal_margin_interrupt(margin_interrupt_outcome(conditions))
  }
  if (!is.null(conditions$error)) stop(conditions$error)
  value
}

# Materialize the direct result or preserve Boolean names in a derived result.
# Only the direct companion supplies declared types and Margin order (ADR 0031).
#' @exportS3Method dplyr::compute
#' @noRd
compute.marginplyr_sqlite_typed_result <- function(x, name = NULL,
                                                   temporary = TRUE,
                                                   overwrite = FALSE,
                                                   unique_indexes = list(),
                                                   indexes = list(),
                                                   analyze = TRUE, ...,
                                                   sql_options = NULL, cte,
                                                   in_transaction = FALSE) {
  direct <- sqlite_typed_result_direct(x)
  public <- if (direct) attr(x, "marginplyr_public_columns") else
    as.character(dplyr::tbl_vars(x))
  if (!direct && length(sqlite_boolean_columns(public)) == 0L) {
    return(NextMethod())
  }
  sql_options <- sqlite_deprecated_cte_options("compute", sql_options, cte)
  if (direct) {
    public_query <- attr(x, "marginplyr_public_query")
    anchor <- attr(x, "marginplyr_public_anchor")
    boolean_aliases <- attr(x, "marginplyr_boolean_aliases")
    sorted <- length(attr(x, "marginplyr_order_keys")) > 0L
  } else {
    public_query <- sqlite_boolean_derived_query(x)
    class(public_query) <- setdiff(class(public_query),
                                   "marginplyr_sqlite_typed_result")
    aliases <- sqlite_boolean_query_aliases(public_query)
    boolean_aliases <- aliases[intersect(public, names(aliases))]
    anchor <- sqlite_boolean_query(
      dplyr::filter(public_query, FALSE), NULL, aliases, public = FALSE
    )
    # This projection returns no rows. Public ORDER BY aliases are no longer
    # available after its columns become private; only the INSERT needs order.
    anchor$lazy_query$order_by <- NULL
    sorted <- FALSE
  }
  # The zero-row anchor only creates the table. A direct anchor still carries
  # the input's window ordering, which dbplyr's compute() would replay; the
  # result's own grouping and window ordering are restored after the INSERT.
  anchor$lazy_query$order_vars <- list()
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
  pronoun_output <- any(c(".data", ".env") %in% public)
  if (pronoun_output && is.null(name) && !temporary) {
    abort_marginplyr("`name` must be provided when `temporary = FALSE`.",
                     call = rlang::caller_call())
  }
  private_indexes <- function(index) {
    positions <- match(tolower(index), tolower(names(boolean_aliases)))
    matched <- !is.na(positions)
    index[matched] <- unname(boolean_aliases[positions[matched]])
    index
  }
  unique_indexes <- lapply(unique_indexes, private_indexes)
  indexes <- lapply(indexes, private_indexes)
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
  insert_from <- sqlite_direct_execution_sql(
    public_query, sql_options = sql_options
  )
  call <- rlang::caller_call()
  sqlite_with_compute_savepoint(con, destination, function(destination) {
    # Delay the public name until the last source read has finished. The
    # populated table itself is renamed, so rows are inserted only once.
    publish <- sqlite_plan_temp_destination(
      con, destination, temporary, overwrite, insert_from, call
    )
    requested <- destination
    if (publish) {
      rename_mode <- DBI::dbGetQuery(con, "PRAGMA legacy_alter_table")
      legacy <- rename_mode$legacy_alter_table
      if (length(legacy) != 1L) {
        abort_marginplyr(
          paste0(
            "Can't preserve the SQLite source under this temporary name. ",
            "Choose a different destination name."
          ),
          call = rlang::caller_call()
        )
      }
      if (!pronoun_output) {
        sqlite_check_compute_indexes(anchor, indexes, "indexes")
        sqlite_check_compute_indexes(anchor, unique_indexes, "unique_indexes")
      }
      destination <- sqlite_new_compute_name(con)
    } else if (temporary && is.null(destination)) {
      destination <- sqlite_new_compute_name(con)
    }
    create_temporary <- temporary || publish
    create_unique_indexes <- if (publish) list() else unique_indexes
    create_indexes <- if (publish) list() else indexes
    if (pronoun_output) {
      # dbplyr's compute() selects every column by symbol before creating the
      # table; tidyselect reads generated `.data` and `.env` as pronouns.
      if (is.null(destination)) {
        destination <- dbplyr::as_table_path(
          basename(tempfile(pattern = "marginplyr_result_")), con
        )
      }
      anchor_sql <- dbplyr::sql_render(anchor, sql_options = sql_options)
      created <- dbplyr::db_compute(
        con, destination, anchor_sql, temporary = create_temporary,
        overwrite = overwrite && !publish,
        unique_indexes = create_unique_indexes,
        indexes = create_indexes, analyze = FALSE, ...,
        in_transaction = FALSE
      )
      result <- dplyr::tbl(con, created, vars = public)
    } else {
      result <- dplyr::compute(
        anchor, name = destination, temporary = create_temporary,
        overwrite = overwrite && !publish,
        unique_indexes = create_unique_indexes,
        indexes = create_indexes, analyze = FALSE, ...,
        sql_options = sql_options,
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
          schema = if (create_temporary) "temp" else "main",
          table = parts[[1L]]
        ),
        con
      )
    }
    physical <- public
    if (publish) {
      positions <- match(names(boolean_aliases), public)
      physical[positions] <- unname(boolean_aliases)
    } else {
      sqlite_restore_compute_names(con, destination, boolean_aliases)
    }
    columns <- paste(DBI::dbQuoteIdentifier(con, physical), collapse = ", ")
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", as.character(destination),
      " (", columns, ") ", as.character(insert_from)
    ))
    if (publish) {
      destination <- sqlite_publish_destination(
        con, destination, requested, legacy, unique_indexes, indexes
      )
      sqlite_restore_compute_names(con, destination, boolean_aliases)
    }
    table_sql <- as.character(destination)
    if (analyze) {
      DBI::dbExecute(con, paste("ANALYZE", table_sql))
    }
    result <- dplyr::tbl(con, destination, vars = public)
    result <- dplyr::group_by(result, !!!rlang::syms(dplyr::group_vars(x)))
    if (sorted) {
      result <- dbplyr::window_order(
        dplyr::arrange(result, !!dbplyr::sql(rowid_alias[[1L]]))
      )
    } else {
      result <- dbplyr::window_order(result, !!!dbplyr::op_sort(x))
    }
    result
  })
}
