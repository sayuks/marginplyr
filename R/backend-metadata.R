# Read column names from backend metadata without applying selection rules.
# Newly produced local summary names need not be valid tidyselect names.
margin_col_names <- function(data) {
  as.character(dplyr::tbl_vars(data))
}

# Names retained anywhere in a reusable Arrow input's source graph, including
# fields hidden by selection or renaming and Hive directory keys absent from
# a Dataset schema. This reads metadata, never rows.
arrow_source_names <- function(.data) {
  if (is.null(.data)) {
    return(character())
  }
  columns <- names(.data)
  if (inherits(.data, "FileSystemDataset")) {
    # Hive keys can survive WithSchema() in fragment partition expressions.
    # Reserve directory keys conservatively, using only the file-path metadata.
    directories <- unlist(
      strsplit(unique(dirname(.data$files)), "/", fixed = TRUE),
      use.names = FALSE
    )
    keys <- sub("=.*$", "", directories[grepl("=", directories, fixed = TRUE)])
    # Nonpartition directories may contain literal, unescaped percent signs.
    encoded <- keys[!grepl("%(?![[:xdigit:]]{2})", keys, perl = TRUE)]
    return(unique(c(columns, keys, utils::URLdecode(encoded))))
  }
  if (inherits(.data, "UnionDataset")) {
    return(unique(c(columns, unlist(
      lapply(.data$children, arrow_source_names), use.names = FALSE
    ))))
  }
  if (!inherits(.data, "arrow_dplyr_query")) {
    return(columns)
  }
  query <- unclass(.data)
  sources <- list(
    query[[".data"]],
    query[["join"]][["right_data"]],
    query[["union_all"]][["right_data"]]
  )
  unique(c(columns, unlist(
    lapply(sources, arrow_source_names), use.names = FALSE
  )))
}

get_col_names <- function(data, ...) {
  selected <- dplyr::select(.data = data, ...)
  # Drop the grouping metadata attached to dplyr's variable-name vector.
  as.character(dplyr::tbl_vars(selected))
}

# A schema-only copy of a metadata-safe dtplyr graph. Its roots are isolated
# zero-row tables and its row predicates are removed. The caller checks
# metadata safety before using this rewrite (ADRs 0020 and 0029).
zero_row_dtplyr_proxy_input <- function(.data) {
  stopifnot(inherits(.data, "dtplyr_step"))
  if (inherits(.data, "dtplyr_step_assign")) {
    return(zero_row_dtplyr_proxy_input(.data[["parent"]]))
  }
  step <- .data
  # Expanded caches expose reference writes against shared zero-row roots.
  step[["needs_copy"]] <- TRUE
  # Downstream steps inherit cached expressions independently of their parent.
  step[["locals"]] <- list()
  inputs <- dtplyr_step_input_fields(step)
  if (length(inputs) > 0L) {
    for (field in inputs) {
      step[[field]] <- zero_row_dtplyr_proxy_input(step[[field]])
    }
    if (inherits(step, "dtplyr_step_subset") &&
          !is.null(dtplyr_metadata_subset_columns(step))) {
      step[["i"]] <- NULL
    }
  } else {
    step[["parent"]] <- utils::head(step[["parent"]], n = 0L)
  }
  step
}

# The typed selection proxy for a derived dtplyr step. The registered dtplyr
# method evaluates only isolated zero-row roots; calling it directly avoids
# routing the step through `collect()` or `as_tibble()`.
isolated_dtplyr_proxy <- function(.data) {
  proxy <- utils::head(zero_row_dtplyr_proxy_input(.data), n = 0L)
  as_data_table <- utils::getS3method(
    "as.data.table",
    "dtplyr_step",
    envir = asNamespace("data.table")
  )
  as_data_table(proxy)
}

# Only base arithmetic over known plain numeric bindings has an output type
# fixed independently of values. Other calls may derive levels or types from
# the rows, even when their zero-row evaluation returns a plausible column.
dtplyr_value_stable_arithmetic <- function(expr, columns, env) {
  if (is.numeric(expr) && length(expr) == 1L && !is.object(expr)) {
    return(TRUE)
  }
  if (is.symbol(expr)) {
    return(as.character(expr) %in% columns)
  }
  if (is.call(expr) && is.symbol(expr[[1L]])) {
    head <- as.character(expr[[1L]])
    if (head %in% c("+", "-", "*", "/") &&
          identical(get(head, envir = env, inherits = TRUE),
                    get(head, envir = baseenv()))) {
      args <- as.list(expr)[-1L]
      return(length(args) %in% c(1L, 2L) && all(vapply(args, function(arg) {
        dtplyr_value_stable_arithmetic(arg, columns, env)
      }, logical(1))))
    }
  }
  FALSE
}

# A join or set over plain atomic roots has no factor levels or class-specific
# coercion to infer from matched rows. Derived expressions are checked by the
# step predicate before this condition is used.
dtplyr_plain_roots <- function(step) {
  inputs <- dtplyr_step_input_fields(step)
  if (length(inputs) > 0L) {
    return(all(vapply(inputs, function(field) {
      dtplyr_plain_roots(step[[field]])
    }, logical(1))))
  }
  all(vapply(step[["parent"]], function(x) {
    is.atomic(x) && !is.object(x) && is.null(dim(x)) &&
      typeof(x) %in% c("logical", "integer", "double", "character")
  }, logical(1)))
}

# Even plain join keys can promote from integer to double when real rows
# match. Require matching input types before using an empty-input result.
dtplyr_join_set_types_match <- function(step) {
  tryCatch({
    left <- dtplyr_selection_proxy(step[["parent"]], mutable = TRUE)
    right <- dtplyr_selection_proxy(step[["parent2"]], mutable = TRUE)
    if (inherits(step, "dtplyr_step_join")) {
      on <- step[["on"]]
      if (!is.list(on) || length(on[["x"]]) != length(on[["y"]]) ||
            !all(on[["x"]] %in% names(left)) ||
            !all(on[["y"]] %in% names(right))) {
        return(FALSE)
      }
      left_types <- vapply(on[["x"]], function(col) {
        typeof(left[[col]])
      }, character(1), USE.NAMES = FALSE)
      right_types <- vapply(on[["y"]], function(col) {
        typeof(right[[col]])
      }, character(1), USE.NAMES = FALSE)
      return(identical(left_types, right_types))
    }
    identical(
      unname(vapply(left, typeof, character(1))),
      unname(vapply(right, typeof, character(1)))
    )
  }, error = function(...) FALSE)
}

# A subset is metadata preserving only when `j` names existing columns. Its
# `i` may be any row predicate; the proxy below never evaluates it.
dtplyr_metadata_subset_columns <- function(step) {
  parent_vars <- step[["parent"]][["vars"]]
  j <- step[["j"]]
  if (is.null(j)) {
    if (identical(step[["vars"]], parent_vars)) {
      return(parent_vars)
    }
    return(NULL)
  }
  if (!is.call(j)) {
    return(NULL)
  }
  if (identical(j[[1L]], as.name(":=")) && length(j) == 3L &&
        is.null(j[[3L]])) {
    removed <- j[[2L]]
    if (is.call(removed) && identical(removed[[1L]], as.name("c"))) {
      removed <- as.list(removed)[-1L]
      if (!all(vapply(removed, function(x) {
        is.character(x) && length(x) == 1L
      }, logical(1)))) {
        return(NULL)
      }
      removed <- unlist(removed, use.names = FALSE)
    }
    if (is.character(removed) &&
          identical(step[["vars"]], parent_vars[!parent_vars %in% removed])) {
      return(step[["vars"]])
    }
    return(NULL)
  }
  if (!identical(j[[1L]], as.name("."))) {
    return(NULL)
  }
  columns <- as.list(j)[-1L]
  if (length(columns) != length(step[["vars"]]) ||
        !all(vapply(columns, is.symbol, logical(1)))) {
    return(NULL)
  }
  names <- vapply(columns, as.character, character(1))
  if (!all(names %in% parent_vars)) {
    return(NULL)
  }
  names
}

# Whether a dtplyr step's typed metadata can be read without source rows.
# A mutate directly over a root admits constants, column copies, and the
# arithmetic grammar above; later steps may have recast a root column.
dtplyr_metadata_safe_step <- function(step) {
  inputs <- dtplyr_step_input_fields(step)
  if (!all(vapply(inputs, function(field) {
    dtplyr_metadata_safe_step(step[[field]])
  }, logical(1)))) {
    return(FALSE)
  }
  if (inherits(step, "dtplyr_step_first")) {
    return(TRUE)
  }
  if (inherits(step, "dtplyr_step_mutate")) {
    parent <- step[["parent"]]
    if (!inherits(parent, "dtplyr_step_first")) {
      return(FALSE)
    }
    source <- parent[["parent"]]
    numeric_columns <- names(source)[vapply(source, function(x) {
      is.numeric(x) && !is.object(x)
    }, logical(1))]
    updates <- step[["new_vars"]]
    columns <- names(source)
    scalar_columns <- character()
    for (i in seq_along(updates)) {
      column <- names(updates)[[i]]
      expr <- updates[[i]]
      if (is.null(expr)) {
        columns <- setdiff(columns, column)
        numeric_columns <- setdiff(numeric_columns, column)
        scalar_columns <- setdiff(scalar_columns, column)
        next
      }
      # Braced mutations retain destination columns until the final write,
      # including after a local NULL update.
      existing <- column %in% names(source)
      if (is.atomic(expr) && length(expr) == 1L && !is.object(expr)) {
        scalar <- TRUE
        numeric <- is.numeric(expr)
      } else if (is.symbol(expr) && as.character(expr) %in% columns) {
        scalar <- as.character(expr) %in% scalar_columns
        numeric <- as.character(expr) %in% numeric_columns
      } else {
        if (!dtplyr_value_stable_arithmetic(
          expr, numeric_columns, step[["env"]]
        )) {
          return(FALSE)
        }
        scalar <- dtplyr_value_stable_arithmetic(
          expr, intersect(numeric_columns, scalar_columns), step[["env"]]
        )
        numeric <- TRUE
      }
      # Scalar writes, including aliases, can retain stale zero-row metadata.
      if (existing && scalar) {
        return(FALSE)
      }
      columns <- union(columns, column)
      scalar_columns <- setdiff(scalar_columns, column)
      if (scalar) {
        scalar_columns <- union(scalar_columns, column)
      }
      numeric_columns <- setdiff(numeric_columns, column)
      if (numeric) {
        numeric_columns <- union(numeric_columns, column)
      }
    }
    return(TRUE)
  }
  # Grouping and cached-table aliases retain their parent's column metadata.
  if (inherits(step, c("dtplyr_step_group", "dtplyr_step_assign"))) {
    return(identical(step[["vars"]], step[["parent"]][["vars"]]))
  }
  if (inherits(step, "dtplyr_step_call")) {
    return(step[["fun"]] %in% c("copy", "setnames", "setcolorder", "setorder"))
  }
  if (inherits(step, "dtplyr_step_subset")) {
    return(!is.null(dtplyr_metadata_subset_columns(step)))
  }
  if (inherits(step, c("dtplyr_step_join", "dtplyr_step_set"))) {
    return(dtplyr_plain_roots(step) && dtplyr_join_set_types_match(step))
  }
  FALSE
}

# The caller can explicitly materialize a step when its derived schema cannot
# be established without executing upstream operations on source rows.
abort_unsafe_dtplyr_metadata <- function() {
  abort_marginplyr(c(
    paste0(
      "Can't safely determine column metadata for {.arg .data} ",
      "from this dtplyr step."
    ),
    i = paste0(
      "Obtaining its types or factor levels here could evaluate upstream ",
      "operations on source rows."
    ),
    i = paste0(
      "If you want that work to happen now, collect the input with ",
      "{.code dplyr::collect()} and pass the resulting data frame."
    )
  ))
}

# A faithful typed proxy for a dtplyr step, or a refusal before its source rows
# run. `mutable` is true only for inspection; Margin verbs refuse that input
# before this function is called.
dtplyr_selection_proxy <- function(.data, mutable = FALSE) {
  if (!dtplyr_metadata_safe_step(.data)) {
    abort_unsafe_dtplyr_metadata()
  }
  if (inherits(.data, c("dtplyr_step_group", "dtplyr_step_assign"))) {
    return(dtplyr_selection_proxy(.data[["parent"]], mutable = mutable))
  }
  if (inherits(.data, "dtplyr_step_subset")) {
    parent <- dtplyr_selection_proxy(.data[["parent"]], mutable = mutable)
    columns <- dtplyr_metadata_subset_columns(.data)
    # The list converter refuses `.SD`; assign names only on the fresh proxy.
    proxy <- data.table::as.data.table(
      lapply(unname(columns), function(column) parent[[column]])
    )
    data.table::setnames(proxy, .data[["vars"]])
    return(proxy)
  }
  if (mutable || !inherits(.data, "dtplyr_step_first")) {
    return(tryCatch(
      isolated_dtplyr_proxy(.data),
      error = function(...) abort_unsafe_dtplyr_metadata()
    ))
  }
  grouping_selection_proxy(.data)
}

grouping_selection_proxy <- function(.data,
                                     backend = grouping_backend(.data)) {
  if (identical(backend$kind, "arrow")) {
    schema <- arrow::schema(.data)
    proxy <- as.data.frame(schema)
    # Keep the same snapshot's physical types for omitted Margin dimensions.
    attr(proxy, "marginplyr_arrow_schema") <- schema
    return(proxy)
  }
  if (backend$collect_selection_proxy) {
    proxy <- utils::head(.data, n = 0L)
    return(with_sent_query("selection_proxy", proxy, dplyr::collect(proxy)))
  }
  .data
}

# The selection proxy's columns, as a plain named list.
#
# `[` is not read here, and a data frame subclass is the reason. The public API
# admits any object dplyr can group (#77), so the proxy for a local backend is
# the caller's own object, and a subclass is free to give `[` other semantics:
# `data.table`'s reads a character index as a join key and errors rather than
# selecting columns. Reading one name at a time is also what keeps this from
# constructing an object of the subclass at all -- the list below is what the
# metadata is read from, so no subclass behaviour reaches the rest of this file.
#
# `[[` is not merely the other base operator. It is the read dplyr itself
# performs on any data frame it accepts -- `dplyr:::pull.data.frame()` is
# `.data[[var]]` -- so routing this through `dplyr::pull()` would reach the same
# operator with a tidyselect resolution on top, and a subclass that redefined it
# would already be failing inside dplyr. #77 admits exactly what dplyr can
# group, so depending on the read dplyr depends on adds no assumption.
#
# That is a boundary rather than a guarantee, and it is worth naming because the
# failure on the wrong side of it is silent. A subclass whose `[[` returned a
# wrong value rather than `NULL` would have that value read as the column's
# levels and prototype, so the Margin label would be added to the wrong factor
# and no diagnostic would say so -- the check below catches only the absent
# case. No detection is available that dplyr does not already need: a column
# read is a column read, and a class breaking it is producing wrong answers in
# the pipeline that grouped it long before reaching here. No such class is
# known, and marginplyr is not the layer that would find one.
#
# Every name is known to be a column by the time this runs, having been resolved
# against the same data by tidyselect, so a `NULL` from `[[` reports a defect
# rather than anything a caller can rewrite their way out of -- either a proxy
# that does not answer for its own columns, or a subclass whose `[[` is not
# column extraction. It is still worth stopping on, and stopping bare: silence
# here would report a column with no levels and no prototype as one the input
# declared that way.
proxy_columns <- function(data_proxy, cols) {
  columns <- stats::setNames(
    lapply(cols, function(col) data_proxy[[col]]),
    cols
  )
  absent <- cols[vapply(columns, is.null, logical(1))]
  if (length(absent) > 0L) {
    stop(
      "The selection proxy has no column",
      if (length(absent) == 1L) " " else "s ",
      paste0("`", absent, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  columns
}

# Typed SQL missing values for PostgreSQL dimensions. A scalar subquery over
# an empty relation is NULL with its selected column's type, including derived
# columns. The caller supplies the prepared input and resolved dimensions;
# rendering these expressions sends no query (ADR 0020).
postgres_margin_prototypes <- function(.data, dimensions) {
  con <- dbplyr::remote_con(.data)
  empty <- dplyr::filter(
    dplyr::select(.data, dplyr::all_of(dimensions)), FALSE
  )
  # dbplyr reads a generated bare `.env` reference as its lexical pronoun.
  empty_sql <- dbplyr::sql_render(sql_quote_generated_columns(empty))
  stats::setNames(lapply(dimensions, function(column) {
    dbplyr::build_sql(
      "(SELECT ", dbplyr::ident(column), " FROM (", empty_sql,
      ") AS ", dbplyr::ident("..marginplyr_type"), ")", con = con
    )
  }), dimensions)
}

# `carried` names the input columns that reach the result beside the Margin
# dimensions -- a fixed `.by` key, a column the verb passes through, or one it
# folds into a cell. They cross the same branch union and lose a declared NA
# level in the same place, so `factors` covers them too (#415, #421). Which
# columns those are is settled by `prepare_margin_operation()`'s
# `carried_columns`.
#
# `packed` and `arrays` retain the dimension names whose R shapes reject a
# non-missing label, even when their omitted values use backend-native NULLs.
#
# `prototypes` stays keyed to the dimensions alone. It stands for the value a
# branch omitting a dimension writes, and only a dimension is ever omitted.
#
# No query is added. The selection proxy already holds every column for each
# kind that can restore factors, so reading more of it is a second read of the
# one snapshot ADR 0002 acquired rather than a second acquisition.
margin_column_info <- function(data_proxy,
                               dimensions,
                               backend,
                               carried = character()) {
  read <- unique(c(dimensions, carried))
  if (length(read) == 0L) {
    return(list(factors = list(), prototypes = list()))
  }

  if (identical(backend$kind, "postgres") && length(dimensions) > 0L) {
    return(list(
      factors = list(),
      prototypes = postgres_margin_prototypes(data_proxy, dimensions)
    ))
  }

  if (!backend$can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- proxy_columns(data_proxy, read)

  prototypes <- lapply(schema[dimensions], function(x) {
    if (is.data.frame(x) || (is.array(x) && length(dim(x)) > 1L)) {
      vctrs::vec_init(x, 1L)
    } else if (identical(backend$kind, "duckdb")) {
      dbplyr::sql("NULL")
    } else {
      x[NA_integer_]
    }
  })
  arrow_schema <- attr(data_proxy, "marginplyr_arrow_schema", exact = TRUE)
  if (!is.null(arrow_schema)) {
    # The R prototype loses Arrow widths, precision, and temporal units.
    prototypes <- Map(
      function(value, dimension) {
        type <- arrow_schema$GetFieldByName(dimension)$type
        if (inherits(type, c("DictionaryType", "StructType"))) {
          # A packed R missing row has a valid STRUCT parent. Non-string
          # dictionaries also lose their value type in the R factor proxy.
          # Cast a null scalar to retain the physical type and null validity.
          return(arrow::Scalar$create(NA, type = arrow::null())$cast(type))
        }
        # Arrow cannot rebuild a fixed-size-list Array from its classed R
        # proxy; a bare missing list lets the declared field type supply size.
        if (inherits(value, "arrow_fixed_size_list")) {
          value <- list(NULL)
        }
        arrow::Scalar$create(
          value,
          type = type
        )
      },
      prototypes,
      dimensions
    )
    names(prototypes) <- dimensions
  }
  factors <- if (backend$can_restore_factors) {
    lapply(
      names(schema)[vapply(schema, is.factor, logical(1))],
      function(col) {
        x <- schema[[col]]
        has_na_in_level <- anyNA(levels(x))
        list(
          col = col,
          levels = levels(x),
          ordered = is.ordered(x),
          has_na_in_level = has_na_in_level,
          preserve_missing_value = backend$can_encode_factor_missing_values,
          # Whether this column takes the encode-and-rebuild route even where
          # its Margin label is missing and so adds no level. All three terms
          # are necessary and none is implied by the others: only an NA level
          # is at risk, only a union that drops one puts it there, and only a
          # sentinel keeps a value on that level apart from the typed missing
          # a margin row carries, which `as.character()` spells the same way
          # (ADR 0012). Settled here so that both sides of the route --
          # `label_margin_branch()` and `restore_margin_factors()` -- read one
          # answer rather than deciding twice, which is what they would do:
          # neither is handed the backend.
          encode_missing_label = has_na_in_level &&
            backend$drops_na_factor_level_on_union &&
            backend$can_encode_factor_missing_values
        )
      }
    )
  } else {
    list()
  }

  packed <- dimensions[vapply(prototypes, is.data.frame, logical(1))]
  arrays <- dimensions[vapply(prototypes, function(x) {
    is.array(x) && length(dim(x)) > 1L
  }, logical(1))]
  if (identical(backend$kind, "duckdb")) {
    # Retain R shapes for label validation before replacing omitted values.
    # DuckDB resolves SQL NULL against the included branch's SQL key type.
    prototypes <- lapply(prototypes, function(x) dbplyr::sql("NULL"))
  }
  list(
    factors = factors, prototypes = prototypes, packed = packed, arrays = arrays
  )
}
