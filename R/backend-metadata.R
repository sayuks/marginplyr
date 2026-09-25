get_col_names <- function(data, ...) {
  selected <- dplyr::select(.data = data, ...)
  # Drop the grouping metadata attached to dplyr's variable-name vector.
  as.character(dplyr::tbl_vars(selected))
}

# A copy of a Mutable-step graph rooted in zero-row tables, used to acquire an
# inspection selection proxy. The derived step may already hold a reference-
# writing `:=` call, so changing its root's permission is too late; replacing
# each copied root's source gives those calls isolated schema-only tables to
# write to. Row-only predicates are removed because they cannot affect column
# metadata. ADR 0029 records why Margin operations refuse rather than use this
# rewrite.
zero_row_dtplyr_proxy_input <- function(.data) {
  stopifnot(inherits(.data, "dtplyr_step"))
  step <- .data
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

# Only base arithmetic over plain numeric source columns has an output type
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
    return(all(vapply(step[["new_vars"]], function(expr) {
      if (is.null(expr) ||
            (is.atomic(expr) && length(expr) == 1L && !is.object(expr))) {
        return(TRUE)
      }
      if (is.symbol(expr) && as.character(expr) %in% names(source)) {
        return(TRUE)
      }
      dtplyr_value_stable_arithmetic(expr, numeric_columns, step[["env"]])
    }, logical(1))))
  }
  if (inherits(step, "dtplyr_step_group")) {
    return(!is.null(step[["name"]]) &&
             identical(step[["vars"]], step[["parent"]][["vars"]]))
  }
  if (inherits(step, "dtplyr_step_call")) {
    return(step[["fun"]] %in% c("setnames", "setcolorder"))
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
  if (inherits(.data, "dtplyr_step_subset")) {
    parent <- dtplyr_selection_proxy(.data[["parent"]], mutable = mutable)
    columns <- dtplyr_metadata_subset_columns(.data)
    return(data.table::as.data.table(stats::setNames(
      lapply(columns, function(column) parent[[column]]),
      .data[["vars"]]
    )))
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
    record_sent_query("selection_proxy", proxy)
    return(dplyr::collect(proxy))
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

# `carried` names the input columns that reach the result beside the Margin
# dimensions -- a fixed `.by` key, a column the verb passes through, or one it
# folds into a cell. They cross the same branch union and lose a declared NA
# level in the same place, so `factors` covers them too (#415, #421). Which
# columns those are is settled by `prepare_margin_operation()`'s
# `carried_columns`.
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

  if (!backend$can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- proxy_columns(data_proxy, read)

  prototypes <- lapply(schema[dimensions], function(x) {
    if (is.data.frame(x) || is.matrix(x)) {
      vctrs::vec_init(x, 1L)
    } else {
      x[NA_integer_]
    }
  })
  arrow_schema <- attr(data_proxy, "marginplyr_arrow_schema", exact = TRUE)
  if (!is.null(arrow_schema)) {
    # The R prototype loses Arrow widths, precision, and temporal units.
    prototypes <- Map(
      function(value, dimension) {
        arrow::Scalar$create(
          value,
          type = arrow_schema$GetFieldByName(dimension)$type
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

  list(factors = factors, prototypes = prototypes)
}
