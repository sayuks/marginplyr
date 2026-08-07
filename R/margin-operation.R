new_margin_operation <- function(data,
                                 backend,
                                 data_vars,
                                 data_proxy,
                                 plan,
                                 column_info,
                                 set_id_name,
                                 margin_label,
                                 margin_labels,
                                 margin_label_position,
                                 check_margin_label,
                                 sort,
                                 call) {
  structure(
    list(
      data = data,
      backend = backend,
      data_vars = data_vars,
      data_proxy = data_proxy,
      plan = plan,
      column_info = column_info,
      set_id_name = set_id_name,
      margin_label = margin_label,
      margin_labels = margin_labels,
      margin_label_position = margin_label_position,
      check_margin_label = check_margin_label,
      sort = sort,
      call = call
    ),
    class = "marginplyr_margin_operation"
  )
}

# What an executor leaves the finalizer to work with. A Margin order is a
# property of the Grouping plan, so the key itself is built by the finalizer;
# all an executor reports is `sort_id`, the Grouping set identifier column its
# query left behind for the key's Grouping bits to be read from. The finalizer
# drops that column again unless the caller asked for it with `.id`.
#
# One identifier serves every adapter because it is the only form of the key
# that stays resolvable in the `FROM` clause of the query carrying the
# `ORDER BY`. A Grouping bit is nameable in SQL only as `GROUPING(d)`, which
# resolves in the aggregate query alone, and dbplyr discards the ordering of
# any query it goes on to wrap in a subquery — which labelling the omitted
# dimensions and placing the grouping columns both do.
new_margin_execution <- function(result, sort_id = NULL) {
  structure(
    list(result = result, sort_id = sort_id),
    class = "marginplyr_margin_execution"
  )
}

margin_sorting <- function(operation) {
  !identical(operation$sort, "none")
}

# The Grouping set identifier a Margin order reads its Grouping bits from: the
# caller's `.id` when there is one, and otherwise an internal column the
# finalizer drops again. Every executor allocates it only after it has chosen
# an adapter, so that asking for an order never changes which one runs.
margin_sort_identifier <- function(operation, set_id_name, used_names) {
  if (!margin_sorting(operation)) {
    return(NULL)
  }
  if (!is.null(set_id_name)) {
    return(set_id_name)
  }
  new_margin_internal_names(
    1L,
    used_names = used_names,
    prefix = "..marginplyr_sort_"
  )
}

check_margin_operation <- function(operation) {
  stopifnot(inherits(operation, "marginplyr_margin_operation"))
  invisible(operation)
}

# Package conditions report the Margin verb the caller wrote rather than the
# internal frame that raised them. Everything else keeps its own provenance.
with_margin_error_call <- function(expr, call) {
  tryCatch(
    expr,
    error = function(cnd) {
      if (inherits(cnd, "marginplyr_error")) {
        cnd$call <- call
      }
      stop(cnd)
    }
  )
}

# One vocabulary per shared option, so a choice list and the guards that
# re-check it cannot drift apart. The public verbs still spell their defaults
# out literally because those formals are the documented signature; a test in
# test-grouping-interface.R holds each formal to the constant it mirrors.
# Verb-specific vocabularies live with the verb that owns them.
margin_duplicates_choices <- c("error", "drop", "keep")

margin_label_position_choices <- c("last", "first")

margin_sort_choices <- c("none", "last", "first")

match_margin_choice <- function(value, choices, arg_name) {
  call <- rlang::caller_call()
  force(value)
  tryCatch(
    match.arg(value, choices),
    error = function(...) {
      abort_marginplyr(
        paste0(
          "`", arg_name, "` must be one of ",
          paste0("\"", choices, "\"", collapse = ", "),
          "."
        ),
        call = call
      )
    }
  )
}

normalize_margin_options <- function(.margin_label,
                                     .margin_label_position,
                                     .check_margin_label,
                                     .duplicates,
                                     .sort,
                                     .id = NULL) {
  assert_logical_scalar(.check_margin_label)
  .id <- normalize_margin_id(.id)

  list(
    set_id_name = .id,
    margin_label = normalize_margin_label(.margin_label),
    margin_label_position = match_margin_choice(
      .margin_label_position,
      choices = margin_label_position_choices,
      arg_name = ".margin_label_position"
    ),
    check_margin_label = .check_margin_label,
    duplicates = match_margin_choice(
      .duplicates,
      choices = margin_duplicates_choices,
      arg_name = ".duplicates"
    ),
    sort = match_margin_choice(
      .sort,
      choices = margin_sort_choices,
      arg_name = ".sort"
    )
  )
}

normalize_margin_id <- function(.id) {
  if (is.null(.id)) {
    return(NULL)
  }
  if (
    !is.character(.id) ||
      length(.id) != 1L ||
      is.na(.id) ||
      !nzchar(.id)
  ) {
    abort_marginplyr(
      "`.id` must be `NULL` or one non-missing, non-empty character string."
    )
  }
  .id
}

check_margin_id_collision <- function(.id, names, where) {
  if (!is.null(.id) && .id %in% names) {
    abort_marginplyr(
      sprintf("`.id` (`%s`) conflicts with %s.", .id, where)
    )
  }
  invisible(NULL)
}

prepare_margin_operation <- function(.data,
                                     by_quo,
                                     grouping_quo,
                                     .margin_label,
                                     .margin_label_position,
                                     .check_margin_label,
                                     .duplicates,
                                     .sort,
                                     .id = NULL,
                                     validate_grouping = NULL,
                                     call = rlang::caller_call()) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))
  stopifnot(is.null(validate_grouping) || is.function(validate_grouping))

  with_margin_error_call(
    {
      options <- normalize_margin_options(
        .margin_label = .margin_label,
        .margin_label_position = .margin_label_position,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates,
        .sort = .sort,
        .id = .id
      )
      set_id_name <- options$set_id_name
      .margin_label <- options$margin_label
      .margin_label_position <- options$margin_label_position
      .check_margin_label <- options$check_margin_label
      .duplicates <- options$duplicates
      .sort <- options$sort

      grouping <- prepare_grouping_plan(
        .data,
        by_quo = by_quo,
        grouping_quo = grouping_quo,
        .duplicates = .duplicates,
        validate_grouping = validate_grouping,
        validate_names = function(data_vars) {
          check_margin_id_collision(
            set_id_name,
            data_vars,
            "a source column"
          )
        },
        call = call
      )
      data <- grouping$data
      backend <- grouping$backend
      data_vars <- grouping$data_vars
      data_proxy <- grouping$data_proxy
      plan <- grouping$plan
      column_info <- margin_column_info(
        data_proxy,
        plan$dimensions,
        backend = backend
      )
      margin_labels <- resolve_margin_labels(
        .margin_label,
        dimensions = plan$dimensions
      )

      new_margin_operation(
        data = data,
        backend = backend,
        data_vars = data_vars,
        data_proxy = data_proxy,
        plan = plan,
        column_info = column_info,
        set_id_name = set_id_name,
        margin_label = .margin_label,
        margin_labels = margin_labels,
        margin_label_position = .margin_label_position,
        check_margin_label = .check_margin_label,
        sort = .sort,
        call = call
      )
    },
    call = call
  )
}

validate_margin_operation <- function(operation) {
  check_margin_operation(operation)
  with_margin_error_call(
    validate_margin_label(
      operation$data,
      dimensions = operation$plan$dimensions,
      by = operation$plan$by,
      .margin_label = operation$margin_label,
      margin_labels = operation$margin_labels,
      .check_margin_label = operation$check_margin_label,
      column_info = operation$column_info
    ),
    call = operation$call
  )
}

finalize_margin_operation <- function(operation, execution) {
  check_margin_operation(operation)
  stopifnot(inherits(execution, "marginplyr_margin_execution"))
  result <- dplyr::ungroup(execution$result)
  result <- restore_margin_factors(
    result,
    factor_info = operation$column_info$factors,
    margin_labels = operation$margin_labels,
    position = operation$margin_label_position
  )
  margin_cols <- c(
    operation$plan$by,
    operation$plan$dimensions,
    operation$set_id_name
  )
  result <- dplyr::select(
    result,
    dplyr::all_of(margin_cols),
    dplyr::everything()
  )

  order_margin_result(operation, result, execution)
}

# Ordering comes last so that the `ORDER BY` is the outermost one, and after
# factor restoration so that a factor dimension sorts by its restored levels
# rather than by the character values the branches carried.
order_margin_result <- function(operation, result, execution) {
  if (!margin_sorting(operation)) {
    return(result)
  }

  sort_id <- execution$sort_id
  # An invariant, not a Package condition (ADR-0015): an executor that reports
  # no identifier to derive the Grouping bits from would return the rows in an
  # order that only looks sorted.
  stopifnot(
    length(operation$plan$dimensions) == 0L || !is.null(sort_id)
  )

  terms <- margin_order_terms(
    plan = operation$plan,
    sort = operation$sort,
    sort_id = sort_id
  )
  if (length(terms) > 0L) {
    result <- dplyr::arrange(result, !!!terms)
  }
  if (
    !is.null(sort_id) &&
      !identical(sort_id, operation$set_id_name)
  ) {
    result <- dplyr::select(result, -dplyr::all_of(sort_id))
  }
  forget_margin_window_order(result, backend = operation$backend)
}

# Where a backend records a window ordering, `arrange()` has just written the
# key into two places, and only one of them is a Margin order. ADR 0018's rule
# -- the key must be resolvable in the `FROM` clause of the query carrying the
# `ORDER BY` -- is what splits them. The `ORDER BY` reads the Grouping set
# identifier out of `FROM`, so it survives the projection above dropping that
# column; the window ordering is rewritten by that same projection and loses
# every term naming the identifier, leaving a truncated key that orders a
# margin row by where its label falls.
#
# Recording that is worse than recording nothing, and it is also unusable:
# `compute()` replays a window ordering through `window_order()`, which takes a
# bare column name or `desc()` of one and nothing else, so the computed terms a
# Margin key is made of stop it and no sorted result can be materialized at all
# (#102). Clearing it takes nothing away, because the rows still arrive in the
# Margin order -- that is the `ORDER BY`, which this leaves alone.
forget_margin_window_order <- function(result, backend) {
  if (!backend$records_window_order) {
    return(result)
  }
  dbplyr::window_order(result)
}

# The key of ADR 0018, built from the Grouping plan alone:
#
#   is.na(by1), by1, …, bit(d1), is.na(d1), d1, …, [set_id]
#
# Fixed-key priority and the Grouping set identifier tiebreak are consequences
# of following the result's own leading grouping columns, not separate rules,
# and a composite dimension needs no special case because its columns share a
# Grouping bit.
#
# Every column in the key carries a missingness term, fixed keys included, so
# that missing values come last wherever they appear rather than wherever the
# dialect puts them. A fixed key takes no Grouping bit, because it is in every
# grouping set and never holds a Margin label.
#
# `"first"` reverses the Grouping bits alone. Missingness and values stay
# ascending, because first and last position margins and not missing values.
margin_order_terms <- function(plan, sort, sort_id) {
  terms <- unlist(
    lapply(plan$by, function(key) {
      list(margin_missing_last_expr(key), margin_column_pronoun(key))
    }),
    recursive = FALSE
  )

  for (dimension in plan$dimensions) {
    bit <- margin_grouping_bit_expr(plan, dimension, sort_id)
    if (!is.null(bit)) {
      terms <- c(terms, list(
        if (identical(sort, "first")) {
          rlang::expr(dplyr::desc(!!bit))
        } else {
          bit
        }
      ))
    }
    terms <- c(
      terms,
      list(
        margin_missing_last_expr(dimension),
        margin_column_pronoun(dimension)
      )
    )
  }

  if (!is.null(sort_id)) {
    terms <- c(terms, list(margin_column_pronoun(sort_id)))
  }
  terms
}

# One column's missingness term. Written as a comparison rather than as the
# bare `is.na()` predicate, because ordering by a boolean is not accepted by
# every dialect the portable adapter renders for.
margin_missing_last_expr <- function(column) {
  rlang::expr(dplyr::if_else(
    is.na(!!margin_column_pronoun(column)),
    1L,
    0L
  ))
}

# One dimension's Grouping bit, read from the Grouping set identifier the
# adapter left in the result. `NULL` when the plan makes the bit constant, so
# that a term with nothing to order by is not emitted.
margin_grouping_bit_expr <- function(plan, dimension, sort_id) {
  if (is.null(sort_id)) {
    return(NULL)
  }
  margin_ids <- as.integer(
    plan$set_ids[plan$grouping_masks[, dimension] == 1L]
  )
  if (
    length(margin_ids) == 0L ||
      length(margin_ids) == length(plan$set_ids)
  ) {
    return(NULL)
  }

  rlang::expr(dplyr::if_else(
    (!!margin_column_pronoun(sort_id)) %in% !!margin_ids,
    1L,
    0L
  ))
}
