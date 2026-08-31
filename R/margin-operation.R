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
# internal frame that raised them. An External condition passes through with
# its own class, diagnostic, and cause. Its Condition context is restated where
# a grouping-set branch raises one, by `with_branch_conditions()`, rather than
# here: the grouping values in that context are reported under internal column
# names, and the branch is the only frame that knows what they stand for.
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
# Verb-specific vocabularies live with the verb that owns them, and a verb that
# narrows one hands its own list down rather than being re-checked against the
# wider one: see `duplicates_choices` below.
margin_duplicates_choices <- c("error", "drop", "keep")

margin_label_position_choices <- c("last", "first")

margin_sort_choices <- c("none", "last", "first")

# An option argument admits its documented spellings and nothing else.
# `match.arg()` also resolved any unambiguous prefix of one, so `.sort = "f"`
# and `.duplicates = "k"` were accepted (#110). Nothing documents a prefix, so
# every one of them was API by accident: a later value sharing a prefix would
# have redefined what an accepted abbreviation resolves to. The one
# `match.arg()` behaviour the signatures rely on is kept: an untouched formal
# default arrives as the whole vocabulary and stands for its first entry.
#
# `match.arg(NULL, choices)` returned that first entry too, so a `NULL` written
# by a caller used to select the default silently. The `identical()` guard below
# is the whole of what refuses one now, and refusing it is a documented contract
# rather than a leftover of the rewrite (#144): the *Option arguments* section
# on `?summarize_with_margins` states it and says why an option vocabulary is
# the place `NULL` does not mean "use the default".
#
# `rlang::arg_match()` and `rlang::arg_match0()` were measured against this
# helper rather than adopted. They agree with it on every input but one -- both
# accept a permutation of the vocabulary and return its first entry, where the
# `identical()` guard requires the order too, so they are looser on exactly the
# input #210 asks about. What they would add is a "Did you mean" suggestion,
# and what they would cost is the `NULL` diagnostic naming the vocabulary,
# which the help page promises, and ownership of a sentence the tests compare
# whole. The measurements, the diagnostics, and what a migration would have to
# carry are in investigation/rlang-arg-match-for-option-arguments.md.
match_margin_choice <- function(value, choices, arg_name) {
  call <- rlang::caller_call()
  if (identical(value, choices)) {
    return(choices[[1L]])
  }
  if (rlang::is_string(value) && value %in% choices) {
    return(value)
  }
  # The vocabulary is a list of alternatives, which is the case ADR 0023 gives
  # `{.or}`: the bare comma this used to join with was one of the three
  # spellings that ADR converged, and `"error", "drop", or "keep"` is what the
  # defaults answer for three entries and `"error" or "drop"` for two. It stays
  # in the line that offers it, the vocabulary being the verb's own and not
  # something the caller decides the length of.
  abort_marginplyr(
    "{.arg {arg_name}} must be one of {.or {.val {choices}}}.",
    call = call
  )
}

# `duplicates_choices` has no default because it is the one vocabulary a verb
# may narrow, and a default here is what let the nesting verbs be validated
# against a list their own formals exclude. Every caller states the list its
# own signature documents.
normalize_margin_options <- function(.margin_label,
                                     .margin_label_position,
                                     .check_margin_label,
                                     .duplicates,
                                     .sort,
                                     duplicates_choices,
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
      choices = duplicates_choices,
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
    abort_marginplyr(paste0(
      "{.arg .id} must be {.code NULL} or one non-missing, non-empty ",
      "character string."
    ))
  }
  .id
}

# `where` names what the identifier collided with, and it arrives interpolated
# as a value: each caller writes its own phrase, so a template built from it
# would be a template bound elsewhere, which the structural gate refuses.
#
# Three of the four phrases name no subject -- `a summary output` twice and
# `a source column` -- and lose no markup by arriving that way. The nesting one
# does name one, and spells it in backticks it typed itself:
# ``nesting `.key` ``. cli does not interpret a value, so those bytes are what
# a reader sees, which are the bytes `{.arg .key}` would have rendered. What is
# lost is that the style table stops deciding it -- a later cli styling
# `{.arg}` differently would style the three subjects in this sentence's own
# template and not that one. Recorded rather than fixed: what a caller passes
# has to stay a value.
#
# `{(.id)}` for the reason `execute_margin_nest()` records: cli reads a `{}`
# expression opening with a dot as one of its own styles.
check_margin_id_collision <- function(.id, names, where) {
  if (!is.null(.id) && .id %in% names) {
    abort_marginplyr("{.arg .id} ({.var {(.id)}}) conflicts with {where}.")
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
                                     duplicates_choices,
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
        duplicates_choices = duplicates_choices,
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
        duplicates_choices = duplicates_choices,
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

  # The identifier the key reads, and `NULL` where it reads none (ADR 0018).
  # Both the terms and the position of the projection below derive from this
  # one variable, so no term can name a column the projection has taken away.
  key_id <- if (length(operation$plan$set_ids) > 1L) sort_id else NULL
  staged_id <- margin_staged_sort_identifier(operation, sort_id)

  # A staged identifier the key does not read is dropped before the ordering
  # rather than after it (ADR 0018).
  if (!is.null(staged_id) && is.null(key_id)) {
    result <- dplyr::select(result, -dplyr::all_of(staged_id))
  }

  terms <- margin_order_terms(
    plan = operation$plan,
    sort = operation$sort,
    sort_id = key_id
  )
  if (length(terms) > 0L) {
    result <- dplyr::arrange(result, !!!terms)
  }
  if (!is.null(staged_id) && !is.null(key_id)) {
    result <- dplyr::select(result, -dplyr::all_of(staged_id))
  }
  forget_margin_window_order(result, backend = operation$backend)
}

# The Grouping set identifier the finalizer drops again: the one an executor
# staged for the order alone. An identifier the caller asked for with `.id` is
# the result's own column and is never dropped.
margin_staged_sort_identifier <- function(operation, sort_id) {
  if (identical(sort_id, operation$set_id_name)) NULL else sort_id
}

# Where a backend records a window ordering, `arrange()` has written the key
# into two places and only one of them is a Margin order, so the second is
# cleared. ADR 0018's *a lazy result carries the order and records no window
# ordering* is authoritative for which is which, for why clearing it takes
# nothing away, and for the `ORDER BY` it leaves alone.
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
