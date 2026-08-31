# Options the verb once had. A caller following older material writes them as
# if they were still arguments, and `...` would otherwise accept the value as
# an ordinary summary and return a constant column. Each entry carries its own
# guidance, because what replaces a removed option is specific to that option
# and a shared sentence parameterized by the name could say none of them.
#
# `.sort` was here until ADR 0018 returned it as a live argument. A removed
# option that becomes live leaves this list rather than staying as a second
# answer, because a name the verb has matches its own formal and never reaches
# `...`.
removed_summary_options <- list(
  .groups = "Margin-summary results are always ungrouped."
)

# Every dot-prefixed name the summary verb answers to. `...` sits before them
# in the signature, so R matches them exactly and a name that reaches `...`
# was either misspelled or spliced in.
#
# `.data` and `...` are excluded. Neither can reach `...` as a mistaken option
# name — `.data` matches its own formal exactly, and `...` is not a name a
# caller can write — so keeping them would only widen the near-miss net over
# ordinary output names, which is how `.date` came to resemble `.data`.
summary_option_names <- function() {
  formal_names <- names(formals(summarize_with_margins))
  setdiff(formal_names[startsWith(formal_names, ".")], c(".data", "..."))
}

# Leading-dot names a caller writes on purpose, exempt from the net below even
# though each sits one character from an option. `.group` is one deletion from
# the removed `.groups`, and a group label column is the likelier reading of it.
#
# The exemption is by name because no rule over the distance separates these
# from the mistakes: `.duplicate` is the same one-character deletion from
# `.duplicates` and is worth catching. What differs is whether callers write
# the name deliberately, which only a list can record. The cost is that a
# caller who typed `.group` meaning `.groups` gets a column instead of the
# guidance — accepted, because the name is common enough that the net cost
# landed on people who meant it.
summary_output_name_exemptions <- c(".group")

# Only dot-prefixed names are examined, and only against an exact match or a
# one-character difference: that catches the pluralizations real callers
# write (`.margin_labels`, `.groupings`, `.duplicate`) while leaving ordinary
# leading-dot output names such as `.n` alone.
nearest_summary_option <- function(name, known_options) {
  if (name %in% known_options) {
    return(name)
  }
  # After the exact match, so a name here would still be answered if it ever
  # became an option — the exemption covers resemblance, not the option itself.
  if (name %in% summary_output_name_exemptions) {
    return(NULL)
  }
  distances <- utils::adist(name, known_options)[1L, ]
  nearest <- known_options[distances <= 1L]
  if (length(nearest) == 0L) {
    return(NULL)
  }
  nearest[[1L]]
}

check_option_named_summaries <- function(dots) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    return(invisible(NULL))
  }
  candidates <- unique(dot_names[startsWith(dot_names, ".")])
  if (length(candidates) == 0L) {
    return(invisible(NULL))
  }

  # A call can carry more than one option-shaped name, and this loop answers
  # the first one the caller wrote. `.groups` used to be checked ahead of every
  # name by its own function, so it won regardless of where it appeared; inside
  # the shared loop it has no such standing.
  #
  # Written order is the rule rather than an ordering over the kinds of match,
  # because a caller who wrote two of these has to fix both, and the only thing
  # an ordering would change is which one they are sent to first. Reading in
  # written order keeps that walk down the call instead of jumping around it.
  known_options <- c(summary_option_names(), names(removed_summary_options))
  for (name in candidates) {
    matched <- nearest_summary_option(name, known_options)
    if (is.null(matched)) {
      next
    }
    # Both messages name what the caller wrote, not only what it resembles: a
    # caller who wrote `.sorts` never wrote `.sort`, and an error naming only
    # the option they were reaching for leaves them looking for a word that is
    # not in their code.
    if (matched %in% names(removed_summary_options)) {
      # Two `abort_marginplyr()` calls rather than one: the branch chooses a
      # whole main line, which ADR 0023's third amendment admits, where one
      # call would need a template bound elsewhere, which its *Two rules are
      # gated* refuses. The cost is the `i` bullet the two arms share, written
      # out in each.
      #
      # That bullet is the option's own guidance, which is written in the
      # table above rather than beside either call, so it arrives interpolated
      # as a value -- a template may not be bound elsewhere, and a value may.
      # It names no subject, so it loses no markup by arriving that way, and
      # the `;` the flat form joined it with is now the break between the
      # refusal and its bullet.
      if (identical(name, matched)) {
        abort_marginplyr(c(
          "{.fun summarize_with_margins} has no {.arg {matched}} argument.",
          i = "{removed_summary_options[[matched]]}"
        ))
      }
      abort_marginplyr(c(
        paste0(
          "{.arg {name}} is not an argument of ",
          "{.fun summarize_with_margins}, and neither is the ",
          "{.arg {matched}} it resembles."
        ),
        i = "{removed_summary_options[[matched]]}"
      ))
    }
    # One string under two styles: `{.arg}` where the caller wrote it as an
    # argument name, `{.var}` for the column it became instead. The two render
    # alike, which is the point the style table separates them over -- what
    # the sentence is made of is what each one records.
    abort_marginplyr(c(
      paste0(
        "{.arg {name}} is not an argument of {.fun summarize_with_margins}, ",
        "so it was captured as a summary named {.var {name}}."
      ),
      i = "Did you mean {.arg {matched}}?",
      i = "Rename the summary if the column is intended."
    ))
  }
  invisible(NULL)
}

# The refusal every reader of a summary expression holds. R's empty argument is
# a marker, and a reader that binds it to a local raises `missingArgError` on
# the next read of that local -- `R/share.R` holds four such readers, so the
# guard runs ahead of all of them rather than inside one (#340).
#
# Every empty argument `rlang::enquos(...)` captures is refused, named or not,
# including one spliced in. What it captures no argument for is a trailing
# comma, which keeps the reading `grouping_set(region, )` already has.
#
# An unnamed one is named `..n`, which is how dplyr refers to it and what
# `name_unnamed_by_position()` below spells.
check_empty_summaries <- function(dots) {
  empty <- vapply(dots, rlang::quo_is_missing, logical(1), USE.NAMES = FALSE)
  if (!any(empty)) {
    return(invisible(NULL))
  }

  # The first one the caller wrote, which is the order
  # `check_option_named_summaries()` above answers its own candidates in.
  #
  # `name` is read from the cli template below and nowhere else, which
  # `codetools` cannot follow into.
  labels <- name_unnamed_by_position(rlang::names2(dots), "..")
  name <- labels[[which(empty)[[1L]]]] # nolint: object_usage_linter.
  # `{.arg}` rather than `{.var}`: the caller wrote this as an argument name,
  # and the refusal is what stops it becoming a column.
  abort_marginplyr(c(
    "Summary {.arg {name}} is empty.",
    i = "Remove the summary, or write the expression it computes."
  ))
}

check_summary_context_helpers <- function(dots) {
  unsupported <- unique(unlist(
    lapply(
      dots,
      function(dot) {
        find_summary_context_helpers(rlang::quo_get_expr(dot))
      }
    ),
    use.names = FALSE
  ))
  if (length(unsupported) == 0L) {
    return(invisible(NULL))
  }

  # `does not support` opens the message deliberately, which is what ADR 0019
  # asks of it.
  #
  # The helpers do not follow that phrase in the same line. They arrive alone
  # in an `i` bullet, per ADR 0023's condition 2, because how many of them the
  # caller wrote is the caller's decision.
  #
  # What the last bullet says is ADR 0019's decision. It inflects a
  # demonstrative, a noun, and two verbs, and `cli::qty()` is what carries the
  # count to all four: the vector deciding it is no longer in the line they sit
  # in.
  abort_marginplyr(c(
    "{.fun summarize_with_margins} does not support:",
    i = "{.fun {unsupported}}.",
    i = paste0(
      "{cli::qty(length(unsupported))}{?This/These} spelling{?s} {?is/are} ",
      "reserved inside a Margin summary and {?is/are} not resolved from the ",
      "calling environment."
    ),
    i = paste0(
      "These helpers describe one branch-local dplyr grouping or data mask, ",
      "but a margin result combines multiple grouping sets."
    ),
    i = paste0(
      "Use {.fun grouping_bit} or {.fun grouping_id} when identifying margin ",
      "levels."
    )
  ))
}

check_summary_group_overwrite <- function(output_names, group_vars) {
  overwritten_groups <- intersect(output_names, unique(group_vars))
  if (length(overwritten_groups) == 0L) {
    return(invisible(NULL))
  }

  # The columns arrive alone in an `i` bullet, per ADR 0023's condition 2: how
  # many of them there are is the caller's decision. `cli::qty()` is what
  # carries the count across that split, the noun it inflects no longer sitting
  # beside the vector.
  abort_marginplyr(c(
    paste0(
      "{cli::qty(length(overwritten_groups))}Summary results cannot ",
      "overwrite grouping column{?s}:"
    ),
    i = "{.var {overwritten_groups}}."
  ))
}

# The three questions to ask of the names a summary really produced, which the
# pre-execution checks can only ask of the names the static predictor could
# guess. Both execution paths ask them, and the point of asking twice is that
# the two agree: a call one backend rejects must be rejected on every other.
# Composing them here is what keeps the checks, their wording, and their order
# from drifting apart. Only `internal_names` differs between the callers,
# because each path puts columns of its own beside the summary outputs.
check_summary_output_names <- function(output_names,
                                       group_vars,
                                       internal_names,
                                       set_id_name,
                                       set_id_is_internal = FALSE) {
  # A Grouping set identifier the package allocated for itself is one of the
  # internal columns, not the caller's `.id`, and reporting it as `.id` names
  # an argument the caller never wrote and a column they cannot see.
  if (set_id_is_internal) {
    internal_names <- c(internal_names, set_id_name)
    set_id_name <- NULL
  }

  check_internal_summary_names(output_names, internal_names)
  check_summary_group_overwrite(output_names, group_vars = group_vars)
  check_margin_id_collision(set_id_name, output_names, "a summary output")
}

check_internal_summary_names <- function(output_names, internal_names) {
  conflicting_names <- intersect(output_names, internal_names)
  if (length(conflicting_names) == 0L) {
    return(invisible(NULL))
  }

  # The columns arrive alone in an `i` bullet, per ADR 0023's condition 2: how
  # many of them there are is the caller's decision. The `:` the flat form
  # already introduced them with is the break they arrive across, so the noun
  # ahead of it needs no inflection -- it was written plural whatever arrived.
  abort_marginplyr(c(
    paste0(
      "Dynamically generated summary output names conflict with internal ",
      "grouping columns:"
    ),
    i = "{.var {conflicting_names}}.",
    i = "Use different summary output names."
  ))
}

# What execution carries for the caller's summary arguments: the dots to hand
# dplyr, beside the caller's own label for each. Constructed at the one point
# both halves are final -- after every rewrite -- so a pair that stops agreeing
# in length cannot be built at all, which is an invariant rather than a Package
# condition (ADR 0015): no call produces it, and a map built from a misaligned
# pair would quote one argument's expression under another.
#
# The labels default to the dots' own, which is the truth for a caller reaching
# an adapter directly: what it passed is what it wrote. Nothing is restated
# there, because `branch_argument_map()` drops a label a rewrite left alone --
# so "no spelling to restore" needs no representation of its own, and a length
# is checked once rather than only when a second value says to.
new_summary_arguments <- function(dots,
                                  labels = summary_argument_labels(dots)) {
  stopifnot(
    is.list(dots),
    is.character(labels),
    length(labels) == length(dots)
  )
  list(dots = dots, labels = labels)
}

plan_summary_expressions <- function(dots,
                                     data_proxy,
                                     data_vars,
                                     plan,
                                     backend_kind,
                                     set_id_name,
                                     call) {
  stopifnot(inherits(plan, "margin_grouping_plan"))
  group_vars <- c(plan$by, plan$dimensions)
  # Read before anything is rewritten, which is the whole of what makes these
  # the caller's own labels: every rewrite below runs after this line, and ADR
  # 0007 has already captured the dots at the public verb.
  caller_labels <- summary_argument_labels(dots)
  selection_proxy <- dplyr::select(
    data_proxy,
    dplyr::all_of(setdiff(
      data_vars,
      unique(group_vars)
    ))
  )
  dots <- resolve_summary_selections(
    dots,
    data_proxy = data_proxy,
    data_vars = data_vars,
    group_vars = group_vars,
    normalize_across_names = FALSE,
    skip_shares = TRUE
  )
  summary_plan <- plan_share_expressions(
    dots,
    selection_proxy = selection_proxy,
    plan = plan,
    set_id_name = set_id_name,
    validate_cardinality = wraps_share_sources_in_summary(backend_kind)
  )
  # Share planning is the one step that moves a dot, so it reports where each
  # dot it produced came from and the labels are subscripted by that. Every
  # other rewrite here answers one dot with one dot in place.
  caller_labels <- caller_labels[summary_plan$origin_positions]
  summary_plan$dots <- resolve_summary_selections(
    summary_plan$dots,
    data_proxy = data_proxy,
    data_vars = data_vars,
    group_vars = group_vars,
    normalize_across_names = identical(backend_kind, "dtplyr")
  )
  if (length(summary_plan$cardinality) > 0L) {
    summary_plan$dots <- wrap_share_sources(
      summary_plan$dots,
      cardinality = summary_plan$cardinality,
      call = call,
      backend_kind = backend_kind
    )
  }
  list(
    summaries = new_summary_arguments(summary_plan$dots, caller_labels),
    requests = summary_plan$requests
  )
}

find_summary_context_helpers <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(character())
  }

  call_name <- static_call_name(expr)
  refused <- static_spelling_name(expr, "refused")
  found <- if (is.null(refused)) character() else refused

  # The arguments the mask evaluates, and the language the call evaluates. A
  # helper name the caller quoted describes no grouping this call has to
  # combine -- nothing calls it -- and refusing the call over one refused a
  # summary that only names the helper. Handing one to `eval()` is the opposite
  # case and needs the opposite answer: the helper runs, and it answers the
  # branch-local identifier this guard exists to refuse (#179).
  c(
    found,
    unlist(
      lapply(
        searched_call_parts(expr, call_name = call_name),
        find_summary_context_helpers
      ),
      use.names = FALSE
    )
  )
}

resolve_summary_selections <- function(dots,
                                       data_proxy,
                                       data_vars,
                                       group_vars,
                                       normalize_across_names = FALSE,
                                       skip_shares = FALSE) {
  selectable_vars <- setdiff(data_vars, unique(group_vars))
  selection_proxy <- dplyr::select(
    data_proxy,
    dplyr::all_of(selectable_vars)
  )

  lapply(
    dots,
    function(dot) {
      expr <- rlang::quo_get_expr(dot)
      if (
        skip_shares &&
          contains_share_helper(expr)
      ) {
        return(dot)
      }
      expr <- rewrite_summary_selections(
        expr,
        env = rlang::quo_get_env(dot),
        data_proxy = selection_proxy,
        normalize_across_names = normalize_across_names
      )
      rlang::new_quosure(expr, env = rlang::quo_get_env(dot))
    }
  ) |>
    stats::setNames(names(dots))
}

rewrite_summary_selections <- function(expr,
                                       env,
                                       data_proxy,
                                       normalize_across_names) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }

  # An injected quosure carries an environment of its own, and that is what
  # injecting one is for, so a selection inside it resolves there rather than
  # in the environment of the dot that contains it. Reading the outer one would
  # look up a name the caller never put in it.
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
  }

  # A selection the caller quoted is language data, so the walk descends past
  # it and gives the object back as it was written. Resolving it turned
  # `quote(dplyr::across(value, mean))` into
  # `quote(dplyr::across(dplyr::all_of("value"), mean))`, which is a different
  # expression to whatever the caller meant to carry (#179).
  expr <- rewrite_evaluated_call_parts(
    expr,
    function(part) {
      rewrite_summary_selections(
        part,
        env = env,
        data_proxy = data_proxy,
        normalize_across_names = normalize_across_names
      )
    }
  )

  call_name <- static_spelling_name(expr, "selection")
  if (is.null(call_name)) {
    return(expr)
  }

  # The head is qualified before either branch reads the call, so every rebuild
  # below inherits it through `rebuild_static_call()` and no branch has to
  # remember. ADR 0019's *Analysis and execution must agree* is authoritative
  # for why a recognized head is qualified at all.
  expr <- qualify_static_spelling(expr, "selection", call_name)

  if (call_name %in% c("across", "if_any", "if_all")) {
    return(rewrite_across_selection(
      expr,
      env,
      data_proxy,
      normalize_across_names = normalize_across_names,
      call_name = call_name
    ))
  }
  if (identical(call_name, "pick")) {
    return(rewrite_pick_selection(expr, env, data_proxy))
  }

  # An invariant, not a Package condition (ADR 0015): the branches above answer
  # every name the `selection` family holds, so reaching this means a spelling
  # was registered without a rewrite. Falling through to one of them instead
  # would rewrite the new spelling as whichever helper happened to be last,
  # which is a silently wrong selection rather than a missing one.
  stop(
    "No rewrite is registered for the selection helper `",
    call_name,
    "()`.",
    call. = FALSE
  )
}

# `call_name` is the caller's answer rather than one asked again here. Asking
# again would answer the same, since the shared read answers a formula as no
# name at all (#163); it would just be a second question about an expression
# the only caller enters this function having already named, and the two
# answers would then have to be kept in step by hand.
rewrite_across_selection <- function(expr,
                                     env,
                                     data_proxy,
                                     normalize_across_names,
                                     call_name) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  selection_index <- parsed$cols_index

  # One resolution, whichever way the selection was written: `parsed$cols` is
  # already the `dplyr::everything()` that an omitted `.cols` selects, and an
  # argument the caller left empty is omitted in exactly that sense. What the
  # branch decides is only where the resolved selection goes -- prepended when
  # no argument occupies `.cols`, and written back over the argument that does,
  # so an empty one keeps its position instead of being dropped from the
  # middle of the call (#174).
  selected <- resolve_summary_selection(
    parsed$cols,
    env = env,
    data_proxy = data_proxy
  )
  if (selection_index == 0L) {
    call_args <- append(
      list(.cols = summary_all_of_expr(selected, data_proxy)),
      call_args
    )
    selection_index <- 1L
  } else {
    call_args[[selection_index]] <- summary_all_of_expr(selected, data_proxy)
  }

  if (identical(call_name, "across") && normalize_across_names) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    unpack_is_false <- is.null(parsed$unpack) || isFALSE(tryCatch(
      rlang::eval_tidy(parsed$unpack, env = env),
      error = function(cnd) NULL
    ))
    function_names <- known_across_function_names(parsed)

    if (
      !is.null(parsed$names) &&
        unpack_is_false &&
        length(function_names) == 1L
    ) {
      output_names <- known_across_output_names(expr, env, data_proxy)
      if (length(output_names) == length(selected)) {
        names(selected) <- output_names
        call_args[[selection_index]] <- summary_all_of_expr(
          selected,
          data_proxy
        )

        if (
          rlang::is_call(parsed$fns, "list") &&
            length(parsed$fns) == 2L
        ) {
          call_args[[parsed$fns_index]] <- parsed$fns[[2L]]
        }
        call_args <- call_args[-parsed$names_index]
      }
    }
  }

  if (identical(call_name, "across")) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    if (!is.null(parsed$names)) {
      call_args[[parsed$names_index]] <- rlang::eval_tidy(
        parsed$names,
        env = env
      )
    }
  }

  rebuild_static_call(expr, call_args)
}

rewrite_pick_selection <- function(expr, env, data_proxy) {
  call_args <- static_call_args(expr)
  selection <- if (length(call_args) == 0L) {
    rlang::expr(dplyr::everything())
  } else {
    rlang::call2("c", !!!call_args)
  }
  selected <- resolve_summary_selection(
    selection,
    env = env,
    data_proxy = data_proxy
  )

  rebuild_static_call(expr, list(summary_all_of_expr(selected, data_proxy)))
}

resolve_summary_selection <- function(expr, env, data_proxy) {
  tidyselect::eval_select(
    rlang::new_quosure(expr, env = env),
    data = data_proxy,
    strict = TRUE,
    allow_rename = TRUE
  )
}

summary_all_of_expr <- function(selected, data_proxy) {
  source_names <- get_col_names(
    data_proxy,
    dplyr::everything()
  )[unname(selected)]
  output_names <- names(selected)
  if (!identical(output_names, source_names)) {
    source_names <- stats::setNames(source_names, output_names)
  }
  rlang::expr(dplyr::all_of(!!source_names))
}

known_summary_output_names <- function(dots, data_proxy) {
  unlist(
    lapply(
      dots,
      function(dot) {
        expr <- rlang::quo_get_expr(dot)
        env <- rlang::quo_get_env(dot)
        known_data_frame_output_names(expr, env, data_proxy)
      }
    ),
    use.names = FALSE
  )
}

known_data_frame_output_names <- function(expr, env, data_proxy) {
  if (!rlang::is_call(expr)) {
    return(character())
  }

  # Two families rather than one, because the owner differs and the owner is
  # what recognition tests: tibble owns `tibble()` and `data_frame()`, base
  # owns `data.frame()`. Neither is a Contextual helper -- nothing rewrites
  # them, and a caller who binds `tibble` gets their own function -- so what is
  # read here is only which output names the summary is going to produce
  # (ADR 0019).
  if (is_any_static_spelling_call(expr, c("tibble_frame", "base_frame"))) {
    call_args <- static_call_args(expr)
    arg_names <- names(call_args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_args))
    }
    injected_names <- vapply(
      call_args[arg_names == ""],
      known_injected_argument_name,
      character(1)
    )
    return(setdiff(
      c(arg_names[nzchar(arg_names)], injected_names[nzchar(injected_names)]),
      ".name_repair"
    ))
  }

  if (is_static_spelling_call(expr, "selection", "pick")) {
    call_args <- static_call_args(expr)
    selection <- if (length(call_args) == 0L) {
      rlang::expr(dplyr::everything())
    } else {
      rlang::call2("c", !!!call_args)
    }
    return(names(resolve_summary_selection(selection, env, data_proxy)))
  }

  if (is_static_spelling_call(expr, "selection", "across")) {
    return(known_across_output_names(expr, env, data_proxy))
  }

  character()
}

known_injected_argument_name <- function(expr) {
  if (!rlang::is_call(expr, ":=") || length(expr) != 3L) {
    return("")
  }

  # By subscript, because a name-position argument the caller left empty is R's
  # missing marker: `lhs <- expr[[2L]]` binds it and raises `missingArgError`
  # on the first read of that name (#174). It names no output, which is what
  # the fall-through below already says.
  if (
    is.character(expr[[2L]]) &&
      length(expr[[2L]]) == 1L &&
      !is.na(expr[[2L]])
  ) {
    return(expr[[2L]])
  }
  if (is_name_part(expr[[2L]])) {
    return(rlang::as_name(expr[[2L]]))
  }
  ""
}

known_across_output_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  cols_expr <- parsed$cols
  column_names <- names(resolve_summary_selection(cols_expr, env, data_proxy))

  if (is.null(parsed$names)) {
    if (rlang::is_call(parsed$fns, "list")) {
      function_names <- known_across_function_names(parsed)
      return(unlist(
        lapply(
          column_names,
          paste,
          function_names,
          sep = "_"
        ),
        use.names = FALSE
      ))
    }
    return(column_names)
  }
  names_template <- tryCatch(
    rlang::eval_tidy(parsed$names, env = env),
    error = function(cnd) NULL
  )
  if (
    !is.character(names_template) ||
      length(names_template) != 1L ||
      is.na(names_template)
  ) {
    return(character())
  }

  function_names <- known_across_function_names(parsed)
  if (length(function_names) == 0L) {
    return(character())
  }

  unlist(
    lapply(
      column_names,
      function(column) {
        vapply(
          function_names,
          function(fn) {
            expanded <- expand_across_name(names_template, column, fn, env)
            check_across_name_count(expanded, names_template, column)
          },
          character(1)
        )
      }
    ),
    use.names = FALSE
  )
}

expand_across_name <- function(template, column, function_name, env) {
  as.character(glue::glue_data(
    list(.col = column, .fn = function_name),
    template,
    .envir = env
  ))
}

# The expansion names one output per selected column, so a template that
# expands to any other number is one `across()` will reject too. That is what
# separates this from the `character()` the caller above returns: there the
# template could not be evaluated at all and the analysis simply does not
# know the names, whereas here it knows them and knows they are wrong. Saying
# so here reaches the caller before the summary is staged, rather than as a
# size error out of the query built from it (ADR-0005).
check_across_name_count <- function(expanded, template, column) {
  if (length(expanded) == 1L) {
    return(expanded)
  }

  # The template and the column both stay in the main line, under ADR 0023's
  # element-count reading of its condition 2: each is one caller subject rather
  # than a part the caller decides the count of, however long a template
  # renders. `{.code}` is what a spelling the caller typed takes, and the
  # braces inside one are inert because it arrives interpolated as a value.
  abort_marginplyr(c(
    paste0(
      "The {.fun across} {.arg .names} template {.code {template}} must ",
      "produce one name per column, but it produced {length(expanded)} for ",
      "column {.var {column}}."
    ),
    i = "Use a template that expands to a single name."
  ))
}

known_across_source_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  selected <- resolve_summary_selection(parsed$cols, env, data_proxy)
  get_col_names(data_proxy, dplyr::everything())[unname(selected)]
}

# `"1"` is what `{.fn}` expands to for a `.fns` that is one function, and for
# one that was never supplied: dplyr numbers a single function by its position
# whether the caller wrote it or took the identity default.
known_across_function_names <- function(parsed) {
  if (!rlang::is_call(parsed$fns, "list")) {
    return("1")
  }

  fns <- static_call_args(parsed$fns)
  fns_names <- names(fns)
  if (is.null(fns_names)) {
    fns_names <- rep("", length(fns))
  }
  name_unnamed_by_position(fns_names, "")
}

# Both callers name the unnamed entries of an argument list by position, which
# is how dplyr refers to them: an argument forwarded through `across()`'s `...`
# is `..n`, and an unnamed `.fns` list entry takes its index. The replacement
# has to be indexed by the same positions that select it. Building it over the
# whole list instead makes the two sides differ in length whenever any entry is
# named, so base R recycles -- warning from a call that otherwise succeeds --
# and numbers the survivors by their position among the unnamed entries rather
# than among all of them (#104).
name_unnamed_by_position <- function(arg_names, prefix) {
  unnamed <- which(arg_names == "")
  arg_names[unnamed] <- paste0(prefix, unnamed)
  arg_names
}

# The one place that knows an `across()` argument can be empty, so that no
# caller has to. R's empty argument is what a caller leaves in a position they
# omitted, and R answers it as an omission: `across(v, )` takes `.fns`'s
# default exactly as `across(v)` does, which is why `dplyr::across()` treats
# the two alike down to the `{.fn}` expansion and the `.cols` deprecation. The
# value fields below therefore answer for an empty argument what they answer
# for an absent one, and the index fields keep naming the position it occupies:
# a rewrite puts its replacement back where the caller wrote it rather than
# dropping an argument, which would slide every positional argument after it
# into a formal that is not its own (#174).
#
# Reading a value here rather than out of `call_args` is also what keeps the
# empty argument from being bound to a name downstream. `parts[[index]]` is
# safe, and passing what it returns straight to a function is safe, but binding
# it -- `for (part in parts)` as in #168, or `part <- parts[[index]]` as here --
# raises base R's untyped `missingArgError` on the first read of that name.
parse_across_arguments <- function(expr) {
  # Through the shared reader, so that the arguments parsed here are the
  # arguments of the call recognized as `across()`. Both readings see through a
  # redundant pair of parentheses, and a parse that did not would read
  # `(across(v, sum))` as one argument -- the `across()` call itself -- and hand
  # it to `eval_select()` as a selection (#178).
  call_args <- static_call_args(expr)
  arg_names <- names(call_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(call_args))
  }
  unnamed <- which(arg_names == "")
  cols_index <- match(".cols", arg_names, nomatch = 0L)
  if (cols_index == 0L && length(unnamed) > 0L) {
    cols_index <- unnamed[[1L]]
  }
  fns_index <- match(".fns", arg_names, nomatch = 0L)
  if (fns_index == 0L) {
    positional <- setdiff(unnamed, cols_index)
    if (length(positional) > 0L) {
      fns_index <- positional[[1L]]
    }
  }
  names_index <- match(".names", arg_names, nomatch = 0L)
  unpack_index <- match(".unpack", arg_names, nomatch = 0L)
  used <- c(cols_index, fns_index, names_index, unpack_index)
  additional <- setdiff(seq_along(call_args), used[used > 0L])
  additional_names <- name_unnamed_by_position(arg_names[additional], "..")
  supplied <- function(index) {
    index > 0L && !rlang::is_missing(call_args[[index]])
  }

  list(
    call_args = call_args,
    cols_index = cols_index,
    fns_index = fns_index,
    names_index = names_index,
    unpack_index = unpack_index,
    cols = if (supplied(cols_index)) {
      call_args[[cols_index]]
    } else {
      rlang::expr(dplyr::everything())
    },
    fns = if (supplied(fns_index)) call_args[[fns_index]] else NULL,
    names = if (supplied(names_index)) call_args[[names_index]] else NULL,
    unpack = if (supplied(unpack_index)) call_args[[unpack_index]] else NULL,
    additional = additional_names
  )
}
