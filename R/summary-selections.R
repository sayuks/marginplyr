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
      opening <- if (identical(name, matched)) {
        paste0("`summarize_with_margins()` has no `", matched, "` argument")
      } else {
        paste0(
          "`", name, "` is not an argument of `summarize_with_margins()`, ",
          "and neither is the `", matched, "` it resembles"
        )
      }
      abort_marginplyr(
        paste0(opening, "; ", removed_summary_options[[matched]])
      )
    }
    abort_marginplyr(
      paste0(
        "`", name, "` is not an argument of `summarize_with_margins()`, so ",
        "it was captured as a summary named `", name, "`. Did you mean `",
        matched, "`? Rename the summary if the column is intended."
      )
    )
  }
  invisible(NULL)
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

  abort_marginplyr(
    paste0(
      "`summarize_with_margins()` does not support ",
      paste0("`", unsupported, "()`", collapse = ", "),
      ". These helpers describe one branch-local dplyr grouping or data mask, ",
      "but a margin result combines multiple grouping sets. Use ",
      "`grouping_bit()` or ",
      "`grouping_id()` when identifying margin levels."
    )
  )
}

check_summary_group_overwrite <- function(output_names, group_vars) {
  overwritten_groups <- intersect(output_names, unique(group_vars))
  if (length(overwritten_groups) == 0L) {
    return(invisible(NULL))
  }

  abort_marginplyr(
    paste0(
      "Summary results cannot overwrite grouping column",
      if (length(overwritten_groups) == 1L) " " else "s ",
      paste0("`", overwritten_groups, "`", collapse = ", "),
      "."
    )
  )
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

  abort_marginplyr(
    paste0(
      "Dynamically generated summary output names conflict with ",
      "internal grouping columns: ",
      paste0("`", conflicting_names, "`", collapse = ", "),
      ". Use different summary output names."
    )
  )
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
  summary_plan$cardinality <- NULL
  summary_plan
}

find_summary_context_helpers <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(character())
  }

  call_name <- static_call_name(expr)
  call_ns <- static_call_ns(expr)
  unsupported <- c(
    "cur_group",
    "cur_group_id",
    "cur_group_rows",
    "cur_data",
    "cur_data_all"
  )
  found <- if (
    !is.null(call_name) &&
      call_name %in% unsupported &&
      (is.null(call_ns) || identical(call_ns, "dplyr"))
  ) {
    call_name
  } else {
    character()
  }

  pieces <- as.list(expr)[-1L]
  c(
    found,
    unlist(lapply(pieces, find_summary_context_helpers), use.names = FALSE)
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

  call_args <- as.list(expr)[-1L]
  call_args <- lapply(
    call_args,
    rewrite_summary_selections,
    env = env,
    data_proxy = data_proxy,
    normalize_across_names = normalize_across_names
  )
  names(call_args) <- names(as.list(expr)[-1L])
  expr <- rlang::call2(expr[[1L]], !!!call_args)

  call_name <- static_call_name(expr)
  call_ns <- static_call_ns(expr)
  is_dplyr_call <- is.null(call_ns) || identical(call_ns, "dplyr")
  if (!is_dplyr_call) {
    return(expr)
  }

  if (!is.null(call_name) && call_name %in% c("across", "if_any", "if_all")) {
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

  expr
}

# `call_name` is the caller's answer rather than one asked again here: the only
# caller reaches this after reading it, and asking a second time is the pairing
# that made an unrecognized shape -- a formula -- answer as one of the three
# names this handles (#163).
rewrite_across_selection <- function(expr,
                                     env,
                                     data_proxy,
                                     normalize_across_names,
                                     call_name) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  selection_index <- parsed$cols_index

  if (selection_index == 0L || is.na(selection_index)) {
    selected <- resolve_summary_selection(
      rlang::expr(dplyr::everything()),
      env = env,
      data_proxy = data_proxy
    )
    call_args <- append(
      list(.cols = summary_all_of_expr(selected, data_proxy)),
      call_args
    )
    selection_index <- 1L
  } else {
    selected <- resolve_summary_selection(
      call_args[[selection_index]],
      env = env,
      data_proxy = data_proxy
    )
    call_args[[selection_index]] <- summary_all_of_expr(selected, data_proxy)
  }

  if (identical(call_name, "across") && normalize_across_names) {
    parsed <- parse_across_arguments(rlang::call2(expr[[1L]], !!!call_args))
    names_index <- parsed$names_index
    unpack_index <- parsed$unpack_index
    unpack_is_false <- unpack_index == 0L || isFALSE(tryCatch(
      rlang::eval_tidy(call_args[[unpack_index]], env = env),
      error = function(cnd) NULL
    ))
    function_names <- known_across_function_names(parsed)

    if (
      names_index > 0L &&
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

        fns_index <- parsed$fns_index
        if (
          fns_index > 0L &&
            rlang::is_call(call_args[[fns_index]], "list") &&
            length(call_args[[fns_index]]) == 2L
        ) {
          call_args[[fns_index]] <- call_args[[fns_index]][[2L]]
        }
        call_args <- call_args[-names_index]
      }
    }
  }

  if (identical(call_name, "across")) {
    parsed <- parse_across_arguments(rlang::call2(expr[[1L]], !!!call_args))
    if (parsed$names_index > 0L) {
      call_args[[parsed$names_index]] <- rlang::eval_tidy(
        call_args[[parsed$names_index]],
        env = env
      )
    }
  }

  rlang::call2(expr[[1L]], !!!call_args)
}

rewrite_pick_selection <- function(expr, env, data_proxy) {
  call_args <- as.list(expr)[-1L]
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

  rlang::call2(
    expr[[1L]],
    summary_all_of_expr(selected, data_proxy)
  )
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

  call_name <- static_call_name(expr)
  call_ns <- static_call_ns(expr)
  is_tibble_constructor <-
    !is.null(call_name) &&
    call_name %in% c("tibble", "data_frame") &&
    (is.null(call_ns) || identical(call_ns, "tibble"))
  is_data_frame_constructor <-
    identical(call_name, "data.frame") &&
    (is.null(call_ns) || identical(call_ns, "base"))
  if (is_tibble_constructor || is_data_frame_constructor) {
    call_args <- as.list(expr)[-1L]
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

  if (
    identical(call_name, "pick") &&
      (is.null(call_ns) || identical(call_ns, "dplyr"))
  ) {
    call_args <- as.list(expr)[-1L]
    selection <- if (length(call_args) == 0L) {
      rlang::expr(dplyr::everything())
    } else {
      rlang::call2("c", !!!call_args)
    }
    return(names(resolve_summary_selection(selection, env, data_proxy)))
  }

  if (
    identical(call_name, "across") &&
      (is.null(call_ns) || identical(call_ns, "dplyr"))
  ) {
    return(known_across_output_names(expr, env, data_proxy))
  }

  character()
}

known_injected_argument_name <- function(expr) {
  if (!rlang::is_call(expr, ":=") || length(expr) != 3L) {
    return("")
  }

  lhs <- expr[[2L]]
  if (is.character(lhs) && length(lhs) == 1L && !is.na(lhs)) {
    return(lhs)
  }
  if (rlang::is_symbol(lhs)) {
    return(rlang::as_name(lhs))
  }
  ""
}

known_across_output_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  cols_expr <- parsed$cols
  column_names <- names(resolve_summary_selection(cols_expr, env, data_proxy))

  names_index <- parsed$names_index
  if (names_index == 0L) {
    fns_index <- parsed$fns_index
    if (
      fns_index > 0L &&
        rlang::is_call(call_args[[fns_index]], "list")
    ) {
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
    rlang::eval_tidy(call_args[[names_index]], env = env),
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

  abort_marginplyr(
    paste0(
      "The `across()` `.names` template `", template, "` must produce one ",
      "name per column, but it produced ", length(expanded),
      " for column `", column,
      "`. Use a template that expands to a single name."
    )
  )
}

known_across_source_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  selected <- resolve_summary_selection(parsed$cols, env, data_proxy)
  get_col_names(data_proxy, dplyr::everything())[unname(selected)]
}

known_across_function_names <- function(parsed) {
  fns_index <- parsed$fns_index
  if (fns_index == 0L) {
    return("1")
  }

  fns_expr <- parsed$call_args[[fns_index]]
  if (!rlang::is_call(fns_expr, "list")) {
    return("1")
  }
  fns <- as.list(fns_expr)[-1L]
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

parse_across_arguments <- function(expr) {
  call_args <- rlang::call_args(expr)
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

  list(
    call_args = call_args,
    cols_index = cols_index,
    fns_index = fns_index,
    names_index = names_index,
    unpack_index = unpack_index,
    cols = if (cols_index == 0L) {
      rlang::expr(dplyr::everything())
    } else {
      call_args[[cols_index]]
    },
    fns = if (fns_index == 0L) NULL else call_args[[fns_index]],
    names = if (names_index == 0L) NULL else call_args[[names_index]],
    unpack = if (unpack_index == 0L) NULL else call_args[[unpack_index]],
    additional = additional_names
  )
}
