check_removed_groups_argument <- function(dots) {
  if (!".groups" %in% names(dots)) {
    return(invisible(NULL))
  }

  stop(
    "`summarize_with_margins()` does not support `.groups`; ",
    "Margin-summary results are always ungrouped.",
    call. = FALSE
  )
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

  stop(
    "`summarize_with_margins()` does not support ",
    paste0("`", unsupported, "()`", collapse = ", "),
    ". These helpers describe one branch-local dplyr grouping or data mask, ",
    "but a margin result combines multiple grouping sets. Use ",
    "`grouping_bit()` or ",
    "`grouping_id()` when identifying margin levels.",
    call. = FALSE
  )
}

check_summary_group_overwrite <- function(output_names, group_vars) {
  overwritten_groups <- intersect(output_names, unique(group_vars))
  if (length(overwritten_groups) == 0L) {
    return(invisible(NULL))
  }

  stop(
    "Summary results cannot overwrite grouping column",
    if (length(overwritten_groups) == 1L) " " else "s ",
    paste0("`", overwritten_groups, "`", collapse = ", "),
    ".",
    call. = FALSE
  )
}

plan_summary_expressions <- function(dots,
                                     data_proxy,
                                     data_vars,
                                     plan,
                                     backend_kind,
                                     set_id_name) {
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
    skip_parent_shares = TRUE
  )
  summary_plan <- plan_parent_share_expressions( # nolint: object_usage_linter
    dots,
    selection_proxy = selection_proxy,
    plan = plan,
    set_id_name = set_id_name,
    validate_cardinality = identical(backend_kind, "local")
  )
  summary_plan$dots <- resolve_summary_selections(
    summary_plan$dots,
    data_proxy = data_proxy,
    data_vars = data_vars,
    group_vars = group_vars,
    normalize_across_names = identical(backend_kind, "dtplyr")
  )
  if (length(summary_plan$cardinality) > 0L) {
    summary_plan$dots <- wrap_parent_sources( # nolint: object_usage_linter
      summary_plan$dots,
      cardinality = summary_plan$cardinality
    )
  }
  summary_plan$cardinality <- NULL
  summary_plan
}

find_summary_context_helpers <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(character())
  }

  call_name <- rlang::call_name(expr)
  call_ns <- rlang::call_ns(expr)
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
                                       skip_parent_shares = FALSE) {
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
        skip_parent_shares &&
          contains_parent_share(expr) # nolint: object_usage_linter
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

  call_name <- rlang::call_name(expr)
  call_ns <- rlang::call_ns(expr)
  is_dplyr_call <- is.null(call_ns) || identical(call_ns, "dplyr")
  if (!is_dplyr_call) {
    return(expr)
  }

  if (!is.null(call_name) && call_name %in% c("across", "if_any", "if_all")) {
    return(rewrite_across_selection(
      expr,
      env,
      data_proxy,
      normalize_across_names = normalize_across_names
    ))
  }
  if (identical(call_name, "pick")) {
    return(rewrite_pick_selection(expr, env, data_proxy))
  }

  expr
}

rewrite_across_selection <- function(expr,
                                     env,
                                     data_proxy,
                                     normalize_across_names) {
  call_name <- rlang::call_name(expr)
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
  tryCatch(
    tidyselect::eval_select(
      rlang::new_quosure(expr, env = env),
      data = data_proxy,
      strict = TRUE,
      allow_rename = TRUE
    ),
    error = function(cnd) {
      stop(
        "Invalid column selection in a summary expression: ",
        conditionMessage(cnd),
        call. = FALSE
      )
    }
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

  call_name <- rlang::call_name(expr)
  call_ns <- rlang::call_ns(expr)
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
            expand_across_name(names_template, column, fn, env)
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
  fns_names[fns_names == ""] <- as.character(which(fns_names == ""))
  fns_names
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
  additional_names <- arg_names[additional]
  additional_names[additional_names == ""] <- paste0(
    "..",
    seq_along(additional_names)
  )

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
