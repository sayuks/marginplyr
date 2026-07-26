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
                                       normalize_across_names = FALSE) {
  selectable_vars <- setdiff(data_vars, unique(group_vars))
  selection_proxy <- dplyr::select(
    data_proxy,
    dplyr::all_of(selectable_vars)
  )

  lapply(
    dots,
    function(dot) {
      expr <- rewrite_summary_selections(
        rlang::quo_get_expr(dot),
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
  call_args <- as.list(expr)[-1L]
  arg_names <- names(call_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(call_args))
  }

  selection_index <- match(".cols", arg_names, nomatch = 0L)
  if (selection_index == 0L) {
    unnamed <- which(arg_names == "")
    selection_index <- if (length(unnamed) > 0L) unnamed[[1L]] else 0L
  }

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
    arg_names <- names(call_args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_args))
    }
    unnamed <- which(arg_names == "")
    names_index <- match(".names", arg_names, nomatch = 0L)
    unpack_index <- match(".unpack", arg_names, nomatch = 0L)
    unpack_is_false <- unpack_index == 0L || isFALSE(tryCatch(
      rlang::eval_tidy(call_args[[unpack_index]], env = env),
      error = function(cnd) NULL
    ))
    function_names <- known_across_function_names(
      call_args,
      arg_names,
      unnamed,
      selection_index
    )

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

        fns_index <- across_function_index(
          arg_names,
          unnamed,
          selection_index
        )
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
  source_names <- colnames(data_proxy)[unname(selected)]
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
      return(character())
    }
    return(setdiff(arg_names[nzchar(arg_names)], ".name_repair"))
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

known_across_output_names <- function(expr, env, data_proxy) {
  call_args <- as.list(expr)[-1L]
  arg_names <- names(call_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(call_args))
  }

  cols_index <- match(".cols", arg_names, nomatch = 0L)
  unnamed <- which(arg_names == "")
  if (cols_index == 0L && length(unnamed) > 0L) {
    cols_index <- unnamed[[1L]]
  }
  cols_expr <- if (cols_index == 0L) {
    rlang::expr(dplyr::everything())
  } else {
    call_args[[cols_index]]
  }
  column_names <- names(resolve_summary_selection(cols_expr, env, data_proxy))

  names_index <- match(".names", arg_names, nomatch = 0L)
  if (names_index == 0L) {
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

  if (!grepl("{.fn}", names_template, fixed = TRUE)) {
    return(vapply(
      column_names,
      function(column) {
        sub("{.col}", column, names_template, fixed = TRUE)
      },
      character(1)
    ))
  }

  function_names <- known_across_function_names(
    call_args,
    arg_names,
    unnamed,
    cols_index
  )
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
            output <- sub("{.col}", column, names_template, fixed = TRUE)
            sub("{.fn}", fn, output, fixed = TRUE)
          },
          character(1)
        )
      }
    ),
    use.names = FALSE
  )
}

known_across_function_names <- function(call_args,
                                        arg_names,
                                        unnamed,
                                        cols_index) {
  fns_index <- across_function_index(arg_names, unnamed, cols_index)
  if (fns_index == 0L) {
    return("1")
  }

  fns_expr <- call_args[[fns_index]]
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

across_function_index <- function(arg_names, unnamed, cols_index) {
  fns_index <- match(".fns", arg_names, nomatch = 0L)
  if (fns_index == 0L) {
    positional <- setdiff(unnamed, cols_index)
    if (length(positional) > 0L) {
      fns_index <- positional[[1L]]
    }
  }
  fns_index
}
