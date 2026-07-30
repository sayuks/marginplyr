#' Calculate a summary's share of its rollup parent
#'
#' `share_of_parent()` is a contextual summary helper for
#' [summarize_with_margins()]. It divides one preceding named numeric scalar
#' summary by the corresponding value in the immediately less detailed
#' [rollup()] level. Fixed `.by` columns partition the calculation.
#'
#' The helper must be the complete right-hand side of an explicitly named
#' summary, or the direct `.fns` argument of [dplyr::across()]. In the latter
#' form, `.cols` selects preceding ordinary summaries and `.names` is required.
#' Parent shares cannot be used by later summaries in the same call; create
#' derived values in a following [dplyr::mutate()] instead.
#'
#' @section Eligible source summaries:
#' The source must be defined exactly once before the Parent share. It must be
#' a top-level named summary or a statically named output from a preceding
#' [dplyr::across()], return one plain integer or double per grouping row, and
#' be self-contained. A source cannot depend on an earlier summary alias.
#' Parent shares, forward references, overwritten names, aggregate or
#' calculated arguments, strings, and columns expanded from unnamed
#' data-frame-valued summaries are rejected.
#'
#' Later independent ordinary summaries are allowed. A later summary cannot
#' use a Parent share in the same call because many database backends cannot
#' reuse aliases in one summary projection. Use a following [dplyr::mutate()]
#' for percentages, rounding, labels, or other derived values.
#'
#' @section Column-wise Parent shares:
#' In `across(.cols, share_of_parent, .names = ...)`, `.cols` sees only
#' preceding ordinary summaries. It supports name-based tidyselect, including
#' direct names, ranges, Boolean and negative selection, [dplyr::all_of()],
#' [dplyr::any_of()], [dplyr::everything()], and name-pattern helpers.
#' Type/value predicates such as `where()` are not supported.
#'
#' `.fns` must be `share_of_parent` or
#' `marginplyr::share_of_parent` directly. Formulas, anonymous functions,
#' function lists, and additional function arguments are rejected. `.names`
#' is required, must generate unique non-conflicting names, and `.unpack` may
#' be omitted or `FALSE`.
#'
#' @section Value rules:
#' The parent is the immediate strictly less detailed [rollup()] level within
#' each fixed `.by` partition. Composite dimensions are added or removed
#' together. Duplicate Grouping set occurrences remain in the result but are
#' skipped while finding the next coarser parent.
#'
#' A root Parent share is `1.0`, even when its source is zero or missing.
#' A missing numerator, missing denominator, or zero denominator gives
#' `NA_real_`; local `NaN` is missing. Other finite ratios are unclamped
#' doubles, so negative values and values above one are retained. Parent
#' matching uses internal Grouping set metadata rather than `.id` or displayed
#' Margin labels, and it never adds missing rows.
#'
#' Parent-share execution supports local data frames and lazy dbplyr, Arrow,
#' and dtplyr inputs for one pure [rollup()], including composite dimensions.
#' Lazy results remain lazy: ordinary summaries are followed by one
#' Parent-share mapping and join shared by every requested measure.
#'
#' General dbplyr backends are not queried solely to discover an arbitrary
#' summary result's type or cardinality. Statically detectable syntax and
#' dependency errors remain local, while an incompatible lazy summary may
#' report its backend error when [dplyr::collect()] executes the staged query.
#' The portable value guarantee covers finite numbers, missing values, and
#' zero denominators; backend-specific non-finite values are outside it.
#'
#' @param x The bare name of one preceding eligible ordinary summary.
#'
#' @return A double vector when used inside [summarize_with_margins()].
#' @export
#' @examples
#' summarize_with_margins(
#'   retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' summarize_with_margins(
#'   retail_sales,
#'   dplyr::across(c(units, revenue), sum),
#'   dplyr::across(
#'     c(units, revenue),
#'     share_of_parent,
#'     .names = "{.col}_share"
#'   ),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
share_of_parent <- function(x) {
  stop(
    "`share_of_parent()` can only be used inside ",
    "`summarize_with_margins()` with a `rollup()`. To derive a value from ",
    "an existing Parent share, use a following `dplyr::mutate()`.",
    call. = FALSE
  )
}

preflight_parent_shares <- function(dots) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  has_parent_shares <- FALSE

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    output_name <- dot_names[[i]]

    if (is_parent_share_call(expr)) {
      validate_parent_direct_syntax(expr, output_name)
      has_parent_shares <- TRUE
      next
    }
    if (is_across_call(expr) && contains_parent_share(expr)) {
      validate_parent_across_syntax(expr, env, output_name)
      has_parent_shares <- TRUE
      next
    }
    if (contains_parent_share(expr)) {
      stop(
        "`share_of_parent()` must be the complete right-hand side of a ",
        "named summary, or the direct `.fns` argument of `across()`.",
        call. = FALSE
      )
    }
  }

  has_parent_shares
}

validate_parent_share_grouping <- function(grouping_spec) {
  kind <- if (is.null(grouping_spec)) NULL else grouping_spec$type
  if (!identical(kind, "rollup")) {
    stop(
      "`share_of_parent()` requires `.grouping` to be one pure `rollup()`. ",
      "`grouping_sets()`, `cube()`, `grouping_spec()`, and other grouping ",
      "specifications do not define one unambiguous parent.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

plan_parent_share_expressions <- function(dots,
                                          selection_proxy,
                                          plan,
                                          set_id_name) {
  stopifnot(is.list(dots))
  stopifnot(inherits(plan, "margin_grouping_plan"))
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }

  analyses <- analyze_ordinary_summaries(dots, selection_proxy)
  ordinary_records <- unlist(
    lapply(analyses, `[[`, "records"),
    recursive = FALSE
  )
  ordinary_names <- vapply(
    ordinary_records,
    `[[`,
    character(1),
    "name"
  )
  ordinary_counts <- table(ordinary_names)
  planning_context <- list(
    all_records = ordinary_records,
    ordinary_counts = ordinary_counts,
    conflicting_names = unique(c(
      plan$by,
      plan$dimensions,
      set_id_name
    ))
  )

  planned_dots <- as.list(dots)
  requests <- list()
  preceding_ordinary <- list()
  preceding_parent_names <- character()

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    output_name <- dot_names[[i]]

    if (is_parent_share_call(expr)) {
      request <- plan_direct_parent_share(
        expr,
        output_name = output_name,
        preceding = preceding_ordinary,
        parent_names = preceding_parent_names,
        context = planning_context
      )
      planned_dots[[i]] <- rlang::new_quosure(
        NA_real_,
        env = rlang::empty_env()
      )
      requests <- c(requests, list(request))
      preceding_parent_names <- c(
        preceding_parent_names,
        request$outputs
      )
      next
    }

    if (is_across_call(expr) && contains_parent_share(expr)) {
      request <- plan_across_parent_share(
        expr,
        env = env,
        output_name = output_name,
        preceding = preceding_ordinary,
        parent_names = preceding_parent_names,
        context = planning_context
      )
      if (length(request$outputs) == 0L) {
        planned_dots[i] <- list(NULL)
        next
      }
      planned_dots[[i]] <- parent_share_placeholder(request$outputs)
      requests <- c(requests, list(request))
      preceding_parent_names <- c(
        preceding_parent_names,
        request$outputs
      )
      next
    }

    if (contains_parent_share(expr)) {
      stop(
        "`share_of_parent()` must be the complete right-hand side of a ",
        "named summary, or the direct `.fns` argument of `across()`.",
        call. = FALSE
      )
    }

    parent_dependency <- expression_alias_dependencies(
      expr,
      preceding_parent_names
    )
    if (length(parent_dependency) > 0L) {
      stop(
        "Ordinary summaries cannot use an earlier Parent share (`",
        parent_dependency[[1L]],
        "`) in the same `summarize_with_margins()` call. Use a following ",
        "`dplyr::mutate()` for derived values.",
        call. = FALSE
      )
    }
    preceding_ordinary <- c(preceding_ordinary, analyses[[i]]$records)
  }

  if (length(requests) > 0L) {
    check_parent_grouping_kind(plan)
  }

  keep <- !vapply(planned_dots, is.null, logical(1))
  planned_dots <- unlist(
    lapply(
      planned_dots[keep],
      function(dot) {
        if (inherits(dot, "marginplyr_parent_placeholders")) {
          unclass(dot)
        } else {
          list(dot)
        }
      }
    ),
    recursive = FALSE
  )
  list(dots = planned_dots, requests = requests)
}

analyze_ordinary_summaries <- function(dots, selection_proxy) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  preceding_names <- character()

  Map(
    function(quo, output_name, position) {
      expr <- rlang::quo_get_expr(quo)
      env <- rlang::quo_get_env(quo)
      if (contains_parent_share(expr)) {
        return(list(records = list()))
      }

      if (nzchar(output_name)) {
        output_names <- output_name
        eligible <- !is_across_call(expr)
      } else if (is_across_call(expr)) {
        output_names <- known_across_output_names(
          expr,
          env,
          selection_proxy
        )
        eligible <- TRUE
      } else {
        output_names <- known_data_frame_output_names(
          expr,
          env,
          selection_proxy
        )
        eligible <- FALSE
      }

      selected_dependencies <- if (is_across_call(expr)) {
        intersect(
          known_across_source_names( # nolint: object_usage_linter
            expr,
            env,
            selection_proxy
          ),
          preceding_names
        )
      } else {
        character()
      }
      dependencies <- unique(c(
        expression_alias_dependencies(expr, preceding_names),
        selected_dependencies
      ))
      records <- lapply(
        output_names,
        function(name) {
          list(
            name = name,
            position = position,
            eligible = eligible,
            dependencies = dependencies
          )
        }
      )
      preceding_names <<- c(preceding_names, output_names)
      list(records = records)
    },
    dots,
    dot_names,
    seq_along(dots)
  )
}

plan_direct_parent_share <- function(expr,
                                     output_name,
                                     preceding,
                                     parent_names,
                                     context) {
  args <- validate_parent_direct_syntax(expr, output_name)
  source <- rlang::as_name(args[[1L]])
  validate_parent_share_request(
    outputs = output_name,
    sources = source,
    preceding = preceding,
    parent_names = parent_names,
    context = context
  )
  list(outputs = output_name, sources = source)
}

validate_parent_direct_syntax <- function(expr, output_name) {
  if (!nzchar(output_name)) {
    stop(
      "A direct `share_of_parent()` summary must have an explicit output ",
      "name.",
      call. = FALSE
    )
  }
  args <- rlang::call_args(expr)
  if (length(args) != 1L || !rlang::is_symbol(args[[1L]])) {
    stop(
      "`", output_name, " = share_of_parent(...)` requires exactly one ",
      "bare name of a preceding ordinary summary.",
      call. = FALSE
    )
  }
  args
}

plan_across_parent_share <- function(expr,
                                     env,
                                     output_name,
                                     preceding,
                                     parent_names,
                                     context) {
  syntax <- validate_parent_across_syntax(expr, env, output_name)
  args <- syntax$args
  names_template <- syntax$names_template

  selectable <- vapply(
    preceding,
    function(record) {
      isTRUE(record$eligible) &&
        length(record$dependencies) == 0L &&
        context$ordinary_counts[[record$name]] == 1L
    },
    logical(1)
  )
  preceding_names <- unique(vapply(
    preceding[selectable],
    `[[`,
    character(1),
    "name"
  ))
  if (contains_selection_predicate(
    args$cols,
    env = env,
    selectable_names = preceding_names
  )) {
    abort_parent_predicate()
  }
  sources <- resolve_parent_share_selection(
    args$cols,
    env = env,
    preceding_names = preceding_names
  )
  outputs <- vapply(
    sources,
    function(source) {
      # nolint start: object_usage_linter.
      expand_across_name(names_template, source, "1", env)
      # nolint end
    },
    character(1)
  )

  validate_parent_share_request(
    outputs = outputs,
    sources = sources,
    preceding = preceding,
    parent_names = parent_names,
    context = context
  )
  list(outputs = outputs, sources = sources)
}

validate_parent_across_syntax <- function(expr, env, output_name) {
  if (nzchar(output_name)) {
    stop(
      "An `across()` Parent-share expression must be unnamed; use its ",
      "required `.names` argument to name the output columns.",
      call. = FALSE
    )
  }
  args <- parent_across_args(expr)
  if (!is_parent_share_function(args$fns)) {
    stop(
      "For Parent shares, `across()` `.fns` must be `share_of_parent` or ",
      "`marginplyr::share_of_parent` directly. Use two ordered `across()` ",
      "expressions instead of a formula, anonymous function, or function list.",
      call. = FALSE
    )
  }
  if (length(args$additional) > 0L) {
    stop(
      "Parent-share `across()` does not accept additional function ",
      "arguments: ",
      paste0("`", args$additional, "`", collapse = ", "),
      ". Put missing-value handling in the preceding ordinary summary.",
      call. = FALSE
    )
  }
  if (!is.null(args$unpack)) {
    unpack <- tryCatch(
      rlang::eval_tidy(args$unpack, env = env),
      error = function(cnd) NULL
    )
    if (!isFALSE(unpack)) {
      stop(
        "Parent-share `across()` requires `.unpack = FALSE` or an omitted ",
        "`.unpack` argument.",
        call. = FALSE
      )
    }
  }
  if (is.null(args$names)) {
    stop(
      "Parent-share `across()` requires an explicit `.names` argument, for ",
      "example `.names = \"{.col}_share\"`.",
      call. = FALSE
    )
  }
  names_template <- tryCatch(
    rlang::eval_tidy(args$names, env = env),
    error = function(cnd) NULL
  )
  if (
    !is.character(names_template) ||
      length(names_template) != 1L ||
      is.na(names_template)
  ) {
    stop(
      "Parent-share `across()` `.names` must be one non-missing character ",
      "template.",
      call. = FALSE
    )
  }
  if (contains_selection_predicate(args$cols, env = env)) {
    abort_parent_predicate()
  }
  list(args = args, names_template = names_template)
}

validate_parent_share_request <- function(outputs,
                                          sources,
                                          preceding,
                                          parent_names,
                                          context) {
  if (length(outputs) == 0L) {
    return(invisible(NULL))
  }
  if (any(!nzchar(outputs))) {
    stop("Parent-share output names must not be empty.", call. = FALSE)
  }
  if (anyDuplicated(outputs)) {
    stop(
      "Parent-share output names must be unique; duplicate name `",
      outputs[[anyDuplicated(outputs)]],
      "` was generated.",
      call. = FALSE
    )
  }

  preceding_names <- vapply(preceding, `[[`, character(1), "name")
  all_names <- vapply(
    context$all_records,
    `[[`,
    character(1),
    "name"
  )
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    output <- outputs[[i]]
    if (source %in% parent_names) {
      stop(
        "Parent share `", output, "` cannot use Parent share `", source,
        "` as its source.",
        call. = FALSE
      )
    }
    if (!source %in% preceding_names) {
      if (source %in% all_names) {
        stop(
          "Parent share `", output, "` must refer to an ordinary summary ",
          "defined before it; `", source, "` is a forward reference.",
          call. = FALSE
        )
      }
      stop(
        "Parent share `", output, "` refers to unknown preceding ordinary ",
        "summary `", source, "`.",
        call. = FALSE
      )
    }
    if (
      !is.na(context$ordinary_counts[[source]]) &&
        context$ordinary_counts[[source]] != 1L
    ) {
      stop(
        "Parent share `", output, "` requires source summary `", source,
        "` to be defined exactly once. Use one uniquely named ordinary ",
        "summary.",
        call. = FALSE
      )
    }
    record <- preceding[[max(which(preceding_names == source))]]
    if (!isTRUE(record$eligible)) {
      stop(
        "Parent share `", output, "` cannot use `", source,
        "` because it was expanded from a data-frame-valued summary. Rewrite ",
        "it as a top-level named summary or a preceding `across()` output.",
        call. = FALSE
      )
    }
    if (length(record$dependencies) > 0L) {
      stop(
        "Parent share `", output, "` cannot use source summary `", source,
        "` because it depends on earlier summary alias `",
        record$dependencies[[1L]],
        "`. Combine the calculation into one ordinary summary expression.",
        call. = FALSE
      )
    }
  }

  conflicts <- intersect(
    outputs,
    unique(c(
      context$conflicting_names,
      all_names,
      parent_names
    ))
  )
  if (length(conflicts) > 0L) {
    stop(
      "Parent-share output name `", conflicts[[1L]],
      "` conflicts with a grouping key, `.id`, ordinary summary, source ",
      "summary, or earlier Parent share.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

check_parent_grouping_kind <- function(plan) {
  if (!identical(plan$kind, "rollup")) {
    stop(
      "`share_of_parent()` requires `.grouping` to be one pure `rollup()`. ",
      "`grouping_sets()`, `cube()`, `grouping_spec()`, and other grouping ",
      "specifications do not define one unambiguous parent.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

apply_parent_shares <- function(result,
                                requests,
                                data,
                                plan,
                                set_id_name) {
  if (length(requests) == 0L) {
    return(result)
  }
  if (!is.data.frame(result)) {
    return(apply_lazy_parent_shares(
      result,
      requests = requests,
      plan = plan,
      set_id_name = set_id_name
    ))
  }
  check_parent_share_cardinality(
    result,
    data = data,
    plan = plan,
    requests = requests,
    set_id_name = set_id_name
  )
  parent_ids <- parent_set_ids(plan)
  for (request in requests) {
    for (i in seq_along(request$outputs)) {
      result[[request$outputs[[i]]]] <- calculate_parent_share(
        result,
        output = request$outputs[[i]],
        source = request$sources[[i]],
        plan = plan,
        parent_ids = parent_ids,
        set_id_name = set_id_name
      )
    }
  }
  result
}

apply_lazy_parent_shares <- function(result,
                                     requests,
                                     plan,
                                     set_id_name) {
  parent_ids <- parent_set_ids(plan)
  root_ids <- plan$set_ids[is.na(parent_ids)]
  pairs <- parent_share_pairs(requests)
  sources <- unique(vapply(pairs, `[[`, character(1), "source"))
  result_names <- get_col_names(result, dplyr::everything())
  denominator_names <- new_margin_internal_names(
    length(sources),
    used_names = result_names,
    prefix = "..marginplyr_parent_value_"
  )
  names(denominator_names) <- sources

  child_ids <- plan$set_ids[!is.na(parent_ids)]
  if (length(child_ids) > 0L) {
    mapping <- build_lazy_parent_mapping(
      result,
      child_ids = child_ids,
      parent_ids = parent_ids,
      sources = sources,
      denominator_names = denominator_names,
      plan = plan,
      set_id_name = set_id_name
    )
    join_key_names <- new_margin_internal_names(
      length(plan$dimensions),
      used_names = c(result_names, denominator_names),
      prefix = "..marginplyr_parent_key_"
    )
    names(join_key_names) <- plan$dimensions
    result <- add_lazy_parent_join_keys(
      result,
      plan = plan,
      parent_ids = parent_ids,
      set_id_name = set_id_name,
      join_key_names = join_key_names
    )
    mapping <- add_lazy_parent_join_keys(
      mapping,
      plan = plan,
      parent_ids = parent_ids,
      set_id_name = set_id_name,
      join_key_names = join_key_names
    )
    mapping <- dplyr::select(
      mapping,
      dplyr::all_of(c(
        set_id_name,
        plan$by,
        unname(join_key_names),
        unname(denominator_names)
      ))
    )
    join_names <- c(set_id_name, plan$by, unname(join_key_names))
    if (inherits(result, "tbl_lazy")) {
      right_join_names <- new_margin_internal_names(
        length(join_names),
        used_names = c(
          result_names,
          denominator_names,
          join_key_names
        ),
        prefix = "..marginplyr_parent_match_"
      )
      rename_pairs <- rlang::set_names(
        rlang::syms(join_names),
        right_join_names
      )
      mapping <- dplyr::rename(mapping, !!!rename_pairs)
      result <- dplyr::left_join(
        result,
        mapping,
        sql_on = lazy_parent_sql_on(
          con = dbplyr::remote_con(result),
          left_names = join_names,
          right_names = right_join_names
        ),
        x_as = "LHS",
        y_as = "RHS"
      )
    } else {
      right_join_names <- character()
      result <- dplyr::left_join(
        result,
        mapping,
        by = join_names,
        na_matches = "na"
      )
    }
  }

  share_exprs <- lapply(
    pairs,
    function(pair) {
      source <- pair$source
      denominator <- denominator_names[[source]]
      if (length(child_ids) == 0L) {
        return(rlang::expr(1.0))
      }
      rlang::expr(
        dplyr::if_else(
          .data[[!!set_id_name]] %in% !!root_ids,
          1.0,
          dplyr::if_else(
            is.na(.data[[!!source]]) |
              is.na(.data[[!!denominator]]) |
              .data[[!!denominator]] == 0,
            NA_real_,
            as.double(.data[[!!source]]) /
              as.double(.data[[!!denominator]])
          )
        )
      )
    }
  )
  names(share_exprs) <- vapply(pairs, `[[`, character(1), "output")
  result <- dplyr::mutate(result, !!!share_exprs)

  internal_names <- c(
    unname(denominator_names),
    if (exists("right_join_names", inherits = FALSE)) {
      right_join_names
    } else {
      character()
    },
    if (exists("join_key_names", inherits = FALSE)) {
      unname(join_key_names)
    } else {
      character()
    }
  )
  if (length(internal_names) > 0L) {
    result <- dplyr::select(result, -dplyr::all_of(internal_names))
  }
  result
}

lazy_parent_sql_on <- function(con, left_names, right_names) {
  stopifnot(length(left_names) == length(right_names))
  left_alias <- "LHS"
  right_alias <- "RHS"
  terms <- Map(
    function(left_name, right_name) {
      dbplyr::sql_glue2(
        con,
        paste0(
          "(({.id left_alias}.{.id left_name} = ",
          "{.id right_alias}.{.id right_name}) OR ",
          "({.id left_alias}.{.id left_name} IS NULL AND ",
          "{.id right_alias}.{.id right_name} IS NULL))"
        )
      )
    },
    left_names,
    right_names
  )
  dbplyr::sql(paste(
    vapply(terms, as.character, character(1)),
    collapse = " AND "
  ))
}

parent_share_pairs <- function(requests) {
  unlist(
    lapply(
      requests,
      function(request) {
        Map(
          function(output, source) {
            list(output = output, source = source)
          },
          request$outputs,
          request$sources
        )
      }
    ),
    recursive = FALSE
  )
}

build_lazy_parent_mapping <- function(result,
                                      child_ids,
                                      parent_ids,
                                      sources,
                                      denominator_names,
                                      plan,
                                      set_id_name) {
  group_vars <- unique(c(plan$by, plan$dimensions))
  key_exprs <- lapply(
    group_vars,
    function(var) rlang::expr(.data[[!!var]])
  )
  names(key_exprs) <- group_vars
  denominator_exprs <- lapply(
    sources,
    function(source) rlang::expr(.data[[!!source]])
  )
  names(denominator_exprs) <- unname(denominator_names[sources])

  mappings <- lapply(
    child_ids,
    function(child_id) {
      parent_id <- parent_ids[[child_id]]
      parent_rows <- dplyr::filter(
        result,
        .data[[set_id_name]] == !!parent_id
      )
      child_id_expr <- stats::setNames(
        list(rlang::expr(as.integer(!!child_id))),
        set_id_name
      )
      dplyr::transmute(
        parent_rows,
        !!!key_exprs,
        !!!child_id_expr,
        !!!denominator_exprs
      )
    }
  )
  Reduce(dplyr::union_all, mappings)
}

add_lazy_parent_join_keys <- function(result,
                                      plan,
                                      parent_ids,
                                      set_id_name,
                                      join_key_names) {
  join_key_exprs <- lapply(
    plan$dimensions,
    function(dimension) {
      matching_child_ids <- plan$set_ids[vapply(
        plan$set_ids,
        function(set_id) {
          parent_id <- parent_ids[[set_id]]
          !is.na(parent_id) && dimension %in% plan$sets[[parent_id]]
        },
        logical(1)
      )]
      rlang::expr(
        dplyr::if_else(
          .data[[!!set_id_name]] %in% !!matching_child_ids,
          .data[[!!dimension]],
          NA
        )
      )
    }
  )
  names(join_key_exprs) <- unname(join_key_names[plan$dimensions])
  dplyr::mutate(result, !!!join_key_exprs)
}

calculate_parent_share <- function(result,
                                   output,
                                   source,
                                   plan,
                                   parent_ids,
                                   set_id_name) {
  values <- result[[source]]
  if (
    !typeof(values) %in% c("integer", "double") ||
      is.object(values)
  ) {
    detected_type <- if (is.object(values)) class(values) else typeof(values)
    stop(
      "Parent share `", output, "` requires source summary `", source,
      "` to be a plain integer or double scalar; detected type ",
      paste(detected_type, collapse = "/"),
      ". Convert it explicitly in the ordinary summary.",
      call. = FALSE
    )
  }

  shares <- rep(NA_real_, nrow(result))
  for (set_id in plan$set_ids) {
    rows <- which(result[[set_id_name]] == set_id)
    if (length(rows) == 0L) {
      next
    }
    parent_id <- parent_ids[[set_id]]
    if (is.na(parent_id)) {
      shares[rows] <- 1
      next
    }

    keys <- plan$sets[[parent_id]]
    parent_rows <- which(result[[set_id_name]] == parent_id)
    if (length(keys) == 0L) {
      denominator <- rep(values[parent_rows[[1L]]], length(rows))
    } else {
      denominator_name <- new_margin_internal_names(
        1L,
        used_names = names(result),
        prefix = "..marginplyr_parent_value_"
      )
      lookup <- result[parent_rows, keys, drop = FALSE]
      lookup[[denominator_name]] <- values[parent_rows]
      child <- result[rows, keys, drop = FALSE]
      matched <- dplyr::left_join(
        child,
        lookup,
        by = keys,
        na_matches = "na"
      )
      denominator <- matched[[denominator_name]]
    }
    numerator <- values[rows]
    invalid <- is.na(numerator) |
      is.na(denominator) |
      denominator == 0
    shares[rows] <- ifelse(
      invalid,
      NA_real_,
      as.double(numerator) / as.double(denominator)
    )
  }
  shares
}

check_parent_share_cardinality <- function(result,
                                           data,
                                           plan,
                                           requests,
                                           set_id_name) {
  output <- requests[[1L]]$outputs[[1L]]
  source <- requests[[1L]]$sources[[1L]]
  for (set_id in plan$set_ids) {
    keys <- plan$sets[[set_id]]
    rows <- result[result[[set_id_name]] == set_id, , drop = FALSE]
    if (length(keys) == 0L) {
      valid <- nrow(rows) == 1L
    } else {
      expected <- dplyr::distinct(
        data,
        dplyr::across(dplyr::all_of(keys))
      )
      actual <- dplyr::count(
        rows,
        dplyr::across(dplyr::all_of(keys)),
        name = "..marginplyr_parent_n"
      )
      missing <- dplyr::anti_join(
        expected,
        actual,
        by = keys,
        na_matches = "na"
      )
      valid <- nrow(missing) == 0L &&
        nrow(actual) == nrow(expected) &&
        all(actual[["..marginplyr_parent_n"]] == 1L)
    }
    if (!valid) {
      stop(
        "Parent share `", output, "` requires source summary `", source,
        "`, which must return exactly one value per grouping row.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

parent_set_ids <- function(plan) {
  result <- rep(NA_integer_, length(plan$sets))
  variable_sets <- lapply(
    plan$sets,
    setdiff,
    y = plan$by
  )
  for (i in seq_along(variable_sets)) {
    child <- variable_sets[[i]]
    candidates <- which(vapply(
      variable_sets,
      function(parent) {
        length(parent) < length(child) && all(parent %in% child)
      },
      logical(1)
    ))
    candidates <- candidates[candidates > i]
    if (length(candidates) > 0L) {
      result[[i]] <- candidates[[1L]]
    }
  }
  result
}

parent_cardinality_request <- function(requests, message) {
  pairs <- unlist(
    lapply(
      requests,
      function(request) {
        Map(
          list,
          output = request$outputs,
          source = request$sources
        )
      }
    ),
    recursive = FALSE
  )
  source_matches <- vapply(
    pairs,
    function(pair) {
      grepl(
        paste0("`", pair$source, "` must be size"),
        message,
        fixed = TRUE
      )
    },
    logical(1)
  )
  if (any(source_matches)) {
    return(pairs[[which(source_matches)[[1L]]]])
  }
  pairs[[1L]]
}

parent_share_placeholder <- function(outputs) {
  placeholders <- lapply(
    outputs,
    function(output) {
      rlang::new_quosure(NA_real_, env = rlang::empty_env())
    }
  )
  names(placeholders) <- outputs
  structure(placeholders, class = "marginplyr_parent_placeholders")
}

is_parent_share_call <- function(expr) {
  rlang::is_call(expr) &&
    identical(rlang::call_name(expr), "share_of_parent") &&
    (is.null(rlang::call_ns(expr)) ||
       identical(rlang::call_ns(expr), "marginplyr"))
}

contains_parent_share <- function(expr) {
  if (is_parent_share_function(expr)) {
    return(TRUE)
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (is_parent_share_call(expr)) {
    return(TRUE)
  }
  any(vapply(
    as.list(expr)[-1L],
    contains_parent_share,
    logical(1)
  ))
}

is_parent_share_function <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(identical(rlang::as_name(expr), "share_of_parent"))
  }
  rlang::is_call(expr, "::") &&
    length(expr) == 3L &&
    rlang::is_symbol(expr[[2L]], "marginplyr") &&
    rlang::is_symbol(expr[[3L]], "share_of_parent")
}

is_across_call <- function(expr) {
  rlang::is_call(expr) &&
    identical(rlang::call_name(expr), "across") &&
    (is.null(rlang::call_ns(expr)) ||
       identical(rlang::call_ns(expr), "dplyr"))
}

parent_across_args <- function(expr) {
  args <- rlang::call_args(expr)
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
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
  used <- c(
    cols_index,
    fns_index,
    match(".names", arg_names, nomatch = 0L),
    match(".unpack", arg_names, nomatch = 0L)
  )
  additional <- setdiff(seq_along(args), used[used > 0L])
  additional_names <- arg_names[additional]
  additional_names[additional_names == ""] <- paste0(
    "..",
    seq_along(additional_names)
  )

  list(
    cols = if (cols_index == 0L) {
      rlang::expr(dplyr::everything())
    } else {
      args[[cols_index]]
    },
    fns = if (fns_index == 0L) NULL else args[[fns_index]],
    names = {
      i <- match(".names", arg_names, nomatch = 0L)
      if (i == 0L) NULL else args[[i]]
    },
    unpack = {
      i <- match(".unpack", arg_names, nomatch = 0L)
      if (i == 0L) NULL else args[[i]]
    },
    additional = additional_names
  )
}

resolve_parent_share_selection <- function(expr, env, preceding_names) {
  proxy <- stats::setNames(
    as.list(seq_along(preceding_names)),
    preceding_names
  )
  tryCatch(
    names(tidyselect::eval_select(
      rlang::new_quosure(expr, env = env),
      data = proxy,
      strict = TRUE,
      allow_rename = FALSE
    )),
    error = function(cnd) {
      stop(
        "Invalid Parent-share `across()` selection. Select only eligible ",
        "preceding ordinary summaries: ",
        conditionMessage(cnd),
        call. = FALSE
      )
    }
  )
}

contains_selection_predicate <- function(expr,
                                         env,
                                         selectable_names = NULL) {
  if (rlang::is_symbol(expr)) {
    if (
      is.null(selectable_names) ||
        rlang::as_name(expr) %in% selectable_names
    ) {
      return(FALSE)
    }
    value <- tryCatch(
      rlang::env_get(env, rlang::as_name(expr), inherit = TRUE),
      error = function(cnd) NULL
    )
    return(is.function(value))
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (identical(rlang::call_name(expr), "where")) {
    return(TRUE)
  }
  any(vapply(
    as.list(expr)[-1L],
    contains_selection_predicate,
    logical(1),
    env = env,
    selectable_names = selectable_names
  ))
}

abort_parent_predicate <- function() {
  stop(
    "Parent-share `across()` only supports name-based tidyselect. Replace ",
    "`where()` or another type/value predicate with explicit summary names.",
    call. = FALSE
  )
}

expression_alias_dependencies <- function(expr, aliases) {
  if (length(aliases) == 0L) {
    return(character())
  }
  intersect(unique(expression_data_symbols(expr)), aliases)
}

expression_data_symbols <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(rlang::as_name(expr))
  }
  if (!rlang::is_call(expr)) {
    return(character())
  }
  if (identical(rlang::call_name(expr), "get") && length(expr) >= 2L) {
    if (get_has_external_env(expr)) {
      return(character())
    }
    args <- rlang::call_args(expr)
    arg_names <- names(args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(args))
    }
    name_index <- match("x", arg_names, nomatch = 0L)
    if (name_index == 0L) {
      name_index <- which(arg_names == "")[[1L]]
    }
    name <- args[[name_index]]
    if (
      is.character(name) &&
        length(name) == 1L &&
        !is.na(name)
    ) {
      return(name)
    }
  }
  if (
    rlang::call_name(expr) %in% c("$", "[[") &&
      length(expr) >= 3L &&
      rlang::is_symbol(expr[[2L]])
  ) {
    pronoun <- rlang::as_name(expr[[2L]])
    if (identical(pronoun, ".env")) {
      return(character())
    }
    if (identical(pronoun, ".data")) {
      column <- expr[[3L]]
      if (rlang::is_symbol(column)) {
        return(rlang::as_name(column))
      }
      if (
        is.character(column) &&
          length(column) == 1L &&
          !is.na(column)
      ) {
        return(column)
      }
      return(character())
    }
  }
  args <- as.list(expr)[-1L]
  unique(unlist(
    lapply(args, expression_data_symbols),
    use.names = FALSE
  ))
}

get_has_external_env <- function(expr) {
  args <- rlang::call_args(expr)
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  if (any(arg_names %in% c("pos", "envir"))) {
    return(TRUE)
  }

  unnamed_count <- sum(arg_names == "")
  x_is_named <- "x" %in% arg_names
  unnamed_count > as.integer(!x_is_named)
}
