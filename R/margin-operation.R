new_margin_operation <- function(data,
                                 by,
                                 backend,
                                 data_vars,
                                 data_proxy,
                                 plan,
                                 column_info,
                                 margin_label,
                                 check_margin_label,
                                 sort,
                                 call) {
  structure(
    list(
      data = data,
      by = by,
      backend = backend,
      data_vars = data_vars,
      data_proxy = data_proxy,
      plan = plan,
      column_info = column_info,
      margin_label = margin_label,
      check_margin_label = check_margin_label,
      sort = sort,
      call = call
    ),
    class = "marginplyr_margin_operation"
  )
}

check_margin_operation <- function(operation) {
  stopifnot(inherits(operation, "marginplyr_margin_operation"))
  invisible(operation)
}

with_margin_error_call <- function(expr, call) {
  tryCatch(
    expr,
    error = function(cnd) {
      cnd$call <- call
      stop(cnd)
    }
  )
}

validate_grouping_spec_early <- function(grouping_spec) {
  if (is.null(grouping_spec)) {
    return(invisible(NULL))
  }
  if (!inherits(grouping_spec, "margin_grouping_spec")) {
    stop(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`.",
      call. = FALSE
    )
  }

  type <- grouping_spec$type
  args <- grouping_spec$args
  if (
    !is.character(type) ||
      length(type) != 1L ||
      !type %in% c("set", "sets", "rollup", "cube", "product") ||
      !is.list(args)
  ) {
    stop("Invalid grouping specification.", call. = FALSE)
  }
  if (identical(type, "sets") && length(args) == 0L) {
    stop(
      "`grouping_sets()` requires at least one set. Use `grouping_set()` ",
      "for the empty grouping set.",
      call. = FALSE
    )
  }
  if (type %in% c("rollup", "cube") && length(args) == 0L) {
    stop(
      sprintf("`%s()` requires at least one dimension.", type),
      call. = FALSE
    )
  }

  invisible(NULL)
}

prepare_margin_operation <- function(.data,
                                     by_quo,
                                     grouping_quo,
                                     .margin_label,
                                     .check_margin_label,
                                     .duplicates,
                                     .sort,
                                     call = rlang::caller_call()) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))

  with_margin_error_call(
    {
      assert_logical_scalar(.check_margin_label)
      assert_logical_scalar(.sort)
      .margin_label <- normalize_margin_label(.margin_label)
      .duplicates <- match.arg(
        .duplicates,
        choices = c("error", "drop", "keep")
      )

      grouping_spec <- rlang::eval_tidy(grouping_quo)
      validate_grouping_spec_early(grouping_spec)

      input <- prepare_margin_input(.data, by_quo)
      data <- input$data
      by <- input$by
      backend <- grouping_backend(data)
      data_vars <- get_col_names(data, dplyr::everything())
      data_proxy <- grouping_selection_proxy(data, backend = backend)
      plan <- compile_grouping_spec(
        grouping_spec,
        data_vars = data_vars,
        data_proxy = data_proxy,
        .by = by,
        .duplicates = .duplicates
      )
      column_info <- margin_column_info(
        data_proxy,
        plan$dimensions,
        backend = backend
      )

      new_margin_operation(
        data = data,
        by = by,
        backend = backend,
        data_vars = data_vars,
        data_proxy = data_proxy,
        plan = plan,
        column_info = column_info,
        margin_label = .margin_label,
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
      .margin_label = operation$margin_label,
      .check_margin_label = operation$check_margin_label,
      column_info = operation$column_info,
      backend = operation$backend
    ),
    call = operation$call
  )
}

finalize_margin_operation <- function(operation, result) {
  check_margin_operation(operation)
  result <- dplyr::ungroup(result)
  finish_margin_result(
    result,
    plan = operation$plan,
    factor_info = operation$column_info$factors,
    .margin_label = operation$margin_label,
    .sort = operation$sort
  )
}
