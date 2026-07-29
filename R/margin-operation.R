new_margin_operation <- function(data,
                                 backend,
                                 plan,
                                 column_info,
                                 margin_label,
                                 check_margin_label,
                                 sort,
                                 call) {
  structure(
    list(
      data = data,
      backend = backend,
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
      grouping_spec <- preflight_grouping_spec(grouping_spec, data_vars)
      plan <- if (is_name_only_grouping_spec(grouping_spec)) {
        compile_grouping_spec_impl(
          grouping_spec,
          data_vars = data_vars,
          data_proxy = grouping_name_proxy(data_vars),
          .by = by,
          .duplicates = .duplicates
        )
      } else {
        NULL
      }
      data_proxy <- grouping_selection_proxy(data, backend = backend)
      if (is.null(plan)) {
        plan <- compile_grouping_spec_impl(
          grouping_spec,
          data_vars = data_vars,
          data_proxy = data_proxy,
          .by = by,
          .duplicates = .duplicates
        )
      }
      column_info <- margin_column_info(
        data_proxy,
        plan$dimensions,
        backend = backend
      )

      new_margin_operation(
        data = data,
        backend = backend,
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
