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
  legacy_package_error <- FALSE
  tryCatch(
    withCallingHandlers(
      expr,
      error = function(cnd) {
        if (!inherits(cnd, "marginplyr_error")) {
          legacy_package_error <<- error_signaled_by_marginplyr()
        }
      }
    ),
    error = function(cnd) {
      if (
        inherits(cnd, "marginplyr_error") ||
          isTRUE(legacy_package_error)
      ) {
        cnd$call <- call
      }
      stop(cnd)
    }
  )
}

error_signaled_by_marginplyr <- function() {
  frames <- sys.frames()
  # Exclude this helper and the calling handler in with_margin_error_call().
  candidates <- utils::head(seq_along(frames), -2L)

  for (i in rev(candidates)) {
    top <- topenv(frames[[i]])
    if (!isNamespace(top)) {
      return(FALSE)
    }
    namespace <- getNamespaceName(top)
    if (namespace %in% c("base", "rlang")) {
      next
    }
    return(identical(namespace, "marginplyr"))
  }
  FALSE
}

match_margin_choice <- function(value, choices, arg_name) {
  call <- rlang::caller_call()
  force(value)
  tryCatch(
    match.arg(value, choices),
    error = function(...) {
      abort_marginplyr( # nolint: object_usage_linter
        paste0(
          "`", arg_name, "` must be one of ",
          paste0("\"", choices, "\"", collapse = ", "),
          "."
        ),
        class = "simpleError",
        call = call
      )
    }
  )
}

normalize_margin_options <- function(.margin_label,
                                     .margin_label_position,
                                     .check_margin_label,
                                     .duplicates,
                                     .id = NULL) {
  if (
    !is.logical(.check_margin_label) ||
      length(.check_margin_label) != 1L ||
      is.na(.check_margin_label)
  ) {
    abort_marginplyr( # nolint: object_usage_linter
      "`.check_margin_label` must be a logical scalar (`TRUE` or `FALSE`).",
      class = "simpleError"
    )
  }
  .id <- normalize_margin_id(.id)

  list(
    set_id_name = .id,
    margin_label = normalize_margin_label(.margin_label),
    margin_label_position = match_margin_choice(
      .margin_label_position,
      choices = c("last", "first"),
      arg_name = ".margin_label_position"
    ),
    check_margin_label = .check_margin_label,
    duplicates = match_margin_choice(
      .duplicates,
      choices = c("error", "drop", "keep"),
      arg_name = ".duplicates"
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
    abort_marginplyr( # nolint: object_usage_linter
      "`.id` must be `NULL` or one non-missing, non-empty character string.",
      class = "simpleError"
    )
  }
  .id
}

check_margin_id_collision <- function(.id, names, where) {
  if (!is.null(.id) && .id %in% names) {
    abort_marginplyr( # nolint: object_usage_linter
      sprintf("`.id` (`%s`) conflicts with %s.", .id, where),
      class = "simpleError"
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
        .id = .id
      )
      set_id_name <- options$set_id_name
      .margin_label <- options$margin_label
      .margin_label_position <- options$margin_label_position
      .check_margin_label <- options$check_margin_label
      .duplicates <- options$duplicates

      grouping <- prepare_grouping_plan( # nolint: object_usage_linter
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
      margin_labels <- resolve_margin_labels( # nolint: object_usage_linter
        .margin_label,
        dimensions = plan$dimensions,
        by = plan$by
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
        call = call
      )
    },
    call = call
  )
}

validate_margin_operation <- function(operation) {
  check_margin_operation(operation)
  with_margin_error_call( # nolint: object_usage_linter
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

finalize_margin_operation <- function(operation, result) {
  check_margin_operation(operation)
  result <- dplyr::ungroup(result)
  result <- restore_margin_factors( # nolint: object_usage_linter
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

  result
}
