normalize_margin_label <- function(.margin_label) {
  if (is.null(.margin_label)) {
    return(NULL)
  }
  if (!is.character(.margin_label) || length(.margin_label) != 1L) {
    stop(
      "`.margin_label` must be `NULL` or a character vector of length 1.",
      call. = FALSE
    )
  }
  .margin_label
}

margin_column_info <- function(.data, dimensions) {
  if (length(dimensions) == 0L) {
    return(list(factors = list(), prototypes = list()))
  }

  can_read_schema <- inherits(
    .data,
    c(
      "data.frame",
      "dtplyr_step",
      "arrow_dplyr_query",
      "ArrowTabular",
      "Dataset",
      "tbl_duckdb_connection"
    )
  )

  if (!can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- dplyr::collect(utils::head(x = .data, n = 0L))
  schema <- schema[dimensions]

  prototypes <- lapply(schema, function(x) x[NA_integer_])
  can_restore_factors <- inherits(
    .data,
    c("data.frame", "dtplyr_step", "tbl_duckdb_connection")
  )
  factors <- if (can_restore_factors) {
    lapply(
      names(schema)[vapply(schema, is.factor, logical(1))],
      function(col) {
        x <- schema[[col]]
        list(
          col = col,
          levels = levels(x),
          ordered = is.ordered(x),
          has_na_in_level = anyNA(levels(x))
        )
      }
    )
  } else {
    list()
  }

  list(factors = factors, prototypes = prototypes)
}

validate_margin_label <- function(.data,
                                  dimensions,
                                  .margin_label,
                                  .check_margin_label,
                                  column_info) {
  assert_logical_scalar(.check_margin_label)

  if (is.null(.margin_label) || length(dimensions) == 0L) {
    return(invisible(NULL))
  }

  factor_info <- column_info$factors
  if (
    inherits(.data, "tbl_duckdb_connection") &&
      is.na(.margin_label) &&
      length(factor_info) > 0L
  ) {
    stop(
      "DuckDB factor grouping columns require a non-missing ",
      "`.margin_label`.",
      call. = FALSE
    )
  }

  if (is.na(.margin_label) && length(factor_info) > 0L) {
    na_level_cols <- Filter(function(x) x$has_na_in_level, factor_info)
    na_level_cols <- vapply(na_level_cols, function(x) x$col, character(1))
    if (length(na_level_cols) > 0L) {
      stop(
        "If `.margin_label` is `NA_character_`, these factor columns must ",
        "not contain `<NA>` as a level: ",
        paste0("`", na_level_cols, "`", collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (!.check_margin_label) {
    return(invisible(NULL))
  }

  check_data <- dplyr::mutate(
    .data,
    dplyr::across(dplyr::all_of(dimensions), as.character)
  )
  assert_margin_name(check_data, dimensions, .margin_label)
}

label_margin_branch <- function(.data,
                                plan,
                                grouping_set,
                                .margin_label,
                                prototypes = list()) {
  included <- intersect(plan$dimensions, grouping_set)
  omitted <- setdiff(plan$dimensions, grouping_set)

  if (!is.null(.margin_label)) {
    if (length(included) > 0L) {
      .data <- dplyr::mutate(
        .data,
        dplyr::across(dplyr::all_of(included), as.character)
      )
    }
    values <- rep(list(.margin_label), length(omitted))
  } else {
    values <- lapply(
      omitted,
      function(col) {
        value <- prototypes[[col]]
        if (is.null(value)) NA else value
      }
    )
  }

  if (length(omitted) > 0L) {
    names(values) <- omitted
    .data <- dplyr::mutate(.data, !!!values)
  }

  relocate_before_union_all(.data, c(plan$by, plan$dimensions))
}

restore_margin_factors <- function(.data, factor_info, .margin_label) {
  if (is.null(.margin_label) || length(factor_info) == 0L) {
    return(.data)
  }

  Reduce(
    function(data, info) reconstruct_factor(data, info, .margin_label),
    factor_info,
    init = .data
  )
}

finish_margin_result <- function(.data,
                                 plan,
                                 factor_info,
                                 .margin_label,
                                 .sort) {
  .data <- restore_margin_factors(.data, factor_info, .margin_label)
  .data <- relocate_post_proc(
    .data,
    dplyr::all_of(c(plan$by, plan$dimensions))
  )

  if (.sort) {
    .data <- arrange_impl(
      .data,
      dplyr::all_of(c(plan$by, plan$dimensions))
    )
  }

  .data
}
