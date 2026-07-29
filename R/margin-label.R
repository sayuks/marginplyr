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

validate_margin_label <- function(.data,
                                  dimensions,
                                  .margin_label,
                                  .check_margin_label,
                                  column_info,
                                  backend = grouping_backend(.data)) {
  assert_logical_scalar(.check_margin_label)

  if (is.null(.margin_label) || length(dimensions) == 0L) {
    return(invisible(NULL))
  }

  factor_info <- column_info$factors
  if (
    identical(backend$kind, "duckdb") &&
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
  check_margin_label_collision(check_data, dimensions, .margin_label)
}

check_margin_label_collision <- function(data, col_names, margin_label) {
  assert_string_scalar(margin_label)
  stopifnot(is.character(col_names), !anyNA(col_names))

  data <- dplyr::select(.data = data, dplyr::all_of(col_names))
  checks <- lapply(
    col_names,
    function(col) {
      column <- rlang::sym(col)
      condition <- if (is.na(margin_label)) {
        rlang::expr(is.na(!!column))
      } else {
        rlang::expr(!!column == !!margin_label)
      }
      rlang::expr(
        sum(
          dplyr::if_else(!!condition, 1L, 0L, missing = 0L),
          na.rm = TRUE
        )
      )
    }
  )
  names(checks) <- col_names
  found <- dplyr::collect(dplyr::summarize(data, !!!checks))
  found <- vapply(
    col_names,
    function(col) {
      nrow(found) > 0L && isTRUE(found[[col]][[1L]] > 0)
    },
    logical(1)
  )

  if (!any(found)) {
    return(invisible(NULL))
  }

  bad_cols <- paste0("`", names(found)[found], "`", collapse = ", ")
  label <- if (is.na(margin_label)) "NA" else paste0('"', margin_label, '"')
  stop(
    label,
    " is already present in grouping column",
    if (sum(found) == 1L) " " else "s ",
    bad_cols,
    ". Choose another `.margin_label` or set ",
    "`.check_margin_label = FALSE`.",
    call. = FALSE
  )
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

  dplyr::select(
    .data,
    dplyr::all_of(c(plan$by, plan$dimensions)),
    dplyr::everything()
  )
}
