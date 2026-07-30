normalize_margin_label <- function(.margin_label) {
  if (is.null(.margin_label)) {
    return(NULL)
  }
  if (!is.character(.margin_label) || length(.margin_label) == 0L) {
    stop(
      "`.margin_label` must be `NULL`, an unnamed character scalar, or a ",
      "named character vector.",
      call. = FALSE
    )
  }
  label_names <- names(.margin_label)
  if (is.null(label_names)) {
    if (length(.margin_label) != 1L) {
      stop(
        "An unnamed `.margin_label` must be a character vector of length 1.",
        call. = FALSE
      )
    }
    return(.margin_label)
  }
  if (anyNA(label_names)) {
    stop("`.margin_label` names must not be missing.", call. = FALSE)
  }
  if (any(!nzchar(label_names))) {
    stop("`.margin_label` names must not be empty.", call. = FALSE)
  }
  if (anyDuplicated(label_names)) {
    stop("`.margin_label` names must not be duplicated.", call. = FALSE)
  }
  .margin_label
}

resolve_margin_labels <- function(.margin_label, dimensions, by) {
  stopifnot(is.character(dimensions), is.character(by))

  if (is.null(.margin_label)) {
    return(stats::setNames(rep(list(NULL), length(dimensions)), dimensions))
  }
  if (is.null(names(.margin_label))) {
    return(stats::setNames(
      rep(as.list(.margin_label), length(dimensions)),
      dimensions
    ))
  }

  labels <- as.list(.margin_label[match(dimensions, names(.margin_label))])
  stats::setNames(labels, dimensions)
}

validate_margin_label_names <- function(.margin_label, dimensions, by) {
  if (is.null(.margin_label) || is.null(names(.margin_label))) {
    return(invisible(NULL))
  }

  label_names <- names(.margin_label)
  fixed_names <- intersect(label_names, by)
  if (length(fixed_names) > 0L) {
    stop(
      "`.margin_label` must not name fixed `.by` column",
      if (length(fixed_names) == 1L) " " else "s ",
      paste0("`", fixed_names, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unknown_names <- setdiff(label_names, dimensions)
  if (length(unknown_names) > 0L) {
    stop(
      "`.margin_label` has unknown dimension name",
      if (length(unknown_names) == 1L) " " else "s ",
      paste0("`", unknown_names, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  missing_names <- setdiff(dimensions, label_names)
  if (length(missing_names) > 0L) {
    stop(
      "`.margin_label` must name every Margin dimension; missing ",
      paste0("`", missing_names, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

is_missing_margin_label <- function(label) {
  is.null(label) || is.na(label)
}

validate_margin_label <- function(.data,
                                  dimensions,
                                  by,
                                  .margin_label,
                                  margin_labels,
                                  .check_margin_label,
                                  column_info) {
  assert_logical_scalar(.check_margin_label)
  validate_margin_label_names(
    .margin_label,
    dimensions = dimensions,
    by = by
  )

  if (length(dimensions) == 0L) {
    return(invisible(NULL))
  }

  factor_info <- column_info$factors
  na_label_cols <- names(Filter(
    function(label) !is.null(label) && is.na(label),
    margin_labels
  ))
  if (length(na_label_cols) > 0L && length(factor_info) > 0L) {
    na_level_cols <- Filter(
      function(x) {
        x$col %in% na_label_cols && x$has_na_in_level
      },
      factor_info
    )
    na_level_cols <- vapply(na_level_cols, function(x) x$col, character(1))
    if (length(na_level_cols) > 0L) {
      stop(
        "`NA_character_` is already a factor level in grouping column",
        if (length(na_level_cols) == 1L) " " else "s ",
        paste0("`", na_level_cols, "`", collapse = ", "),
        ". Use `NULL` for a typed-missing Margin label while preserving the ",
        "NA level.",
        call. = FALSE
      )
    }
  }

  if (!.check_margin_label) {
    return(invisible(NULL))
  }

  check_margin_label_collision(
    .data,
    margin_labels = margin_labels,
    factor_info = factor_info
  )
}

check_margin_label_collision <- function(data,
                                         margin_labels,
                                         factor_info = list()) {
  col_names <- names(margin_labels)
  stopifnot(is.character(col_names), !anyNA(col_names))

  checked_labels <- Filter(function(label) !is.null(label), margin_labels)
  if (length(checked_labels) == 0L) {
    return(invisible(NULL))
  }

  factor_cols <- vapply(factor_info, function(info) info$col, character(1))
  factor_levels <- stats::setNames(rep(FALSE, length(col_names)), col_names)
  for (info in factor_info) {
    label <- margin_labels[[info$col]]
    if (!is.null(label) && !is.na(label)) {
      factor_levels[[info$col]] <- label %in% info$levels
    }
  }
  if (any(factor_levels)) {
    abort_margin_label_collision(margin_labels, factor_levels)
  }

  data <- dplyr::select(.data = data, dplyr::all_of(col_names))
  checks <- lapply(
    col_names,
    function(col) {
      margin_label <- margin_labels[[col]]
      if (
        is.null(margin_label) ||
          (col %in% factor_cols && !is.na(margin_label))
      ) {
        return(rlang::expr(0L))
      }
      column <- rlang::sym(col)
      condition <- if (is.na(margin_label)) {
        rlang::expr(is.na(!!column))
      } else {
        rlang::expr(as.character(!!column) == !!margin_label)
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

  abort_margin_label_collision(margin_labels, found)
}

abort_margin_label_collision <- function(margin_labels, found) {
  stopifnot(is.logical(found), any(found))

  bad_cols <- paste0("`", names(found)[found], "`", collapse = ", ")
  bad_labels <- margin_labels[names(found)[found]]
  label_values <- vapply(
    bad_labels,
    function(label) if (is.na(label)) "NA" else paste0('"', label, '"'),
    character(1)
  )
  one_label <- length(unique(label_values)) == 1L
  label <- if (one_label) unique(label_values) else "Margin labels"
  stop(
    label,
    if (one_label) " is" else " are",
    " already present in grouping column",
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
                                margin_labels,
                                prototypes = list(),
                                factor_info = list()) {
  included <- intersect(plan$dimensions, grouping_set)
  omitted <- setdiff(plan$dimensions, grouping_set)

  labelled_included <- Filter(
    function(col) !is_missing_margin_label(margin_labels[[col]]),
    included
  )
  encoded_factors <- Filter(
    function(info) {
      info$col %in% labelled_included &&
        isTRUE(info$preserve_missing_value)
    },
    factor_info
  )
  encoded_factor_cols <- character()
  if (length(encoded_factors) > 0L) {
    encoded_exprs <- lapply(
      encoded_factors,
      function(info) {
        col <- info$col
        missing_sentinel <- factor_missing_sentinel( # nolint: object_usage_linter
          info,
          margin_labels[[col]]
        )
        rlang::expr(
          encode_factor_for_margin( # nolint: object_usage_linter
            .data[[!!col]],
            missing_sentinel = !!missing_sentinel,
            preserve_missing_value = TRUE
          )
        )
      }
    )
    encoded_factor_cols <- vapply(
      encoded_factors,
      function(info) info$col,
      character(1)
    )
    names(encoded_exprs) <- encoded_factor_cols
    .data <- dplyr::mutate(.data, !!!encoded_exprs)
  }
  labelled_as_character <- setdiff(
    labelled_included,
    encoded_factor_cols
  )
  if (length(labelled_as_character) > 0L) {
    .data <- dplyr::mutate(
      .data,
      dplyr::across(dplyr::all_of(labelled_as_character), as.character)
    )
  }
  values <- lapply(
    omitted,
    function(col) {
      label <- margin_labels[[col]]
      if (!is_missing_margin_label(label)) {
        return(label)
      }
      value <- prototypes[[col]]
      if (is.null(value)) NA else value
    }
  )

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
