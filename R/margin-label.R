normalize_margin_label <- function(.margin_label) {
  if (is.null(.margin_label)) {
    return(NULL)
  }
  if (!is.character(.margin_label) || length(.margin_label) == 0L) {
    abort_marginplyr_flat(
      paste0(
        "`.margin_label` must be `NULL`, an unnamed character scalar, or a ",
        "named character vector."
      )
    )
  }
  label_names <- names(.margin_label)
  if (is.null(label_names)) {
    if (length(.margin_label) != 1L) {
      abort_marginplyr_flat(
        "An unnamed `.margin_label` must be a character vector of length 1."
      )
    }
    return(.margin_label)
  }
  if (anyNA(label_names)) {
    abort_marginplyr_flat(
      "`.margin_label` names must not be missing."
    )
  }
  if (any(!nzchar(label_names))) {
    abort_marginplyr_flat(
      "`.margin_label` names must not be empty."
    )
  }
  if (anyDuplicated(label_names)) {
    abort_marginplyr_flat(
      "`.margin_label` names must not be duplicated."
    )
  }
  .margin_label
}

resolve_margin_labels <- function(.margin_label, dimensions) {
  stopifnot(is.character(dimensions))

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
    abort_marginplyr_flat(
      paste0(
        "`.margin_label` must not name fixed `.by` column",
        if (length(fixed_names) == 1L) " " else "s ",
        paste0("`", fixed_names, "`", collapse = ", "),
        "."
      )
    )
  }
  unknown_names <- setdiff(label_names, dimensions)
  if (length(unknown_names) > 0L) {
    abort_marginplyr_flat(
      paste0(
        "`.margin_label` has unknown dimension name",
        if (length(unknown_names) == 1L) " " else "s ",
        paste0("`", unknown_names, "`", collapse = ", "),
        "."
      )
    )
  }
  missing_names <- setdiff(dimensions, label_names)
  if (length(missing_names) > 0L) {
    abort_marginplyr_flat(
      paste0(
        "`.margin_label` must name every Margin dimension; missing ",
        paste0("`", missing_names, "`", collapse = ", "),
        "."
      )
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
      abort_marginplyr_flat(
        paste0(
          "`NA_character_` is already a factor level in grouping column",
          if (length(na_level_cols) == 1L) " " else "s ",
          paste0("`", na_level_cols, "`", collapse = ", "),
          ". Use `NULL` for a typed-missing Margin label while preserving ",
          "the NA level."
        )
      )
    }
  }

  # A declared collision is read off the levels ADR 0002 already acquired, so
  # finding it sends no query and nothing had to be asked for (ADR 0020). It
  # sits above the gate for the reason the NA-level check above it does.
  check_declared_label_collision(
    margin_labels = margin_labels,
    factor_info = factor_info
  )

  if (!.check_margin_label) {
    return(invisible(NULL))
  }

  check_observed_label_collision(
    .data,
    margin_labels = margin_labels,
    factor_info = factor_info
  )
}

check_declared_label_collision <- function(margin_labels,
                                           factor_info = list()) {
  stopifnot(is.character(names(margin_labels)), !anyNA(names(margin_labels)))

  if (length(factor_info) == 0L) {
    return(invisible(NULL))
  }

  declared <- vapply(
    factor_info,
    function(info) {
      label <- margin_labels[[info$col]]
      !is_missing_margin_label(label) && label %in% info$levels
    },
    logical(1)
  )
  names(declared) <- vapply(factor_info, function(info) info$col, character(1))
  if (!any(declared)) {
    return(invisible(NULL))
  }

  abort_margin_label_collision(margin_labels, declared, kind = "declared")
}

check_observed_label_collision <- function(data,
                                           margin_labels,
                                           factor_info = list()) {
  col_names <- names(margin_labels)
  stopifnot(is.character(col_names), !anyNA(col_names))

  factor_cols <- vapply(factor_info, function(info) info$col, character(1))
  # A factor dimension states its values in its levels, and a label equal to
  # one of them was rejected above, so reading its column could only find a
  # value the levels do not contain. A missing label is the exception: whether
  # the column holds a missing value is not something the levels record.
  read_cols <- Filter(
    function(col) {
      label <- margin_labels[[col]]
      !is.null(label) && !(col %in% factor_cols && !is.na(label))
    },
    col_names
  )
  # No column left to read is no query, rather than a query selecting nothing:
  # a lazy input is not contacted to aggregate a set of constants.
  if (length(read_cols) == 0L) {
    return(invisible(NULL))
  }

  data <- dplyr::select(.data = data, dplyr::all_of(read_cols))
  checks <- lapply(
    read_cols,
    function(col) {
      margin_label <- margin_labels[[col]]
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
  names(checks) <- read_cols
  found <- dplyr::collect(dplyr::summarize(data, !!!checks))
  found <- vapply(
    read_cols,
    function(col) {
      nrow(found) > 0L && isTRUE(found[[col]][[1L]] > 0)
    },
    logical(1)
  )

  if (!any(found)) {
    return(invisible(NULL))
  }

  abort_margin_label_collision(margin_labels, found, kind = "observed")
}

abort_margin_label_collision <- function(margin_labels, found, kind) {
  stopifnot(
    is.logical(found),
    any(found),
    identical(kind, "declared") || identical(kind, "observed")
  )

  bad_cols <- paste0("`", names(found)[found], "`", collapse = ", ")
  bad_labels <- margin_labels[names(found)[found]]
  label_values <- vapply(
    bad_labels,
    function(label) if (is.na(label)) "NA" else paste0('"', label, '"'),
    character(1)
  )
  one_label <- length(unique(label_values)) == 1L
  label <- if (one_label) unique(label_values) else "Margin labels"
  verb <- if (one_label) " is" else " are"
  presence <- if (identical(kind, "declared")) {
    if (one_label) " a factor level in" else " factor levels in"
  } else {
    " present in"
  }
  # `.check_margin_label = FALSE` is offered only where it is a remedy. It
  # turns off the read, and a declared collision is not found by reading, so
  # naming it there would send a caller to an argument that changes nothing.
  remedy <- if (identical(kind, "declared")) {
    "Choose another `.margin_label`."
  } else {
    "Choose another `.margin_label` or set `.check_margin_label = FALSE`."
  }
  abort_marginplyr_flat(
    paste0(
      label,
      verb,
      " already",
      presence,
      " grouping column",
      if (sum(found) == 1L) " " else "s ",
      bad_cols,
      ". ",
      remedy
    )
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
        missing_sentinel <- factor_missing_sentinel(
          info,
          margin_labels[[col]]
        )
        rlang::expr(
          encode_factor_for_margin(
            !!margin_column_pronoun(col),
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
