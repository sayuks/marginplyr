normalize_margin_label <- function(.margin_label) {
  if (is.null(.margin_label)) {
    return(NULL)
  }
  if (!is.character(.margin_label) || length(.margin_label) == 0L) {
    abort_marginplyr(paste0(
      "{.arg .margin_label} must be {.code NULL}, an unnamed character ",
      "scalar, or a named character vector."
    ))
  }
  label_names <- names(.margin_label)
  if (is.null(label_names)) {
    if (length(.margin_label) != 1L) {
      abort_marginplyr(paste0(
        "An unnamed {.arg .margin_label} must be a character vector of ",
        "length 1."
      ))
    }
    return(.margin_label)
  }
  if (anyNA(label_names)) {
    abort_marginplyr("{.arg .margin_label} names must not be missing.")
  }
  if (any(!nzchar(label_names))) {
    abort_marginplyr("{.arg .margin_label} names must not be empty.")
  }
  if (anyDuplicated(label_names)) {
    abort_marginplyr("{.arg .margin_label} names must not be duplicated.")
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
  # Each refusal below names whichever names it is refusing, and each carries
  # them in an `i` bullet rather than in its main line, per ADR 0023's
  # surviving line condition: how many of them arrive is the caller's decision.
  fixed_names <- intersect(label_names, by)
  if (length(fixed_names) > 0L) {
    abort_marginplyr(c(
      paste0(
        "{.arg .margin_label} must not name fixed {.arg .by} ",
        "{cli::qty(length(fixed_names))}column{?s}:"
      ),
      i = "{.var {fixed_names}}."
    ))
  }
  unknown_names <- setdiff(label_names, dimensions)
  if (length(unknown_names) > 0L) {
    abort_marginplyr(c(
      paste0(
        "{.arg .margin_label} has unknown dimension ",
        "{cli::qty(length(unknown_names))}name{?s}:"
      ),
      i = "{.var {unknown_names}}."
    ))
  }
  missing_names <- setdiff(dimensions, label_names)
  if (length(missing_names) > 0L) {
    abort_marginplyr(c(
      "{.arg .margin_label} must name every Margin dimension.",
      i = "Missing {.var {missing_names}}."
    ))
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
      abort_marginplyr(c(
        paste0(
          "{.code NA_character_} is already a factor level in grouping ",
          "{cli::qty(length(na_level_cols))}column{?s}:"
        ),
        i = "{.var {na_level_cols}}.",
        i = paste0(
          "Use {.code NULL} for a typed-missing Margin label while ",
          "preserving the NA level."
        )
      ))
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

  bad_cols <- names(found)[found]
  # `vapply()` rather than `unlist()`, which would silently drop a `NULL` and
  # leave the arms below reading a shorter vector. No caller can pass one --
  # a missing label is filtered out before either collision is looked for --
  # so the shape it would arrive in is a defect, and this stops on it.
  bad_labels <- vapply(
    margin_labels[bad_cols],
    identity,
    character(1),
    USE.NAMES = FALSE
  )
  if (identical(kind, "declared")) {
    abort_declared_label_collision(bad_cols, bad_labels)
  }
  abort_observed_label_collision(bad_cols, bad_labels)
}

# The two kinds are written out rather than shared, in the shape `R/share.R`
# uses wherever two calls differ by a whole clause: the kind chooses how the
# collision is named, and under the declared kind that clause pluralizes with
# the subject, so one template would have to pick between two noun pairs. The
# subject is the same shape one level down -- either the one colliding label or
# a plural noun standing in for several distinct ones -- which is a whole
# element rather than a plural of one word, and `{?}` cannot interpolate a
# value into the arm it picks. What the shape costs is the bullet the two arms
# share, written out in each.
#
# `.check_margin_label = FALSE` is offered only where it is a remedy. It turns
# off the read, and a declared collision is not found by reading, so naming it
# here would send a caller to an argument that changes nothing.
abort_declared_label_collision <- function(bad_cols, bad_labels) {
  if (length(unique(bad_labels)) == 1L) {
    abort_marginplyr(c(
      paste0(
        "{.val {unique(bad_labels)}} is already a factor level in grouping ",
        "{cli::qty(length(bad_cols))}column{?s}:"
      ),
      i = "{.var {bad_cols}}.",
      i = "Choose another {.arg .margin_label}."
    ))
  }
  abort_marginplyr(c(
    paste0(
      "Margin labels are already factor levels in grouping ",
      "{cli::qty(length(bad_cols))}column{?s}:"
    ),
    i = "{.var {bad_cols}}.",
    i = "Choose another {.arg .margin_label}."
  ))
}

abort_observed_label_collision <- function(bad_cols, bad_labels) {
  if (length(unique(bad_labels)) == 1L) {
    abort_marginplyr(c(
      paste0(
        "{.val {unique(bad_labels)}} is already present in grouping ",
        "{cli::qty(length(bad_cols))}column{?s}:"
      ),
      i = "{.var {bad_cols}}.",
      i = paste0(
        "Choose another {.arg .margin_label} or set ",
        "{.code .check_margin_label = FALSE}."
      )
    ))
  }
  abort_marginplyr(c(
    paste0(
      "Margin labels are already present in grouping ",
      "{cli::qty(length(bad_cols))}column{?s}:"
    ),
    i = "{.var {bad_cols}}.",
    i = paste0(
      "Choose another {.arg .margin_label} or set ",
      "{.code .check_margin_label = FALSE}."
    )
  ))
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
