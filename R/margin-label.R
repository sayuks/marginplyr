# The Margin label a caller wrote, as the shape the rest of the operation
# reads: `NULL`, an unnamed character scalar, or a named list of per-dimension
# labels. A named character vector is converted to that list here, so the two
# named spellings are one downstream (ADR 0012).
normalize_margin_label <- function(.margin_label) {
  if (is.null(.margin_label)) {
    return(NULL)
  }
  # An unnamed list is not the list form: its elements name no dimension, and
  # a caller who meant one label wrote the scalar. Bare, because a one-row
  # data frame is otherwise a named list whose columns pass as labels.
  is_label_list <- rlang::is_bare_list(.margin_label) &&
    !is.null(names(.margin_label))
  if (!is_label_list &&
        (!is.character(.margin_label) || length(.margin_label) == 0L)) {
    abort_marginplyr(paste0(
      "{.arg .margin_label} must be {.code NULL}, an unnamed character ",
      "scalar, or a named character vector or list."
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
  if (is_label_list) {
    validate_margin_label_elements(.margin_label)
    return(.margin_label)
  }
  as.list(.margin_label)
}

# Each element of a named-list `.margin_label` is one dimension's label, so it
# is what an unnamed scalar may be, plus the `NULL` the list exists to carry.
validate_margin_label_elements <- function(.margin_label) {
  bad <- vapply(
    .margin_label,
    function(label) {
      !is.null(label) && !(is.character(label) && length(label) == 1L)
    },
    logical(1)
  )
  if (!any(bad)) {
    return(invisible(NULL))
  }
  # Read only from the cli template below, which codetools cannot see.
  # nolint start: object_usage_linter.
  bad_names <- names(.margin_label)[bad]
  # nolint end
  abort_marginplyr(c(
    paste0(
      "{.arg .margin_label} list elements must each be {.code NULL} or a ",
      "character scalar:"
    ),
    i = "{.var {bad_names}}."
  ))
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
  # them in an `i` bullet rather than in its main line, per
  # ADR 0023's condition 2: how many of them arrive is the caller's decision.
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

# One column's Margin label, and `NULL` for a column that has none.
# `factor_info` names the factor columns crossing the branch union while
# `margin_labels` is keyed to the Margin dimensions alone, so every read that
# iterates the former reaches a name the latter does not hold (#415), where
# `[[` raises a subscript error rather than answering that it is unlabelled.
margin_label_of <- function(margin_labels, col) {
  if (!col %in% names(margin_labels)) {
    return(NULL)
  }
  margin_labels[[col]]
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
        # The list spelling, because a bare `NULL` is the whole of
        # `.margin_label` and so is not one dimension's answer (ADR 0012).
        i = paste0(
          "Use {.code NULL} in a named-list {.arg .margin_label} for a ",
          "typed-missing Margin label while preserving the NA level."
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
      label <- margin_label_of(margin_labels, info$col)
      !is_missing_margin_label(label) && label %in% info$levels
    },
    logical(1)
  )
  names(declared) <- vapply(factor_info, function(info) info$col, character(1))
  if (!any(declared)) {
    return(invisible(NULL))
  }

  abort_margin_label_collision(
    margin_labels,
    declared,
    on_collision = abort_declared_label_collision
  )
}

check_observed_label_collision <- function(data,
                                           margin_labels,
                                           factor_info = list()) {
  col_names <- names(margin_labels)
  stopifnot(is.character(col_names), !anyNA(col_names))

  factor_cols <- vapply(factor_info, function(info) info$col, character(1))
  # A factor dimension states its values in its levels, and a label equal to
  # one of them was rejected above, so reading its column could only find a
  # value the levels do not contain. A typed-missing label is not a collision
  # (ADR 0012), so neither spelling of one selects a column either.
  read_cols <- Filter(
    function(col) {
      !is_missing_margin_label(margin_labels[[col]]) && !(col %in% factor_cols)
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
      rlang::expr(
        sum(
          dplyr::if_else(
            as.character(!!column) == !!margin_label,
            1L,
            0L,
            missing = 0L
          ),
          na.rm = TRUE
        )
      )
    }
  )
  names(checks) <- read_cols
  query <- dplyr::summarize(data, !!!checks)
  record_sent_query("observed_label_collision", query)
  found <- dplyr::collect(query)
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

  abort_margin_label_collision(
    margin_labels,
    found,
    on_collision = abort_observed_label_collision
  )
}

# The columns a collision was found in and the labels that collided with them,
# which is what both refusals below name. Each caller hands in the refusal that
# speaks for its own kind rather than a word naming that kind -- the shape
# `R/grouping-plan.R`'s two renaming refusals take, since a discriminator this
# function would only translate back into a call is one neither caller needs to
# compute.
abort_margin_label_collision <- function(margin_labels, found, on_collision) {
  stopifnot(is.logical(found), any(found), is.function(on_collision))

  bad_cols <- names(found)[found]
  bad_labels <- vapply(
    margin_labels[bad_cols],
    identity,
    character(1),
    USE.NAMES = FALSE
  )
  on_collision(bad_cols, bad_labels)
}

# Four arms across the two refusals below, rather than one template with two
# branches inside it. ADR 0023's third amendment was written about this
# refusal and is authoritative for what the arms differ by and for why a
# branch on a count is still inside its `{?}` rule.
#
# What the shape costs is every element the arms share, written out in each:
# the bullet carrying the columns four times, the noun `column{?s}` inflects
# four times, and each remedy twice.
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
  sentinels <- margin_factor_sentinels(factor_info, margin_labels)
  # Columns whose Margin label is missing and which cross the union as
  # character anyway, because it would otherwise drop their declared NA level
  # (`encodes_missing_label_factor()`). Empty on every other backend, and the
  # set is read from the whole plan rather than from this branch so that both
  # arms below put the column in the same encoding.
  missing_label_encoded <- vapply(
    Filter(
      function(info) {
        encodes_missing_label_factor(
          info,
          margin_label_of(margin_labels, info$col)
        )
      },
      factor_info
    ),
    function(info) info$col,
    character(1)
  )
  # A column `factor_info` names that is not a dimension is a fixed `.by` key
  # or one the verb passes through, so it is in every branch rather than in
  # the ones that include it, and it is never labelled: only the encoded arm
  # above can select it (#415).
  encoded_factors <- Filter(
    function(info) {
      present <- !(info$col %in% plan$dimensions) || info$col %in% included
      present &&
        (info$col %in% missing_label_encoded ||
           (info$col %in% labelled_included &&
              isTRUE(info$preserve_missing_value)))
    },
    factor_info
  )
  encoded_factor_cols <- character()
  if (length(encoded_factors) > 0L) {
    encoded_exprs <- lapply(
      encoded_factors,
      function(info) {
        col <- info$col
        rlang::expr(
          encode_factor_for_margin(
            !!margin_column_pronoun(col),
            missing_sentinel = !!sentinels[[col]],
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
  # dtplyr renders a scalar overwrite as data.table `:=`. On a multi-row factor
  # column, data.table recycles it without replacing the column class, leaving
  # an ordered factor opposite the character branch at `funion()`. A full-size
  # value replaces the column so factor restoration remains after the union.
  dtplyr_factor_cols <- if (inherits(.data, "dtplyr_step")) {
    vapply(factor_info, function(info) info$col, character(1))
  } else {
    character()
  }
  values <- lapply(
    omitted,
    function(col) {
      label <- margin_labels[[col]]
      if (!is_missing_margin_label(label)) {
        if (col %in% dtplyr_factor_cols) {
          return(rlang::expr(rep(!!label, dplyr::n())))
        }
        return(label)
      }
      if (col %in% missing_label_encoded) {
        # The typed missing this margin row carries, spelled as the included
        # branches spell one. `as.character()` of the prototype cannot: it is
        # `NA`, which is also what a value on the NA level becomes, and the
        # union is where the two would stop being distinguishable (ADR 0012).
        # Full size for the reason the labelled arm above is.
        return(rlang::expr(rep(!!sentinels[[col]], dplyr::n())))
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
