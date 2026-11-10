reconstruct_factor <- function(data, info, .margin_name, position = "last") {
  UseMethod("reconstruct_factor")
}

factor_missing_sentinel <- function(info, .margin_name) {
  sentinel <- "..marginplyr_missing_factor_code"
  reserved <- c(info$levels, .margin_name)
  while (sentinel %in% reserved) {
    sentinel <- paste0(sentinel, "_")
  }
  sentinel
}

# Whether a factor column crosses the branch union as character despite a
# missing Margin label, which is what `encode_missing_label` in
# `R/backend-metadata.R` records for the column. Both sides of the route read
# this, so neither can encode what the other does not rebuild.
encodes_missing_label_factor <- function(info, label) {
  is_missing_margin_label(label) && isTRUE(info$encode_missing_label)
}

# The sentinel each factor column is carried as while it crosses the branch
# union as character, keyed by column. Derived once so that the value a branch
# omitting a dimension writes is the one the branches including it encode to,
# rather than the two arms deriving it apart.
margin_factor_sentinels <- function(factor_info, margin_labels) {
  stats::setNames(
    lapply(
      factor_info,
      function(info) {
        factor_missing_sentinel(info, margin_label_of(margin_labels, info$col))
      }
    ),
    vapply(factor_info, function(info) info$col, character(1))
  )
}

# Compare a captured factor before any branch can erase its stored definition.
# Matching values pass through with their classes and attributes untouched.
validate_margin_factor <- function(x, levels, ordered, col) {
  stored <- as.vector(unclass(attr(x, "levels", exact = TRUE)))
  if (!is.factor(x) || !identical(stored, levels) ||
        !identical(is.ordered(x), ordered)) {
    abort_marginplyr(c(
      "The captured factor definition of {.var {col}} has changed.",
      i = "Rebuild the Margin result after changing the input schema."
    ))
  }
  x
}

# Validate the R factors the operation captured using the input already being
# processed. Deferred expressions retain construction-time definitions.
validate_margin_factor_input <- function(data, factor_info, backend,
                                         plan, margin_labels) {
  if (!(backend$kind %in% c("local", "dtplyr")) ||
        length(factor_info) == 0L) {
    return(data)
  }
  encoded <- vapply(factor_info, function(info) {
    label <- margin_label_of(margin_labels, info$col)
    encodes_missing_label_factor(info, label) ||
      (info$col %in% plan$dimensions && !is_missing_margin_label(label))
  }, logical(1))
  # A single native branch neither decomposes nor combines factors. Leave
  # that passthrough under dplyr's delegated class rules (ADR 0016).
  if (nrow(plan$grouping_masks) == 1L && !any(encoded)) {
    return(data)
  }
  columns <- vapply(factor_info, function(info) info$col, character(1))
  margin_safe_column_reads(data, columns, function(source, safe_name) {
    expressions <- lapply(factor_info, function(info) {
      rlang::call2(
        marginplyr_private_call("validate_margin_factor"),
        margin_column_pronoun(safe_name(info$col)),
        levels = info$levels, ordered = info$ordered, col = info$col
      )
    })
    names(expressions) <- unname(vapply(columns, safe_name, character(1)))
    dplyr::mutate(source, !!!expressions)
  })
}

# Encode stored level values for reconstruction, preserving missing codes
# separately from observations on an NA level when requested.
encode_factor_for_margin <- function(x,
                                     missing_sentinel,
                                     preserve_missing_value,
                                     preserve_element_names = FALSE) {
  # Reconstruction matches declared levels, not a subclass's display strings.
  result <- as.character.factor(x)
  if (preserve_element_names) names(result) <- names(x)
  if (!isTRUE(preserve_missing_value)) {
    return(result)
  }

  # A subclass can report an observation on its NA level as missing.
  result[is.na(unclass(x))] <- missing_sentinel
  result
}

reconstruct_factor_vector <- function(x,
                                      new_levels,
                                      ordered,
                                      missing_sentinel) {
  missing_value <- !is.na(x) & x == missing_sentinel
  codes <- match(x, new_levels)
  # Supplied definitions were checked before branching. An ordinary value
  # outside this domain is a package-created invalid mapping (ADR 0015).
  stopifnot(
    "An unmatched value reached factor reconstruction." =
      !any(!is.na(x) & !missing_value & is.na(codes))
  )
  codes[missing_value] <- NA_integer_
  structure(
    codes,
    names = names(x),
    levels = new_levels,
    class = if (ordered) c("ordered", "factor") else "factor"
  )
}

margin_factor_levels <- function(info, .margin_name, position) {
  # An invariant, not a Package condition (ADR-0015). A Margin label equal to
  # a level `info` holds has already been rejected before anything executes:
  # every Margin verb calls `validate_margin_operation()` before the
  # `finalize_margin_operation()` that calls `restore_margin_factors()`, which
  # is the only caller of this. A level equal to the label would otherwise be
  # deduplicated away and re-appended, moving it to the end of the levels for no
  # reason a caller could see.
  stopifnot(
    "A Margin label equal to a declared factor level reached execution." =
      !(.margin_name %in% info$levels)
  )

  if (identical(position, "first")) {
    c(.margin_name, info$levels)
  } else {
    c(info$levels, .margin_name)
  }
}

restore_margin_factors <- function(.data,
                                   factor_info,
                                   margin_labels,
                                   position = "last") {
  if (length(factor_info) == 0L) {
    return(.data)
  }

  Reduce(
    function(data, info) {
      label <- margin_label_of(margin_labels, info$col)
      # A missing label adds no level, so a column the branches carried as
      # their own class arrives with its levels already intact. The exception
      # is one that crossed the union as character to keep them, which is
      # rebuilt on its declared levels: `NULL` appends none.
      if (is_missing_margin_label(label)) {
        if (!encodes_missing_label_factor(info, label)) {
          return(data)
        }
        return(reconstruct_factor(data, info, NULL, position = position))
      }
      reconstruct_factor(data, info, label, position = position)
    },
    factor_info,
    init = .data
  )
}

#' @exportS3Method
#' @noRd
reconstruct_factor.data.frame <- function(data,
                                          info,
                                          .margin_name,
                                          position = "last") {
  col <- info$col
  new_levels <- margin_factor_levels(info, .margin_name, position)
  ord <- info$ordered
  missing_sentinel <- factor_missing_sentinel(info, .margin_name)
  # Every value is injected rather than named. A bare name here resolves
  # against the data mask first, so a source column called `new_levels`,
  # `ord`, or `missing_sentinel` would supply the argument instead of the
  # local, silently rebuilding the factor from the wrong levels.
  #
  # The dtplyr method below shares this deferred expression, so its helper
  # lookup uses the bound namespace accessor.
  dplyr::mutate(
    .data = data,
    "{col}" := !!rlang::call2(
      marginplyr_private_call("reconstruct_factor_vector"),
      margin_column_pronoun(col),
      new_levels = new_levels,
      ordered = ord,
      missing_sentinel = missing_sentinel
    )
  )
}

#' @exportS3Method
#' @noRd
reconstruct_factor.dtplyr_step <- function(data,
                                           info,
                                           .margin_name,
                                           position = "last") {
  margin_safe_column_reads(
    data, info$col,
    function(source, safe_name) {
      safe_info <- info
      safe_info$col <- safe_name(info$col)
      reconstruct_factor.data.frame(
        source, safe_info, .margin_name, position = position
      )
    }
  )
}

# https://github.com/duckdb/duckdb-r/issues/188#issuecomment-2294095426
#' @exportS3Method
#' @noRd
reconstruct_factor.tbl_duckdb_connection <- function(data,
                                                     info,
                                                     .margin_name,
                                                     position = "last") {
  # Both are read only from the glue string below, which codetools cannot see.
  # nolint start: object_usage_linter.
  col <- info$col
  new_levels <- margin_factor_levels(info, .margin_name, position)
  # nolint end
  con <- dbplyr::remote_con(data)
  sql_query <- dbplyr::sql_glue2(
    con,
    "CAST({.id col} AS ENUM {new_levels*})"
  )
  # Injected for the same reason as the data frame method: a source column
  # named `sql_query` would otherwise replace the cast with that column.
  dplyr::mutate(
    .data = data,
    "{col}" := !!sql_query
  )
}
