reconstruct_factor <- function(data, info, .margin_name) {
  UseMethod("reconstruct_factor")
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

#' @exportS3Method
#' @noRd
reconstruct_factor.data.frame <- function(data, info, .margin_name) {
  col <- info$col
  # force .margin_name to the beginning of the level
  new_levels <- union(.margin_name, info$levels)
  ord <- info$ordered
  exc <- if (info$has_na_in_level) NULL else NA
  dplyr::mutate(
    .data = data,
    "{col}" := factor(
      x = .data[[col]], # nolint: object_usage_linter
      # x,
      levels = new_levels,
      # If .margin_name is not NA and there is NA in the original level,
      # keep it (set exclude = NULL).
      # The case where .margin_name is NA and level contains NA is
      # not present here, as it is an error in the prior step.
      # This means that if .margin_name is NA,
      # info$has_na_in_level always returns FALSE, so exclude = NA.
      # This excludes NA from the level.
      # This is consistent with the default base::factor().
      exclude = exc,
      ordered = ord
    )
  )
}

#' @exportS3Method
#' @noRd
reconstruct_factor.dtplyr_step <- reconstruct_factor.data.frame

# https://github.com/duckdb/duckdb-r/issues/188#issuecomment-2294095426
#' @exportS3Method
#' @noRd
reconstruct_factor.tbl_duckdb_connection <- function(data,
                                                     info,
                                                     .margin_name) {
  col <- info$col # nolint: object_usage_linter
  new_levels <- union(.margin_name, info$levels) # nolint: object_usage_linter
  con <- dbplyr::remote_con(data)
  sql_query <- dbplyr::sql_glue2(
    con,
    "CAST({.id col} AS ENUM {new_levels*})"
  )
  dplyr::mutate(
    .data = data,
    "{col}" := sql_query
  )
}
