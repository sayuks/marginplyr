reconstruct_factor <- function(data, info, .margin_name) {
  UseMethod("reconstruct_factor")
}

#' @method reconstruct_factor data.frame
reconstruct_factor.data.frame <- function(data, info, .margin_name) {
  col <- info$col
  # force .margin_name to the beginning of the level
  new_levels <- union(.margin_name, info$levels)
  dplyr::mutate(
    .data = data,
    "{col}" := factor(
      dplyr::pull(dplyr::pick(dplyr::all_of(col))),
      levels = new_levels,
      # If .margin_name is not NA and there is NA in the original level,
      # keep it (set exclude = NULL).
      # The case where .margin_name is NA and level contains NA is
      # not present here, as it is an error in the prior step.
      # This means that if .margin_name is NA,
      # info$has_na_in_level always returns FALSE, so exclude = NA.
      # This excludes NA from the level.
      # This is consistent with the default base::factor().
      exclude = if (info$has_na_in_level) NULL else NA,
      ordered = info$ordered
    )
  )
}

# https://github.com/duckdb/duckdb-r/issues/188#issuecomment-2294095426
#' @method reconstruct_factor tbl_duckdb_connection
reconstruct_factor.tbl_duckdb_connection <- function(data,
                                                     info,
                                                     .margin_name) {
  col <- info$col
  # force .margin_name to the beginning of the level　
  new_levels <- union(.margin_name, info$levels)
  sql_query <- sprintf(
    "cast(%s AS ENUM (%s))",
    col,
    paste0("'", new_levels, "'", collapse = ", ")
  )
  dplyr::mutate(
    .data = data,
    "{col}" := dplyr::sql(sql_query)
  )
}
