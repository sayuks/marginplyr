# Materialize a direct DuckDB expansion without dbplyr's symbol-based select.
# That select reads a generated `.env` symbol as the tidyselect pronoun.
# The caller has a lazy result whose SQL already quotes its public columns.
#' @exportS3Method dplyr::compute
#' @noRd
compute.marginplyr_duckdb_env <- function(x, name = NULL,
                                          temporary = TRUE,
                                          overwrite = FALSE,
                                          unique_indexes = list(),
                                          indexes = list(),
                                          analyze = TRUE, ...,
                                          sql_options = NULL) {
  if (is.null(name)) {
    if (!temporary) {
      abort_marginplyr("`name` must be provided when `temporary = FALSE`.",
                       call = rlang::caller_call())
    }
    name <- basename(tempfile(pattern = "marginplyr_result_"))
  }
  name <- dbplyr::as_table_path(name, x$con)
  sql <- dbplyr::sql_render(x, sql_options = sql_options)
  created <- dbplyr::db_compute(
    x$con, name, sql, temporary = temporary, overwrite = overwrite,
    unique_indexes = unique_indexes, indexes = indexes, analyze = analyze, ...
  )
  dplyr::tbl(x$con, created, vars = as.character(dplyr::tbl_vars(x)))
}
