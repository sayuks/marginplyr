# Materialize a DuckDB expansion without dbplyr's symbol-based select.
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
  vars <- as.character(dplyr::tbl_vars(x))
  duckdb_check_indexes(indexes, vars, "indexes")
  duckdb_check_indexes(unique_indexes, vars, "unique_indexes")
  sql <- dbplyr::sql_render(x, sql_options = sql_options)
  created <- dbplyr::db_compute(
    x$con, name, sql, temporary = temporary, overwrite = overwrite,
    unique_indexes = unique_indexes, indexes = indexes, analyze = analyze, ...
  )
  result <- dplyr::tbl(x$con, created, vars = vars)
  result <- dplyr::group_by(result, !!!rlang::syms(dplyr::group_vars(x)))
  dbplyr::window_order(result, !!!dbplyr::op_sort(x))
}

# Reject index columns absent from the public result before a destination write.
# The caller supplies the lazy result's names, without executing its query.
duckdb_check_indexes <- function(indexes, vars, arg,
                                 call = rlang::caller_call(2)) {
  if (is.null(indexes)) {
    return(invisible(NULL))
  }
  if (!is.list(indexes) && !is.character(indexes)) {
    abort_marginplyr(
      "{.arg {arg}} must be a character vector or a list of characters.",
      call = call
    )
  }
  missing <- setdiff(unlist(indexes), vars)
  if (length(missing) > 0L) {
    abort_marginplyr(c(
      "All columns specified through {.arg {arg}} must exist in {.arg x}.",
      i = "Missing columns in {.arg {arg}}: {.field {missing}}."
    ), call = call)
  }
  invisible(NULL)
}
