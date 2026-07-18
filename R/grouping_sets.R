supports_grouping_sets <- function(.data, plan = NULL) {
  if (!inherits(.data, "tbl_lazy")) {
    return(FALSE)
  }

  dialect <- tryCatch(
    dbplyr::sql_dialect(dbplyr::remote_con(.data)),
    error = function(cnd) NULL
  )
  if (is.null(dialect)) {
    return(FALSE)
  }

  is_duckdb <- inherits(dialect, c("duckdb_connection", "sql_dialect_duckdb"))
  is_postgres <- inherits(dialect, "sql_dialect_postgres")

  if (!is.null(plan) && identical(plan$duplicates, "keep") && !is_duckdb) {
    return(FALSE)
  }

  is_duckdb || is_postgres
}

summarize_grouping_sets <- function(.data,
                                    dots,
                                    plan,
                                    .margin_label) {
  con <- dbplyr::remote_con(.data)
  dots <- rewrite_grouping_dots(
    dots,
    plan = plan,
    sql = TRUE,
    con = con
  )
  group_vars <- unique(c(plan$by, plan$dimensions))

  used_names <- unique(c(colnames(.data), names(dots)))
  flag_names <- make_grouping_flag_names(plan$dimensions, used_names)
  flag_quos <- Map(
    function(var, name) {
      rlang::new_quosure(
        grouping_sql_expr(var, con),
        env = rlang::empty_env()
      )
    },
    plan$dimensions,
    flag_names
  )
  names(flag_quos) <- flag_names

  result <- dplyr::summarize(
    .data = dplyr::group_by(
      .data,
      dplyr::pick(dplyr::all_of(group_vars))
    ),
    !!!dots,
    !!!flag_quos,
    .groups = "drop"
  )

  result$lazy_query$marginplyr_grouping_sets <- plan$sets
  class(result$lazy_query) <- c(
    "lazy_grouping_sets_query",
    class(result$lazy_query)
  )

  if (!is.null(.margin_label) && length(plan$dimensions) > 0L) {
    labels <- Map(
      function(var, flag) {
        rlang::expr(
          dplyr::if_else(
            .data[[!!flag]] == 1L,
            !!.margin_label,
            as.character(.data[[!!var]])
          )
        )
      },
      plan$dimensions,
      flag_names
    )
    names(labels) <- plan$dimensions
    result <- dplyr::mutate(result, !!!labels)
  }

  dplyr::select(result, -dplyr::all_of(flag_names))
}

make_grouping_flag_names <- function(dimensions, used_names) {
  vapply(
    seq_along(dimensions),
    function(i) {
      candidate <- paste0("..marginplyr_grouping_", i)
      while (candidate %in% used_names) {
        candidate <- paste0(candidate, "_")
      }
      used_names <<- c(used_names, candidate)
      candidate
    },
    character(1)
  )
}

#' @export
#' @importFrom dbplyr sql_build
sql_build.lazy_grouping_sets_query <- function(op,
                                               con,
                                               ...,
                                               sql_options = NULL) {
  grouping_sets <- op$marginplyr_grouping_sets
  op$marginplyr_grouping_sets <- NULL
  class(op) <- setdiff(class(op), "lazy_grouping_sets_query")

  query <- dbplyr::sql_build(
    op,
    con = con,
    ...,
    sql_options = sql_options
  )

  set_sql <- lapply(
    grouping_sets,
    function(vars) {
      quoted_vars <- dbplyr::escape(dbplyr::ident(vars), con = con)
      dbplyr::build_sql(
        "(",
        dbplyr::sql_vector(
          quoted_vars,
          parens = FALSE,
          collapse = ", ",
          con = con
        ),
        ")",
        con = con
      )
    }
  )

  query$group_by <- dbplyr::build_sql(
    "GROUPING SETS (",
    dbplyr::sql_vector(
      do.call(c, set_sql),
      parens = FALSE,
      collapse = ", ",
      con = con
    ),
    ")",
    con = con
  )
  query
}
