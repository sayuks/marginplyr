summarize_grouping_sets <- function(.data,
                                    dots,
                                    plan,
                                    .margin_label,
                                    reserved_names) {
  con <- dbplyr::remote_con(.data)
  dots <- rewrite_grouping_dots(
    dots,
    plan = plan,
    sql = TRUE,
    con = con
  )
  group_vars <- unique(c(plan$by, plan$dimensions))

  flag_names <- new_margin_internal_names(
    length(plan$dimensions),
    used_names = reserved_names,
    prefix = "..marginplyr_grouping_"
  )
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
      dbplyr::sql_glue2(con, "{.id vars*}")
    }
  )
  set_sql <- vapply(set_sql, as.character, character(1))
  grouping_sets_sql <- paste(set_sql, collapse = ", ")

  query$group_by <- dbplyr::sql_glue2(
    con,
    "GROUPING SETS ({.sql grouping_sets_sql})"
  )
  query
}
