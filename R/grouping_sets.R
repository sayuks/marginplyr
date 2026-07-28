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

  needs_display_flags <-
    !is.null(.margin_label) && length(plan$dimensions) > 0L
  if (needs_display_flags) {
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
  } else {
    flag_names <- character()
    flag_quos <- list()
  }

  result <- dplyr::summarize(
    .data = dplyr::group_by(
      .data,
      dplyr::pick(dplyr::all_of(group_vars))
    ),
    !!!dots,
    !!!flag_quos,
    .groups = "drop"
  )

  result$lazy_query <- dbplyr::lazy_query(
    "grouping_sets",
    x = result$lazy_query,
    grouping_sets = plan$sets,
    group_vars = character()
  )

  if (needs_display_flags) {
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
    result <- dplyr::select(result, -dplyr::all_of(flag_names))
  }

  result
}

#' @export
#' @importFrom dbplyr op_vars
op_vars.lazy_grouping_sets_query <- function(op) {
  dbplyr::op_vars(op$x)
}

#' @export
#' @importFrom dbplyr sql_build
sql_build.lazy_grouping_sets_query <- function(op,
                                               con,
                                               ...,
                                               sql_options = NULL) {
  grouping_sets <- op$grouping_sets

  query <- dbplyr::sql_build(
    op$x,
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
  grouping_sets_sql <- paste(set_sql, collapse = ", ") # nolint: object_usage_linter

  query$group_by <- dbplyr::sql_glue2(
    con,
    "GROUPING SETS ({.sql grouping_sets_sql})"
  )
  query
}
