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

  result <- attach_grouping_sets_query(result, plan$sets)

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

attach_grouping_sets_query <- function(result, grouping_sets) {
  if (!inherits(result, "tbl_lazy") || !is.list(result)) {
    abort_dbplyr_representation()
  }
  lazy_query <- result$lazy_query
  if (!inherits(lazy_query, "lazy_query")) {
    abort_dbplyr_representation()
  }

  grouping_query <- dbplyr::lazy_query(
    "grouping_sets",
    x = lazy_query,
    grouping_sets = grouping_sets,
    group_vars = character()
  )
  validate_grouping_sets_query(grouping_query)
  result$lazy_query <- grouping_query
  result
}

validate_grouping_sets_query <- function(op) {
  fields <- if (is.list(op)) names(op) else NULL
  if (
    is.null(fields) ||
      !all(c("x", "grouping_sets") %in% fields) ||
      !inherits(op$x, "lazy_query") ||
      !is.list(op$grouping_sets) ||
      length(op$grouping_sets) == 0L ||
      !all(vapply(op$grouping_sets, is.character, logical(1)))
  ) {
    abort_dbplyr_representation()
  }
  invisible(op)
}

abort_dbplyr_representation <- function() {
  stop(
    "The dbplyr query representation has changed and is not compatible with ",
    "this version of marginplyr (dbplyr ",
    as.character(utils::packageVersion("dbplyr")),
    "). Please report this at ",
    "https://github.com/sayuks/marginplyr/issues.",
    call. = FALSE
  )
}

#' @export
#' @importFrom dbplyr op_vars
op_vars.lazy_grouping_sets_query <- function(op) {
  validate_grouping_sets_query(op)
  dbplyr::op_vars(op$x)
}

#' @export
#' @importFrom dbplyr sql_build
sql_build.lazy_grouping_sets_query <- function(op,
                                               con,
                                               ...,
                                               sql_options = NULL) {
  validate_grouping_sets_query(op)
  grouping_sets <- op$grouping_sets

  query <- dbplyr::sql_build(
    op$x,
    con = con,
    ...,
    sql_options = sql_options
  )
  if (
    !is.list(query) ||
      is.null(names(query)) ||
      !"group_by" %in% names(query)
  ) {
    abort_dbplyr_representation()
  }

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
