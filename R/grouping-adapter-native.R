# `summaries` is `stage_margin_summaries()`'s, taken whole for the reason its
# header gives, and read here for its labels as well as its dots.
#
# `call` is the Margin verb a Condition context is owed instead of the internal
# `summarize()` this adapter issues. It has no default, unlike
# `summarize_margin_union()`'s, because the executor is this adapter's only
# caller: a default would stand for a caller that does not exist, and an
# optional argument nothing omits is one the reader has to establish is never
# omitted.
summarize_margin_native <- function(.data,
                                    summaries,
                                    plan,
                                    margin_labels,
                                    reserved_names,
                                    call,
                                    set_id_name = NULL,
                                    set_id_is_internal = FALSE,
                                    parent_key_names = character()) {
  con <- dbplyr::remote_con(.data)
  dots <- rewrite_grouping_dots(
    summaries$dots,
    plan = plan,
    sql = TRUE,
    con = con
  )
  # After the rewrite, because the rewritten dots are the expressions dplyr
  # will quote.
  restatements <- branch_argument_map(dots, summaries$labels)
  group_vars <- unique(c(plan$by, plan$dimensions))
  if (!is.null(set_id_name)) {
    set_id_quo <- rlang::new_quosure(
      grouping_set_id_sql_expr(plan, con),
      env = rlang::empty_env()
    )
    set_id_quos <- stats::setNames(list(set_id_quo), set_id_name)
  } else {
    set_id_quos <- list()
  }

  labelled_dimensions <- names(Filter(
    function(label) !is_missing_margin_label(label),
    margin_labels
  ))
  needs_display_flags <- length(labelled_dimensions) > 0L
  if (needs_display_flags) {
    flag_names <- new_margin_internal_names(
      length(labelled_dimensions),
      used_names = reserved_names,
      prefix = "..marginplyr_grouping_",
      backend = grouping_backend(.data)
    )
    flag_quos <- lapply(
      labelled_dimensions,
      function(var) {
        rlang::new_quosure(
          grouping_sql_expr(var, con),
          env = rlang::empty_env()
        )
      }
    )
    names(flag_quos) <- flag_names
  } else {
    flag_names <- character()
    flag_quos <- list()
  }

  # dbplyr translates the caller's expressions here; the grouped summarize
  # below builds only the internal columns. That error is the whole of what
  # ADR 0022 restates in this adapter. The call is forced out of the check's
  # lazy argument so that the check's own Package conditions stay outside the
  # catch.
  #
  # The blamed call is rewritten alongside the argument, both being parts of
  # one Condition context (CONTEXT.md). It is assigned after the restatement
  # rather than inside it, because that function returns the condition
  # untouched when the map is empty, and this half is owed either way.
  summary_select <- tryCatch(
    native_summary_select(.data, dots),
    error = function(cnd) {
      cnd <- restate_condition_arguments(cnd, restatements)
      cnd$call <- call
      stop(cnd)
    }
  )
  check_summary_output_names(
    summary_select$name,
    group_vars = group_vars,
    internal_names = c(flag_names, unname(parent_key_names)),
    set_id_name = set_id_name,
    set_id_is_internal = set_id_is_internal
  )
  check_margin_sql_public_names(
    unique(c(group_vars, summary_select$name,
             if (!set_id_is_internal) set_id_name)),
    grouping_backend(.data)
  )

  result <- dplyr::summarize(
    .data = dplyr::group_by(
      .data,
      dplyr::pick(dplyr::all_of(group_vars))
    ),
    !!!set_id_quos,
    !!!flag_quos,
    .groups = "drop"
  )
  # The summary's lazy-query rows already hold dbplyr's partially evaluated
  # expressions. Feeding them to another `summarize()` would evaluate them a
  # second time; a `pick()` resolved to a list cannot survive that pass.
  internal_select <- result$lazy_query$select
  key_rows <- seq_along(group_vars)
  internal_rows <- setdiff(seq_len(nrow(internal_select)), key_rows)
  result$lazy_query$select <- dplyr::bind_rows(
    internal_select[key_rows, , drop = FALSE],
    summary_select,
    internal_select[internal_rows, , drop = FALSE]
  )

  result <- attach_grouping_sets_query(result, plan$sets)

  if (length(parent_key_names) > 0L) {
    original_keys <- lapply(names(parent_key_names), margin_column_pronoun)
    names(original_keys) <- unname(parent_key_names)
    result <- dplyr::mutate(result, !!!original_keys)
  }

  if (needs_display_flags) {
    labels <- Map(
      function(var, flag, label) {
        rlang::expr(
          dplyr::if_else(
            (!!margin_column_pronoun(flag)) == 1L,
            !!label,
            as.character(!!margin_column_pronoun(var))
          )
        )
      },
      labelled_dimensions,
      flag_names,
      margin_labels[labelled_dimensions]
    )
    names(labels) <- labelled_dimensions
    result <- dplyr::mutate(result, !!!labels)
    result <- dplyr::select(result, -dplyr::all_of(flag_names))
  }

  result
}

# Resolve the caller's summary names once, before adding grouping columns that
# an output could overwrite. The grouped query uses these resolved select rows,
# so a stateful `.names` expression cannot change names between validation and
# construction. The lazy query is built without reading the input (ADR 0020).
native_summary_select <- function(.data, dots) {
  summary <- dplyr::summarize(dplyr::ungroup(.data), !!!dots)
  select <- summary$lazy_query$select
  stopifnot(
    is.character(select$name),
    is.list(select$expr),
    identical(select$name, get_col_names(summary, dplyr::everything()))
  )
  select
}

grouping_set_id_sql_expr <- function(plan, con) {
  stopifnot(inherits(plan, "margin_grouping_plan"))
  if (length(plan$dimensions) == 0L) {
    # Read only from the glue string below, which codetools cannot see.
    set_id <- plan$set_ids[[1L]] # nolint: object_usage_linter.
    return(dbplyr::sql_glue2(con, "{set_id}"))
  }

  clauses <- Map(
    function(mask, set_id) {
      terms <- Map(
        function(var, bit) {
          # Read only from the glue string below, which codetools cannot see.
          grouping_call <- grouping_sql_expr( # nolint: object_usage_linter.
            var,
            con
          )
          dbplyr::sql_glue2(
            con,
            "{.sql grouping_call} = {bit}"
          )
        },
        plan$dimensions,
        as.integer(mask)
      )
      # Read only from the glue string below, which codetools cannot see.
      condition <- Reduce( # nolint: object_usage_linter.
        function(x, y) dbplyr::sql_glue2(con, "{.sql x} AND {.sql y}"),
        terms
      )
      dbplyr::sql_glue2(
        con,
        "WHEN {.sql condition} THEN {set_id}"
      )
    },
    split(plan$grouping_masks, row(plan$grouping_masks)),
    plan$set_ids
  )
  clauses <- dbplyr::sql(paste(
    vapply(clauses, as.character, character(1)),
    collapse = " "
  ))
  dbplyr::sql_glue2(con, "CASE {.sql clauses} END")
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

# Not a Package condition: no rewrite of the call avoids an upstream
# representation change. See ADR 0015.
abort_dbplyr_representation <- function() {
  stop(
    paste0(
      "The dbplyr query representation has changed and is not compatible ",
      "with this version of marginplyr (dbplyr ",
      as.character(utils::packageVersion("dbplyr")),
      "). Please report this at ",
      "https://github.com/sayuks/marginplyr/issues."
    ),
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
  # Read only from the glue string below, which codetools cannot see.
  # nolint start: object_usage_linter.
  grouping_sets_sql <- paste(set_sql, collapse = ", ")
  # nolint end

  query$group_by <- dbplyr::sql_glue2(
    con,
    "GROUPING SETS ({.sql grouping_sets_sql})"
  )
  query
}
