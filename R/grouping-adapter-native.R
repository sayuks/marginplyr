# `caller_labels` is `new_summary_arguments()`'s, parallel to the `dots` this
# is handed, and is what the translation error below is restated with. It is
# required rather than defaulted, because a default computed here would be
# computed from dots this has already rewritten, and would restate a spelling
# the caller never wrote (ADR 0024).
summarize_margin_native <- function(.data,
                                    dots,
                                    plan,
                                    margin_labels,
                                    reserved_names,
                                    caller_labels,
                                    set_id_name = NULL,
                                    set_id_is_internal = FALSE) {
  con <- dbplyr::remote_con(.data)
  dots <- rewrite_grouping_dots(
    dots,
    plan = plan,
    sql = TRUE,
    con = con
  )
  # After the rewrite, because the rewritten dots are the expressions dplyr
  # will quote.
  restatements <- branch_argument_map(dots, caller_labels)
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
      prefix = "..marginplyr_grouping_"
    )
    flag_quos <- Map(
      function(var, name) {
        rlang::new_quosure(
          grouping_sql_expr(var, con),
          env = rlang::empty_env()
        )
      },
      labelled_dimensions,
      flag_names
    )
    names(flag_quos) <- flag_names
  } else {
    flag_names <- character()
    flag_quos <- list()
  }

  # Forced here rather than left to lazy evaluation inside the check, so that
  # the translation error a summarize of the caller's expressions can raise is
  # restated and the check's own Package conditions are not reached by the
  # restatement at all.
  output_names <- with_native_restatement(
    native_summary_output_names(.data, dots),
    restatements
  )
  check_summary_output_names(
    output_names,
    group_vars = group_vars,
    internal_names = flag_names,
    set_id_name = set_id_name,
    set_id_is_internal = set_id_is_internal
  )

  result <- with_native_restatement(
    dplyr::summarize(
      .data = dplyr::group_by(
        .data,
        dplyr::pick(dplyr::all_of(group_vars))
      ),
      !!!dots,
      !!!set_id_quos,
      !!!flag_quos,
      .groups = "drop"
    ),
    restatements
  )

  result <- attach_grouping_sets_query(result, plan$sets)

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

# The error dbplyr raises when it cannot translate a rewritten expression is
# the one condition this adapter produces while the verb runs; everything else
# it does builds a query. Both of the calls that hand dplyr the caller's
# expressions are wrapped, because either can be the first to translate them,
# and nothing between them is.
#
# Not `with_branch_conditions()`: this adapter issues its summarize once and
# repeats nothing, so the deduplication and the grouping-value restatement have
# nothing to act on (ADR 0022, *Amendment*).
with_native_restatement <- function(expr, restatements) {
  tryCatch(
    expr,
    error = function(cnd) {
      stop(restate_condition_arguments(cnd, restatements))
    }
  )
}

# What `check_summary_output_names()` needs and the grouped summarize above
# cannot supply. A summary output that shadows a grouping dimension takes that
# dimension's place in the result, so the name is present exactly once whether
# or not the collision happened, and the two cases are indistinguishable there.
# Building the same expressions over the ungrouped table names the outputs on
# their own. Every `across()`, `pick()`, and `if_any()` selection was resolved
# to an `all_of()` literal before either adapter ran, so dropping the grouping
# cannot change which columns they cover.
#
# A lazy summarize computes its result names without reading from the backend,
# so this stays a locally detectable error rejected before any backend read
# (ADR 0005).
native_summary_output_names <- function(.data, dots) {
  get_col_names(
    dplyr::summarize(dplyr::ungroup(.data), !!!dots),
    dplyr::everything()
  )
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
