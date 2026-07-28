#' Identify rows produced by a grouping operation
#'
#' [grouping_bit()] and [grouping_id()] are contextual summary helpers for
#' [summarize_with_margins()]. They distinguish values that are absent from a
#' grouping set from ordinary missing values in the source data.
#'
#' [grouping_bit()] corresponds to SQL `GROUPING(x)`: it returns `1L` when
#' `x` is absent from the grouping set and `0L` otherwise.
#' [grouping_id()] combines those flags as a bit mask; its last argument is
#' the least-significant bit. It accepts between 1 and 31 distinct grouping
#' columns.
#'
#' The `_bit` suffix emphasizes the `0`/`1` result and deliberately avoids
#' masking [base::grouping()], an unrelated function that returns a permutation
#' for placing equal values next to one another.
#'
#' Columns fixed by `.by` may also be passed to these helpers. Because a `.by`
#' column belongs to every grouping set, [grouping_bit()] is always `0L` for it
#' and it contributes a zero bit to [grouping_id()].
#'
#' @param x A bare grouping column.
#' @param ... Bare grouping columns.
#'
#' @return A numeric grouping flag or identifier when used inside
#'   [summarize_with_margins()]. Local results use integer vectors; database
#'   result types follow the backend.
#' @export
#' @examples
#' # Online-direct sales have a source NA store. grouping_bit(store) is 0 for
#' # that detail row but 1 when ROLLUP removes store to create a subtotal.
#' summarize_with_margins(
#'   dplyr::filter(retail_sales, year == 2026L, month == "Jan"),
#'   revenue = sum(revenue),
#'   year_is_fixed = grouping_bit(year),
#'   store_is_total = grouping_bit(store),
#'   level = grouping_id(region, store),
#'   .by = year,
#'   .grouping = rollup(region, store)
#' )
#'
#' # Keeping typed missing values makes the grouping bits essential: source
#' # missing values and generated totals are both displayed as NA.
#' summarize_with_margins(
#'   dplyr::filter(retail_sales, year == 2026L, month == "Jan"),
#'   revenue = sum(revenue),
#'   store_is_total = grouping_bit(store),
#'   level = grouping_id(region, store),
#'   .grouping = rollup(region, store),
#'   .margin_label = NULL
#' )
grouping_bit <- function(x) {
  stop(
    "`grouping_bit()` can only be used inside `summarize_with_margins()`.",
    call. = FALSE
  )
}

#' @rdname grouping_bit
#' @export
grouping_id <- function(...) {
  stop(
    "`grouping_id()` can only be used inside `summarize_with_margins()`.",
    call. = FALSE
  )
}

rewrite_grouping_dots <- function(dots,
                                  plan,
                                  grouping_set = NULL,
                                  sql = FALSE,
                                  con = NULL) {
  stopifnot(is.list(dots), inherits(plan, "margin_grouping_plan"))

  rewritten <- lapply(
    dots,
    function(quo) {
      rlang::new_quosure(
        rewrite_grouping_expr(
          rlang::quo_get_expr(quo),
          plan = plan,
          grouping_set = grouping_set,
          sql = sql,
          con = con
        ),
        env = rlang::quo_get_env(quo)
      )
    }
  )
  stats::setNames(rewritten, names(dots))
}

rewrite_grouping_expr <- function(expr,
                                  plan,
                                  grouping_set,
                                  sql,
                                  con) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }

  helper <- grouping_helper_name(expr)
  if (!is.null(helper)) {
    args <- as.list(expr)[-1]
    vars <- grouping_helper_vars(args, helper, plan)

    if (identical(helper, "grouping_bit")) {
      if (sql) {
        return(grouping_sql_expr(vars[[1]], con))
      }
      return(as.integer(!vars[[1]] %in% grouping_set))
    }

    if (sql) {
      return(grouping_id_sql_expr(vars, con))
    }

    bits <- as.integer(!vars %in% grouping_set)
    weights <- 2L ^ rev(seq_along(bits) - 1L)
    return(as.integer(sum(bits * weights)))
  }

  pieces <- as.list(expr)
  pieces[-1] <- lapply(
    pieces[-1],
    rewrite_grouping_expr,
    plan = plan,
    grouping_set = grouping_set,
    sql = sql,
    con = con
  )
  as.call(pieces)
}

grouping_helper_name <- function(expr) {
  fn <- expr[[1]]

  if (is.symbol(fn)) {
    name <- as.character(fn)
    if (name %in% c("grouping_bit", "grouping_id")) {
      return(name)
    }
    return(NULL)
  }

  if (
    rlang::is_call(fn, c("::", ":::")) &&
      length(fn) == 3L &&
      identical(as.character(fn[[2]]), "marginplyr")
  ) {
    name <- as.character(fn[[3]])
    if (name %in% c("grouping_bit", "grouping_id")) {
      return(name)
    }
  }

  NULL
}

grouping_helper_vars <- function(args, helper, plan) {
  if (identical(helper, "grouping_bit") && length(args) != 1L) {
    stop("`grouping_bit()` requires exactly one column.", call. = FALSE)
  }
  if (identical(helper, "grouping_id") && length(args) == 0L) {
    stop("`grouping_id()` requires at least one column.", call. = FALSE)
  }

  is_symbol <- vapply(args, is.symbol, logical(1))
  if (!all(is_symbol)) {
    stop(
      sprintf("`%s()` only accepts bare grouping columns.", helper),
      call. = FALSE
    )
  }

  vars <- vapply(args, as.character, character(1))
  if (anyDuplicated(vars)) {
    stop(
      sprintf("`%s()` does not accept duplicate columns.", helper),
      call. = FALSE
    )
  }
  if (identical(helper, "grouping_id") && length(vars) > 31L) {
    stop("`grouping_id()` supports at most 31 columns.", call. = FALSE)
  }
  allowed <- unique(c(plan$by, plan$dimensions))
  unknown <- setdiff(vars, allowed)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Column%s %s %s not part of `.by` or `.grouping`.",
        if (length(unknown) == 1L) "" else "s",
        paste0("`", unknown, "`", collapse = ", "),
        if (length(unknown) == 1L) "is" else "are"
      ),
      call. = FALSE
    )
  }

  vars
}

grouping_sql_expr <- function(var, con) {
  if (is.null(con)) {
    stop("A database connection is required for SQL grouping expressions.")
  }
  dbplyr::sql_glue2(con, "GROUPING({.id var})")
}

grouping_id_sql_expr <- function(vars, con) {
  terms <- lapply(
    seq_along(vars),
    function(i) {
      grouping_call <- grouping_sql_expr(vars[[i]], con)
      weight <- 2L ^ (length(vars) - i)
      if (weight == 1L) {
        grouping_call
      } else {
        dbplyr::sql_glue2(
          con,
          "{.sql grouping_call} * {weight}"
        )
      }
    }
  )

  Reduce(
    function(x, y) {
      dbplyr::sql_glue2(con, "{.sql x} + {.sql y}")
    },
    terms
  )
}
