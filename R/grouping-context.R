#' Identify rows produced by a grouping operation
#'
#' `grouping()` and `grouping_id()` are contextual summary helpers for
#' [summarize_with_margins()]. They distinguish values that are absent from a
#' grouping set from ordinary missing values in the source data.
#'
#' `grouping(x)` returns `1L` when `x` is absent from the grouping set and `0L`
#' otherwise. `grouping_id(...)` combines those flags as a bit mask; its last
#' argument is the least-significant bit. It accepts between 1 and 31 distinct
#' grouping columns.
#'
#' @param x A bare grouping column.
#' @param ... Bare grouping columns.
#'
#' @return A numeric grouping flag or identifier when used inside
#'   [summarize_with_margins()]. Local results use integer vectors; database
#'   result types follow the backend.
#' @export
#' @examples
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   is_cyl_total = grouping(cyl),
#'   level = grouping_id(cyl, vs),
#'   .grouping = rollup(cyl, vs)
#' )
grouping <- function(x) {
  stop(
    "`grouping()` can only be used inside `summarize_with_margins()`.",
    call. = FALSE
  )
}

#' @rdname grouping
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

    if (identical(helper, "grouping")) {
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
    if (name %in% c("grouping", "grouping_id")) {
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
    if (name %in% c("grouping", "grouping_id")) {
      return(name)
    }
  }

  NULL
}

grouping_helper_vars <- function(args, helper, plan) {
  if (identical(helper, "grouping") && length(args) != 1L) {
    stop("`grouping()` requires exactly one column.", call. = FALSE)
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
  dbplyr::sql_call2(
    "GROUPING",
    dbplyr::ident(var),
    con = con
  )
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
        dbplyr::build_sql(grouping_call, " * ", weight, con = con)
      }
    }
  )

  Reduce(
    function(x, y) dbplyr::build_sql(x, " + ", y, con = con),
    terms
  )
}
