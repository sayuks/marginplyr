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
#' @section Grouping identity values:
#' marginplyr exposes four related values. Two describe *where* a Grouping-set
#' occurrence sits in one ordered Grouping plan; two describe *which*
#' dimensions are absent from a row. Only the first pair can tell repeated
#' identical Grouping sets apart:
#'
#' | Value | Meaning | Duplicate Grouping-set occurrences |
#' |---|---|---|
#' | `.id` | One-based position in the resolved Grouping plan | Distinct with `.duplicates = "keep"` | # nolint: line_length_linter
#' | [inspect_grouping()] `$set_id` | The same position before execution | Distinct with `.duplicates = "keep"` | # nolint: line_length_linter
#' | [grouping_bit()] | Whether one chosen dimension is absent | The same for identical absence patterns | # nolint: line_length_linter
#' | [grouping_id()] | Bit mask for chosen absent dimensions | The same for identical absence patterns | # nolint: line_length_linter
#'
#' The correspondence between `.id` and `set_id` holds for one resolved `.by`,
#' `.grouping`, and `.duplicates`. Reordering or deduplicating a Grouping
#' specification changes both, so `.id` is not a durable business key.
#'
#' Grouping identifiers encode absence and need not be consecutive. A
#' `rollup(region, store)` plan therefore has `.id` values `1L`, `2L`, and
#' `3L` but [grouping_id()] values `0L`, `1L`, and `3L`: identifier `2` would
#' mean `region` absent while `store` remains, which the declared hierarchy
#' never produces. A `cube(region, store)` plan contains every mask, so its
#' identifiers are `0L`, `1L`, `2L`, and `3L`.
#'
#' Neither value records physical row order. Call [dplyr::arrange()] whenever
#' presentation order matters.
#'
#' Displayed Margin labels are not identity. A source value can equal its
#' Margin label, and a source missing value can print exactly like a
#' typed-missing Margin value, so retain one of these helpers or `.id` when
#' structural identity matters. See
#' *[Display labels and grouping identity][summarize_with_margins]* for the
#' complete label, factor, and missing-value contract.
#'
#' The [grouping identity guide][guide] works through these values with
#' executable examples.
#'
#' [guide]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html
#'
#' @param x A bare grouping column.
#' @param ... Bare grouping columns.
#'
#' @return A grouping flag or identifier when used inside
#'   [summarize_with_margins()]. Local data frames and `dtplyr` steps return R
#'   integers, because the value is known for each Grouping-set branch and is
#'   substituted before execution. Arrow and every backend taking the portable
#'   `UNION ALL` path receive that same constant as a literal in their own
#'   query. Only a backend actually running native `GROUP BY GROUPING SETS`
#'   emits SQL `GROUPING()`, so PostgreSQL falls back to the literal whenever
#'   `.duplicates = "keep"` sends it down the portable path. In every remote
#'   case the collected type comes from the backend, so cast explicitly when a
#'   downstream calculation depends on it.
#' @family grouping plans and grouping identity
#' @family contextual summary helpers
#' @export
#' @examples
#' # Online-direct sales have a source NA store. grouping_bit(store) is 0 for
#' # that detail row but 1 when ROLLUP removes store to create a subtotal.
#' summarize_with_margins(
#'   .data = dplyr::filter(retail_sales, year == 2026L, month == "Jan"),
#'   revenue = sum(revenue),
#'   year_is_fixed = grouping_bit(year),
#'   store_is_total = grouping_bit(store),
#'   level = grouping_id(region, store),
#'   .by = year,
#'   .grouping = rollup(region, store)
#' )
#'
#' # Keeping typed missing values makes the grouping bits essential: source
#' # missing values, factor NA levels, and generated margins may all print as
#' # NA even though their structural identities differ.
#' summarize_with_margins(
#'   .data = dplyr::filter(retail_sales, year == 2026L, month == "Jan"),
#'   revenue = sum(revenue),
#'   store_is_total = grouping_bit(store),
#'   level = grouping_id(region, store),
#'   .grouping = rollup(region, store),
#'   .margin_label = NULL
#' )
grouping_bit <- function(x) {
  abort_marginplyr(
    "`grouping_bit()` can only be used inside `summarize_with_margins()`."
  )
}

#' @rdname grouping_bit
#' @export
grouping_id <- function(...) {
  abort_marginplyr(
    "`grouping_id()` can only be used inside `summarize_with_margins()`."
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
    args <- static_call_args(expr)
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

  # A helper the caller quoted is language data rather than a request to
  # compile, so the rewrite descends past it: `deparse1(quote(
  # grouping_bit(region)))` answered `"0L"` in one branch and `"1L"` in
  # another, which is the branch-local constant this helper exists to produce
  # and not the object the caller wrote (#179).
  rewrite_evaluated_call_parts(
    expr,
    function(part) {
      rewrite_grouping_expr(
        part,
        plan = plan,
        grouping_set = grouping_set,
        sql = sql,
        con = con
      )
    }
  )
}

grouping_helper_name <- function(expr) {
  name <- static_call_name(expr)
  namespace <- static_call_ns(expr)
  if (
    !is.null(name) &&
      name %in% c("grouping_bit", "grouping_id") &&
      (is.null(namespace) || identical(namespace, "marginplyr"))
  ) {
    return(name)
  }

  NULL
}

grouping_helper_vars <- function(args, helper, plan) {
  # Each argument is read as a value rather than bound to a name, for the reason
  # `static_call_args()` gives: one of them may be R's empty argument, which is
  # a symbol whose name is `""`. `is.symbol()` therefore read one as a bare
  # column and `as.character()` wrote `""` into `vars`, so the caller was told
  # about a column nobody wrote -- `grouping_id(region, )` named `` `` `` as
  # missing from the plan, and `grouping_id(, )` named it a duplicate, because
  # both empty arguments read as the same name (#181). `is_name_part()` is the
  # actually being asked: a symbol, and not the empty argument.
  not_a_column_message <- sprintf(
    "`%s()` only accepts bare grouping columns.", helper
  )

  # The empty argument is refused ahead of the arity checks so that the answer
  # does not depend on which check runs first. `grouping_bit(, )` is two
  # arguments to the parser, so arity is what catches it today, and a message
  # counting columns describes neither of the two things the caller wrote.
  # Nothing else moves: a non-column that is not empty -- `grouping_bit(1, 2)`
  # -- still reaches the arity diagnostic first, which is the one a caller
  # passing two of anything needs.
  if (any(vapply(args, rlang::is_missing, logical(1)))) {
    abort_marginplyr(not_a_column_message)
  }

  if (identical(helper, "grouping_bit") && length(args) != 1L) {
    abort_marginplyr(
      "`grouping_bit()` requires exactly one column."
    )
  }
  if (identical(helper, "grouping_id") && length(args) == 0L) {
    abort_marginplyr(
      "`grouping_id()` requires at least one column."
    )
  }

  # The compound question, asked here even though the check above has already
  # answered half of it: this is the test protecting `as.character()` below, and
  # what it must be true of is the whole of what a column is. Narrowing it to
  # `is.symbol()` on the grounds that nothing empty survives the pre-check would
  # make the reordering above load-bearing for correctness rather than for which
  # diagnostic a caller reads.
  if (!all(vapply(args, is_name_part, logical(1)))) {
    abort_marginplyr(not_a_column_message)
  }

  vars <- vapply(args, as.character, character(1))
  if (anyDuplicated(vars)) {
    abort_marginplyr(
      sprintf("`%s()` does not accept duplicate columns.", helper)
    )
  }
  if (identical(helper, "grouping_id") && length(vars) > 31L) {
    abort_marginplyr(
      "`grouping_id()` supports at most 31 columns."
    )
  }
  allowed <- unique(c(plan$by, plan$dimensions))
  unknown <- setdiff(vars, allowed)
  if (length(unknown) > 0L) {
    abort_marginplyr(
      sprintf(
        "Column%s %s %s not part of `.by` or `.grouping`.",
        if (length(unknown) == 1L) "" else "s",
        paste0("`", unknown, "`", collapse = ", "),
        if (length(unknown) == 1L) "is" else "are"
      )
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
