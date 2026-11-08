#' Build a Grouping specification
#'
#' These constructors describe SQL-style grouping operations for the
#' `.grouping` argument of [summarize_with_margins()] and related verbs.
#' Start with [rollup()] when the report follows a hierarchy such as
#' store to region to company. Use [grouping_sets()] for an exact selection of
#' levels, and [cube()] only when every combination is useful.
#'
#' - [grouping_set()] creates one grouping set. With no columns it represents
#'   the empty set `()`.
#' - [grouping_sets()] forms the union of its arguments.
#' - [rollup()] creates hierarchical prefixes.
#' - [cube()] creates every subset of its dimensions.
#' - [grouping_spec()] forms the Cartesian product of its arguments — every
#'   grouping set of one combined with every grouping set of the next — like
#'   comma-separated SQL `GROUP BY` items.
#'
#' A [grouping_set()] nested directly inside [rollup()] or [cube()] is a
#' composite dimension. Its columns are added or removed together.
#'
#' A resolved Grouping plan can be read back in four ways: `.id`,
#' [inspect_grouping()]`$set_id`, [grouping_bit()], and [grouping_id()].
#' [grouping_bit()] compares them; the [grouping identity guide][guide] works
#' through the same values with executable examples.
#'
#' [guide]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html
#'
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Column selections
#'   and, where supported, nested Grouping specifications. In
#'   [grouping_set()], the selections form one grouping set; nested Grouping
#'   specifications are not allowed, and no arguments form the empty grouping
#'   set. In [rollup()] and [cube()], each selected column is one dimension;
#'   wrap multiple columns in a non-empty [grouping_set()] to make a composite
#'   dimension. These constructors accept no other nested Grouping
#'   specifications and require at least one resolved dimension.
#'   [grouping_sets()] forms the union of its arguments, accepts any valid
#'   nested Grouping specification, and requires at least one argument.
#'   [grouping_spec()] combines its arguments by Cartesian product, accepts any
#'   valid nested Grouping specification, and with no arguments represents the
#'   identity product (the empty grouping set). Which of the two readings an
#'   argument gets, and what is refused rather than read either way, is *How an
#'   argument is read*.
#'
#' @return A Grouping specification for use in `.grouping`.
#'
#' @section How an argument is read:
#' A nested Grouping specification is recognized by how it is written: a
#' call to one of these constructors, or a name bound to a specification.
#' Redundant parentheses are transparent to that reading, because `(` is the
#' identity function: `(s)` is the specification `s` is, and
#' `(rollup(region))` is the call it wraps, however many pairs deep.
#' Any other argument is a column selection, so a call of your own that
#' returns a specification is refused where it is nested even though it is
#' accepted as `.grouping` itself. Assign what it returns to a name and use
#' that name: `s <- my_spec(region)`, then `grouping_sets(s, grade)`. A
#' specification written inside a selection, as in `c(s, grade)`, is a
#' selection containing something it cannot select, and is refused as one —
#' unless the input has a column named `s`, when the selection takes that
#' column, as any selection does with a name the data holds.
#'
#' An argument left empty gets neither reading, and is refused naming the
#' constructor and the position: `grouping_sets(, region)` reports that its
#' first argument is empty. A trailing comma is not an empty argument,
#' because R captures no argument for it, so `grouping_sets(region, )` is
#' `grouping_sets(region)`, and passing no arguments at all means what `...`
#' above says it means for each constructor.
#'
#' A name both readings claim is refused rather than guessed. Where the
#' input has a column named `s` and `s` is also bound to a nested Grouping
#' specification the position accepts, the call names both readings and the
#' spelling that settles each: `all_of("s")` selects the column whatever is
#' bound, and `!!s` uses the specification whatever columns the input has.
#' What a position accepts is what decides whether there are two readings,
#' so a colliding name is refused in [grouping_sets()] and [grouping_spec()]
#' whatever specification it is bound to, in [rollup()] and [cube()] only
#' when it is bound to a [grouping_set()], and never in [grouping_set()],
#' which takes no nested Grouping specification at all. A name bound to
#' anything that is not a specification is a column, as it always was.
#'
#' A nested reference back to an ancestor specification is refused as cyclic.
#' When extending a specification, use a different name or inject the previous
#' value: after `s <- grouping_set(region)`, write
#' `s <- grouping_sets(!!s, grouping_set())`. Writing `grouping_sets(s, ...)`
#' under the same name makes `s` refer to the new specification itself.
#' Reusing a specification in independent branches is allowed.
#'
#' A Grouping dimension keeps its input column name. Inside a selection,
#' `c(region = region)` and `all_of(c(region = "region"))` both select `region`
#' under its own name. A selection that changes the name, such as
#' `c(area = region)`, is refused. Rename the result with [dplyr::rename()].
#'
#' Constructors take positional arguments, so `rollup(region = region)` and
#' `rollup(area = region)` both raise errors. A call such as
#' `rollup(region, .by = year)` reports `.by` as a named constructor argument.
#' Pass `.by = year` to the Margin verb.
#'
#' @family grouping plans and grouping identity
#' @seealso [summarize_with_margins()], [expand_with_margins()],
#'   [nest_with_margins()], and [nest_by_with_margins()], the Margin verbs
#'   that consume a Grouping specification.
#' @export
#' @examples
#' # The operations team needs store, region, and company totals for each
#' # reporting month. Columns in `.by` remain in every grouping set.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' # Finance instead needs a few specific views, including the all-period
#' # grand total represented by an empty grouping set.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = grouping_sets(
#'     grouping_set(year, month),
#'     grouping_set(region, product),
#'     grouping_set()
#'   )
#' )
#'
#' # A cube gives merchandising every combination of product and channel.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = cube(product, channel)
#' )
#'
#' # grouping_sets() unions two independent hierarchies. Both rollups contain
#' # the empty set, so duplicate grouping sets are dropped explicitly.
#' independent_totals <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = grouping_sets(
#'     rollup(year, month),
#'     rollup(region, store)
#'   ),
#'   .duplicates = "drop"
#' )
#'
#' # Keeping duplicates intentionally returns the grand total once per
#' # occurrence of the empty set.
#' repeated_grand_totals <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = grouping_sets(
#'     rollup(year, month),
#'     rollup(region, store)
#'   ),
#'   .duplicates = "keep"
#' )
#' dplyr::filter(
#'   repeated_grand_totals,
#'   year == "Total",
#'   region == "Total"
#' )
#'
#' # grouping_spec() takes their Cartesian product instead. Each of the first
#' # rollup's three grouping sets is combined with each of the second's, so
#' # inspect_grouping() reports 3 x 3 = 9 of them, from (year, month, region,
#' # store) down to the grand total.
#' combined_specification <- grouping_spec(
#'   rollup(year, month),
#'   rollup(region, store)
#' )
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = combined_specification
#' )
#' combined_totals <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = combined_specification
#' )
#'
#' # A nested grouping_set() is a composite dimension: region and store are
#' # included or removed together. Tidy-select expressions are also accepted.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = cube(
#'     grouping_set(region, store),
#'     dplyr::all_of(c("product", "channel"))
#'   )
#' )
#'
#' # A simulated PostgreSQL table shows the native GROUPING SETS translation
#' # without requiring a database server.
#' postgres_sales <- dbplyr::tbl_lazy(
#'   retail_sales,
#'   con = dbplyr::simulate_postgres()
#' )
#' postgres_sales |>
#'   summarize_with_margins(
#'     revenue = sum(revenue, na.rm = TRUE),
#'     .grouping = grouping_spec(
#'       rollup(year, month),
#'       rollup(region, store)
#'     )
#'   ) |>
#'   dplyr::show_query()
grouping_set <- function(...) {
  args <- capture_grouping_args(...)
  new_grouping_spec("set", args)
}

#' @rdname grouping_set
#' @export
grouping_sets <- function(...) {
  args <- capture_grouping_args(...)
  new_grouping_spec("sets", args)
}

#' @rdname grouping_set
#' @export
rollup <- function(...) {
  args <- capture_grouping_args(...)
  new_grouping_spec("rollup", args)
}

#' @rdname grouping_set
#' @export
cube <- function(...) {
  args <- capture_grouping_args(...)
  new_grouping_spec("cube", args)
}

#' @rdname grouping_set
#' @export
grouping_spec <- function(...) {
  args <- capture_grouping_args(...)
  new_grouping_spec("product", args)
}

# Capture a public .grouping argument before an empty splice can make an
# authored constructor position look like an omitted trailing comma.
capture_public_grouping <- function(quo, written) {
  if (is_captured_grouping_quosure(written, quo)) {
    return(quo)
  }
  capture <- function(x) rlang::enquo(x)
  env <- rlang::quo_get_env(quo)
  expr <- mark_grouping_empty_args(rlang::quo_get_expr(quo))
  rlang::eval_bare(rlang::call2(capture, expr), env)
}

# Mark calls whose last argument can become an authored empty after splicing.
# Quoted operands and supplied function literals remain caller-owned syntax.
mark_grouping_empty_args <- function(expr) {
  if (rlang::is_missing(expr) || rlang::is_quosure(expr) ||
        !(is.call(expr) || is.pairlist(expr))) {
    return(expr)
  }
  if (is.call(expr)) {
    capture <- expr
    # Grouping capture preserves language even through a parenthesized callee.
    capture[[1L]] <- unparenthesized_value(capture[[1L]])
    if (identical(static_call_name(capture), "function")) {
      return(expr)
    }
    captured <- captured_call_parts(capture)
    if (any(captured)) {
      parts <- as.list(expr)
      for (i in seq_along(captured)) {
        if (!captured[[i]]) {
          parts[i + 1L] <- list(mark_grouping_empty_args(parts[[i + 1L]]))
        } else if (is_bquote_capture(capture, captured)) {
          parts[i + 1L] <- list(map_bquote_substitutions(
            parts[[i + 1L]], mark_grouping_empty_args
          ))
        }
      }
      out <- as.call(parts)
      attributes(out) <- attributes(expr)
      return(out)
    }
    if (identical(static_call_name(capture[[1L]]), "function")) {
      expr[[1L]] <- mark_executed_grouping_body(expr[[1L]])
    }
  }
  parts <- as.list(expr)
  positions <- seq_along(parts)[-c(1L, length(parts))]
  keep_empty <- !is.null(static_spelling_name(expr, "grouping_constructor")) &&
    any(vapply(parts[positions], rlang::is_missing, logical(1))) &&
    !rlang::is_missing(parts[[length(parts)]])
  parts <- lapply(parts, mark_grouping_empty_args)
  out <- if (is.call(expr)) as.call(parts) else as.pairlist(parts)
  attributes(out) <- attributes(expr)
  if (keep_empty) {
    attr(out, "marginplyr_keep_empty") <- TRUE
  }
  out
}

# Mark only the body of an immediately called function literal. The caller
# establishes that the callee is a literal, possibly wrapped in parentheses.
mark_executed_grouping_body <- function(expr) {
  if (is_parenthesized(expr)) {
    expr[[2L]] <- mark_executed_grouping_body(expr[[2L]])
  } else {
    expr[[3L]] <- mark_grouping_empty_args(expr[[3L]])
  }
  expr
}

# Capture tagged arguments without interpreting their values, so preflight can
# report their original names. Untagged arguments retain tidy-eval capture,
# including injections and the omission of a trailing empty argument. Already
# captured quosures retain their own injection boundary. The caller must be
# the constructor itself, whose invocation can carry the outer capture marker.
capture_grouping_args <- function(...) {
  keep_empty <- isTRUE(attr(sys.call(-1L), "marginplyr_keep_empty"))
  written <- as.list(substitute(list(...)))[-1L]
  args <- rlang::enquos0(...)
  if (length(args) == 0L || any(nzchar(rlang::names2(args)))) {
    return(args)
  }
  expressions <- lapply(args, rlang::quo_get_expr)
  captured <- vapply(seq_along(args), function(i) {
    is_captured_grouping_quosure(written[[i]], args[[i]])
  }, logical(1))
  protected <- lapply(expressions, preserve_grouping_quosures)
  if (!any(captured) && identical(expressions, protected)) {
    return(rlang::enquos(
      ..., .ignore_empty = if (keep_empty) "none" else "trailing"
    ))
  }
  capture <- function(...) rlang::enquos(..., .ignore_empty = "none")
  positions <- seq_along(args)
  # Omit the last raw empty argument before splicing can remove a successor.
  if (!keep_empty &&
        rlang::is_missing(expressions[[length(args)]])) {
    positions <- positions[-length(positions)]
  }
  args <- lapply(positions, function(i) {
    arg <- args[[i]]
    if (captured[[i]]) {
      return(list(arg))
    }
    rlang::eval_bare(rlang::call2(
      capture, !!!list(protected[[i]])
    ), rlang::quo_get_env(arg))
  })
  rlang::new_quosures(as.list(unlist(args, recursive = FALSE)))
}

# Whether defusal exposed a passed quosure's body. substitute() drops its
# attributes; an ordinary formula keeps its tilde in the captured expression.
is_captured_grouping_quosure <- function(written, captured) {
  !rlang::is_missing(written) && rlang::is_call(written, "~", n = 1L) &&
    !identical(written, rlang::quo_get_expr(captured))
}

# Defuse a non-active binding, retaining its raw syntax and captured scope.
capture_grouping_binding <- function(expr, env) {
  tryCatch(list(
    written = list(rlang::eval_bare(rlang::call2(base::substitute, expr), env)),
    quo = rlang::eval_bare(rlang::call2(rlang::enquo0, expr), env)
  ), error = function(cnd) NULL)
}

# Keep already captured quosures opaque to a constructor's next injection pass.
preserve_grouping_quosures <- function(expr) {
  if (rlang::is_missing(expr)) {
    return(rlang::missing_arg())
  }
  if (rlang::is_quosure(expr)) {
    # Injection must receive the captured value rather than evaluate its body.
    value <- function() expr
    return(rlang::call2("!", rlang::call2("!", rlang::call2(value))))
  }
  if (is.call(expr) || is.pairlist(expr)) {
    parts <- lapply(as.list(expr), preserve_grouping_quosures)
    out <- if (is.call(expr)) as.call(parts) else as.pairlist(parts)
    attributes(out) <- attributes(expr)
    return(out)
  }
  expr
}

new_grouping_spec <- function(type, args) {
  structure(
    list(type = type, args = args),
    class = "margin_grouping_spec"
  )
}

#' @exportS3Method
#' @noRd
print.margin_grouping_spec <- function(x, ...) {
  # The kind is read through `grouping_spec_kind()`, the reader the two guards
  # that can be handed an unvalidated object share, because this site asks
  # their question of the same field: whether the object can be asked for a
  # kind at all. It is not every object a class can sit on that answers, and
  # reading `$` before establishing that much printed no line at all -- base
  # R's own error from this line instead (#264). #262 fixed the guards and left
  # this site, because a guard has a refusal to reach and a print method has
  # none, so the answer there was not available here.
  #
  # What is printed for an object that cannot be asked is the line an object
  # answering `NULL` already prints: the naming below answers nothing for a
  # kind that is absent, and a read that raised is absent for this line's
  # purposes, whatever raised it. Reporting which of the two it was would be a
  # printed line saying what the object is, and that is the guards' sentence to
  # say -- the same distinction `grouping_spec_kind()` itself declines to draw.
  #
  # Binding the kind is what makes the read one: what is named below is the
  # value the read returned, not the field asked a second time. ADR 0008's
  # amendment for a specification the printer could not read decides that
  # count, and states what reading through a shared function costs.
  kind <- grouping_spec_kind(x)
  cat(
    "<marginplyr Grouping specification: ",
    grouping_kind_printed_name(kind),
    ">\n",
    sep = ""
  )
  invisible(x)
}

# The name a kind prints under, or the empty string where it has none. The
# caller holds a kind some object answered with, or `NULL` where none could be
# read; nothing about it has been established.
#
# The kind stored on a specification is internal, and three of the five are not
# the name of any function a caller can write, so the rules' own constructor
# names them: a new kind names itself correctly here without an edit. The three
# answers are a kind a rule names, a kind that is one name no rule knows and so
# names itself, and a kind that is no name at all. ADR 0008's amendment for a
# kind that is no name decides the last, which falls here as the empty string
# (#268).
#
# Deciding which of the three a kind takes is `grouping_kind_name()`'s, which
# every reader of an unvalidated kind shares. What this site holds afterwards is
# a name or nothing, so the `cat()` below takes a character whichever branch
# answers.
grouping_kind_printed_name <- function(kind) {
  name <- grouping_kind_name(kind)
  if (is.null(name)) {
    return("")
  }
  rule <- find_grouping_kind_rule(name)
  if (is.null(rule)) name else rule$constructor
}
