#' Build a grouping specification
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
#' - [grouping_spec()] forms the Cartesian product of its arguments, like
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
#'   identity product (the empty grouping set).
#'
#'   A nested Grouping specification is recognized by how it is written: a
#'   call to one of these constructors, or a name bound to a specification.
#'   Any other argument is a column selection, so a call of your own that
#'   returns a specification is refused where it is nested even though it is
#'   accepted as `.grouping` itself. Assign what it returns to a name and use
#'   that name: `s <- my_spec(region)`, then `grouping_sets(s, grade)`. A
#'   specification written inside a selection, as in `c(s, grade)`, is a
#'   selection containing something it cannot select, and is refused as one —
#'   unless the input has a column named `s`, when the selection takes that
#'   column, as any selection does with a name the data holds.
#'
#'   A name both readings claim is refused rather than guessed. Where the
#'   input has a column named `s` and `s` is also bound to a nested Grouping
#'   specification the position accepts, the call names both readings and the
#'   spelling that settles each: `all_of("s")` selects the column whatever is
#'   bound, and `!!s` uses the specification whatever columns the input has.
#'   What a position accepts is what decides whether there are two readings,
#'   so a colliding name is refused in [grouping_sets()] and [grouping_spec()]
#'   whatever specification it is bound to, in [rollup()] and [cube()] only
#'   when it is bound to a [grouping_set()], and never in [grouping_set()],
#'   which takes no nested Grouping specification at all. A name bound to
#'   anything that is not a specification is a column, as it always was.
#'
#'   A dimension is a column of the input, so a selection cannot rename it:
#'   `c(area = region)` is an error rather than a dimension named `area`.
#'   Rename the result afterwards with [dplyr::rename()].
#'
#' @return A grouping specification for use in `.grouping`.
#' @family grouping plans and grouping identity
#' @seealso [summarize_with_margins()], [expand_with_margins()],
#'   [nest_with_margins()], and [nest_by_with_margins()], the Margin verbs
#'   that consume a grouping specification.
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
#' # grouping_spec() takes their Cartesian product, producing combinations
#' # such as (year, month, region, store) and (year, region).
#' combined_totals <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = grouping_spec(
#'     rollup(year, month),
#'     rollup(region, store)
#'   )
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
  new_grouping_spec("set", rlang::enquos(...))
}

#' @rdname grouping_set
#' @export
grouping_sets <- function(...) {
  new_grouping_spec("sets", rlang::enquos(...))
}

#' @rdname grouping_set
#' @export
rollup <- function(...) {
  new_grouping_spec("rollup", rlang::enquos(...))
}

#' @rdname grouping_set
#' @export
cube <- function(...) {
  new_grouping_spec("cube", rlang::enquos(...))
}

#' @rdname grouping_set
#' @export
grouping_spec <- function(...) {
  new_grouping_spec("product", rlang::enquos(...))
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
    "<marginplyr grouping specification: ",
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
# every reader of an unvalidated kind shares: a value whose own `is.na()` or
# `length()` raises is no name there rather than an error here. What this site
# holds afterwards is a name or nothing, so the `cat()` below takes a character
# whichever branch answers.
grouping_kind_printed_name <- function(kind) {
  name <- grouping_kind_name(kind)
  if (is.null(name)) {
    return("")
  }
  rule <- find_grouping_kind_rule(name)
  if (is.null(rule)) name else rule$constructor
}
