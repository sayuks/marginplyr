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
  cat("<marginplyr grouping specification: ", x$type, ">\n", sep = "")
  invisible(x)
}
