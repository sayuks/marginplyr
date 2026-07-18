#' Build a grouping specification
#'
#' These constructors describe SQL-style grouping operations for the
#' `.grouping` argument of [summarize_with_margins()] and related verbs.
#'
#' - `grouping_set()` creates one grouping set. With no columns it represents
#'   the empty set `()`.
#' - `grouping_sets()` forms the union of its arguments.
#' - `rollup()` creates hierarchical prefixes.
#' - `cube()` creates every subset of its dimensions.
#' - `grouping_spec()` forms the Cartesian product of its arguments, like
#'   comma-separated SQL `GROUP BY` items.
#'
#' A `grouping_set()` nested directly inside `rollup()` or `cube()` is a
#' composite dimension. Its columns are added or removed together.
#'
#' @param ... Bare columns, tidy-select expressions, or nested grouping
#'   specifications as appropriate for the constructor.
#'
#' @return A grouping specification for use in `.grouping`.
#' @export
#' @examples
#' rollup(region, store)
#' cube(product, channel)
#'
#' grouping_sets(
#'   grouping_set(region, store),
#'   grouping_set(region, product),
#'   grouping_set()
#' )
#'
#' grouping_spec(
#'   rollup(region, store),
#'   cube(product, channel)
#' )
#'
#' cube(grouping_set(country, state), year)
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
