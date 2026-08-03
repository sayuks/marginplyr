#' Monthly sales for a fictional retail chain
#'
#' A small synthetic data set used throughout the package documentation.
#' Each row records sales for one product, channel, and store combination.
#' Online-direct records have a missing `store`, which makes it possible to
#' distinguish a source missing value from a subtotal produced by a grouping
#' operation.
#'
#' @format A tibble with 24 rows and 8 variables:
#' \describe{
#'   \item{year}{Calendar year.}
#'   \item{month}{Three-letter month abbreviation.}
#'   \item{region}{Sales region: `"East"` or `"West"`.}
#'   \item{store}{Store name, or `NA` for an online-direct record.}
#'   \item{product}{Product category.}
#'   \item{channel}{Sales channel: `"Online"` or `"Store"`.}
#'   \item{units}{Number of units sold.}
#'   \item{revenue}{Revenue in US dollars.}
#' }
#' @source Synthetic data created for marginplyr examples.
#' @seealso [summarize_with_margins()], [expand_with_margins()],
#'   [nest_with_margins()], and [nest_by_with_margins()] for the Margin verbs
#'   these columns are used with, and [grouping_bit()] for telling the missing
#'   `store` values apart from a subtotal.
#' @keywords datasets
#' @usage data(retail_sales)
#' @name retail_sales
NULL
