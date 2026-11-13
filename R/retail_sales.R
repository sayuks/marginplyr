#' Monthly sales for a fictional retail chain
#'
#' A small synthetic data set used throughout the package documentation.
#' Each row records one month's sales of one product, through one channel and
#' store. Online-direct records have a missing `store`, which makes it possible
#' to distinguish a source missing value from a subtotal produced by a grouping
#' operation.
#'
#' @format A data frame with 24 rows and 8 variables:
#' \describe{
#'   \item{year}{Calendar year, an integer: `2025` or `2026`.}
#'   \item{month}{Month abbreviation, a character: `"Jan"` or `"Feb"`. As
#'     text, `"Feb"` sorts before `"Jan"`.}
#'   \item{region}{Sales region: `"East"` or `"West"`.}
#'   \item{store}{Store name, or `NA` for an online-direct record. The four
#'     `NA` rows are all East, Headphones, Online: one per month.}
#'   \item{product}{Product category: `"Headphones"`, `"Laptop"`, or
#'     `"Monitor"`.}
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
"retail_sales"
