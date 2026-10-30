pkgload::load_all('.', quiet = TRUE)
new_bucket <- function(x) vctrs::new_vctr(x, class = 'hunt_bucket')
vec_proxy_equal.hunt_bucket <- function(x, ...) floor(vctrs::vec_data(x))
format.hunt_bucket <- function(x, ...) as.character(vctrs::vec_data(x))
as.character.hunt_bucket <- function(x, ...) as.character(vctrs::vec_data(x))
x <- tibble::tibble(g = new_bucket(c(1.1, 1.2)), row_id = 1:2)
control <- dplyr::summarise(dplyr::group_by(x, g), rows = list(row_id))
stopifnot(nrow(control) == 1L, identical(control$rows[[1]], 1:2))
upstream <- dplyr::nest_by(x, g)
stopifnot(nrow(upstream) == 1L, identical(upstream$data[[1L]]$row_id, 1:2))
print(control)
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  fixed <- verb(x, .by = g)
  typed <- verb(x, .grouping = rollup(g), .margin_label = NULL)
  labeled <- verb(x, .grouping = rollup(g), .id = 'set')
  summary <- summarize_with_margins(x, n = dplyr::n(), .grouping = rollup(g))
  stopifnot(nrow(fixed) == 1L, nrow(typed) == 2L, nrow(summary) == 2L)
  print(labeled)
  print(lapply(labeled$data, function(cell) cell$row_id))
  print(summary)
  if ('--assert' %in% commandArgs(TRUE)) {
    stopifnot(nrow(labeled) == 2L)
  }
}
