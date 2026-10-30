# Dated observation replay. --assert requires the correct public membership.
pkgload::load_all('.', quiet = TRUE)
new_case_key <- function(x) vctrs::new_vctr(x, class = 'hunt_case_key')
vec_proxy_equal.hunt_case_key <- function(x, ...) {
  tolower(vctrs::vec_data(x))
}
format.hunt_case_key <- function(x, ...) vctrs::vec_data(x)
as.character.hunt_case_key <- function(x, ...) vctrs::vec_data(x)
x <- tibble::tibble(g = new_case_key(c('A', 'a')), row_id = 1:2)
source_groups <- vctrs::vec_group_loc(x['g'])$loc
stopifnot(identical(source_groups, list(1:2)))
upstream <- dplyr::nest_by(x, g)
stopifnot(nrow(upstream) == 1L, identical(upstream$data[[1L]]$row_id, 1:2))
summary_control <- summarize_with_margins(x, .grouping = grouping_set(g), n = dplyr::n())
stopifnot(nrow(summary_control) == 1L, identical(summary_control$n, 2L))
cat('source equality groups / ordinary dplyr::nest_by() / Margin summary: one two-row group\n')
observations <- list()
for (verb in c('nest_with_margins', 'nest_by_with_margins')) {
  actual <- get(verb)(x, .grouping = grouping_set(g))
  null_control <- get(verb)(x, .grouping = grouping_set(g), .margin_label = NULL)
  fixed_control <- get(verb)(x, .by = g)
  stopifnot(nrow(null_control) == 1L, identical(null_control$data[[1L]]$row_id, 1:2))
  stopifnot(nrow(fixed_control) == 1L, identical(fixed_control$data[[1L]]$row_id, 1:2))
  cat(verb, ': default label yielded', nrow(actual), 'groups; cell memberships:',
      paste(vapply(actual$data, function(cell) paste(cell$row_id, collapse = ','), character(1)), collapse = ' / '), '\n')
  observations[[verb]] <- nrow(actual) == 1L && identical(actual$data[[1L]]$row_id, 1:2)
}
if ('--assert' %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(unlist(observations)))
} else {
  stopifnot(!any(unlist(observations)))
  cat('PASS: recorded the dated wrong-membership observations; no fix applied\n')
}
