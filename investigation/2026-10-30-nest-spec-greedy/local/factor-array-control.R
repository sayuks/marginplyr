pkgload::load_all('.', quiet = TRUE)
f <- structure(array(c(1L, 2L, NA_integer_, 3L, 2L), dim = c(5L, 1L, 1L)),
               levels = c('x', NA_character_, 'unused'), class = 'factor')
x <- tibble::tibble(g = f, row_id = 1:5)
commands <- list(
  upstream_group = function() dplyr::group_by(x, g),
  upstream_vctrs = function() vctrs::vec_group_loc(x['g']),
  nest = function() nest_with_margins(x, .grouping = rollup(g), .margin_label = NULL)
)
for (name in names(commands)) {
  error <- tryCatch(commands[[name]](), error = identity)
  stopifnot(inherits(error, 'error'))
  cat(name, ':', class(error)[[1L]], ':', conditionMessage(error), '\n')
}
