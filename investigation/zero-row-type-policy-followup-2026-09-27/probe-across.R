args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(pkgload::load_all(args[[1L]], quiet = TRUE))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
src <- dplyr::copy_to(con, tibble::tibble(g = 'a', v = 1, w = 2), 'src')
cases <- list(
  formula = rlang::quos(dplyr::across(c(v, w), ~grouping_id())),
  fn = rlang::quos(dplyr::across(c(v, w), function(x) grouping_id())),
  block = rlang::quos(dplyr::across(c(v, w), function(x) {grouping_id()})),
  paren = rlang::quos(dplyr::across(c(v, w), (~(grouping_id())))),
  mixed = rlang::quos(dplyr::across(c(v, w), list(
    mask = ~grouping_id(), bit = ~grouping_bit(g),
    text = ~as.character(grouping_id())
  ), .names = 'out_{.col}_{.fn}'))
)
for (empty in c(FALSE, TRUE)) for (name in names(cases)) {
  cat('\nCASE:', name, 'empty:', empty, '\n')
  input <- if (empty) dplyr::filter(src, FALSE) else src
  query <- summarize_with_margins(input, !!!cases[[name]],
    .grouping = grouping_set(g), .sort = 'none', .margin_label = NULL)
  print(attr(query, 'marginplyr_declared_types'))
  print(vapply(dplyr::collect(query), typeof, character(1)))
  print(vapply(dplyr::collect(dplyr::compute(query)), typeof, character(1)))
}
k <- 0L
bump <- function() { k <<- k + 1L; k }
for (empty in c(FALSE, TRUE)) {
  cat('\nDYNAMIC .names; empty:', empty, '\n')
  tryCatch({
    input <- if (empty) dplyr::filter(src, FALSE) else src
    query <- summarize_with_margins(input,
      dplyr::across(v, ~grouping_id(), .names = '{.col}_{bump()}'),
      .grouping = grouping_set(g), .sort = 'none', .margin_label = NULL)
    print(dplyr::tbl_vars(query))
    print(attr(query, 'marginplyr_declared_types'))
    print(dplyr::collect(query))
  }, error = function(e) cat(conditionMessage(e), '\n'))
}
DBI::dbDisconnect(con)
