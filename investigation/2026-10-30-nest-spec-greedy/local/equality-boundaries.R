pkgload::load_all('.', quiet = TRUE)
new_ci <- function(x) vctrs::new_vctr(x, class = 'hunt_ci')
vec_proxy_equal.hunt_ci <- function(x, ...) tolower(vctrs::vec_data(x))
format.hunt_ci <- function(x, ...) vctrs::vec_data(x)
as.character.hunt_ci <- function(x, ...) vctrs::vec_data(x)
source_cases <- list(two = c('A', 'a'), reversed = c('a', 'A'), four = c('A', 'a', 'B', 'b'), single = 'A', empty = character())
records <- list()
total_checks <- 0L
for (case in names(source_cases)) for (plan in c('single', 'rollup', 'fixed')) {
  x <- tibble::tibble(g = new_ci(source_cases[[case]]), row_id = seq_along(source_cases[[case]]))
  before <- serialize(x, NULL)
  expected <- vctrs::vec_group_loc(x['g'])$loc
  for (verb in c('nest_with_margins', 'nest_by_with_margins')) for (keep in c(FALSE, TRUE)) for (sorting in c('none', 'first', 'last')) for (label in c('string', 'NULL', 'NA')) {
    margin_label <- switch(label, string = 'Total', 'NULL' = NULL, 'NA' = NA_character_)
    spec <- if (plan == 'single') grouping_set(g) else if (plan == 'rollup') rollup(g) else NULL
    fixed <- if (plan == 'fixed') rlang::sym('g') else NULL
    out <- rlang::inject((get(verb))(x, .by = !!fixed, .grouping = !!spec, .keep = keep, .sort = sorting, .margin_label = margin_label, .id = 'set'))
    stopifnot(identical(before, serialize(x, NULL)))
    actual <- out$data[out$set == 1L]
    if (plan == 'rollup') {
      total <- out$data[out$set == 2L]
      stopifnot(length(total) == as.integer(nrow(x) > 0L))
      if (length(total)) stopifnot(identical(total[[1L]]$row_id, x$row_id))
      total_checks <- total_checks + 1L
    }
    signature <- function(cells) sort(vapply(cells, function(cell) paste(cell, collapse = ','), character(1)))
    actual_sig <- signature(lapply(actual, function(cell) cell$row_id))
    expected_sig <- signature(expected)
    record <- list(case = case, plan = plan, verb = verb, keep = keep, sorting = sorting, label = label, correct = identical(actual_sig, expected_sig), actual = paste(actual_sig, collapse = ';'), expected = paste(expected_sig, collapse = ';'))
    records[[length(records) + 1L]] <- record
    if (keep) for (cell in out$data) stopifnot(identical(cell$g, vctrs::vec_slice(x$g, cell$row_id)))
  }
}
results <- do.call(rbind, lapply(records, as.data.frame))
write.csv(results, 'investigation/2026-10-30-nest-spec-greedy/local/equality-boundaries.csv', row.names = FALSE)
print(with(results, table(case, plan, label, correct)))
cat('calls:', nrow(results), 'faithful memberships:', sum(results$correct), 'incorrect memberships:', sum(!results$correct), '\n')
stopifnot(all(results$correct[results$label != 'string' | results$plan == 'fixed' | results$case %in% c('single', 'empty')]))
stopifnot(!any(results$correct[results$label == 'string' & results$plan != 'fixed' & !results$case %in% c('single', 'empty')]))
cat('Grand total membership controls:', total_checks, '\n')
stopifnot(total_checks == 180L)
cat('PASS: dated defect/control observation assertions; no fix applied\n')
