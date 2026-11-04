base <- 'investigation/2026-11-04-summary-grouping-share-bug-hunt'
targets <- c('summary/reproduce-noncanonical-dots.R',
  'summary/reproduce-forwarded-factory.R', 'share/dtplyr-ratio-reproduction.R',
  'share/formal-default-reproduction.R')
results <- list()
for (target in targets) for (assert in c(FALSE, TRUE)) {
  label <- paste0(sub('[.]R$', '', basename(target)), if (assert) '-assert' else '-report')
  args <- c(file.path(base, target), if (assert) '--assert')
  log <- file.path(base, 'coordinator', paste0(label, '.log'))
  status <- system2(file.path(R.home('bin'), 'Rscript'), shQuote(args), stdout = log, stderr = log)
  expected <- if (assert) 1L else 0L
  results[[length(results) + 1L]] <- data.frame(target, assert, status, expected, pass = status == expected)
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(base, 'coordinator', 'replay-results.csv'), row.names = FALSE)
print(results, row.names = FALSE)
stopifnot(all(results$pass))
