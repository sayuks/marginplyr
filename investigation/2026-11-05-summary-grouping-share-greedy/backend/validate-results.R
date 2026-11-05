results <- read.csv('investigation/2026-11-05-summary-grouping-share-greedy/backend/name-results.csv')
expected <- ifelse(results$name %in% c('...','..1'), ifelse(results$pos <= 4L,'local-error','backend-error'), 'match')
stopifnot(nrow(results)==736L,identical(results$verdict,expected))
print(aggregate(pos ~ backend + verdict, results, length))
