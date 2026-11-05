Sys.setenv(TESTTHAT_PARALLEL = 'false')
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', unset = getwd()), quiet = TRUE)
results <- list()
for (style in c('full', 'left', 'right', 'inner', 'semi', 'anti')) {
  for (masked in c(FALSE, TRUE)) {
    for (position in c('grouping', 'fixed')) {
      e <- new.env(parent = globalenv())
      e$events <- integer()
      if (masked) {
        e$merge <- function(x, y, ...) {
          events <<- c(events, nrow(x), nrow(y))
          result <- get('merge.data.table', asNamespace('data.table'))(x, y, ...)
          if (!nrow(result)) result[, g := integer()]
          result
        }
        environment(e$merge) <- e
      }
      e$x <- evalq(dtplyr::lazy_dt(data.table::data.table(g = c('a', 'b'), v = 1:2)), e)
      e$join <- get(paste0(style, '_join'))
      q <- evalq(join(x, x, by = 'g'), e)
      # Every call executes in the same outer environment as ordinary collection.
      native <- collect(q)
      stopifnot(is.character(native$g))
      for (verb in c('inspect_grouping', 'nest_with_margins', 'nest_by_with_margins')) {
        e$events <- integer()
        ans <- if (position == 'grouping') {
          get(verb)(q, .grouping = grouping_set(where(is.character)))
        } else {
          get(verb)(q, .by = where(is.character))
        }
        construction_events <- e$events
        if (verb == 'inspect_grouping') {
          field <- if (position == 'grouping') 'included' else 'fixed'
          correct <- identical(ans[[field]], '(g)')
        } else {
          if (inherits(ans, 'dtplyr_step')) ans <- collect(ans)
          correct <- 'g' %in% names(ans) && nrow(ans) == length(unique(native$g))
        }
        results[[length(results) + 1L]] <- data.frame(
          style, masked, position, verb, correct,
          construction_events = paste(construction_events, collapse = ','),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
results <- do.call(rbind, results)
print(results[!results$correct, ], row.names = FALSE)
cat('observations:',nrow(results),'; expected output:',sum(results$correct),'; wrong output:',sum(!results$correct),'\n')
stopifnot(all(results$correct == !(results$style == 'full' & results$masked)))
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else file.path(tempdir(), 'merge-frontier-controls.csv')
write.csv(results, output, row.names = FALSE)
