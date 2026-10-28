args <- commandArgs(TRUE)
root <- if (length(args)) args[[1]] else getwd()
pkgload::load_all(root, quiet = TRUE)
data <- data.frame(g = c('a', 'b'), x = c(1, 3))
reads <- 0L
stamp <- function(x) { reads <<- reads + 1L; as.double(reads) }
result <- summarize_with_margins(
  data, value = stamp(x), fraction = share_of_total(value),
  .grouping = grouping_sets(grouping_set(g), grouping_set(), grouping_set()),
  .duplicates = 'keep', .id = 'set', .margin_label = NULL
)
print(result)
totals <- result$value[result$set %in% c(2L, 3L)]
stopifnot(length(totals) == 2L, length(unique(totals)) == 2L)
stopifnot(identical(result$fraction[result$set %in% c(2L, 3L)], c(1, 1)))
ref <- paste(readLines(file.path(root, 'man', 'summarize_with_margins.Rd')), collapse = '\n')
misleading <- grepl('the same values and are interchangeable.', ref, fixed = TRUE)
cat('distinct displayed Grand total values:', paste(totals, collapse = ', '), '\n')
cat('public reference claims identical values:', misleading, '\n')
stopifnot(!misleading)
