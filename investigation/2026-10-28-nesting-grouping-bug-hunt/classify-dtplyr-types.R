x <- readRDS('/tmp/marginplyr-4306-dt-types-diffs.rds')
output <- Filter(function(z) !inherits(z$actual, 'error'), x)
errors <- Filter(function(z) inherits(z$actual, 'error'), x)
signatures <- function(tab) {
  count <- length(tab[[1L]])
  sort(vapply(seq_len(count), function(i) {
    row <- lapply(tab, function(col) vctrs::vec_slice(col, i))
    paste(as.character(serialize(row, NULL, version = 3)), collapse = '')
  }, character(1)))
}
bad <- names(Filter(function(z) !identical(signatures(z$actual), signatures(z$expected)), output))
cat('CANDIDATES',length(x),'EXTERNAL_ERRORS',length(errors),'SUCCESSFUL_OUTPUTS',length(output),'MULTISET_MISMATCHES',length(bad),'\n')
if(length(bad))print(bad)
stopifnot(length(bad)==0L)
cat('External-error candidates by source type:\n')
print(table(vapply(names(errors),function(z) strsplit(z,' ',fixed=TRUE)[[1]][1],character(1))))
