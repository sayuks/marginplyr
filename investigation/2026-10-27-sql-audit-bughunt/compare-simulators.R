# Compare fresh captures from the prior investigation's unchanged simulator harness.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
a <- readRDS(file.path(args[[1L]], 'sim-off.rds'))
b <- readRDS(file.path(args[[1L]], 'sim-on.rds'))
stopifnot(length(a) == 528L, identical(names(a), names(b)))
failures <- character()
for (key in names(a)) for (field in c('value', 'conditions', 'render', 'seed')) {
  if (!identical(a[[key]][[field]], b[[key]][[field]])) {
    failures <- c(failures, paste(key, field, sep = '/'))
  }
}
cat('Simulator pairs:', length(a), '; failures:', length(failures), '\n')
print(failures)
if (length(failures)) stop('Simulator differential failure')
