# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages(pkgload::load_all('.'))
suppressPackageStartupMessages(library(dplyr))
source <- arrow::Table$create(tibble(.data = 'x', b = 'y'))
read_entries <- 0L
read_rows <- integer()
suppressMessages(trace('collect',where=asNamespace('dplyr'),
  tracer=quote({.GlobalEnv$read_entries <- .GlobalEnv$read_entries + 1L}),
  exit=quote({.GlobalEnv$read_rows <- c(.GlobalEnv$read_rows,nrow(returnValue()))}),
  print=FALSE))
result <- suppressWarnings(tryCatch(
  expand_with_margins(source,.grouping=grouping_set('.data','b')),
  error=identity
))
suppressMessages(untrace('collect',where=asNamespace('dplyr')))
cat('automatic collect entries:',read_entries,'\n')
cat('completed automatic collect row counts:',paste(read_rows,collapse=','),'\n')
cat('outcome:',if(inherits(result,'error'))conditionMessage(result) else paste(class(result),collapse='/'),'\n')
stopifnot('Margin construction must not collect input rows' = read_entries == 0L,
          'Result must remain lazy' = inherits(result,'arrow_dplyr_query'))
