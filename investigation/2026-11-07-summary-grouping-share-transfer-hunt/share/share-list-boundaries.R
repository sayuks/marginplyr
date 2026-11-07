args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(3L, 4L) || !args[[3L]] %in% c('observation', 'control', 'contract')) {
  stop('Usage: Rscript share-list-boundaries.R SOURCE OUTPUT_DIR observation|control|contract [all|transfer|rename|nested]')
}
source <- normalizePath(args[[1L]], mustWork = TRUE)
out <- args[[2L]]
mode <- args[[3L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(source, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- data.frame(g = c('a', 'b'), x = c(1, 3), prior = c(10, 20))
forms <- list(
 identity_list = quote((function(prior) {
   carrier <- list(function() sum(prior))
   copied <- identity(carrier)
   REMOVE
   copied[[1L]]()
 })(1)),
 invisible_list = quote((function(prior) {
   carrier <- list(function() sum(prior))
   copied <- invisible(carrier)
   REMOVE
   copied[[1L]]()
 })(1)),
 force_list = quote((function(prior) {
   carrier <- list(function() sum(prior))
   copied <- force(carrier)
   REMOVE
   copied[[1L]]()
 })(1)),
 renamed = quote((function(prior) {
   carrier <- list(read = function() sum(prior))
   names(carrier) <- 'other'
   REMOVE
   carrier$other()
 })(1)),
 nested = quote((function(prior) {
   carrier <- list(outer = list(read = function() sum(prior)))
   REMOVE
   carrier$outer$read()
 })(1)),
 bare_control = quote((function(prior) {
   carrier <- list(function() sum(prior))
   copied <- carrier
   REMOVE
   copied[[1L]]()
 })(1)),
 renamed_numeric_control = quote((function(prior) {
   carrier <- list(read = function() sum(prior))
   names(carrier) <- 'other'
   REMOVE
   carrier[[1L]]()
 })(1))
)
run_summary <- function(input, read, kind, direction) {
  if (direction == 'after_share') {
    helper <- rlang::call2(paste0('share_of_', kind), quote(value))
    summarize_with_margins(
      input, value = sum(x), prior = !!helper, observed = !!read,
      .grouping = rollup(g)
    )
  } else {
    helper <- rlang::call2(paste0('share_of_', kind), quote(observed))
    summarize_with_margins(
      input, prior = sum(x), observed = !!read, ratio = !!helper,
      .grouping = rollup(g)
    )
  }
}
family <- if (length(args) == 4L) args[[4L]] else 'all'
stopifnot(family %in% c('all', 'transfer', 'rename', 'nested'))
selected <- switch(family, all = names(forms),
  transfer = c('identity_list', 'invisible_list', 'force_list'),
  rename = 'renamed', nested = 'nested')
forms <- forms[selected]
rows <- list()
for (input_name in c('data.frame', 'tibble', 'data.table')) {
  input <- switch(input_name, data.frame = data,
                  tibble = tibble::as_tibble(data),
                  data.table = data.table::as.data.table(data))
  before <- serialize(input, NULL)
  for (kind in c('parent', 'total')) {
    for (direction in c('after_share', 'source')) {
      for (form in names(forms)) {
        for (remove in c(TRUE, FALSE)) {
          if (mode == 'control' && remove &&
              !form %in% c('bare_control', 'renamed_numeric_control')) next
          read <- do.call(substitute, list(forms[[form]], list(
            REMOVE = if (remove) quote(rm(prior)) else quote(NULL)
          )))
          result <- tryCatch(run_summary(input, read, kind, direction),
                             error = identity)
          pattern <- if (direction == 'after_share') {
            'Ordinary summaries cannot use an earlier'
          } else 'depends on earlier summary alias'
          refused <- inherits(result, 'marginplyr_error') &&
            grepl(pattern, conditionMessage(result), fixed = TRUE)
          correct_legal <- !inherits(result, 'error') &&
            identical(as.numeric(result$observed), rep(1, 3L))
          expected_repaired <- !remove ||
            form %in% c('bare_control', 'renamed_numeric_control')
          okay <- if (remove) refused else correct_legal
          status <- if (inherits(result, 'error')) {
            class(result)[[1L]]
          } else paste(result$observed, collapse = ',')
          verdict <- if (remove) {
            if (refused) 'refused' else 'missing refusal'
          } else if (correct_legal) 'legal OK' else 'bad legal'
          observation_ok <- if (expected_repaired) okay else {
            !inherits(result, 'error') &&
              identical(as.numeric(result$observed),
                        if (direction == 'after_share') c(10, 20, 30) else c(1, 3, 4))
          }
          rows[[length(rows) + 1L]] <- data.frame(
            input = input_name, kind, direction, form, remove, status, verdict,
            contract_ok = okay, observation_ok
          )
        }
      }
    }
  }
  stopifnot(identical(before, serialize(input, NULL)))
}
result <- do.call(rbind, rows)
write.csv(result, file.path(out, 'results.csv'), row.names = FALSE)
print(table(result$form, result$verdict))
cat('Comparisons:', nrow(result), '\n')
capture.output(sessionInfo(), file = file.path(out, 'session-info.txt'))
if (mode == 'contract') {
  stopifnot(all(result$contract_ok))
} else if (mode == 'control') {
  stopifnot(all(result$contract_ok))
} else {
  stopifnot(all(result$observation_ok))
}
