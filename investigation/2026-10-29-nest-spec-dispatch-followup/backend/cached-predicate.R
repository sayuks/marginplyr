# Investigated 2026-10-29 against 59d55a7867ffc0255f75be32a233d9d475bcb7fb.
# Run with Rscript <script> [repository-root] [--assert].
args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
repo <- if (length(positional)) positional[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
assert <- '--assert' %in% args
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
output_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1L]])))
} else tempdir()
source_eval_sizes <- integer()
probe <- function(x) { source_eval_sizes <<- c(source_eval_sizes, length(x)); x > 0L }
source <- data.table::data.table(g = c('a', 'b'), v = 1:2)
before <- data.table::copy(source)
root <- dtplyr::lazy_dt(source)
# Positive control: the original expression is evaluated on all source rows.
collected <- collect(filter(root, probe(v)) |> compute() |> arrange(v))
stopifnot(identical(source_eval_sizes, 2L), identical(collected$v, 1:2))
source_eval_sizes <- integer()
input <- filter(root, probe(v)) |> compute() |> arrange(v)
invisible(inspect_grouping(input, .grouping = rollup(where(is.character))))
cat('cached arranged inspection predicate sizes:', paste(source_eval_sizes, collapse = ','), '\n')
proxy_callbacks <- source_eval_sizes
stopifnot(all(source_eval_sizes == 0L), identical(source, before))
# All traced schema evaluations have isolated zero-row roots. A full source
# read was not observed; the defect below is false refusal and spurious warning.
proxy_root_sizes <- list()
trace('dt_eval', where = asNamespace('dtplyr'), print = FALSE,
      tracer = quote({proxy_root_sizes[[length(proxy_root_sizes) + 1L]] <<-
        vapply(utils::getFromNamespace('dt_sources', 'dtplyr')(x), nrow, integer(1))}))
ignore <- inspect_grouping(input, .grouping = rollup(where(is.character)))
untrace('dt_eval', where = asNamespace('dtplyr'))
stopifnot(all(unlist(proxy_root_sizes) == 0L), identical(source, before))
cat('metadata dt_eval source sizes:', paste(unlist(proxy_root_sizes), collapse = ','), '\n')

observe <- function(f) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(f(), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd)); invokeRestart('muffleWarning')
  }), error = identity)
  list(value = value, warnings = warnings)
}
normal <- function(x) {
  x <- ungroup(x)
  if ('data' %in% names(x)) x$data <- lapply(x$data, tibble::as_tibble)
  tibble::as_tibble(x)
}
consumers <- list(
  inspect = function(x) inspect_grouping(x, .grouping = rollup(where(is.character)), .format = 'list'),
  nest = function(x) collect(nest_with_margins(x, .grouping = rollup(g), .sort = 'last', .id = 'sid')),
  nest_by = function(x) nest_by_with_margins(x, .grouping = rollup(g), .sort = 'last', .id = 'sid')
)
results <- list(); number <- 0L
for (rows in c(1L, 3L)) for (predicate in c('index', 'minimum')) {
  src <- data.table::data.table(g = letters[seq_len(rows)], v = seq_len(rows))
  old <- data.table::copy(src)
  first <- dtplyr::lazy_dt(src)
  filtered <- if (predicate == 'index') filter(first, v[[1L]] > 0L) else filter(first, min(v) > 0L)
  cached <- compute(filtered)
  other <- dtplyr::lazy_dt(data.frame(g = letters[seq_len(rows)], w = seq_len(rows)))
  derivations <- list(rename = function(x) rename(x, amount = v),
    arrange = function(x) arrange(x, v),
    join = function(x) left_join(x, other, by = 'g'),
    union = function(x) union_all(x, x))
  for (shape in names(derivations)) {
    changed <- derivations[[shape]](cached)
    uncached <- derivations[[shape]](filtered)
    source_control <- observe(function() collect(changed))
    stopifnot(!inherits(source_control$value, 'error'), !length(source_control$warnings))
    materialized <- source_control$value
    for (consumer in names(consumers)) {
      subject <- consumers[[consumer]]
      expected <- observe(function() subject(materialized))
      plain_control <- observe(function() subject(cached))
      control <- observe(function() subject(uncached))
      got <- observe(function() subject(changed))
      stopifnot(!inherits(expected$value, 'error'), !length(expected$warnings),
        !inherits(control$value, 'error'), !length(control$warnings),
        !inherits(plain_control$value, 'error'), !length(plain_control$warnings),
        identical(normal(control$value), normal(expected$value)), identical(src, old))
      number <- number + 1L
      metadata_refusal <- inherits(got$value, 'marginplyr_error') &&
        grepl("Can't safely determine column metadata", conditionMessage(got$value), fixed = TRUE)
      # The cached-rename repeated-reference failure is a distinct finding.
      # This replay identifies its later materialization error separately
      # and asserts only metadata preparation's artificial conditions.
      additional_rename_error <- inherits(got$value, 'error') && !metadata_refusal
      if (additional_rename_error) stopifnot(shape == 'rename', consumer != 'inspect',
        grepl("Items of 'old' not found", conditionMessage(got$value), fixed = TRUE))
      failed <- metadata_refusal || length(got$warnings) > 0L
      if (!inherits(got$value, 'error')) stopifnot(identical(normal(got$value), normal(expected$value)))
      results[[number]] <- data.frame(rows, predicate, shape, consumer,
        error = inherits(got$value, 'error'), metadata_refusal, additional_rename_error,
        error_phase = if (metadata_refusal) 'metadata preparation' else
          if (additional_rename_error) 'materialization' else '',
        warnings = length(got$warnings),
        message = if (inherits(got$value, 'error')) conditionMessage(got$value) else paste(got$warnings, collapse = '; '),
        failed = failed)
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(output_dir, 'cached-predicate-results.csv'), row.names = FALSE)
cat('CASES', nrow(results), 'METADATA_REFUSALS', sum(results$metadata_refusal),
    'SPURIOUS_WARNING_CASES', sum(results$warnings > 0L),
    'SEPARATE_CACHED_RENAME_MATERIALIZATION_ERRORS', sum(results$additional_rename_error),
    'CONTROLS', nrow(results) * 3L, 'CONTROL_FAILURES 0\n')
print(aggregate(cbind(metadata_refusal, warnings, additional_rename_error) ~ predicate + shape, data = results, FUN = sum), row.names = FALSE)
if (any(results$metadata_refusal)) cat('First metadata refusal:\n',
  results$message[which(results$metadata_refusal)[[1L]]], '\n')
if (any(results$warnings > 0L)) cat('First warning:\n',
  results$message[which(results$warnings > 0L)[[1L]]], '\n')
stopifnot(identical(source, before))
if (assert) stopifnot(length(proxy_callbacks) == 0L, !any(results$failed))
