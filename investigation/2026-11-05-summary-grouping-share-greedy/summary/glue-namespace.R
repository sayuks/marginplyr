# Standalone research signal for generated local across naming glue.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
contract <- '--assert' %in% args

run_case <- function(convert = identity, role = 'none', unpack = quote(identity(FALSE)),
                     names = NULL, intercept = TRUE) {
  hits <- 0L
  `:::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (identical(pkg, 'marginplyr') && identical(name, 'local_checked_glue_name')) {
      hits <<- hits + 1L
      if (intercept) return(function(...) 'intercepted')
    }
    get(name, envir = asNamespace(pkg))
  }
  data <- convert(if (role == 'none') {
    data.frame(x = c(1L, 3L))
  } else {
    data.frame(g = c('a', 'b'), x = c(1L, 3L))
  })
  scalar <- identical(unpack, quote(identity(FALSE))) || identical(unpack, quote(FALSE))
  fun <- if (scalar) quote(sum) else quote(~ tibble::tibble(z = sum(.x)))
  expression <- rlang::expr(dplyr::across(x, !!fun, .names = !!names, .unpack = !!unpack))
  summary <- rlang::new_quosure(expression, environment())
  expected <- if (role == 'none') {
    dplyr::summarise(data, !!summary)
  } else {
    dplyr::summarise(data, !!summary, .by = 'g')
  }
  if (role == 'rollup') {
    total <- dplyr::summarise(data, !!summary)
    total$g <- NA_character_
    total <- dplyr::select(total, dplyr::all_of(names(expected)))
    expected <- dplyr::bind_rows(expected, total)
  }
  ordinary_hits <- hits
  actual <- switch(role,
    none = summarize_with_margins(data, !!summary),
    by = summarize_with_margins(data, !!summary, .by = g),
    rollup = summarize_with_margins(data, !!summary, .grouping = rollup(g),
                                     .margin_label = NULL)
  )
  list(expected = expected, actual = actual, ordinary_hits = ordinary_hits,
       margin_hits = hits - ordinary_hits)
}

minimal <- run_case()
print(minimal)
stopifnot(identical(minimal$expected$x, 4L), minimal$ordinary_hits == 0L,
          identical(minimal$actual$intercepted, 4L), minimal$margin_hits == 1L)
if (contract) stopifnot(identical(minimal$actual, minimal$expected),
                       minimal$margin_hits == 0L)

unpack_cases <- alist(dynamic_false = identity(FALSE), dynamic_true = identity(TRUE),
                     dynamic_string = identity('{outer}_{inner}'),
                     literal_false = FALSE, literal_true = TRUE,
                     literal_string = '{outer}_{inner}')
converters <- list(data.frame = identity, tibble = tibble::as_tibble,
                   data.table = data.table::as.data.table)
rows <- list()
for (type in names(converters)) for (role in c('none', 'by', 'rollup')) {
  for (mode in names(unpack_cases)) for (template in c('default', 'custom')) {
    out <- run_case(converters[[type]], role, unpack_cases[[mode]],
                    if (template == 'custom') '{.col}_s' else NULL)
    is_dynamic <- startsWith(mode, 'dynamic')
    same <- identical(out$expected, out$actual)
    stopifnot(out$ordinary_hits == 0L,
              if (is_dynamic) !same && out$margin_hits > 0L else same && out$margin_hits == 0L)
    unshadowed <- run_case(converters[[type]], role, unpack_cases[[mode]],
                          if (template == 'custom') '{.col}_s' else NULL,
                          intercept = FALSE)
    stopifnot(identical(unshadowed$expected, unshadowed$actual))
    rows[[length(rows) + 1L]] <- data.frame(type, role, mode, template, same,
                                           margin_hits = out$margin_hits)
  }
}
results <- do.call(rbind, rows)
write.csv(results, 'investigation/2026-11-05-summary-grouping-share-greedy/summary/glue-matrix.csv',
          row.names = FALSE)
cat('matrix:', nrow(results), 'interception cases;', sum(!results$same),
    'wrong-name cases;', sum(results$same), 'literal controls;',
    nrow(results), 'unshadowed controls\n')
