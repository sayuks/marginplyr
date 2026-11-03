# Distinguish warning conversion from classification and backend behavior.
pkgload::load_all(quiet = TRUE)
out <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping"
run <- function() {
  old <- options()
  on.exit(options(old), add = TRUE)
  data <- tibble::tibble(a = 1L, b = "x")
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  suppressMessages(do.call(trace, list(
    what = "grouping_selection_proxy", where = asNamespace("marginplyr"),
    print = FALSE, tracer = function() {
      state$reads <- state$reads + 1L
    }
  )))
  on.exit(suppressMessages(untrace(
    "grouping_selection_proxy", where = asNamespace("marginplyr")
  )), add = TRUE)
  records <- list()
  for (selector in c("one_of", "pronoun")) {
    expression <- if (selector == "one_of") quote(one_of("absent")) else quote(.data$a)
    spec <- eval(rlang::call2("grouping_set", expression))
    preflight <- get("preflight_grouping_spec", asNamespace("marginplyr"))
    stopifnot(preflight(spec, names(data))$name_only)
    modes <- if (selector == "one_of") "warning" else c("quiet", "warning", "error")
    for (verbosity in modes) {
      for (warn in 0:2) {
        options(warn = warn, lifecycle_verbosity = verbosity)
        state$reads <- 0L
        warnings <- character()
        value <- tryCatch(withCallingHandlers(
          inspect_grouping(data, .grouping = spec),
          warning = function(cnd) {
            warnings <<- c(warnings, conditionMessage(cnd))
          }
        ), error = identity)
        error <- inherits(value, "error")
        row <- data.frame(
          selector = selector, verbosity = verbosity, warn = warn,
          proxy_reads = state$reads, warnings = length(warnings),
          error = error,
          converted = error && grepl("converted from warning", conditionMessage(value), fixed = TRUE),
          lifecycle_error = inherits(value, "lifecycle_error_deprecated")
        )
        if (verbosity == "error") {
          stopifnot(row$lifecycle_error, row$proxy_reads == 0L)
        } else if (verbosity == "quiet") {
          stopifnot(!row$error, row$warnings == 0L, row$proxy_reads == 1L)
        } else if (warn == 2L) {
          stopifnot(row$converted, row$proxy_reads == 1L)
        } else {
          stopifnot(!row$error, row$warnings >= 1L, row$proxy_reads == 1L)
        }
        records[[length(records) + 1L]] <- row
      }
    }
  }
  results <- do.call(rbind, records)
  utils::write.csv(results, file.path(out, "warning-mode-controls.csv"), row.names = FALSE)
  print(results)
  invisible(NULL)
}
run()
