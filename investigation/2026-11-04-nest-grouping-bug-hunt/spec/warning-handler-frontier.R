args <- commandArgs(trailingOnly = TRUE)
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_dir, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")

capture <- function(thunk, warn, handling) {
  previous <- options(warn = warn)
  on.exit(options(previous))
  events <- list()
  result <- tryCatch(withCallingHandlers(thunk(), warning = function(cnd) {
    events[[length(events) + 1L]] <<- list(
      class = class(cnd), message = conditionMessage(cnd), call = conditionCall(cnd)
    )
    if (handling == "muffle") invokeRestart("muffleWarning")
    if (handling == "escalate") {
      stop(structure(list(message = "handler escalation", call = NULL),
                     class = c("probe_escalation", "error", "condition")))
    }
  }), error = identity)
  list(events = events, result = result)
}

data <- tibble::tibble(a = 1L, b = "x")
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
selectors <- list(
  quote(one_of("missing_one")),
  quote(c(a, one_of("missing_one"))),
  quote(c(one_of("missing_one"), one_of("missing_two"))),
  quote(c(one_of("missing_one"), one_of("missing_one"))),
  quote(c(one_of("missing_one"), a, one_of("missing_two")))
)
records <- list()
for (constructor in constructors) {
  for (selector_index in seq_along(selectors)) {
    selector <- selectors[[selector_index]]
    native <- function() tidyselect::eval_select(rlang::new_quosure(selector, globalenv()), data)
    for (warn in c(-1L, 0L, 1L, 2L)) {
      for (handling in c("muffle", "escalate", "continue")) {
        for (typed_by in c(FALSE, TRUE)) {
          expected <- capture(native, warn, handling)
          spec <- do.call(get(constructor, envir = asNamespace("marginplyr")),
                          list(rlang::new_quosure(selector, globalenv())))
          actual <- capture(function() {
            if (typed_by) {
              inspect_grouping(data, .by = where(is.character),
                               .grouping = spec, .duplicates = "drop", .format = "list")
            } else {
              inspect_grouping(data, .grouping = spec,
                               .duplicates = "drop", .format = "list")
            }
          }, warn, handling)
          native_error <- inherits(expected$result, "error")
          actual_error <- inherits(actual$result, "error")
          empty_units <- !native_error && length(expected$result) == 0L &&
            constructor %in% c("rollup", "cube")
          same_events <- identical(expected$events, actual$events)
          same_error <- if (empty_units) {
            inherits(actual$result, "marginplyr_error")
          } else if (native_error) {
            actual_error && identical(class(expected$result)[[1L]], class(actual$result)[[1L]])
          } else !actual_error
          quiet_empty_refusal <- constructor %in% c("rollup", "cube") &&
            selector_index %in% c(1L, 3L, 4L) && warn < 2L
          if (quiet_empty_refusal) {
            same_events <- length(actual$events) == 0L
            same_error <- inherits(actual$result, "marginplyr_error")
          }
          ok <- same_events && same_error
          records[[length(records) + 1L]] <- data.frame(
            constructor, selector_index, warn, handling, typed_by,
            expected_warnings = length(expected$events),
            actual_warnings = length(actual$events), same_events, same_error,
            disposition = if (quiet_empty_refusal) "quiet_empty_unit_refusal" else "native_match", ok
          )
          if (!ok) {
            cat("MISMATCH", constructor, selector_index, warn, handling, typed_by, "\n")
            str(expected)
            str(actual)
          }
        }
      }
    }
  }
}
records <- do.call(rbind, records)
cat("Comparisons:", nrow(records), "\n")
print(table(records$ok))
if (length(args)) utils::write.csv(records, args[[1L]], row.names = FALSE)
stopifnot(all(records$ok))
sessionInfo()
