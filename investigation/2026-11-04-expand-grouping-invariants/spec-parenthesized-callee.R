# Run from the repository root with Rscript. Use --assert for the contract.
pkgload::load_all(quiet = TRUE)

input <- data.frame(a = "detail", b = 1L)
consumers <- list(
  inspect = function(spec) inspect_grouping(input, .grouping = spec,
                                             .format = "list"),
  expand = function(spec) expand_with_margins(input, .grouping = spec,
                                               .id = "set", .margin_label = NULL)
)

observe <- function(parent, child, operator, depth, consumer,
                    second = "different") {
  reads <- 0L
  parentheses <- 0L
  warnings <- 0L
  caller_selector <- function(...) {
    reads <<- reads + 1L
    if (second == "error" && reads == 2L) {
      stop("second selector evaluation")
    }
    if (second == "warn") {
      warning("selector evaluated")
    }
    if (reads == 1L) "a" else "b"
  }

  caller <- new.env(parent = environment())
  caller[[child]] <- caller_selector
  if (!is.na(operator)) {
    caller[[operator]] <- function(package, name) caller_selector
  }
  caller[["("]] <- function(x) {
    parentheses <<- parentheses + 1L
    force(x)
    caller_selector
  }

  head <- as.name(child)
  if (!is.na(operator)) {
    head <- call(operator, quote(marginplyr), head)
  }
  for (i in seq_len(depth)) {
    head <- call("(", head)
  }

  constructor <- getExportedValue("marginplyr", parent)
  spec <- eval(call("constructor", as.call(list(head))), caller)
  result <- tryCatch(
    withCallingHandlers(consumer(spec), warning = function(cnd) {
      if (identical(conditionMessage(cnd), "selector evaluated")) {
        warnings <<- warnings + 1L
        invokeRestart("muffleWarning")
      }
    }),
    error = identity
  )
  first <- consumer(constructor(a))
  later <- consumer(constructor(b))
  list(
    reads = reads,
    parentheses = parentheses,
    warnings = warnings,
    selection = if (identical(result, first)) "a" else if (
      identical(result, later)
    ) "b" else "other",
    error = if (inherits(result, "error")) conditionMessage(result) else NULL
  )
}

# Bare and qualified heads use the same caller selector. Base parentheses
# around a callee are a separate case from the parentheses around a selection.
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                  "grouping_spec")
matrix <- expand.grid(
  parent = constructors,
  child = constructors,
  operator = c(NA_character_, "::", ":::"),
  depth = 0:2,
  consumer = names(consumers),
  stringsAsFactors = FALSE
)
answers <- lapply(seq_len(nrow(matrix)), function(i) {
  case <- matrix[i, ]
  observe(case$parent, case$child, case$operator, case$depth,
          consumers[[case$consumer]])
})
matrix$reads <- vapply(answers, `[[`, integer(1), "reads")
matrix$parentheses <- vapply(answers, `[[`, integer(1), "parentheses")
matrix$selection <- vapply(answers, `[[`, character(1), "selection")
matrix$matches <- matrix$reads == 1L & matrix$selection == "a" &
  matrix$parentheses == matrix$depth
print(with(matrix, table(depth, consumer, reads, selection)))

warning_case <- observe("grouping_sets", "rollup", NA_character_, 1L,
                        consumers$inspect, second = "warn")
error_case <- observe("grouping_sets", "rollup", NA_character_, 1L,
                      consumers$inspect, second = "error")
print(warning_case)
print(error_case)

controls <- matrix$depth == 0L
stopifnot(all(matrix$matches[controls]))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(matrix$matches), warning_case$warnings == 1L,
            is.null(error_case$error), error_case$selection == "a")
}
