# Dated investigation evidence, run from the checkout root.
# --assert checks the desired warning propagation and intentionally fails on
# the investigated snapshot. Ordinary and true-absorption controls must pass.
pkgload::load_all(".", quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)

capture <- function(value) {
  tryCatch(force(value), error = identity)
}

show_value <- function(value) {
  if (inherits(value, "error")) {
    cat(paste(class(value), collapse = ","), conditionMessage(value), "\n")
  } else {
    print(value)
  }
}

new_caller_warning <- function(message) {
  structure(
    list(message = message, call = NULL),
    class = c("caller_warning", "warning", "condition")
  )
}

input <- arrow::arrow_table(data.frame(x = 1))
cause <- new_caller_warning("pulling data into R")
offset <- function() {
  warning(cause)
  1
}
seen <- list()
ordinary <- withCallingHandlers(
  dplyr::summarise(input, total = sum(x) + offset()),
  warning = function(condition) {
    seen[[length(seen) + 1L]] <<- condition
    invokeRestart("muffleWarning")
  }
)
stopifnot(
  inherits(ordinary, "arrow_dplyr_query"),
  length(seen) == 1L, identical(seen[[1L]], cause)
)
expected <- dplyr::collect(ordinary)
stopifnot(identical(expected$total, 2))
cat("MINIMAL ordinary stayed lazy; caller warning identity retained; total=2\n")
show_value(expected)

seen <- list()
actual <- capture(withCallingHandlers(
  summarize_with_margins(input, total = sum(x) + offset()),
  warning = function(condition) {
    seen[[length(seen) + 1L]] <<- condition
    invokeRestart("muffleWarning")
  }
))
cat("MINIMAL Margin outcome:\n")
show_value(actual)
cat("External warnings received:", length(seen), "\n")
minimal_ok <- !inherits(actual, "error") &&
  inherits(actual, "arrow_dplyr_query") && length(seen) == 1L &&
  identical(seen[[1L]], cause)
if (minimal_ok) {
  stopifnot(identical(dplyr::collect(actual)$total, 2))
}

# Refusing datasets cannot absorb. These queries contain an ordinary scalar
# callback and a translatable aggregate, not an unsupported data-dependent call.
path <- tempfile("margin-warning-dataset-")
dir.create(path)
data <- data.frame(g = c("a", "b"), x = c(1, 3))
arrow::write_parquet(data, file.path(path, "data.parquet"))
table <- arrow::arrow_table(data)
batch <- arrow::record_batch(data)
dataset <- arrow::open_dataset(path)
inputs <- list(
  table = table,
  table_query = dplyr::filter(table, x > 0),
  batch = batch,
  batch_query = dplyr::filter(batch, x > 0),
  dataset = dataset,
  dataset_query = dplyr::filter(dataset, x > 0)
)
markers <- c(
  "pulling data into R", "Pulling data into R",
  "Caller says: not pulling data into R here."
)
checks <- 0L
mismatches <- 0L
for (input_name in names(inputs)) {
  for (marker in markers) {
    for (custom in c(FALSE, TRUE)) {
      input <- inputs[[input_name]]
      cause <- if (custom) {
        new_caller_warning(marker)
      } else {
        simpleWarning(marker, call = NULL)
      }
      offset <- function() {
        warning(cause)
        1
      }
      seen <- list()
      ordinary <- withCallingHandlers(
        dplyr::summarise(input, total = sum(x) + offset(), .by = g),
        warning = function(condition) {
          seen[[length(seen) + 1L]] <<- condition
          invokeRestart("muffleWarning")
        }
      )
      stopifnot(
        inherits(ordinary, "arrow_dplyr_query"), length(seen) == 1L,
        identical(seen[[1L]], cause)
      )
      expected <- dplyr::arrange(dplyr::collect(ordinary), g)
      stopifnot(identical(expected$total, c(2, 4)))
      seen <- list()
      actual <- capture(withCallingHandlers(
        summarize_with_margins(input, total = sum(x) + offset(), .by = g),
        warning = function(condition) {
          seen[[length(seen) + 1L]] <<- condition
          invokeRestart("muffleWarning")
        }
      ))
      checks <- checks + 1L
      good <- inherits(actual, "arrow_dplyr_query") && length(seen) == 1L &&
        identical(seen[[1L]], cause)
      if (good) {
        stopifnot(identical(
          dplyr::arrange(dplyr::collect(actual), g), expected
        ))
      } else {
        mismatches <- mismatches + 1L
        cat("MISMATCH", input_name, marker, custom, "warnings", length(seen),
            "class", class(actual)[1L], "\n")
      }
    }
  }
}
cat("MATRIX", checks, "checks;", mismatches, "mismatches\n")

# Changing only caller warning text preserves the evaluable lazy result.
text_control_cause <- simpleWarning("other caller warning", call = NULL)
offset <- function() {
  warning(text_control_cause)
  1
}
seen <- list()
text_control <- withCallingHandlers(
  summarize_with_margins(table, total = sum(x) + offset(), .by = g),
  warning = function(condition) {
    seen[[length(seen) + 1L]] <<- condition
    invokeRestart("muffleWarning")
  }
)
stopifnot(
  inherits(text_control, "arrow_dplyr_query"), length(seen) == 1L,
  identical(seen[[1L]], text_control_cause),
  identical(dplyr::arrange(dplyr::collect(text_control), g)$total, c(2, 4))
)
cat("TEXT CONTROL passed with caller warning identity and lazy result\n")

# True Arrow absorption still needs refusal; this case does not relax it.
absorbed <- withCallingHandlers(
  dplyr::summarise(table, value = paste(x, collapse = ","), .by = g),
  warning = function(condition) invokeRestart("muffleWarning")
)
stopifnot(is.data.frame(absorbed))
refused <- capture(summarize_with_margins(
  table, value = paste(x, collapse = ","), .by = g
))
stopifnot(
  inherits(refused, "marginplyr_error"),
  grepl("Arrow cannot evaluate", conditionMessage(refused), fixed = TRUE)
)
cat("GENUINE ABSORPTION CONTROL: ordinary returned local frame; Margin refused\n")
unlink(path, recursive = TRUE)
cat(R.version.string, "\n")
for (package in c("arrow", "dplyr", "rlang", "marginplyr")) {
  cat(package, as.character(packageVersion(package)), "\n")
}
if (assert_desired) {
  stopifnot(minimal_ok, mismatches == 0L)
}
