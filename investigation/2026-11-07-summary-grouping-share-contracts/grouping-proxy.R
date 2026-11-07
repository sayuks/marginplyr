pkgload::load_all(".", quiet = TRUE)
mode <- commandArgs(trailingOnly = TRUE)
stopifnot(length(mode) == 1L, mode %in% c("observe", "contract"))

source_data <- data.table::data.table(a = c("x", "y"), b = c(1L, 1L))
source_bytes <- serialize(source_data, NULL)
step <- dtplyr::lazy_dt(source_data, immutable = TRUE)
selector <- rlang::quo(tidyselect::peek_vars()[1L])
wrapped <- rlang::quo(identity(!!selector))
literal <- rlang::quo(b)
inspect_fixed <- function(key) {
  marginplyr::inspect_grouping(step, .by = !!key, .format = "list")
}
count_by <- function(key) {
  dplyr::collect(marginplyr::summarize_with_margins(
    step, count = dplyr::n(), .by = !!key
  ))
}
stopifnot(identical(inspect_fixed(selector)$fixed, list("a")),
          identical(count_by(selector)$count, c(1L, 1L)))

# Registration is confined to this standalone R process.
proxy_rows <- integer()
proxy <- function(x) {
  proxy_rows <<- c(proxy_rows, nrow(x))
  x[, c("b", "a")]
}
registerS3method(
  "tidyselect_data_proxy", "data.table", proxy, asNamespace("tidyselect")
)
stopifnot(identical(
  names(tidyselect::eval_select(selector, source_data[0L, ])), "b"
))
stopifnot(identical(inspect_fixed(wrapped), inspect_fixed(literal)),
          identical(count_by(wrapped), count_by(literal)),
          identical(inspect_fixed(literal)$fixed, list("b")),
          identical(count_by(literal)$count, 2L))

actual_fixed <- inspect_fixed(selector)
actual_count <- count_by(selector)
results <- c(
  inspect_fixed = identical(actual_fixed, inspect_fixed(literal)),
  summarize_fixed = identical(actual_count, count_by(literal))
)
print(actual_count)
print(count_by(literal))

constructors <- list(
  grouping_set = function(key) marginplyr::grouping_set(!!key),
  grouping_sets = function(key) {
    marginplyr::grouping_sets(marginplyr::grouping_set(!!key))
  },
  rollup = function(key) marginplyr::rollup(!!key),
  cube = function(key) marginplyr::cube(!!key),
  grouping_spec = function(key) marginplyr::grouping_spec(!!key)
)
for (kind in names(constructors)) {
  construct <- constructors[[kind]]
  inspect <- function(key) {
    tryCatch(marginplyr::inspect_grouping(
      step, .by = a, .grouping = construct(key), .format = "list"
    ), error = identity)
  }
  expected <- inspect(literal)
  actual <- inspect(selector)
  genuine <- inspect(rlang::quo(a))
  stopifnot(identical(expected, inspect(wrapped)),
            identical(expected$included[[1L]], "b"),
            inherits(genuine, "marginplyr_error"))
  results[[kind]] <- identical(actual, expected)
  if (mode == "observe") stopifnot(inherits(actual, "marginplyr_error"))
  if (inherits(actual, "error")) {
    stopifnot(identical(conditionMessage(actual), conditionMessage(genuine)))
    cat(kind, "reports overlap on a\n")
  }
}
print(results)
stopifnot(length(proxy_rows) > 0L, all(proxy_rows == 0L),
          identical(serialize(source_data, NULL), source_bytes))
if (mode == "observe") {
  stopifnot(identical(actual_fixed$fixed, list("a")),
            identical(names(actual_count), c("a", "count")),
            identical(actual_count$a, c("x", "y")),
            identical(actual_count$count, c(1L, 1L)), !any(results))
} else {
  stopifnot(all(results))
}
