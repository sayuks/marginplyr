Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)

data <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L))
step <- dtplyr::lazy_dt(data)
before <- serialize(step, NULL)
proxy <- data.table::as.data.table(data[FALSE, ])
names.data.table <- function(x) {
  columns <- attr(x, "names")
  in_selection <- tryCatch(
    tidyselect::peek_data(), error = function(cnd) NULL
  )
  if (is.null(in_selection)) columns else rev(columns)
}

direct <- rlang::quo(names(tidyselect::peek_data())[1L])
wrapped <- rlang::quo(identity(names(tidyselect::peek_data())[1L]))
expected <- tidyselect::eval_select(direct, proxy)
stopifnot(identical(expected, c(b = 2L)))
stopifnot(identical(tidyselect::eval_select(wrapped, proxy), expected))

fixed <- function(selection) {
  inspect_grouping(step, .by = !!selection, .format = "list")$fixed[[1L]]
}
counts <- function(selection) {
  dplyr::collect(summarize_with_margins(step, n = dplyr::n(),
                                       .by = !!selection))$n
}
dimensions <- function(spec) {
  inspect_grouping(step, .by = a, .grouping = spec,
                   .format = "list")$included
}
construct <- list(
  grouping_set = function(x) grouping_set(!!x),
  grouping_sets = function(x) grouping_sets(grouping_set(!!x)),
  rollup = function(x) rollup(!!x),
  cube = function(x) cube(!!x),
  grouping_spec = function(x) grouping_spec(grouping_set(!!x))
)
stopifnot(identical(fixed(wrapped), "b"), identical(counts(wrapped), 2L))
expected_dimensions <- lapply(construct, function(constructor) {
  valid <- dimensions(constructor(wrapped))
  stopifnot(identical(valid[[1L]], "b"))
  valid
})
results <- lapply(construct, function(constructor) {
  tryCatch(dimensions(constructor(direct)), error = identity)
})
stopifnot(identical(serialize(step, NULL), before))
actual <- fixed(direct)
actual_counts <- counts(direct)
print(actual)
print(actual_counts)
print(lapply(results, function(x) {
  if (inherits(x, "error")) conditionMessage(x) else x
}))
if ("contract" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(
    identical(actual, "b"), identical(actual_counts, 2L),
    identical(results, expected_dimensions)
  )
} else {
  stopifnot(identical(actual, "a"), identical(actual_counts, c(1L, 1L)))
  overlap <- vapply(results, function(x) {
    inherits(x, "marginplyr_error") && grepl(
      "Columns cannot appear in both `.by` and `.grouping`",
      conditionMessage(x), fixed = TRUE
    )
  }, logical(1))
  stopifnot(all(overlap))
}
