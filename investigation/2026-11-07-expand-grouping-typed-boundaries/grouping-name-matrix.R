pkgload::load_all(".", quiet = TRUE)

input <- data.frame(a1 = c("A", "B"), a2 = 1L, z = as.Date("2026-11-07") + 0:1)
selectors <- rlang::quos(
  concatenation = c(a = c(a1, a2)),
  range = c(a = a1:a2),
  helper = c(a = starts_with("a")),
  exact = c(a = all_of(c("a1", "a2")))
)
carriers <- list(
  data.frame = input,
  tibble = tibble::as_tibble(input),
  data.table = data.table::as.data.table(input)
)
constructors <- list(
  grouping_set = grouping_set, grouping_sets = grouping_sets, rollup = rollup,
  cube = cube, grouping_spec = grouping_spec
)
consumers <- list(inspect = inspect_grouping, expand = expand_with_margins)
observations <- list()
append_result <- function(carrier, shape, mode, constructor, consumer, policy, actual) {
  message <- conditionMessage(actual)
  stopifnot(inherits(actual, "marginplyr_error"),
            grepl("Can't rename grouping dimensions", message, fixed = TRUE),
            grepl("a = a1", message, fixed = TRUE),
            grepl("a = a2", message, fixed = TRUE))
  observations[[length(observations) + 1L]] <<- data.frame(
    carrier, shape, mode, constructor, consumer, policy,
    class = class(actual)[[1L]], diagnostic = gsub("\n", " ", message),
    stringsAsFactors = FALSE
  )
}
for (carrier in names(carriers)) {
  data <- carriers[[carrier]]
  before <- serialize(data, NULL)
  for (shape in names(selectors)) {
    selection <- selectors[[shape]]
    stopifnot(identical(tidyselect::eval_select(selection, data), c(a1 = 1L, a2 = 2L)),
              identical(names(dplyr::select(data, !!selection)), c("a1", "a2")),
              identical(tidyselect::eval_select(selection, as.list(data)), c(a = 1L, a = 2L)))
    for (policy in c("error", "drop", "keep")) {
      for (consumer in names(consumers)) {
        verb <- consumers[[consumer]]
        stopifnot(identical(
          verb(data, .by = !!selection, .duplicates = policy),
          verb(data, .by = c(a1, a2), .duplicates = policy)
        ))
        for (constructor in names(constructors)) {
          build <- constructors[[constructor]]
          for (mode in c("name-only", "mixed")) {
            direct <- if (mode == "name-only") build(!!selection) else
              build(!!selection, where(function(x) inherits(x, "Date")))
            literal <- if (mode == "name-only") build(c(a1, a2)) else
              build(c(a1, a2), z)
            typed <- if (mode == "name-only") build(identity(c("a1", "a2"))) else
              build(identity(c("a1", "a2")), where(function(x) inherits(x, "Date")))
            expected <- verb(data, .grouping = literal, .duplicates = policy)
            stopifnot(identical(expected, verb(data, .grouping = typed, .duplicates = policy)))
            actual <- tryCatch(
              verb(data, .grouping = direct, .duplicates = policy),
              error = identity
            )
            append_result(carrier, shape, mode, constructor, consumer, policy, actual)
          }
        }
      }
    }
  }
  stopifnot(identical(serialize(data, NULL), before))
}
result <- do.call(rbind, observations)
stopifnot(nrow(result) == 720L)
print(with(result, table(carrier, mode, consumer)))
cat("confirmed false refusals:", nrow(result), "\n")
for (p in c("dplyr", "tidyselect", "rlang", "vctrs", "data.table")) {
  cat(p, as.character(utils::packageVersion(p)), "\n")
}
cat(R.version.string, R.version$platform, "\n")
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) utils::write.csv(result, args[[1L]], row.names = FALSE)
