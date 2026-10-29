# Dated investigation replay; run from the repository root.
# --assert requires the promised behavior and exits nonzero on the inspected baseline.
pkgload::load_all(quiet = TRUE)
results <- logical()

partition_input <- function(partition) {
  path <- tempfile("marginplyr-partitions-")
  dir.create(path)
  for (i in 1:2) {
    directory <- file.path(path, paste0(partition, "=", i))
    dir.create(directory)
    arrow::write_parquet(
      data.frame(g = 10L - i, row_id = i), file.path(directory, "facts.parquet")
    )
  }
  fields <- stats::setNames(list(arrow::int32()), partition)
  dataset <- arrow::open_dataset(
    path, factory_options = list(selector_ignore_prefixes = character()),
    schema = arrow::schema(g = arrow::int32(), row_id = arrow::int32(), !!!fields),
    partitioning = arrow::hive_partition(!!!fields)
  )
  list(data = dplyr::select(dataset, g, row_id), path = path)
}

for (part in c("sid", "..marginplyr_arrow_dimension_1")) {
  fixture <- partition_input(part)
  input <- fixture$data
  source <- dplyr::collect(input)
  stopifnot(identical(source$g[order(source$row_id)], 9:8))
  actual <- dplyr::collect(expand_with_margins(
    input, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  ))
  control <- dplyr::collect(dplyr::union_all(
    dplyr::mutate(input, sid = 1L),
    dplyr::mutate(input, g = NA_integer_, sid = 2L)
  ))
  control <- control[order(control$sid, control$row_id), ]
  stopifnot(identical(control$g, c(9:8, NA_integer_, NA_integer_)))
  stopifnot(identical(control$sid, c(1L, 1L, 2L, 2L)))
  included <- dplyr::collect(expand_with_margins(
    input, .grouping = grouping_set(g), .margin_label = NULL, .id = "sid"
  ))
  stopifnot(identical(included$g[order(included$row_id)], 9:8))
  stopifnot(all(included$sid == 1L))
  repaired_control <- dplyr::collect(expand_with_margins(
    dplyr::compute(input), .grouping = rollup(g),
    .margin_label = NULL, .id = "sid"
  ))
  repaired_control <- repaired_control[order(repaired_control$sid, repaired_control$row_id), ]
  stopifnot(identical(repaired_control$g, control$g))
  actual <- actual[order(actual$sid, actual$row_id), ]
  ok <- identical(actual$g, control$g) && identical(actual$sid, control$sid)
  results[paste0("Arrow hidden ", part)] <- ok
  cat("Arrow hidden", part, "contract:", ok, "\n")
  print(actual)
  unlink(fixture$path, recursive = TRUE)
}

input <- sf::st_sf(
  g = c("b", "a"), rid = 1:2,
  geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
)
actual <- expand_with_margins(
  input, .by = geometry, .grouping = rollup(g), .id = "sid", .margin_label = NULL
)
control <- expand_with_margins(
  tibble::as_tibble(input), .by = geometry, .grouping = rollup(g),
  .id = "sid", .margin_label = NULL
)
stopifnot(nrow(actual) == 4L, identical(names(control), c("geometry", "g", "sid", "rid")))
stopifnot(identical(actual$g, control$g), identical(actual$sid, control$sid))
dotdot <- expand_with_margins(
  input, .by = geometry, .grouping = rollup(g), .id = "..1", .margin_label = NULL
)
stopifnot(identical(names(dotdot), c("geometry", "g", "..1", "rid")))
results["sf Margin columns"] <- identical(names(actual), names(control))
cat("sf expected:", paste(names(control), collapse = ","), "\n")
cat("sf observed:", paste(names(actual), collapse = ","), "\n")

input <- dplyr::arrange(dbplyr::lazy_frame(
  g = 1L, v = 1L, con = dbplyr::simulate_mssql()
), v)
actual <- as.character(dbplyr::sql_render(expand_with_margins(input)))
control <- as.character(dbplyr::sql_render(dplyr::mutate(input, sid = 1L)))
limited <- as.character(dbplyr::sql_render(expand_with_margins(utils::head(input, 2L))))
nested <- grepl("(?s)FROM\\s*\\(.*ORDER BY", actual, perl = TRUE)
qualified <- grepl("(?i)\\b(TOP|OFFSET)\\b", actual, perl = TRUE)
stopifnot(!grepl("(?s)FROM\\s*\\(.*ORDER BY", control, perl = TRUE))
stopifnot(grepl("TOP 2", limited, fixed = TRUE))
results["SQL Server enclosure"] <- !nested || qualified
cat("SQL Server no-grouping expansion:\n", actual, "\n", sep = "")
print(results)
if ("--assert" %in% commandArgs(trailingOnly = TRUE) && !all(results)) {
  stop("The promised behavior failed in the cases printed above.", call. = FALSE)
}
