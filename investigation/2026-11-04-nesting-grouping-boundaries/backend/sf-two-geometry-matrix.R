# Compare ordinary sf input with a plain tibble holding its identical columns.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(
  Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."),
  quiet = TRUE
)

point <- function(x) sf::st_point(c(x, 2 * x))
raw <- tibble::tibble(
  g = c("b", "a", "b"),
  row = 1:3,
  geometry = sf::st_sfc(point(1), point(2), point(3), crs = 4326),
  other = sf::st_sfc(point(4), point(5), point(6), crs = 3857)
)
placements <- list(
  single = c("g", "row", "geometry"),
  geometries_last = c("g", "row", "geometry", "other"),
  geometries_first = c("geometry", "other", "g", "row")
)
matrix_cases <- expand.grid(
  verb = c("nest", "rowwise"),
  placement = names(placements),
  size = c(0L, 3L),
  plan = c("single", "rollup", "reversed"),
  id = c("", "set", "..1"),
  keep = c(FALSE, TRUE),
  sort = c("none", "first", "last"),
  stringsAsFactors = FALSE
)
specs <- list(
  single = grouping_set(g),
  rollup = rollup(g),
  reversed = grouping_sets(grouping_set(), grouping_set(g))
)

run_case <- function(verb, placement, size, plan, id, keep, sort) {
  columns <- raw[seq_len(size), placements[[placement]]]
  source <- sf::st_as_sf(columns, sf_column_name = "geometry")
  before <- serialize(source, NULL)
  fun <- if (verb == "nest") nest_with_margins else nest_by_with_margins
  run <- function(data) {
    fun(data, .grouping = specs[[plan]], .keep = keep, .sort = sort,
        .id = if (nzchar(id)) id else NULL)
  }
  canonical <- function(result) {
    groups <- if (sort == "none") order(result$g) else seq_len(nrow(result))
    outer <- tibble::as_tibble(result[groups, , drop = FALSE],
                               .name_repair = "minimal")
    outer$data <- lapply(outer$data, function(cell) {
      as.list(cell[order(cell$row), , drop = FALSE])
    })
    as.list(outer)
  }

  expected <- canonical(run(columns))
  actual <- tryCatch(run(source), error = identity)
  stopifnot(identical(serialize(source, NULL), before))
  if (inherits(actual, "error")) return(conditionMessage(actual))
  if (identical(expected, canonical(actual))) "match" else "value difference"
}
matrix_cases$outcome <- mapply(
  run_case, matrix_cases$verb, matrix_cases$placement, matrix_cases$size,
  matrix_cases$plan, matrix_cases$id, matrix_cases$keep, matrix_cases$sort
)
stopifnot(nrow(matrix_cases) == 648L)
cat("Calls:", nrow(matrix_cases), "\n")
print(with(matrix_cases, table(placement, plan, id, outcome == "match")))
print(with(matrix_cases, table(size, verb, outcome == "match")))
print(table(matrix_cases$outcome))
path <- commandArgs(trailingOnly = TRUE)
if (length(path) > 0L) utils::write.csv(matrix_cases, path[[1L]], row.names = FALSE)
