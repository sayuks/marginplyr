# Run from the investigated source, or set MARGINPLYR_HUNT_SOURCE.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(
  Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."),
  quiet = TRUE
)

source <- sf::st_sf(
  g = "a",
  geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326),
  other = sf::st_sfc(sf::st_point(c(1, 1)), crs = 3857)
)
before <- serialize(source, NULL)
geometry <- c("geometry", "other")
cell_geometries <- function(cell) lapply(geometry, function(name) cell[[name]])
expected <- cell_geometries(source)
both_geometries <- function(result) {
  stopifnot(identical(result$g, c("a", "Total")))
  identical(lapply(result$data, cell_geometries), rep(list(expected), 2L))
}
failure <- function(expr) tryCatch(expr, error = identity)

native <- list(
  tidyr::nest(source, .by = g),
  dplyr::nest_by(source, g)
)
stopifnot(all(vapply(native, function(result) {
  identical(cell_geometries(result$data[[1L]]), expected)
}, logical(1))))

detail <- dplyr::mutate(source, source_key = g, set = 1L)
total <- dplyr::mutate(detail, g = "Total", source_key = NA_character_, set = 2L)
combined <- dplyr::union_all(detail, total)
stopifnot(
  identical(combined$g, c("a", "Total")),
  identical(combined$set, c(1L, 2L)),
  identical(sf::st_geometry(combined), rep(source$geometry, 2L)),
  identical(combined$other, rep(source$other, 2L))
)
rename <- failure(dplyr::rename(
  detail, first = g, second = geometry, third = other,
  fourth = source_key, fifth = set
))
cat("Full native rename:", conditionMessage(rename), "\n")

matches <- vapply(
  list(ordinary = nest_with_margins, rowwise = nest_by_with_margins),
  function(verb) {
    typed <- verb(dplyr::as_tibble(source), .grouping = rollup(g),
                  .sort = "last")
    id_alias <- verb(source, .grouping = rollup(g), .sort = "last", .id = "..1")
    single <- verb(source, .grouping = grouping_set(g))
    one_geometry <- verb(source[c("g", "geometry")], .grouping = rollup(g),
                         .sort = "last")
    stopifnot(
      both_geometries(typed), both_geometries(id_alias),
      identical(id_alias[["..1"]], c(1L, 2L)),
      identical(cell_geometries(single$data[[1L]]), expected),
      identical(lapply(one_geometry$data, function(cell) cell$geometry),
                rep(list(source$geometry), 2L))
    )
    result <- failure(verb(source, .grouping = rollup(g), .sort = "last"))
    if (inherits(result, "error")) {
      cat("Margin rollup:", conditionMessage(result), "\n")
      return(FALSE)
    }
    both_geometries(result)
  },
  logical(1)
)
stopifnot(identical(serialize(source, NULL), before))
print(matches)
print(sessionInfo())
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(matches))
}
