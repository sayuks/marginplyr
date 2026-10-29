test_that("expansion places active geometry among leading Margin columns", {
  skip_if_suggest_absent("sf")
  for (geometry in c("geometry", "shape")) {
    source <- sf::st_sf(stats::setNames(list(
      c("b", "a"), 1:2,
      sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
    ), c("g", "rid", geometry)))
    for (position in seq_len(3L)) {
      columns <- append(c("g", "rid"), geometry, after = position - 1L)
      positioned <- dplyr::relocate(source, dplyr::all_of(columns))
      for (empty in c(FALSE, TRUE)) {
        input <- if (empty) positioned[integer(), ] else positioned
        before <- serialize(input, NULL)
        for (fixed in c(FALSE, TRUE)) {
          by <- if (fixed) geometry else "g"
          spec <- if (fixed) {
            rollup(g)
          } else {
            grouping_set(dplyr::all_of(geometry))
          }
          leading <- if (fixed) c(geometry, "g") else c("g", geometry)
          for (id in list(NULL, "sid", "..1")) {
            for (sort in c("none", "first", "last")) {
              # sf selection can move active geometry; the column-frame control
              # observes the package's placement contract under ADR 0016.
              control <- expand_with_margins(
                dplyr::as_tibble(input), .by = dplyr::all_of(by),
                .grouping = spec, .id = id, .sort = sort, .margin_label = NULL
              )
              result <- expand_with_margins(
                input, .by = dplyr::all_of(by), .grouping = spec,
                .id = id, .sort = sort, .margin_label = NULL
              )
              expect_identical(names(result), c(leading, id, "rid"))
              expect_identical(
                nrow(result), if (empty) 0L else if (fixed) 4L else 2L
              )
              expect_identical(result$rid, control$rid)
              expect_identical(result$g, control$g)
              if (!is.null(id)) expect_identical(result[[id]], control[[id]])
              expect_identical(
                result[[geometry]], input[[geometry]][result$rid]
              )
              if (inherits(result, "sf")) {
                expect_identical(sf::st_geometry(result), result[[geometry]])
                expect_setequal(
                  names(sf::st_agr(result)), setdiff(names(result), geometry)
                )
              }
              expect_identical(serialize(input, NULL), before)
            }
          }
        }
      }
    }
  }
})
