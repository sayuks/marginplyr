test_that("summary restores active geometry fixed keys across Margin orders", {
  skip_if_suggest_absent("sf")
  for (geometry in c("geometry", "shape")) {
    source <- sf::st_sf(stats::setNames(list(
      c("b", "b", "a", "a"), c("z", "a", "z", "a"), c(1, 3, 5, 7),
      sf::st_sfc(
        sf::st_point(c(1, 1)), sf::st_point(c(0, 0)),
        sf::st_point(c(1, 1)), sf::st_point(c(0, 0))
      )
    ), c("g", "region", "value", geometry)))
    for (empty in c(FALSE, TRUE)) {
      rows <- if (empty) integer() else seq_len(nrow(source))
      for (multiple in c(FALSE, TRUE)) {
        fixed <- c(if (multiple) "region", geometry)
        for (implicit in c(FALSE, TRUE)) {
          input <- source[rows, ]
          if (implicit) {
            input <- dplyr::group_by(input, dplyr::across(dplyr::all_of(fixed)))
          }
          before <- serialize(input, NULL)
          # An active geometry key is admitted by sf's own summary, including
          # zero rows. Its non-geometry-key restrictions are a separate seam.
          ordinary <- dplyr::summarize(
            source[rows, ], total = sum(value), .by = dplyr::all_of(fixed)
          )
          expect_identical(
            sort(ordinary$total), if (empty) numeric() else c(6, 10)
          )
          for (sort in c("none", "first", "last")) {
            control <- summarize_with_margins(
              dplyr::as_tibble(source[rows, ]), total = sum(value),
              .by = dplyr::all_of(fixed), .grouping = rollup(g), .sort = sort,
              .margin_label = NULL, .id = "sid"
            )
            result <- if (implicit) {
              summarize_with_margins(
                input, total = sum(value), .grouping = rollup(g), .sort = sort,
                .margin_label = NULL, .id = "sid"
              )
            } else {
              summarize_with_margins(
                input, total = sum(value), .by = dplyr::all_of(fixed),
                .grouping = rollup(g), .sort = sort,
                .margin_label = NULL, .id = "sid"
              )
            }
            expect_identical(names(result), c(fixed, "g", "sid", "total"))
            expect_identical(dplyr::group_vars(result), character())
            expect_identical(result$total, control$total)
            expect_identical(result$g, control$g)
            expect_identical(result$sid, control$sid)
            for (key in fixed) expect_identical(result[[key]], control[[key]])
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
})

test_that("geometry-key summaries retain supported special output names", {
  skip_if_suggest_absent("sf")
  source <- sf::st_as_sf(
    data.frame(value = c(1, 3), x = 1:2, y = 3:4),
    coords = c("x", "y")
  )
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) source[integer(), ] else source
    before <- serialize(input, NULL)
    ordinary <- dplyr::summarize(input, total = sum(value), .by = geometry)
    for (name in c("total", "..1")) {
      result <- summarize_with_margins(
        input, !!name := sum(value), .by = geometry
      )
      expect_identical(names(result), c("geometry", name))
      expect_identical(result[[name]], if (empty) numeric() else c(1, 3))
      expect_identical(result$geometry, ordinary$geometry)
      if (inherits(result, "sf")) {
        expect_identical(sf::st_geometry(result), result$geometry)
        expect_setequal(names(sf::st_agr(result)), name)
      }
      expect_identical(serialize(input, NULL), before)
    }
  }
})
