test_that("Total shares match active geometry fixed keys", {
  skip_if_suggest_absent("sf")
  input <- sf::st_sf(
    g = c("a", "b"), value = c(1, 3),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(0, 0)))
  )
  result <- summarize_with_margins(
    input, total = sum(value), p = share_of_total(total),
    .by = geometry, .grouping = rollup(g), .margin_label = NULL
  )
  expect_identical(result$total, c(1, 3, 4))
  expect_identical(result$p, c(0.25, 0.75, 1))
})

test_that("Parent shares distinguish geometry partitions and subtotal levels", {
  skip_if_suggest_absent("sf")
  input <- sf::st_sf(
    g = rep(c("a", "a", "b", "b"), 2),
    h = rep(c("x", "y"), 4), value = c(1, 3, 2, 6, 2, 6, 3, 9),
    geometry = sf::st_sfc(lapply(rep(0:1, each = 4), function(x) {
      sf::st_point(c(x, x))
    }))
  )
  result <- summarize_with_margins(
    input, total = sum(value), p = share_of_parent(total),
    .by = geometry, .grouping = rollup(g, h), .margin_label = NULL, .id = "sid"
  )
  result <- dplyr::arrange(dplyr::as_tibble(result), geometry, sid, g, h)
  expect_identical(result$total, c(1, 3, 2, 6, 4, 8, 12,
                                   2, 6, 3, 9, 8, 12, 20))
  expect_equal(result$p, c(0.25, 0.75, 0.25, 0.75, 1 / 3, 2 / 3, 1,
                           0.25, 0.75, 0.25, 0.75, 0.4, 0.6, 1))
})

test_that("geometry-key shares retain fixed partitions across Margin orders", {
  skip_if_suggest_absent("sf")
  for (geometry in c("geometry", "shape")) {
    source <- sf::st_sf(stats::setNames(list(
      rep(c("a", "b"), 4), rep(c("east", "west"), each = 2, times = 2),
      c(1, 3, 2, 6, 3, 9, 4, 12),
      sf::st_sfc(lapply(rep(0:1, each = 4), function(x) {
        sf::st_point(c(x, x))
      }))
    ), c("g", "region", "value", geometry)))
    for (empty in c(FALSE, TRUE)) {
      source_rows <- if (empty) source[integer(), ] else source
      for (multiple in c(FALSE, TRUE)) {
        fixed <- c(if (multiple) "region", geometry)
        for (implicit in c(FALSE, TRUE)) {
          input <- source_rows
          if (implicit) {
            input <- dplyr::group_by(input, dplyr::across(dplyr::all_of(fixed)))
          }
          before <- serialize(input, NULL)
          for (sort in c("none", "first", "last")) {
            ordinary <- summarize_with_margins(
              source_rows, total = sum(value), .by = dplyr::all_of(fixed),
              .grouping = rollup(g), .sort = sort, .margin_label = NULL,
              .id = "sid"
            )
            control <- summarize_with_margins(
              dplyr::as_tibble(source_rows), total = sum(value),
              p = share_of_parent(total), t = share_of_total(total),
              .by = dplyr::all_of(fixed), .grouping = rollup(g), .sort = sort,
              .margin_label = NULL, .id = "sid"
            )
            shares <- rlang::exprs(
              p = share_of_parent(total), t = share_of_total(total)
            )
            result <- if (implicit) {
              summarize_with_margins(
                input, total = sum(value), !!!shares,
                .grouping = rollup(g), .sort = sort,
                .margin_label = NULL, .id = "sid"
              )
            } else {
              summarize_with_margins(
                input, total = sum(value), !!!shares,
                .by = dplyr::all_of(fixed), .grouping = rollup(g), .sort = sort,
                .margin_label = NULL, .id = "sid"
              )
            }
            expect_identical(
              names(result), c(fixed, "g", "sid", "total", "p", "t")
            )
            expect_identical(dplyr::group_vars(result), character())
            for (name in names(control)) {
              if (name == geometry) {
                expect_identical(
                  lapply(result[[name]], identity),
                  lapply(control[[name]], identity)
                )
              } else {
                expect_identical(result[[name]], control[[name]])
              }
            }
            for (name in names(ordinary)) {
              if (name == geometry) {
                expect_identical(
                  lapply(result[[name]], identity),
                  lapply(ordinary[[name]], identity)
                )
              } else {
                expect_identical(result[[name]], ordinary[[name]])
              }
            }
            expected <- if (empty) numeric() else c(0.25, 0.75, 1)[
              match(ifelse(result$sid == 2L, "root", result$g),
                    c("a", "b", "root"))
            ]
            expect_identical(result$p, expected)
            expect_identical(result$t, expected)
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

test_that("Grand-total-only geometry-key Total shares need no join", {
  skip_if_suggest_absent("sf")
  source <- sf::st_sf(
    value = c(1, 3),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(0, 0)))
  )
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) source[integer(), ] else source
    before <- serialize(input, NULL)
    result <- summarize_with_margins(
      input, total = sum(value), t = share_of_total(total),
      .by = geometry, .grouping = grouping_set()
    )
    expect_identical(result$total, if (empty) numeric() else 4)
    expect_identical(result$t, if (empty) numeric() else 1)
    expect_identical(serialize(input, NULL), before)
  }
})
