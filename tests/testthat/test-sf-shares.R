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

test_that("shares preserve requested summary geometry columns", {
  skip_if_suggest_absent("sf")
  input <- sf::st_sf(
    g = c("a", "b"), value = c(1, 3),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 0)))
  )
  result <- summarize_with_margins(
    input, total = sum(value), geometry = sf::st_union(geometry),
    p = share_of_parent(total), t = share_of_total(total),
    .grouping = rollup(g)
  )
  expect_identical(names(result), c("g", "total", "geometry", "p", "t"))
  expect_identical(result$p, c(0.25, 0.75, 1))
  expect_identical(result$t, c(0.25, 0.75, 1))
})

test_that("sf summary shares retain geometry across fixed keys and orders", {
  skip_if_suggest_absent("sf")
  for (geometry in c("geometry", "shape")) {
    source <- sf::st_sf(stats::setNames(list(
      rep(c("a", "a", "b", "b"), 2), rep(c("x", "y"), 4),
      rep(c("east", "west"), each = 4), c(1, 3, 2, 6, 2, 6, 4, 12),
      sf::st_sfc(lapply(seq_len(8), function(x) sf::st_point(c(x, 0))))
    ), c("g", "h", "region", "value", geometry)))
    ordinary <- rlang::exprs(
      total = sum(value), !!geometry := sf::st_union(.data[[!!geometry]])
    )
    requests <- list(
      rlang::exprs(p = share_of_parent(total)),
      rlang::exprs(t = share_of_total(total)),
      rlang::exprs(p = share_of_parent(total), t = share_of_total(total))
    )
    for (fixed_mode in c("absent", "explicit", "implicit")) {
      fixed <- if (fixed_mode == "absent") character() else "region"
      input <- if (fixed_mode == "implicit") {
        dplyr::group_by(source, region)
      } else {
        source
      }
      before <- serialize(input, NULL)
      detail <- dplyr::summarise(
        source, !!!ordinary, rows = dplyr::n(),
        .by = dplyr::all_of(c(fixed, "g", "h"))
      )
      subtotal <- dplyr::summarise(
        source, !!!ordinary, rows = dplyr::n(),
        .by = dplyr::all_of(c(fixed, "g"))
      )
      root <- dplyr::summarise(
        source, !!!ordinary, rows = dplyr::n(), .by = dplyr::all_of(fixed)
      )
      control <- dplyr::bind_rows(
        dplyr::mutate(detail, sid = 1L),
        dplyr::mutate(subtotal, h = NA_character_, sid = 2L),
        dplyr::mutate(root, g = NA_character_, h = NA_character_, sid = 3L)
      )
      control <- dplyr::arrange(
        dplyr::as_tibble(control), dplyr::across(dplyr::all_of(fixed)),
        sid, g, h
      )
      for (sort in c("none", "first", "last")) {
        for (shares in requests) {
          result <- if (fixed_mode == "implicit") {
            summarize_with_margins(
              input, !!!ordinary, !!!shares, rows = dplyr::n(),
              .grouping = rollup(g, h), .sort = sort,
              .margin_label = NULL, .id = "sid"
            )
          } else {
            summarize_with_margins(
              input, !!!ordinary, !!!shares, rows = dplyr::n(),
              .by = dplyr::all_of(fixed), .grouping = rollup(g, h),
              .sort = sort, .margin_label = NULL, .id = "sid"
            )
          }
          expected_names <- c(
            fixed, "g", "h", "sid", "total", geometry, names(shares), "rows"
          )
          expect_identical(names(result), expected_names)
          tibble_control <- summarize_with_margins(
            dplyr::as_tibble(source), !!!ordinary, !!!shares, rows = dplyr::n(),
            .by = dplyr::all_of(fixed), .grouping = rollup(g, h),
            .sort = sort, .margin_label = NULL, .id = "sid"
          )
          expect_identical(names(tibble_control), expected_names)
          if (inherits(result, "sf")) {
            expect_identical(sf::st_geometry(result), result[[geometry]])
            expect_setequal(
              names(sf::st_agr(result)), setdiff(names(result), geometry)
            )
          }
          aligned <- dplyr::arrange(
            dplyr::as_tibble(result), dplyr::across(dplyr::all_of(fixed)),
            sid, g, h
          )
          for (name in names(control)) {
            if (name == geometry) {
              expect_identical(
                lapply(aligned[[name]], identity),
                lapply(control[[name]], identity)
              )
            } else {
              expect_identical(aligned[[name]], control[[name]])
            }
          }
          for (name in names(result)) {
            if (name == geometry) {
              expect_identical(
                lapply(result[[name]], identity),
                lapply(tibble_control[[name]], identity)
              )
            } else {
              expect_identical(result[[name]], tibble_control[[name]])
            }
          }
          repeats <- if (length(fixed)) 2L else 1L
          if ("p" %in% names(shares)) {
            expect_equal(aligned$p, rep(c(0.25, 0.75, 0.25, 0.75,
                                          1 / 3, 2 / 3, 1), repeats))
          }
          if ("t" %in% names(shares)) {
            expect_equal(aligned$t, rep(c(1 / 12, 0.25, 1 / 6, 0.5,
                                          1 / 3, 2 / 3, 1), repeats))
          }
          expect_identical(dplyr::group_vars(result), character())
          expect_identical(serialize(input, NULL), before)
        }
      }
    }
  }
})

test_that("summary-geometry shares keep legal suffixed public names", {
  skip_if_suggest_absent("sf")
  input <- sf::st_sf(
    g = c("a", "b"), value = c(1, 3),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 0)))
  )
  before <- serialize(input, NULL)
  for (kind in c("parent", "total", "both")) {
    for (role in c("source", "share", "ordinary", "id")) {
      source <- if (role == "source") "geometry.x" else "total"
      share <- if (role == "share") "geometry.x" else "p"
      ordinary_name <- if (role == "ordinary") "geometry.x" else "rows"
      id <- if (role == "id") "geometry.x" else "sid"
      summaries <- rlang::exprs(
        !!source := sum(value), geometry = sf::st_union(geometry)
      )
      shares <- list()
      if (kind %in% c("parent", "both")) {
        shares[[share]] <- rlang::expr(share_of_parent(!!rlang::sym(source)))
      }
      if (kind %in% c("total", "both")) {
        output <- if (kind == "both") "t" else share
        shares[[output]] <- rlang::expr(share_of_total(!!rlang::sym(source)))
      }
      summaries <- c(
        summaries, shares,
        stats::setNames(rlang::exprs(dplyr::n()), ordinary_name)
      )
      result <- summarize_with_margins(
        input, !!!summaries, .grouping = rollup(g), .id = id
      )
      control <- summarize_with_margins(
        dplyr::as_tibble(input), !!!summaries, .grouping = rollup(g), .id = id
      )
      expect_identical(
        names(result),
        c("g", id, source, "geometry", names(shares), ordinary_name)
      )
      expect_identical(names(result), names(control))
      for (name in names(result)) {
        if (name == "geometry") {
          expect_identical(
            lapply(result[[name]], identity), lapply(control[[name]], identity)
          )
        } else {
          expect_identical(result[[name]], control[[name]])
        }
      }
      expect_identical(result[[source]], c(1, 3, 4))
      for (name in names(shares)) {
        expect_identical(result[[name]], c(0.25, 0.75, 1))
      }
      if (inherits(result, "sf")) {
        expect_identical(sf::st_geometry(result), result$geometry)
        expect_setequal(
          names(sf::st_agr(result)), setdiff(names(result), "geometry")
        )
      }
      expect_identical(serialize(input, NULL), before)
    }
  }
})

test_that("unnamed summaries retain sf geometry and share expression order", {
  skip_if_suggest_absent("sf")
  input <- sf::st_sf(
    g = c("a", "b"), value = c(1, 3),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 0)))
  )
  extras <- list(
    rlang::exprs(sum(value) + 1),
    rlang::exprs(tibble::tibble(extra = max(value))),
    rlang::exprs(dplyr::across(value, max, .names = "peak"))
  )
  for (extra in extras) {
    result <- summarize_with_margins(
      input, total = sum(value), geometry = sf::st_union(geometry),
      !!!extra, p = share_of_parent(total), t = share_of_total(total),
      .grouping = rollup(g), .sort = "first"
    )
    control <- summarize_with_margins(
      dplyr::as_tibble(input), total = sum(value),
      geometry = sf::st_union(geometry), !!!extra,
      p = share_of_parent(total), t = share_of_total(total),
      .grouping = rollup(g), .sort = "first"
    )
    expect_identical(names(result), names(control))
    expect_identical(names(result)[1:3], c("g", "total", "geometry"))
    for (name in names(result)) {
      if (name == "geometry") {
        expect_identical(
          lapply(result[[name]], identity), lapply(control[[name]], identity)
        )
      } else {
        expect_identical(result[[name]], control[[name]])
      }
    }
  }
})
