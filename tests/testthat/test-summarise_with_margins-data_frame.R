test_that(".margins works", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  data <- get_data_dummy()

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::ungroup(
      dplyr::summarize(
        .data = data,
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, g1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)",
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, g1, g2),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)",
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, g1, g2, g3),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g1, g2, g3)
  expected <- dplyr::arrange(.data = expected, g1, g2, g3)

  expect_identical(actual, expected)
})

test_that(".without_all works", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  data <- get_data_dummy()

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .without_all = year,
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)",
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)",
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)",
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, g3),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE)
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g1, g2, g3)
  expected <- dplyr::arrange(.data = expected, year, g1, g2, g3)

  expect_identical(actual, expected)
})

test_that(".with_all works", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  data <- get_data_dummy()

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .without_all = year,
    .with_all = c(h1, k1),
    .check_margin_name = TRUE
  )

  expected <- list(
    # all g1, g2, g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)",
        h1 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)",
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, h1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, h1, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g1 = "(all)",
        g2 = "(all)",
        g3 = "(all)"
      )
    ),
    # by g1, all g2, g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)",
        h1 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)",
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, h1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, h1, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = "(all)",
        g3 = "(all)"
      )
    ),
    # by g1, g2, all g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)",
        h1 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)",
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, h1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, h1, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "(all)"
      )
    ),
    # by g1, g2, g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, g3),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        h1 = "(all)",
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, g3, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, g3, h1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        k1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, year, g1, g2, g3, h1, k1),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE)
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g1, g2, g3, h1, k1)
  expected <- dplyr::arrange(.data = expected, year, g1, g2, g3, h1, k1)

  expect_identical(actual, expected)
})

test_that(".margin_name basic (non-NA)", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  data <- get_data_dummy()

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = g3,
    .margin_name = "total",
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::ungroup(
      dplyr::summarize(
        .data = data,
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g3 = "total"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, g3),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE)
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g3)
  expected <- dplyr::arrange(.data = expected, g3)

  expect_identical(actual, expected)
})

test_that(".margin_name basic (NA)", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  data <- get_data_dummy()

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = g2,
    .margin_name = NA_character_,
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::ungroup(
      dplyr::summarize(
        .data = data,
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        g2 = NA_character_
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(data, g2),
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE)
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g2)
  expected <- dplyr::arrange(.data = expected, g2)

  expect_identical(actual, expected)
})

test_that(".margin_name conflict error", {
  data <- get_data_dummy()

  expect_error(
    summarize_with_margins(
      .data = data,
      .margins = g3,
      .margin_name = "RD360",
      .check_margin_name = TRUE
    ),
    "not allowed as a margin name"
  )


  # since NA_character_ is used in g3, it can not be used as margin name
  expect_error(
    summarize_with_margins(
      .data = data,
      .margins = g3,
      .margin_name = NA_character_,
      .check_margin_name = TRUE
    ),
    "not allowed as a margin name"
  )
})

test_that("Can use the margin variable before calculating", {
  old <- options(dplyr.summarise.inform = FALSE)
  on.exit(
    options(dplyr.summarise.inform = old$dplyr.summarise.inform),
    add = TRUE
  )

  # for easier debugging, extract the sample that has more than one row
  data <- dplyr::filter(get_data_dummy(), id == "108")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    g3s = stringr::str_flatten(g3, collapse = "/"),
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .check_margin_name = TRUE
  )

  expected <- list(
    # all g2, g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
        g2 = "(all)",
        g3 = "(all)",
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year, h1),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
        g2 = "(all)",
        g3 = "(all)"
      )
    ),
    # all g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year, g2),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
        g3 = "(all)",
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year, g2, h1),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
        g3 = "(all)"
      )
    ),
    # by g2, g3
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year, g2, g3),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
        h1 = "(all)"
      )
    ),
    dplyr::ungroup(
      dplyr::summarize(
        .data = dplyr::group_by(.data = data, year, g2, g3, h1),
        n = dplyr::n(),
        g3s = stringr::str_flatten(g3, collapse = "/"),
      )
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g2, g3, h1)
  expected <- dplyr::arrange(.data = expected, year, g2, g3, h1)

  expect_identical(actual, expected)
})
