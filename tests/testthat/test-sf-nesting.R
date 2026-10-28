test_that("nesting folds sf source cells with and without fixed keys", {
  skip_if_suggest_absent("sf")
  source <- sf::st_as_sf(
    data.frame(g = c("b", "a"), x = 1:2, y = 3:4),
    coords = c("x", "y")
  )
  before <- serialize(source, NULL)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (fixed in c(FALSE, TRUE)) {
      result <- if (fixed) verb(source, .by = g) else verb(source)
      expect_identical(names(result), if (fixed) c("g", "data") else "data")
      expect_identical(nrow(result), if (fixed) 2L else 1L)
      expect_identical(
        vapply(result$data, nrow, integer(1)),
        if (fixed) c(1L, 1L) else 2L
      )
      for (i in seq_along(result$data)) {
        cell <- result$data[[i]]
        rows <- if (fixed) which(source$g == result$g[[i]]) else 1:2
        expect_identical(cell$geometry, source$geometry[rows])
        if (!fixed) expect_identical(cell$g, source$g)
      }
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("sf rollup cells retain original keys and geometry in Margin order", {
  skip_if_suggest_absent("sf")
  source <- sf::st_as_sf(
    data.frame(g = c("b", "a"), x = 1:2, y = 3:4),
    coords = c("x", "y")
  )
  before <- serialize(source, NULL)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (id in c("set", "..1")) {
      result <- verb(
        source, .grouping = rollup(g), .keep = TRUE,
        .id = id, .sort = "last"
      )
      cells <- result$data
      expect_identical(names(result), c("g", id, "data"))
      expect_identical(result$g, c("a", "b", "Total"))
      expect_identical(result[[id]], c(1L, 1L, 2L))
      expect_identical(vapply(cells, nrow, integer(1)), c(1L, 1L, 2L))
      for (i in seq_along(cells)) {
        cell <- cells[[i]]
        rows <- list(2L, 1L, 1:2)[[i]]
        expect_identical(names(cell), c("g", "geometry"))
        expect_identical(cell$g, source$g[rows])
        expect_identical(cell$geometry, source$geometry[rows])
      }
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("Margin-column placement keeps retained sf metadata valid", {
  skip_if_suggest_absent("sf")
  source <- sf::st_as_sf(
    data.frame(g = c("b", "a"), x = 1:2, y = 3:4),
    coords = c("x", "y")
  )
  source <- dplyr::relocate(source, geometry)
  before <- serialize(source, NULL)
  for (id in c("set", "..1")) {
    result <- expand_with_margins(source, .by = g, .id = id)

    expect_identical(names(result), c("g", id, "geometry"))
    expect_identical(result$geometry, source$geometry)
    if (inherits(result, "sf")) {
      expect_identical(sf::st_geometry(result), source$geometry)
      expect_named(sf::st_agr(result), c("g", id))
    }
    expect_identical(serialize(source, NULL), before)
  }
})

test_that("empty sf nesting keeps each verb's source-cell row count", {
  skip_if_suggest_absent("sf")
  source <- sf::st_as_sf(
    data.frame(g = "a", x = 1, y = 3),
    coords = c("x", "y")
  )
  source <- source[0, ]
  result <- nest_with_margins(source, .id = "set")
  expect_identical(names(result), c("set", "data"))
  expect_identical(nrow(result), 0L)

  result <- nest_by_with_margins(source, .id = "set")
  expect_identical(names(result), c("set", "data"))
  expect_identical(result$set, 1L)
  expect_s3_class(result, "rowwise_df")
  expect_identical(dplyr::group_vars(result), "set")
  expect_identical(nrow(result$data[[1L]]), 0L)
  expect_identical(result$data[[1L]]$g, source$g)
  expect_identical(result$data[[1L]]$geometry, source$geometry)
})
