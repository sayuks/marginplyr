expect_local_selection_shares <- function(data) {
  for (multiple in c(FALSE, TRUE)) {
    grouping <- if (multiple) rollup(dplyr::all_of("g")) else grouping_set()
    helpers <- c("share_of_total", if (multiple) "share_of_parent")
    for (helper in helpers) {
      for (callback in c(FALSE, TRUE)) {
        run <- function(shared) {
          state <- new.env(parent = emptyenv())
          state$contexts <- integer()
          state$choose <- function() {
            state$contexts <- c(state$contexts, dplyr::n())
            if (dplyr::n() == 0L) "x" else "y"
          }
          selection <- if (callback) {
            quote(dplyr::all_of(state$choose()))
          } else {
            quote(dplyr::all_of({
              state$contexts <- c(state$contexts, dplyr::n())
              if (dplyr::n() == 0L) "x" else "y"
            }))
          }
          ordinary <- list(
            rlang::expr(dplyr::across(!!selection, sum, .names = "unused")),
            total = quote(sum(y))
          )
          if (shared) {
            result <- summarize_with_margins(
              data, !!!ordinary,
              dplyr::across(dplyr::starts_with("total"), !!rlang::sym(helper),
                            .names = "share"),
              .grouping = grouping
            )
          } else if (multiple) {
            result <- dplyr::bind_rows(
              dplyr::summarise(data, !!!ordinary, .by = dplyr::all_of("g")),
              dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
            )
          } else {
            result <- dplyr::summarise(data, !!!ordinary)
          }
          list(result = as.data.frame(result), contexts = state$contexts)
        }
        control <- run(FALSE)
        actual <- run(TRUE)
        expect_identical(actual$contexts, control$contexts)
        expect_length(actual$contexts, if (multiple) 2L else 1L)
        expect_identical(actual$result[names(control$result)], control$result)
        expect_identical(actual$result$unused,
                         if (multiple) c(3L, 3L, 6L) else 6L)
        expect_equal(actual$result$share,
                     if (multiple) c(11 / 18, 7 / 18, 1) else 1)
        expect_type(actual$result$share, "double")
      }
    }
  }
}

test_that("independent selection shares preserve local selector evaluation", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  expect_local_selection_shares(data)
  expect_local_selection_shares(tibble::as_tibble(data))
  expect_local_selection_shares(structure(
    data, class = c("local_frame", "data.frame")
  ))
})

test_that("independent selection shares preserve eager data.table selectors", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7)
  )
  expect_local_selection_shares(data)
})

test_that("independent selection shares preserve geometry-key selectors", {
  skip_if_suggest_absent("sf")
  data <- sf::st_as_sf(data.frame(
    g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7), lon = 0, lat = 0
  ), coords = c("lon", "lat"))
  contexts <- integer()
  choose <- function() {
    contexts <<- c(contexts, dplyr::n())
    "x"
  }
  ordinary <- rlang::quos(
    dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
    total = sum(y)
  )
  control <- dplyr::summarise(data, !!!ordinary, .by = geometry)
  expected_contexts <- contexts
  contexts <- integer()
  result <- summarize_with_margins(
    data, !!!ordinary,
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"),
    .by = geometry
  )
  expect_identical(contexts, expected_contexts)
  expect_length(contexts, 1L)
  expect_identical(result$unused, control$unused)
  expect_identical(result$total, control$total)
  expect_identical(result$geometry, control$geometry)
  expect_identical(result$share, 1)
})

test_that("name patterns leave a stateful first selector in each branch", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (selection in rlang::exprs(
    dplyr::starts_with("total"), dplyr::ends_with("otal"),
    dplyr::contains("ota"), dplyr::matches("^total$")
  )) {
    calls <- 0L
    choose <- function() {
      calls <<- calls + 1L
      if (calls == 1L) "x" else "y"
    }
    result <- summarize_with_margins(
      data, dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
      total = sum(x),
      dplyr::across(!!selection, share_of_total, .names = "share"),
      .grouping = rollup(g)
    )
    expect_identical(calls, 2L)
    expect_identical(result$unused, c(1, 3, 40))
    expect_identical(result$total, c(1, 3, 4))
    expect_identical(result$share, c(0.25, 0.75, 1))
  }
})

test_that("selection shares retain matching across sources and naming", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (dynamic in c(FALSE, TRUE)) {
    for (helper in c("share_of_total", "share_of_parent")) {
      reads <- 0L
      template <- function() {
        reads <<- reads + 1L
        "total_{.col}"
      }
      naming <- if (dynamic) quote(template()) else "total_{.col}"
      result <- summarize_with_margins(
        data, dplyr::across(c(x, y), sum, .names = !!naming),
        dplyr::across(dplyr::starts_with("total"), !!rlang::sym(helper),
                      .names = "{.col}_share"),
        .grouping = rollup(g)
      )
      expect_identical(reads, as.integer(dynamic))
      expect_identical(result$total_x, c(1, 3, 4))
      expect_identical(result$total_y, c(10, 30, 40))
      expect_identical(result$total_x_share, c(0.25, 0.75, 1))
      expect_identical(result$total_y_share, c(0.25, 0.75, 1))
    }
  }
})

test_that("independent selection shares retain source and collision guards", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (helper in c("share_of_total", "share_of_parent")) {
    share <- rlang::expr(dplyr::across(
      dplyr::starts_with("total"), !!rlang::sym(helper), .names = "share"
    ))
    for (source in rlang::exprs(as.character(sum(x)), c(1, 2))) {
      expect_error(summarize_with_margins(
        data, dplyr::across(x, sum, .names = "unused"),
        total = !!source, !!share, .grouping = rollup(g)
      ), "scalar", class = "marginplyr_error")
    }
    packed <- summarize_with_margins(
      data, total = dplyr::across(x, sum), !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(packed))
    expect_s3_class(packed$total, "data.frame")
    expect_error(summarize_with_margins(
      data, dplyr::across(
        dplyr::all_of(if (dplyr::n() == 0L) "x" else character()),
        sum, .names = "share"
      ),
      total = sum(x), !!share, .grouping = rollup(g)
    ), "conflicts with", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(
        dplyr::all_of(if (dplyr::n() == 0L) "x" else character()),
        sum, .names = "unused"
      ),
      total = unused + 1, !!share, .grouping = rollup(g)
    ), "earlier summary alias", class = "marginplyr_error")
    dependent <- summarize_with_margins(
      data, dplyr::across(x, sum, .names = "unused"),
      total = unused + 1, !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(dependent))
    duplicated <- summarize_with_margins(
      data, dplyr::across(x, sum, .names = "total"),
      total = sum(x), !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(duplicated))

    explicit <- rlang::expr(dplyr::across(
      c(dplyr::starts_with("total"), total), !!rlang::sym(helper),
      .names = "share"
    ))
    expect_error(summarize_with_margins(
      data, total = dplyr::across(x, sum), !!explicit,
      .grouping = rollup(g)
    ), "not available", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, sum, .names = "unused"),
      total = unused + 1, !!explicit, .grouping = rollup(g)
    ), "not available", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, sum, .names = "total"),
      total = sum(x), !!explicit, .grouping = rollup(g)
    ), "more than once", class = "marginplyr_error")
  }
})

test_that("pattern prediction respects caller bindings and explicit vars", {
  data <- data.frame(x = c(1, 3), y = c(10, 30))
  for (active in c(FALSE, TRUE)) {
    ready <- FALSE
    caller <- rlang::env(rlang::current_env())
    binding <- function() {
      stopifnot(ready)
      function(...) "unused"
    }
    if (active) {
      makeActiveBinding("starts_with", binding, caller)
    } else {
      delayedAssign("starts_with", binding(), assign.env = caller)
    }
    share <- rlang::new_quosure(quote(dplyr::across(
      starts_with("total"), share_of_total, .names = "share"
    )), caller)
    result <- summarize_with_margins(
      data,
      dplyr::across(dplyr::all_of({
        ready <<- TRUE
        "x"
      }), sum, .names = "unused"),
      total = sum(y), !!share
    )
    expect_identical(result$share, 1)
  }
  expect_error(summarize_with_margins(
    data, dplyr::across(x, sum, .names = "unused"), total = sum(y),
    dplyr::across(dplyr::starts_with("total", vars = "total"),
                  share_of_total, .names = "unused")
  ), "conflicts with", class = "marginplyr_error")
})
