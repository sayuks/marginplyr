test_that("dtplyr omitted scalar dimensions use ordinary missing composition", {
  skip_if_suggest_absent("dtplyr")
  for (key in list(
    stats::dist(matrix(c(0, 2), ncol = 1L)),
    stats::logLik(stats::lm(c(1, 3, 2) ~ 1))
  )) {
    input <- dtplyr::lazy_dt(tibble::tibble(g = key, row = 1L))
    expect_identical(class(dplyr::collect(input)$g), class(key))
    missing <- vctrs::vec_init(key, 1L)
    expected <- dplyr::union_all(
      input, dplyr::mutate(input, g = vctrs::vec_rep(!!missing, length(g)))
    ) |>
      dplyr::collect()
    expect_identical(expected$row, c(1L, 1L))
    expect_identical(class(expected$g), class(key))
    expect_identical(as.numeric(expected$g), c(as.numeric(key), NA_real_))
    result <- expand_with_margins(input, .grouping = rollup(g),
                                  .margin_label = NULL)
    expect_s3_class(result, "dtplyr_step")
    expect_identical(dplyr::collect(result), expected)
  }
})

test_that("dtplyr scalar missing labels preserve copies across lazy plans", {
  skip_if_suggest_absent("dtplyr")
  keys <- list(
    stats::dist(matrix(c(0, 2, 5), ncol = 1L)),
    vctrs::vec_rep(stats::logLik(stats::lm(c(1, 3, 2) ~ 1)), 3L)
  )
  for (size in c(0L, 1L, 3L)) {
    for (key in keys) {
      source <- data.table::data.table(
        g = vctrs::vec_slice(key, seq_len(size)), row = seq_len(size),
        other = rep("a", size)
      )
      before <- data.table::copy(source)
      expect_identical(class(source$g), class(key))
      direct <- dtplyr::lazy_dt(source, immutable = TRUE)
      filtered <- dplyr::filter(direct, row > 0L)
      for (input in list(direct, filtered, dplyr::compute(filtered))) {
        plain <- dplyr::collect(input)
        # Use a reusable ordinary root; the package owns cache isolation.
        control <- dtplyr::lazy_dt(plain)
        missing <- vctrs::vec_init(plain$g, 1L)
        for (included in list(TRUE, c(TRUE, FALSE), c(FALSE, TRUE),
                              c(FALSE, TRUE, FALSE, TRUE))) {
          sets <- lapply(included, function(keep) {
            if (keep) grouping_set(g) else grouping_set()
          })
          plan <- grouping_sets(!!!sets)
          for (id in list(NULL, "set")) {
            branches <- lapply(seq_along(included), function(i) {
              branch <- if (included[[i]]) control else dplyr::mutate(
                control, g = vctrs::vec_rep(!!missing, length(g))
              )
              if (!is.null(id)) branch <- dplyr::mutate(branch, set = !!i)
              branch
            })
            ordinary <- Reduce(dplyr::union_all, branches) |>
              dplyr::collect() |>
              dplyr::relocate(dplyr::any_of(c("g", id)))
            expect_identical(ordinary$row, rep(seq_len(size), length(included)))
            expect_identical(class(ordinary$g), class(source$g))
            for (order in c("none", "last")) {
              expected <- if (order == "none") ordinary else dplyr::arrange(
                ordinary, is.na(g), g
              )
              for (label in list(NULL, NA_character_)) {
                result <- expand_with_margins(
                  input, .grouping = plan, .margin_label = label,
                  .duplicates = "keep", .id = id, .sort = order
                )
                expect_s3_class(result, "dtplyr_step")
                expect_identical(source, before)
                actual <- dplyr::collect(result)
                expect_identical(actual, expected)
                expect_identical(is.na(actual$g), is.na(expected$g))
                if (!is.null(id)) {
                  expect_identical(sort(actual$set),
                                   rep(seq_along(included), each = size))
                }
                expect_identical(source, before)
              }
            }
          }
        }
        carried <- input
        for (label in list(NULL, NA_character_)) {
          ordinary <- dplyr::union_all(
            control, dplyr::mutate(control, other = NA_character_)
          ) |>
            dplyr::collect()
          for (fixed in c(FALSE, TRUE)) {
            result <- expand_with_margins(
              carried, .by = if (fixed) "g" else NULL,
              .grouping = rollup(other), .margin_label = label
            ) |>
              dplyr::collect()
            expected <- dplyr::relocate(
              ordinary, dplyr::all_of(c("g"[fixed], "other"))
            )
            expect_identical(result, expected)
          }
        }
        expect_identical(source, before)
      }
    }
  }
})

test_that("scalar prototypes stay deferred and refuse Mutable steps", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(
    g = stats::dist(matrix(c(0, 2), ncol = 1L)), row = 1L
  )
  before <- data.table::copy(source)
  expect_s3_class(source$g, "dist")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x > 0L
  }
  for (cached in c(FALSE, TRUE)) {
    input <- dplyr::filter(dtplyr::lazy_dt(source), probe(row))
    if (cached) input <- dplyr::compute(input)
    seen <- integer()
    result <- expand_with_margins(input, .grouping = rollup(g),
                                  .margin_label = NULL)
    expect_identical(seen, integer())
    expect_identical(source, before)
    expect_identical(dplyr::collect(result)$row, c(1L, 1L))
    expect_true(length(seen) > 0L && all(seen == 1L))
    expect_identical(source, before)
  }
  mutable <- dtplyr::lazy_dt(source, immutable = FALSE)
  expect_error(
    expand_with_margins(mutable, .grouping = rollup(g), .margin_label = NULL),
    "immutable = TRUE", class = "marginplyr_error"
  )
  expect_identical(source, before)
})
