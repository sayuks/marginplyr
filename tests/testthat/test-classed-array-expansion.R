# Retain upstream warnings alongside values for comparison with ordinary
# construction; difftime and integer64 combination may warn or drop shape.
observe_array_expansion <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(expr, warning = function(condition) {
    warnings[[length(warnings) + 1L]] <<- list(
      class = class(condition), message = conditionMessage(condition)
    )
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

# Compare direct and packed arrays built from one classed scalar fixture.
expect_missing_array_control <- function(value) {
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(1L, c(1L, 2L))) {
      key <- rep(value, size * prod(tail))
      dim(key) <- c(size, tail)
      for (packed in c(FALSE, TRUE)) {
        dimension <- if (packed) tibble::tibble(component = key) else key
        input <- tibble::tibble(key = dimension)
        control <- observe_array_expansion(dplyr::union_all(
          input, dplyr::mutate(input, key = vctrs::vec_init(dimension, size))
        ))
        for (label in list(NULL, NA_character_)) {
          expect_identical(
            observe_array_expansion(expand_with_margins(
              input, .grouping = rollup(key),
              .margin_label = label, .sort = "none"
            )),
            control
          )
        }
      }
    }
  }
}

test_that("unsorted expansion validates missing Date matrix components", {
  key <- as.Date("2026-01-01")
  dim(key) <- c(1L, 1L)
  input <- tibble::tibble(key = key, value = 1L)
  control <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, 1L))
  )

  expect_identical(
    expand_with_margins(input, .grouping = grouping_set(key),
                        .margin_label = NULL),
    input
  )
  result <- expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL, .sort = "none"
  )
  expect_identical(result, control)
  expect_identical(class(result$key), "Date")
  expect_identical(dim(result$key), c(2L, 1L))
  expect_identical(as.numeric(result$key), c(as.numeric(key), NA_real_))
  expect_identical(result$value, c(1L, 1L))
})

test_that("classed arrays and their packed components accept missing labels", {
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(1L, 2L, c(1L, 2L))) {
      shape <- c(size, tail)
      values <- rep(c(2, NA, 1), length.out = prod(shape))
      keys <- list(
        Date = as.Date(values, origin = "2026-01-01"),
        POSIXct = as.POSIXct(values, origin = "2026-01-01", tz = "UTC"),
        difftime = as.difftime(values, units = "hours"),
        factor = factor(values, levels = c(1, 2, 3)),
        integer = as.integer(values),
        table = as.table(array(as.integer(values), dim = shape))
      )
      if (identical(tail, 2L)) {
        # Literal right-censored Surv data avoids a survival dependency.
        keys$Surv <- structure(
          c(rep(1, size), rep(0, size)), dim = shape,
          dimnames = list(NULL, c("time", "status")),
          type = "right", class = "Surv"
        )
      }
      for (family in names(keys)) {
        key <- keys[[family]]
        dim(key) <- shape
        for (packed in c(FALSE, TRUE)) {
          dimension <- if (packed) tibble::tibble(
            component = key, note = rep("source", size)
          ) else key
          input <- tibble::tibble(key = dimension, value = seq_len(size))
          ordinary <- observe_array_expansion(dplyr::union_all(
            dplyr::mutate(input, set = 1L),
            dplyr::mutate(
              input, key = vctrs::vec_init(dimension, size), set = 2L
            )
          ))
          control <- dplyr::relocate(ordinary$value, set, .after = key)
          for (label in list(NULL, NA_character_)) {
            expect_identical(
              expand_with_margins(input, .grouping = grouping_set(key),
                                  .margin_label = label, .sort = "none"),
              input
            )
            actual <- observe_array_expansion(expand_with_margins(
              input, .grouping = rollup(key), .margin_label = label,
              .id = "set", .sort = "none"
            ))
            result <- actual$value
            expect_identical(actual$warnings, ordinary$warnings)
            expect_identical(result, control)
            component <- if (packed) result$key$component else result$key
            expected <- if (packed) control$key$component else control$key
            expect_identical(dim(component), dim(expected))
            expect_identical(class(component), class(expected))
            expect_true(all(is.na(vctrs::vec_slice(
              component, size + seq_len(size)
            ))))
            expect_identical(result$value, rep(seq_len(size), 2L))
            expect_identical(result$set, rep(1:2, each = size))
          }
          expect_error(
            expand_with_margins(input, .grouping = rollup(key),
                                .margin_label = "All", .sort = "none"),
            if (packed) "packed grouping dimensions" else
              "matrix or array grouping dimensions",
            class = "marginplyr_error"
          )
        }
      }
    }
  }
})

test_that("nested packed array dimensions validate every missing component", {
  day <- as.Date(c("2026-01-01", NA))
  dim(day) <- c(2L, 1L)
  key <- tibble::tibble(inner = tibble::tibble(day = day), code = 1:2)
  input <- tibble::tibble(key = key, value = 1:2)
  control <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, 2L))
  )
  for (label in list(NULL, NA_character_)) {
    expect_identical(
      expand_with_margins(input, .grouping = rollup(key),
                          .margin_label = label, .sort = "none"),
      control
    )
  }
})

test_that("units arrays remain an ordinary typed-missing expansion control", {
  skip_if_suggest_absent("sf")
  distance <- sf::st_length(sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(3, 4))), crs = 3857
  ))
  expect_s3_class(distance, "units")
  expect_missing_array_control(distance)
})

test_that("integer64 arrays remain an ordinary typed-missing control", {
  skip_if_suggest_absent("bit64")
  expect_missing_array_control(bit64::as.integer64("9007199254740993"))
})
