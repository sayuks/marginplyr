# Compare only generated outer classes; generated cell classes have ADR 0044's
# separate data-frame, contents and row-count contract.
expect_native_outer_classes <- function(input) {
  summary <- summarize_with_margins(
    input, z = sum(.data$z), .grouping = rollup("g")
  )
  expansion <- expand_with_margins(input, .grouping = rollup("g"))
  nesting <- nest_with_margins(input, .grouping = rollup("g"))
  rowwise <- nest_by_with_margins(input, .grouping = rollup("g"))
  expect_identical(
    class(summary),
    class(dplyr::summarise(input, z = sum(.data$z), .by = "g"))
  )
  expect_identical(class(expansion), class(dplyr::union_all(
    input, dplyr::mutate(input, g = "Total")
  )))
  expect_identical(class(nesting), class(tidyr::nest(input, .by = "g")))
  expect_identical(
    class(rowwise), class(dplyr::nest_by(input, !!!rlang::syms("g")))
  )
}

test_that("local outer frames follow the official class reference", {
  input <- data.frame(g = c("a", "b"), z = 1:2)
  expect_native_outer_classes(input)
  expect_native_outer_classes(tibble::as_tibble(input))
})

test_that("direct data.table outer frames follow native classes", {
  skip_if_suggest_absent("data.table")
  expect_native_outer_classes(data.table::data.table(g = c("a", "b"), z = 1:2))
  expect_native_outer_classes(
    data.table::data.table(g = character(), z = integer())
  )
})

# The vector owns its currency attribute; vctrs supplies its native methods.
native_currency <- function(x) {
  vctrs::new_vctr(
    as.double(x), currency = "JPY", class = "margin_native_currency"
  )
}

test_that("source-derived custom vectors keep native class-owned state", {
  input <- tibble::tibble(k = native_currency(c(1, 1, 2)),
                          g = native_currency(c(3, 4, 3)),
                          z = native_currency(c(5, 6, 7)))
  for (source in list(input, as.data.frame(input))) {
    for (plan in list(grouping_set("g"), rollup("g"))) {
      reference <- if (identical(plan, grouping_set("g"))) {
        source
      } else {
        dplyr::union_all(source, dplyr::mutate(source, g = native_currency(NA)))
      }
      expanded <- expand_with_margins(source, .by = "k", .grouping = plan,
                                      .margin_label = NULL)
      for (column in c("k", "g", "z")) {
        expect_identical(expanded[[column]], reference[[column]])
      }
      summary <- summarize_with_margins(
        source, z = dplyr::first(z),
        .by = "k", .grouping = plan, .margin_label = NULL
      )
      native <- dplyr::summarise(source, z = dplyr::first(z), .by = c(k, g))
      if (!identical(plan, grouping_set("g"))) {
        total <- dplyr::summarise(source, z = dplyr::first(z), .by = "k")
        native <- dplyr::union_all(
          native, dplyr::mutate(total, g = native_currency(NA))
        )
      }
      for (column in c("k", "g", "z")) {
        expect_identical(summary[[column]], native[[column]])
      }
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        nested <- verb(source, .by = "k", .grouping = plan,
                       .margin_label = NULL, .keep = TRUE)
        expect_identical(class(nested$k), class(native$k))
        expect_identical(nested$g, native$g)
        expect_identical(attr(nested$k, "currency"), "JPY")
        for (cell in nested$data) {
          for (column in c("k", "g", "z")) {
            expect_identical(class(cell[[column]]), class(source[[column]]))
            expect_identical(attr(cell[[column]], "currency"), "JPY")
          }
          expect_false(anyNA(cell$g))
        }
      }
    }
  }
})

test_that("display replacement and summary evaluation decide classes", {
  input <- tibble::tibble(g = as.Date(c("2020-01-01", "2020-01-02")), z = 1:2)
  string <- expand_with_margins(input, .grouping = rollup("g"))
  expect_identical(string$g, c(as.character(input$g), "Total", "Total"))
  typed <- expand_with_margins(
    input, .grouping = rollup("g"), .margin_label = NULL
  )
  expect_identical(typed$g, c(input$g, as.Date(c(NA, NA))))
  expected <- dplyr::union_all(
    dplyr::summarise(input, g = mean(z)),
    dplyr::summarise(input, g = mean(z))
  )
  result <- summarise_with_margins(
    input, g = mean(z),
    .grouping = grouping_sets(grouping_set(), grouping_set()),
    .duplicates = "keep"
  )
  expect_identical(result$g, expected$g)
})

expect_native_original_cells <- function(objects) {
  input <- tibble::tibble(g = c("a", "a"), objects = objects)
  expected <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
  expanded <- expand_with_margins(input, .grouping = rollup("g"))
  expect_identical(expanded$objects, expected$objects)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    nested <- verb(input, .grouping = rollup("g"), .keep = TRUE)
    for (cell in nested$data) {
      expect_true(is.data.frame(cell))
      expect_identical(cell$objects, objects)
      expect_identical(cell$g, c("a", "a"))
    }
  }
  native <- dplyr::summarise(input, objects = list(objects), .by = "g")
  result <- summarize_with_margins(
    input, objects = list(objects), .grouping = grouping_set("g")
  )
  expect_identical(result$objects, native$objects)
}

test_that("original S3 frame and S4 cell objects survive supported carrying", {
  frame <- structure(
    data.frame(value = 1L), class = c("native_cell", "data.frame"),
    schema = list(unit = "kg")
  )
  generator <- methods::setClass(
    "margin_native_cell", slots = c(value = "integer")
  )
  withr::defer(methods::removeClass("margin_native_cell"))
  expect_native_original_cells(list(frame, generator(value = 2L)))
})

test_that("original S7 cell objects survive supported carrying", {
  skip_if_suggest_absent("S7")
  generator <- S7::new_class("margin_native_cell",
                             properties = list(value = S7::class_integer))
  expect_native_original_cells(list(
    generator(value = 1L), generator(value = 2L)
  ))
})

test_that("summary classes follow native transformation and refusal", {
  input <- tibble::tibble(g = c("a", "b"), z = 1:2)
  make_time <- function(n) {
    if (n == 1L) as.Date("2020-01-01") else as.POSIXct("2020-01-02", tz = "UTC")
  }
  expected <- dplyr::union_all(
    dplyr::summarise(input, z = make_time(dplyr::n()), .by = "g"),
    dplyr::mutate(
      dplyr::summarise(input, z = make_time(dplyr::n())), g = "Total"
    )
  )
  actual <- summarize_with_margins(input, z = make_time(dplyr::n()),
                                   .grouping = rollup("g"))
  expect_identical(actual$z, expected$z)
  incompatible <- function(n) {
    x <- native_currency(n)
    attr(x, "currency") <- if (n == 1L) "JPY" else "USD"
    x
  }
  native <- expect_error(dplyr::union_all(
    dplyr::summarise(input, z = incompatible(dplyr::n()), .by = "g"),
    dplyr::mutate(
      dplyr::summarise(input, z = incompatible(dplyr::n())), g = "Total"
    )
  ))
  margin <- expect_error(summarize_with_margins(
    input, z = incompatible(dplyr::n()), .grouping = rollup("g")
  ))
  expect_s3_class(native, "rlang_error")
  expect_s3_class(margin, "vctrs_error_incompatible_type")
  expect_false(inherits(margin, "marginplyr_error"))
})

test_that("empty classes and zero-column cells keep their contracts", {
  empty <- data.frame(g = character(), z = integer())
  expect_native_outer_classes(empty)
  expect_native_outer_classes(tibble::as_tibble(empty))
  for (source in list(data.frame(g = c("a", "a", "b")),
                      tibble::tibble(g = c("a", "a", "b")))) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(source, .grouping = rollup("g"))
      expect_true(is.list(result$data))
      expect_true(all(vapply(result$data, is.data.frame, logical(1))))
      expect_identical(vapply(result$data, ncol, integer(1)), c(0L, 0L, 0L))
      expect_identical(vapply(result$data, nrow, integer(1)), c(2L, 1L, 3L))
    }
  }
})

# Concrete lazy-step subclasses describe query implementation; compare the
# supported backend family before collection and the exact returned frame after.
expect_native_lazy_classes <- function(input) {
  summary <- summarize_with_margins(
    input, z = sum(.data$z), .grouping = rollup("g")
  )
  native <- dplyr::summarise(input, z = sum(.data$z), .by = "g")
  family <- if (inherits(native, "dtplyr_step")) {
    "dtplyr_step"
  } else {
    class(native)
  }
  expect_true(all(family %in% class(summary)))
  expect_identical(
    class(dplyr::collect(summary)), class(dplyr::collect(native))
  )
  expansion <- expand_with_margins(input, .grouping = rollup("g"))
  expected <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
  expect_true(all(family %in% class(expansion)))
  expect_identical(
    class(dplyr::collect(expansion)), class(dplyr::collect(expected))
  )
  expect_identical(dplyr::collect(expansion)$z, dplyr::collect(expected)$z)
}

test_that("dtplyr direct collection follows native classes and source state", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(
    g = c("a", "b"), z = 1:2,
    payload = as.Date(c("2020-01-01", "2020-01-02"))
  ))
  expect_native_lazy_classes(input)
  expanded <- dplyr::collect(expand_with_margins(
    input, .grouping = rollup("g")
  ))
  native <- dplyr::collect(dplyr::union_all(
    input, dplyr::mutate(input, g = "Total")
  ))
  expect_identical(expanded$payload, native$payload)
  ordinary <- dplyr::collect(nest_with_margins(
    input, .grouping = rollup("g"), .keep = TRUE
  ))
  reference <- dplyr::collect(tidyr::nest(input, data = c(z, payload)))
  expect_identical(class(ordinary), class(reference))
  expect_identical(ordinary$data[[1]]$payload, reference$data[[1]]$payload)
  rowwise <- nest_by_with_margins(input, .grouping = rollup("g"), .keep = TRUE)
  expect_identical(
    class(rowwise), class(dplyr::nest_by(dplyr::collect(input), g))
  )
  expect_identical(rowwise$data[[3]]$payload, dplyr::collect(input)$payload)
  zero <- dplyr::collect(nest_with_margins(
    dplyr::select(input, g), .grouping = rollup("g")
  ))
  expect_true(all(vapply(zero$data, is.data.frame, logical(1))))
  expect_identical(vapply(zero$data, nrow, integer(1)), c(1L, 1L, 2L))
  expect_identical(vapply(zero$data, ncol, integer(1)), c(0L, 0L, 0L))
})

test_that("Arrow collection follows native frame and Date classes", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(tibble::tibble(
    g = c("a", "b"), z = 1:2,
    payload = as.Date(c("2020-01-01", "2020-01-02"))
  ))
  expect_native_lazy_classes(input)
  actual <- dplyr::collect(expand_with_margins(input, .grouping = rollup("g")))
  native <- dplyr::collect(dplyr::union_all(
    input, dplyr::mutate(input, g = "Total")
  ))
  expect_identical(actual$payload, native$payload)
})

test_that("SQLite direct collection follows native output frame classes", {
  skip_if_suggest_absent("RSQLite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(connection))
  input <- dplyr::copy_to(
    connection, tibble::tibble(g = c("a", "b"), z = 1:2), "native_classes"
  )
  expect_native_lazy_classes(input)
})

test_that("ordered factors retain the separately promised reconstructed type", {
  input <- tibble::tibble(
    g = ordered(c("b", "a"), levels = c("a", "b")), z = 1:2
  )
  verbs <- list(expand_with_margins, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    for (sort in c("none", "last")) {
      actual <- verb(input, .grouping = rollup("g"), .sort = sort)
      expect_identical(class(actual$g), class(input$g))
      expect_identical(levels(actual$g), c("a", "b", "Total"))
      typed <- verb(
        input, .grouping = rollup("g"), .sort = sort, .margin_label = NULL
      )
      expect_identical(class(typed$g), class(input$g))
      expect_identical(levels(typed$g), levels(input$g))
    }
  }
})

test_that("direct data.table nesting follows native source-key removal", {
  skip_if_suggest_absent("data.table")
  input <- data.table::data.table(g = c("b", "a"), z = 1:2)
  data.table::setkey(input, g)
  expected <- tidyr::nest(input, .by = "g")
  actual <- nest_with_margins(input, .grouping = rollup("g"))
  expect_identical(data.table::key(actual), data.table::key(expected))
  expect_identical(class(actual), class(expected))
  expect_identical(actual$g, c("a", "b", "Total"))
  expect_identical(data.table::key(input), "g")
})

expect_native_carried_currency <- function(input, collect) {
  native <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
  actual <- expand_with_margins(input, .by = "k", .grouping = rollup("g"))
  native <- collect(native)
  actual <- collect(actual)
  expect_identical(actual$k, native$k)
  expect_identical(actual$z, native$z)
}

test_that("direct data.table carries native-compatible custom vectors", {
  skip_if_suggest_absent("data.table")
  input <- data.table::data.table(
    k = native_currency(c(1, 2)), g = c("a", "b"), z = native_currency(c(3, 4))
  )
  expect_native_carried_currency(input, identity)
})

test_that("dtplyr carries native-compatible custom vectors", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(
    k = native_currency(c(1, 2)), g = c("a", "b"), z = native_currency(c(3, 4))
  ))
  expect_native_carried_currency(input, dplyr::collect)
})
