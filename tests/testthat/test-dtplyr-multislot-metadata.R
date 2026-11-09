# A vector with both numeric and temporal row-sized slots, as in Period and
# Interval. Its proxy and restoration keep every slot aligned with `.Data`.
local_dtplyr_multislot <- function(.env = parent.frame()) {
  class <- "marginplyr_multislot_metadata"
  methods::setClass(class, contains = "numeric", slots = c(
    span = "numeric", start = "POSIXct"
  ))
  withr::defer(methods::removeClass(class), envir = .env)
  methods::setMethod("as.character", class, function(x) {
    sprintf("%s:%s", x@.Data, x@span)
  })
  proxy <- function(x, ...) {
    vctrs::new_data_frame(list(
      span = x@span, start = x@start, value = x@.Data
    ), n = length(x@span))
  }
  restore <- function(x, to, ...) {
    methods::new(class, x$value, span = x$span, start = x$start)
  }
  rlang::local_bindings(
    vec_proxy.marginplyr_multislot_metadata = proxy,
    vec_restore.marginplyr_multislot_metadata = restore,
    .env = asNamespace("vctrs")$.__S3MethodsTable__., .frame = .env
  )
  function(rows) {
    methods::new(
      class, as.double(seq_len(rows)), span = 10 * seq_len(rows),
      start = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(rows)
    )
  }
}

test_that("dtplyr included expansion uses faithful multi-slot metadata", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  column <- local_dtplyr_multislot()
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(k = column(rows))
    before <- serialize(source, NULL)
    root <- dtplyr::lazy_dt(source)
    expected <- tibble::tibble(k = c("1:10", "2:20", "3:30")[seq_len(rows)])
    expect_identical(
      dplyr::collect(dplyr::mutate(root, k = as.character(k))), expected
    )
    expect_identical(dplyr::collect(expand_with_margins(
      root, .grouping = grouping_set(k)
    )), expected)
    typed <- dplyr::collect(expand_with_margins(
      root, .grouping = grouping_set(k), .margin_label = NULL
    ))
    expect_identical(typed, dplyr::collect(root))
    expect_identical(typed$k, column(rows))
    expect_identical(serialize(source, NULL), before)
  }
})

test_that("dtplyr typed inspection aligns all S4 metadata slots", {
  skip_if_suggest_absent("dtplyr")
  column <- local_dtplyr_multislot()
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(k = column(rows), value = seq_len(rows))
    before <- serialize(source, NULL)
    for (immutable in c(TRUE, FALSE)) {
      root <- dtplyr::lazy_dt(source, immutable = immutable)
      seen <- list()
      predicate <- function(x) {
        if (!isS4(x)) return(FALSE)
        seen[[length(seen) + 1L]] <<- x
        TRUE
      }
      expect_identical(inspect_grouping(
        root, .grouping = grouping_set(k)
      )$included, "(k)")
      expect_identical(seen, list())
      expect_identical(inspect_grouping(
        root, .grouping = grouping_set(where(predicate))
      )$included, "(k)")
      expect_identical(seen, list(column(0L)))
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("dtplyr S4 expansion retains fixed keys and ordinary payloads", {
  skip_if_suggest_absent("dtplyr")
  column <- local_dtplyr_multislot()
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      fixed = column(rows), k = column(rows), payload = column(rows),
      f = factor(c("a", "b", "a")[seq_len(rows)], levels = c("b", "a", "c")),
      date = as.Date("2026-01-01") + seq_len(rows),
      time = as.POSIXct("2026-01-01", tz = "Asia/Tokyo") + seq_len(rows)
    )
    before <- serialize(source, NULL)
    root <- dtplyr::lazy_dt(source)
    expected <- dplyr::collect(dplyr::mutate(root, k = as.character(k)))
    result <- expand_with_margins(
      root, .by = fixed, .grouping = grouping_set(k)
    )
    expect_s3_class(result, "dtplyr_step")
    actual <- dplyr::collect(result)
    expect_identical(actual, expected)
    expect_identical(actual$k, c("1:10", "2:20", "3:30")[seq_len(rows)])
    expect_identical(actual$fixed, column(rows))
    expect_identical(actual$payload, column(rows))
    expect_identical(levels(actual$f), c("b", "a", "c"))
    expect_identical(actual$date, source$date)
    expect_identical(actual$time, source$time)
    expect_identical(serialize(source, NULL), before)
  }
})

test_that("dtplyr S4 metadata avoids source work and refuses derivations", {
  skip_if_suggest_absent("dtplyr")
  column <- local_dtplyr_multislot()
  source <- data.table::data.table(k = column(3L), value = 1:3)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  filter_rows <- integer()
  filter <- function(x) {
    filter_rows <<- c(filter_rows, length(x))
    rep(TRUE, length(x))
  }
  step <- dplyr::filter(root, filter(value))
  expect_identical(inspect_grouping(
    step, .grouping = grouping_set(where(isS4))
  )$included, "(k)")
  result <- expand_with_margins(step, .grouping = grouping_set(k))
  invisible(dplyr::show_query(result))
  expect_identical(filter_rows, integer())
  expect_identical(serialize(source, NULL), before)
  expect_identical(dplyr::collect(result), tibble::tibble(
    k = c("1:10", "2:20", "3:30"), value = 1:3
  ))
  expect_identical(filter_rows, 3L)

  derived_rows <- integer()
  derive <- function(x) {
    derived_rows <<- c(derived_rows, length(x))
    as.character(x)
  }
  unsupported <- dplyr::mutate(root, derived = derive(k))
  expect_identical(inspect_grouping(
    unsupported, .grouping = grouping_set(derived)
  )$included, "(derived)")
  expect_error(inspect_grouping(
    unsupported, .grouping = grouping_set(where(isS4))
  ), "Can't safely determine column metadata", fixed = TRUE,
  class = "marginplyr_error")
  expect_error(expand_with_margins(
    unsupported, .grouping = grouping_set(derived)
  ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_identical(derived_rows, integer())
  expect_identical(serialize(source, NULL), before)
  explicit <- dplyr::collect(unsupported)
  expect_identical(explicit$derived, c("1:10", "2:20", "3:30"))
  expect_identical(derived_rows, 3L)
  expect_identical(dplyr::collect(expand_with_margins(
    explicit, .grouping = grouping_set(derived)
  ))$derived, c("1:10", "2:20", "3:30"))
  expect_identical(serialize(source, NULL), before)
})
