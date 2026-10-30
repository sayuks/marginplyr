expect_selector_share_ratios <- function(data) {
  sources <- list(
    quote(sum(dplyr::pick(value))),
    quote(sum(dplyr::across(value, identity)))
  )
  for (source in sources) {
    result <- rlang::inject(summarize_with_margins(
      data,
      source = !!source,
      total_share = share_of_total(source),
      parent_share = share_of_parent(source),
      .grouping = rollup(dplyr::all_of("g"))
    )) |>
      dplyr::collect() |>
      dplyr::arrange(source)

    expect_equal(result$source, c(1, 3, 4))
    expect_identical(result$total_share, c(0.25, 0.75, 1))
    expect_identical(result$parent_share, c(0.25, 0.75, 1))
  }
}

test_that("scalar selector summaries can source local contextual shares", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  expect_selector_share_ratios(data)
  expect_selector_share_ratios(tibble::as_tibble(data))
})

test_that("scalar selector summaries can source eager data.table shares", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), value = c(1, 3))
  expect_selector_share_ratios(data)
})

test_that("named scalar selector summaries retain dtplyr contextual shares", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = c(1, 3)))
  expect_selector_share_ratios(data)
})

test_that("named selector sources still require one numeric scalar", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  sources <- list(
    quote(as.character(sum(dplyr::pick(value)))),
    quote(sum(dplyr::across(value, identity)) > 0),
    quote(dplyr::pick(value)),
    quote(identity(dplyr::across(value, sum))),
    quote(rep(sum(dplyr::pick(value)), 2L)),
    quote(rep(sum(dplyr::across(value, identity)), 0L))
  )
  for (source in sources) {
    for (helper in c("share_of_total", "share_of_parent")) {
      error <- expect_error(rlang::inject(summarize_with_margins(
        data,
        source = !!source,
        share = !!rlang::call2(helper, quote(source)),
        .grouping = rollup(g)
      )), "scalar")
      expect_s3_class(error, "marginplyr_error")
    }
  }
})

test_that("named selector sources retain provenance and ordering checks", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  duplicate_sources <- stats::setNames(rlang::quos(
    sum(dplyr::pick(value)),
    sum(dplyr::across(value, identity))
  ), c("source", "source"))
  for (helper in c("share_of_total", "share_of_parent")) {
    share <- rlang::call2(helper, quote(source))
    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      !!!duplicate_sources,
      share = !!share,
      .grouping = rollup(g)
    )), "defined exactly once")
    expect_s3_class(error, "marginplyr_error")

    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      share = !!share,
      source = sum(dplyr::pick(value)),
      .grouping = rollup(g)
    )), "forward reference")
    expect_s3_class(error, "marginplyr_error")

    for (source in list(
      quote(sum(dplyr::pick(previous))),
      quote(sum(dplyr::across(previous, identity))),
      quote(sum(dplyr::pick("previous"))),
      quote(sum(dplyr::across(dplyr::all_of("previous"), identity))),
      quote(sum(dplyr::pick(dplyr::everything()))),
      quote(sum(dplyr::across(dplyr::starts_with("prev"), identity)))
    )) {
      error <- expect_error(rlang::inject(summarize_with_margins(
        data,
        previous = sum(value),
        source = !!source,
        share = !!share,
        .grouping = rollup(g)
      )), "depends on earlier summary alias `previous`")
      expect_s3_class(error, "marginplyr_error")
    }

    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      tibble::tibble(source = sum(dplyr::pick(value))),
      share = !!share,
      .grouping = rollup(g)
    )), "expanded from a data-frame-valued summary")
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("selector sources preserve local evaluation timing and counts", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  for (source in list(
    quote(aggregate_selected(dplyr::pick(dplyr::all_of(select_value())))),
    quote(aggregate_selected(dplyr::across(
      dplyr::all_of(select_value()), identity
    )))
  )) {
    run <- function(with_shares) {
      selections <- 0L
      aggregations <- 0L
      earlier <- 0L
      record_earlier <- function(x) {
        earlier <<- earlier + 1L
        sum(x)
      }
      select_value <- function() {
        expect_gt(earlier, 0L)
        selections <<- selections + 1L
        "value"
      }
      aggregate_selected <- function(x) {
        aggregations <<- aggregations + 1L
        sum(x)
      }
      shares <- if (with_shares) {
        rlang::quos(
          total_share = share_of_total(source),
          parent_share = share_of_parent(source)
        )
      } else {
        list()
      }
      result <- rlang::inject(summarize_with_margins(
        data,
        previous = record_earlier(value),
        source = !!source,
        !!!shares,
        .grouping = rollup(g)
      )) |>
        dplyr::arrange(source)
      list(
        result = result, selections = selections, aggregations = aggregations
      )
    }

    control <- run(FALSE)
    shared <- run(TRUE)
    expect_equal(shared$result$source, c(1, 3, 4))
    expect_identical(shared$result$total_share, c(0.25, 0.75, 1))
    expect_identical(shared$result$parent_share, c(0.25, 0.75, 1))
    expect_identical(shared$selections, control$selections)
    expect_identical(shared$aggregations, 3L)
    expect_identical(shared$aggregations, control$aggregations)
  }
})

test_that("selector sources reject aliases with runtime output names", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  frame <- function(x) data.frame(previous = sum(x))
  overwrite <- function(x) data.frame(value = sum(x))
  for (helper in c("share_of_total", "share_of_parent")) {
    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      frame(value),
      source = sum(dplyr::pick(dplyr::starts_with("prev"))),
      share = !!rlang::call2(helper, quote(source)),
      .grouping = rollup(g)
    )), "depends on earlier summary alias `previous`")
    expect_s3_class(error, "marginplyr_error")

    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      overwrite(value),
      source = sum(dplyr::pick(dplyr::starts_with("val"))),
      share = !!rlang::call2(helper, quote(source)),
      .grouping = rollup(g)
    )), "depends on earlier summary alias `value`")
    expect_s3_class(error, "marginplyr_error")

    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      dplyr::across(value, sum, .names = paste0("val", "ue")),
      source = sum(dplyr::pick(dplyr::starts_with("val"))),
      share = !!rlang::call2(helper, quote(source)),
      .grouping = rollup(g)
    )), "depends on earlier summary alias `value`")
    expect_s3_class(error, "marginplyr_error")

    error <- expect_error(rlang::inject(summarize_with_margins(
      data,
      value = sum(value),
      source = sum(dplyr::across("value", identity)),
      share = !!rlang::call2(helper, quote(source)),
      .grouping = rollup(g)
    )), "depends on earlier summary alias `value`")
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("tracking across outputs preserves inherited naming bindings", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  naming <- rlang::env(environment(), ..marginplyr_name_rules_1 = "previous")
  summary <- rlang::new_quosure(
    quote(dplyr::across(
      value, sum, .unpack = ,
      .names = "{get('..marginplyr_name_rules_1')}"
    )),
    rlang::env(naming)
  )
  result <- rlang::inject(summarize_with_margins(
    data,
    !!summary,
    source = sum(dplyr::pick(value)),
    share = share_of_total(source),
    .grouping = rollup(g)
  )) |>
    dplyr::arrange(source)

  expect_equal(result$previous, c(1, 3, 4))
  expect_equal(result$source, c(1, 3, 4))
  expect_identical(result$share, c(0.25, 0.75, 1))
})

test_that("omitted across columns retain independent share sources", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value_sum))
    result <- summarize_with_margins(
      data,
      dplyr::across(.fns = sum, .names = "{.col}_sum"),
      fraction = !!helper,
      .grouping = rollup(g)
    )
    expect_equal(result$value_sum, c(1, 3, 4))
    expect_equal(result$fraction, c(0.25, 0.75, 1))
    lazy <- summarize_with_margins(
      dbplyr::tbl_lazy(data),
      dplyr::across(.fns = sum, .names = "{.col}_sum"),
      fraction = !!helper,
      .grouping = rollup(g),
      .check_share_source = FALSE
    )
    expect_s3_class(lazy, "tbl_lazy")
    expect_match(
      as.character(dbplyr::sql_render(lazy)), 'SUM("value")', fixed = TRUE
    )
  }
})
