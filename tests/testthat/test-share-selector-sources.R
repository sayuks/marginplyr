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

test_that("scalar selectors can exclude earlier aliases from share sources", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  for (selection in list(
    quote(c(dplyr::all_of(select_value()), -prior)),
    quote(dplyr::all_of(select_value()) & !prior),
    quote(dplyr::all_of(select_value()) / prior),
    quote(c(dplyr::all_of(select_value()), -.data$prior)),
    quote(dplyr::all_of(select_value()) & !.data[["prior"]])
  )) {
    sources <- rlang::exprs(
      sum(dplyr::pick(!!selection)),
      sum(dplyr::across(!!selection, identity)),
      sum(value * dplyr::if_any(!!selection, ~ .x < 4)),
      sum(value * dplyr::if_all(!!selection, ~ .x < 4))
    )
    for (source in sources) {
      run <- function(shares) {
        selections <- 0L
        select_value <- function() {
          selections <<- selections + 1L
          "value"
        }
        result <- summarize_with_margins(
          data,
          prior = sum(other),
          source = !!source,
          !!!shares,
          .grouping = rollup(g)
        ) |>
          dplyr::arrange(source)
        list(result = result, selections = selections)
      }

      control <- run(list())
      shared <- run(rlang::quos(
        total_share = share_of_total(source),
        parent_share = share_of_parent(source)
      ))
      expect_equal(control$result$source, c(1, 3, 4))
      expect_identical(shared$result[names(control$result)], control$result)
      expect_identical(shared$result$total_share, c(0.25, 0.75, 1))
      expect_identical(shared$result$parent_share, c(0.25, 0.75, 1))
      expect_gt(control$selections, 0L)
      expect_identical(shared$selections, control$selections)
    }
  }
})

test_that("excluding an alias does not conceal reads in a share source", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  choose_value <- function(alias) {
    force(alias)
    "value"
  }
  sources <- rlang::exprs(
    sum(dplyr::pick(c(value, prior))),
    sum(dplyr::pick(c(value, .data$prior))),
    sum(dplyr::pick(c(value, .data[["prior"]]))),
    sum(dplyr::across(value | prior, identity)),
    sum(dplyr::if_any(c(value, prior), ~ .x > 0)),
    sum(dplyr::if_all(c(value, prior), ~ .x > 0)),
    sum(dplyr::pick(c(value, -prior))) + prior,
    sum(dplyr::across(value & !prior, ~ .x + prior)),
    sum(dplyr::pick(c(dplyr::all_of(choose_value(prior)), -prior)))
  )
  for (source in sources) {
    for (helper in c("share_of_total", "share_of_parent")) {
      error <- expect_error(summarize_with_margins(
        data,
        prior = sum(other),
        source = !!source,
        share = !!rlang::call2(helper, quote(source)),
        .grouping = rollup(g)
      ), "depends on earlier summary alias `prior`")
      expect_s3_class(error, "marginplyr_error")
    }
  }
})

test_that("excluded aliases may come from a runtime summary name", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  make_prior <- function(x) data.frame(prior = sum(x))
  selection <- rlang::quo(c(value, -prior))
  result <- summarize_with_margins(
    data,
    make_prior(other),
    source = sum(dplyr::pick(!!selection)),
    total_share = share_of_total(source),
    parent_share = share_of_parent(source),
    .grouping = rollup(g)
  ) |>
    dplyr::arrange(source)

  expect_equal(result$source, c(1, 3, 4))
  expect_identical(result$total_share, c(0.25, 0.75, 1))
  expect_identical(result$parent_share, c(0.25, 0.75, 1))
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

test_that("share source predicates cannot read earlier aliases", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  observed <- rlang::env(calls = 0L)
  predicate <- function(x) {
    observed$calls <- observed$calls + 1L
    is.numeric(x)
  }
  selection <- rlang::expr(c(tidyselect::where(predicate), -prior, -other))
  control <- summarize_with_margins(
    data, prior = sum(other),
    source = sum(dplyr::pick(!!selection)),
    .grouping = rollup(g)
  )
  expect_identical(control$source, c(1, 3, 4))
  expect_gt(observed$calls, 0L)

  for (helper in rlang::syms(c("share_of_total", "share_of_parent"))) {
    observed$calls <- 0L
    expect_error(summarize_with_margins(
      data, prior = sum(other),
      source = sum(dplyr::pick(!!selection)),
      share = (!!helper)(source), .grouping = rollup(g)
    ), "depends on earlier summary alias `prior`", class = "marginplyr_error")
    expect_identical(observed$calls, 0L)

    independent <- summarize_with_margins(
      data,
      source = sum(dplyr::pick(c(tidyselect::where(predicate), -other))),
      share = (!!helper)(source), .grouping = rollup(g)
    )
    expect_identical(independent$source, control$source)
    expect_identical(independent$share, c(0.25, 0.75, 1))
    expect_gt(observed$calls, 0L)
  }
})

test_that("selectors cannot inspect earlier alias values through peek_data", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  choose <- function() {
    prior_value <- tidyselect::peek_data()$prior
    if (prior_value[[1L]] < 18) "other" else "value"
  }
  choose_default <- function(prior_value = tidyselect::peek_data()$prior) {
    if (prior_value[[1L]] < 18) "other" else "value"
  }
  choose_chained <- function(a = b, b = tidyselect::peek_data()$prior) {
    if (a[[1L]] < 18) "other" else "value"
  }
  choose_with <- function(peek) {
    if (peek()$prior[[1L]] < 18) "other" else "value"
  }
  choose_dots <- function(...) {
    prior_value <- list(...)[[1L]]
    if (prior_value[[1L]] < 18) "other" else "value"
  }
  forward <- function(...) choose_dots(...)
  key <- "prior"
  for (column in rlang::exprs(
    choose(),
    choose_default(),
    choose_chained(),
    choose_with(tidyselect::peek_data),
    choose_with(!!tidyselect::peek_data),
    choose_dots(tidyselect::peek_data()$prior),
    forward(tidyselect::peek_data()$prior),
    if (tidyselect::peek_data()[["prior"]][[1L]] < 18) "other" else "value",
    if (tidyselect::peek_data()[[key]][[1L]] < 18) "other" else "value"
  )) {
    ordinary <- rlang::quos(
      prior = 10 + sum(other),
      source = sum(dplyr::pick(c(dplyr::all_of(!!column), -prior)))
    )
    control <- summarize_with_margins(data, !!!ordinary, .grouping = rollup(g))
    expect_identical(control$source, c(7, 9, 4))
    for (helper in rlang::syms(c("share_of_total", "share_of_parent"))) {
      expect_error(summarize_with_margins(
        data, !!!ordinary, share = (!!helper)(source), .grouping = rollup(g)
      ), "depends on earlier summary alias `prior`", class = "marginplyr_error")
    }
  }
})

test_that("selectors can use other columns and names from peek_data", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  ignore_default <- function(column = tidyselect::peek_data()$prior) {
    "value"
  }
  ignore_argument <- function(column) {
    "value"
  }
  choose_chained <- function(a = b, b = tidyselect::peek_data()$prior) {
    if (a[[1L]] < 18) "other" else "value"
  }
  choose <- rlang::new_function(
    pairlist(column = quote(tidyselect::peek_data()$prior)),
    rlang::expr({
      if (FALSE) tidyselect::peek_data()$prior
      column
    })
  )
  for (column in rlang::exprs(
    if (sum(tidyselect::peek_data()$other) > 0) "value" else "other",
    intersect("value", names(tidyselect::peek_data())),
    choose("value"),
    ignore_default(),
    ignore_argument(tidyselect::peek_data()$prior),
    choose_chained(a = 20),
    choose_chained(b = 20)
  )) {
    result <- summarize_with_margins(
      data, prior = sum(other),
      source = sum(dplyr::pick(c(dplyr::all_of(!!column), -prior))),
      parent = share_of_parent(source), total = share_of_total(source),
      .grouping = rollup(g)
    )
    expect_identical(result$source, c(1, 3, 4))
    expect_identical(result$parent, c(0.25, 0.75, 1))
    expect_identical(result$total, c(0.25, 0.75, 1))
  }
})

test_that("checked share sources preserve ordinary selector errors", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3))
  selections <- list(
    quote(dplyr::all_of("absent")),
    quote(`$`(.data, tidyselect::peek_data()$prior)),
    quote(~tidyselect::peek_data()$prior),
    quote(value && tidyselect::peek_data()$prior)
  )
  diagnostics <- c(
    "Element `absent` doesn't exist", "must be a symbol",
    "Formula shorthand", "Can't use scalar `&&`"
  )
  for (i in seq_along(selections)) {
    ordinary <- rlang::quos(
      prior = sum(value),
      source = sum(dplyr::pick(!!selections[[i]]))
    )
    control <- expect_error(
      summarize_with_margins(data, !!!ordinary, .grouping = rollup(g)),
      diagnostics[[i]]
    )
    shared <- expect_error(summarize_with_margins(
      data, !!!ordinary, share = share_of_total(source), .grouping = rollup(g)
    ), diagnostics[[i]])
    expect_identical(class(shared), class(control))
  }
})

test_that("selector operators bypass lexical bindings in selection syntax", {
  data <- data.frame(g = c("a", "b"), value = c(1, 3), other = c(7, 9))
  c <- function(...) {
    if (tidyselect::peek_data()$prior[[1L]] < 18) "other" else "value"
  }
  result <- summarize_with_margins(
    data, prior = 10 + sum(other),
    source = sum(dplyr::pick(c(value, -prior))),
    total = share_of_total(source), parent = share_of_parent(source),
    .grouping = rollup(g)
  )
  expect_identical(result$source, base::c(1, 3, 4))
  expect_identical(result$total, base::c(0.25, 0.75, 1))
  expect_identical(result$parent, base::c(0.25, 0.75, 1))

  expect_error(summarize_with_margins(
    data, prior = 10 + sum(other),
    source = sum(dplyr::pick(c(dplyr::all_of(c()), -prior))),
    total = share_of_total(source), .grouping = rollup(g)
  ), "depends on earlier summary alias `prior`", class = "marginplyr_error")
})
