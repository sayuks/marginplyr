# Arrow names an arranged expression by its first deparsed line, so two
# distinct Grouping bits whose identifier vectors wrap after the same prefix
# would share one sort key name (#1570). Each case compares an Arrow input with
# the local data frame the same call is given.

arrow_order_term_data <- function() {
  data.frame(
    region = c("E", "W", "E", "W"),
    channel = c("web", "web", "shop", "shop"),
    segment = c("b2b", "b2c", "b2c", "b2b"),
    brand = c("x", "y", "x", "y"),
    tier = c("hi", "lo", "lo", "hi"),
    size = c("s", "s", "l", "l"),
    season = c("q1", "q2", "q1", "q2"),
    revenue = c(10, 20, 30, 40)
  )
}

arrow_order_term_sources <- function(data) {
  table <- arrow::arrow_table(data)
  list(
    table = table,
    record_batch = arrow::record_batch(data),
    dataset = arrow::InMemoryDataset$create(table)
  )
}

# `call` receives the input and returns the Margin verb's result. Every Arrow
# source must stay lazy, carry the local result's columns and collect to its
# rows in its order.
expect_arrow_order_as_local <- function(call, info) {
  data <- arrow_order_term_data()
  expected <- call(data)
  for (source in names(arrow_order_term_sources(data))) {
    input <- arrow_order_term_sources(data)[[source]]
    query <- call(input)
    expect_s3_class(query, "arrow_dplyr_query")
    collected <- as.data.frame(dplyr::collect(query))
    expect_identical(collected, expected, info = paste(info, source))
  }
}

test_that("Arrow orders a one-way union of five to seven dimensions", {
  skip_if_suggest_absent("arrow")
  dimensions <- c("region", "channel", "segment", "brand", "tier", "size",
                  "season")
  for (width in 5:7) {
    sets <- lapply(dimensions[seq_len(width)], function(name) {
      rlang::expr(grouping_set(!!rlang::sym(name)))
    })
    grouping <- rlang::expr(grouping_sets(!!!sets))
    for (sort in c("last", "first")) {
      expect_arrow_order_as_local(
        function(input) {
          rlang::inject(summarize_with_margins(
            input, revenue = sum(revenue), .grouping = !!grouping,
            .margin_label = NULL, .sort = sort
          ))
        },
        info = paste(width, sort)
      )
    }
  }
})

test_that("Arrow orders rollups under `.id` names of 3 to 64 characters", {
  skip_if_suggest_absent("arrow")
  groupings <- list(
    rollup = rlang::expr(rollup(region, channel)),
    rollups = rlang::expr(grouping_sets(
      rollup(region, channel), rollup(segment, brand, tier)
    ))
  )
  for (width in c(3L, 30L, 60L, 64L)) {
    id <- substr(strrep("margin_grouping_set_identifier_", 3L), 1L, width)
    for (name in names(groupings)) {
      for (sort in c("last", "first")) {
        expect_arrow_order_as_local(
          function(input) {
            rlang::inject(summarize_with_margins(
              input, revenue = sum(revenue), .grouping = !!groupings[[name]],
              .duplicates = "keep", .sort = sort, .id = id
            ))
          },
          info = paste(width, name, sort)
        )
      }
    }
  }
})

test_that("Arrow orders a plan whose duplicate sets were dropped", {
  skip_if_suggest_absent("arrow")
  grouping <- rlang::expr(grouping_sets(
    rollup(region, channel), rollup(region, segment), grouping_set(brand),
    grouping_set(tier), grouping_set(size)
  ))
  for (id in list(NULL, "margin_grouping_set_identifier")) {
    for (sort in c("last", "first")) {
      expect_arrow_order_as_local(
        function(input) {
          rlang::inject(summarize_with_margins(
            input, revenue = sum(revenue), .grouping = !!grouping,
            .duplicates = "drop", .sort = sort, .id = id
          ))
        },
        info = paste(is.null(id), sort)
      )
    }
  }
})

test_that("Arrow orders the expansion verb's wrapped Grouping bits", {
  skip_if_suggest_absent("arrow")
  groupings <- list(
    one_way = rlang::expr(grouping_sets(
      grouping_set(region), grouping_set(channel), grouping_set(segment),
      grouping_set(brand), grouping_set(tier)
    )),
    rollup = rlang::expr(rollup(region, channel))
  )
  for (name in names(groupings)) {
    for (sort in c("last", "first")) {
      expect_arrow_order_as_local(
        function(input) {
          rlang::inject(expand_with_margins(
            input, .grouping = !!groupings[[name]], .sort = sort,
            .id = "margin_grouping_set_identifier"
          ))
        },
        info = paste(name, sort)
      )
    }
  }
})
