# A Margin order is asserted through the rows a public verb returns, not
# through the key builder. ADR 0018 deliberately leaves each adapter to resolve
# the key in whatever its query can name, so an assertion on the builder would
# freeze one of those mechanisms; the rendered-SQL tests at the end of this file
# carry only what rows cannot show.

margin_order_data <- function() {
  # Deliberately out of order in both dimensions, so an unordered result and an
  # ordered one cannot coincide.
  data.frame(
    region = c("East", "East", "West", "West"),
    store = c("s2", "s1", "s4", "s3"),
    units = c(1, 2, 4, 8)
  )
}

test_that("a rollup puts each subtotal after the rows it summarizes", {
  result <- summarize_with_margins(
    margin_order_data(),
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )

  expect_identical(
    result$region,
    c("East", "East", "East", "West", "West", "West", "Total")
  )
  expect_identical(
    result$store,
    c("s1", "s2", "Total", "s3", "s4", "Total", "Total")
  )
  expect_identical(result$units, c(2, 1, 3, 8, 4, 12, 15))
})

test_that("`first` reverses the Grouping bits and nothing else", {
  result <- summarize_with_margins(
    margin_order_data(),
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "first"
  )

  expect_identical(
    result$region,
    c("Total", "East", "East", "East", "West", "West", "West")
  )
  # Values stay ascending within each Grouping bit group: `s1` still precedes
  # `s2`, and only the subtotal moved to the front of its block.
  expect_identical(
    result$store,
    c("Total", "Total", "s1", "s2", "Total", "s3", "s4")
  )
})

test_that("row order is unspecified by default", {
  summarized <- function(...) {
    summarize_with_margins(
      margin_order_data(),
      units = sum(units),
      .grouping = rollup(dplyr::all_of(c("region", "store"))),
      ...
    )
  }

  expect_identical(summarized(), summarized(.sort = "none"))
  expect_false(identical(summarized()$store, summarized(.sort = "last")$store))
})

test_that("all four Margin verbs order their results", {
  data <- margin_order_data()
  spec <- rollup(region, store)
  expected_region <- c("East", "East", "East", "West", "West", "West", "Total")
  expected_store <- c("s1", "s2", "Total", "s3", "s4", "Total", "Total")

  results <- list(
    summary = summarize_with_margins(
      data,
      units = sum(units),
      .grouping = spec,
      .sort = "last"
    ),
    nest = nest_with_margins(data, .grouping = spec, .sort = "last"),
    nest_by = nest_by_with_margins(data, .grouping = spec, .sort = "last")
  )

  for (name in names(results)) {
    result <- results[[name]]
    expect_identical(as.character(result$region), expected_region, info = name)
    expect_identical(as.character(result$store), expected_store, info = name)
  }

  # Expansion emits one copy of every input row per grouping set, so its own
  # key skeleton is what the order applies to.
  expanded <- expand_with_margins(data, .grouping = spec, .sort = "last")
  expect_identical(
    expanded$region,
    c(rep("East", 4L), rep("West", 4L), rep("Total", 4L))
  )
  expect_identical(
    expanded$store,
    c("s1", "s2", "Total", "Total", "s3", "s4", rep("Total", 6L))
  )
})

test_that("`.sort` is validated with the common Margin choice matcher", {
  data <- margin_order_data()
  operations <- list(
    summarize_with_margins = function(sort) {
      summarize_with_margins(data, n = dplyr::n(), .sort = sort)
    },
    expand_with_margins = function(sort) {
      expand_with_margins(data, .sort = sort)
    },
    nest_with_margins = function(sort) nest_with_margins(data, .sort = sort),
    nest_by_with_margins = function(sort) {
      nest_by_with_margins(data, .sort = sort)
    }
  )

  for (verb in names(operations)) {
    # `.sort = TRUE` is what a caller who remembers the option removed in #15
    # writes; the three choices have to be in the error they get back.
    error <- expect_error(
      operations[[verb]](TRUE),
      "`\\.sort` must be one of \"none\", \"last\", \"first\"\\.",
      class = "marginplyr_error"
    )
    expect_identical(rlang::call_name(conditionCall(error)), verb)
    expect_error(operations[[verb]]("descending"), class = "marginplyr_error")
  }
})

test_that("each fixed key is one contiguous, self-contained block", {
  data <- data.frame(
    year = c(2026L, 2025L, 2026L, 2025L),
    region = c("West", "West", "East", "East"),
    units = c(1, 2, 4, 8)
  )

  result <- summarize_with_margins(
    data,
    units = sum(units),
    .by = year,
    .grouping = rollup(region),
    .sort = "last"
  )

  expect_identical(result$year, c(2025L, 2025L, 2025L, 2026L, 2026L, 2026L))
  expect_identical(
    result$region,
    c("East", "West", "Total", "East", "West", "Total")
  )

  # A partition's internal order does not depend on any other partition: the
  # 2026 rows come out the same way when 2025 is not there at all.
  alone <- summarize_with_margins(
    data[data$year == 2026L, ],
    units = sum(units),
    .by = year,
    .grouping = rollup(region),
    .sort = "last"
  )
  expect_identical(alone$region, c("East", "West", "Total"))
})

test_that("a factor dimension orders by its restored levels", {
  data <- data.frame(
    size = ordered(
      c("large", "small", "medium", "small"),
      levels = c("small", "medium", "large")
    ),
    units = c(1, 2, 4, 8)
  )

  result <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(size),
    .sort = "last"
  )

  # Level order, not the alphabetical order the rendered labels would give.
  expect_identical(
    as.character(result$size),
    c("small", "medium", "large", "Total")
  )
  expect_true(is.ordered(result$size))
})

test_that("`.margin_label_position` moves levels and not rows", {
  data <- data.frame(
    size = factor(c("b", "a"), levels = c("a", "b")),
    units = c(1, 2)
  )
  arguments <- list(
    .data = data,
    .grouping = rollup(size),
    .sort = "last"
  )

  last <- rlang::inject(summarize_with_margins(
    !!!arguments,
    units = sum(units)
  ))
  first <- rlang::inject(summarize_with_margins(
    !!!arguments,
    units = sum(units),
    .margin_label_position = "first"
  ))

  expect_identical(as.character(last$size), as.character(first$size))
  expect_identical(levels(last$size), c("a", "b", "Total"))
  expect_identical(levels(first$size), c("Total", "a", "b"))
})

test_that("a composite dimension orders as the one dimension it is", {
  data <- data.frame(
    year = c("2026", "2026", "2025"),
    month = c("Feb", "Jan", "Jan"),
    quarter = c("Q1", "Q1", "Q1"),
    units = c(1, 2, 4)
  )

  result <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(year, grouping_set(quarter, month)),
    .sort = "last"
  )

  expect_identical(
    result$year,
    c("2025", "2025", "2026", "2026", "2026", "Total")
  )
  # The composite's columns share a Grouping bit, so they enter and leave the
  # key together and neither needs a rule of its own.
  expect_identical(
    result$month,
    c("Jan", "Total", "Feb", "Jan", "Total", "Total")
  )
  expect_identical(
    result$quarter,
    c("Q1", "Total", "Q1", "Q1", "Total", "Total")
  )
})

test_that("margins and source missing values separate by position", {
  data <- data.frame(
    region = c("East", NA, "East"),
    units = c(1, 2, 4)
  )

  result <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(region),
    .margin_label = NULL,
    .sort = "last"
  )

  # Both rows display as a missing value under `.margin_label = NULL`, and the
  # Grouping bit is what tells them apart: the source group first, the margin
  # last.
  expect_identical(result$region, c("East", NA, NA))
  expect_identical(result$units, c(5, 2, 7))
})

test_that("duplicate occurrences come out adjacent and in plan order", {
  data <- data.frame(region = c("West", "East"), units = c(1, 2))
  spec <- grouping_sets(
    grouping_set(region),
    grouping_set(region),
    grouping_set()
  )

  identified <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = spec,
    .duplicates = "keep",
    .id = "set",
    .sort = "last"
  )

  expect_identical(
    identified$region,
    c("East", "East", "West", "West", "Total")
  )
  expect_identical(identified$set, c(1L, 2L, 1L, 2L, 3L))

  # Without `.id` there is no column to break the tie and no observable
  # difference to break, so the combination is simply accepted.
  anonymous <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = spec,
    .duplicates = "keep",
    .sort = "last"
  )
  expect_identical(
    anonymous$region,
    c("East", "East", "West", "West", "Total")
  )
  expect_identical(names(anonymous), c("region", "units"))
})

test_that("a Margin order composes with contextual shares", {
  data <- margin_order_data()

  result <- summarize_with_margins(
    data,
    units = sum(units),
    share = share_of_parent(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )

  expect_identical(names(result), c("region", "store", "units", "share"))
  expect_identical(
    result$store,
    c("s1", "s2", "Total", "s3", "s4", "Total", "Total")
  )
  expect_identical(
    result$share,
    c(2 / 3, 1 / 3, 3 / 15, 8 / 12, 4 / 12, 12 / 15, 1)
  )
})

test_that("a native summary keeps its plan and adds no visible column", {
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_postgres()
  )

  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  sql <- dbplyr::sql_render(query)

  expect_s3_class(query, "tbl_lazy")
  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_false(grepl("UNION ALL", sql, fixed = TRUE))
  # The Grouping bits are derived from a Grouping set identifier that the
  # aggregate query computes from `GROUPING()`, so the outermost `ORDER BY`
  # resolves against the `FROM` clause it is attached to.
  expect_match(sql, "GROUPING(\"region\")", fixed = TRUE)
  expect_match(sql, "ORDER BY", fixed = TRUE)
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("region", "store", "units")
  )
})

test_that("a native summary reuses `.id` rather than staging its own", {
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_postgres()
  )

  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region),
    .id = "set",
    .sort = "last"
  )

  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("region", "set", "units")
  )
  expect_match(
    dbplyr::sql_render(query),
    "ORDER BY",
    fixed = TRUE
  )
})

test_that("the portable adapter keeps its branch identifier resolvable", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_sqlite()
  )

  summary <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region),
    .sort = "last"
  )
  expansion <- expand_with_margins(
    remote,
    .grouping = rollup(region),
    .sort = "last"
  )

  for (query in list(summary, expansion)) {
    sql <- dbplyr::sql_render(query)
    expect_s3_class(query, "tbl_lazy")
    expect_match(sql, "UNION ALL", fixed = TRUE)
    # The union is already a subquery, so the per-branch literal stays
    # resolvable in the `FROM` clause after the projection drops it.
    expect_match(sql, "..marginplyr_sort_1", fixed = TRUE)
    expect_match(sql, "ORDER BY", fixed = TRUE)
    expect_false(
      "..marginplyr_sort_1" %in% as.character(dplyr::tbl_vars(query))
    )
  }
  expect_identical(
    as.character(dplyr::tbl_vars(summary)),
    c("region", "units")
  )
  expect_identical(
    as.character(dplyr::tbl_vars(expansion)),
    c("region", "store", "units")
  )
})

test_that("DuckDB orders natively and agrees with local results", {
  skip_if_backend_absent("duckdb", "DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- margin_order_data()
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_order_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  expect_match(
    dbplyr::sql_render(query),
    "GROUP BY GROUPING SETS",
    fixed = TRUE
  )

  native <- dplyr::collect(query)
  local <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  expect_identical(native$region, local$region)
  expect_identical(native$store, local$store)
  expect_identical(native$units, local$units)

  # Keeping duplicates does not move the work off the native plan, because
  # which adapter runs is decided before a Margin order asks for anything.
  duplicated_query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = grouping_sets(grouping_set(region), grouping_set(region)),
    .duplicates = "keep",
    .sort = "last"
  )
  duplicate_sql <- dbplyr::sql_render(duplicated_query)
  expect_match(duplicate_sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_false(grepl("UNION ALL", duplicate_sql, fixed = TRUE))
  expect_identical(
    dplyr::collect(duplicated_query)$region,
    c("East", "East", "West", "West")
  )
})

test_that("DuckDB orders a factor dimension by its restored levels", {
  skip_if_backend_absent("duckdb", "DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    size = ordered(
      c("large", "small", "medium"),
      levels = c("small", "medium", "large")
    ),
    units = c(1, 2, 4)
  )
  DBI::dbWriteTable(con, "margin_order_factor", data)

  result <- dplyr::collect(summarize_with_margins(
    dplyr::tbl(con, "margin_order_factor"),
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(size),
    .sort = "last"
  ))

  expect_identical(
    as.character(result$size),
    c("small", "medium", "large", "Total")
  )
})

test_that("missing values sort last on every backend", {
  data <- data.frame(
    region = c("East", NA, "West"),
    units = c(1, 2, 4)
  )
  expected <- c("East", "West", NA, NA)

  local <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(region),
    .margin_label = NULL,
    .sort = "last"
  )
  expect_identical(local$region, expected)

  if (backend_available("RSQLite") && backend_available("DBI")) {
    # SQLite returns missing values first by default, which is the disagreement
    # the promise exists to remove.
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    remote <- dplyr::copy_to(con, data, "margin_order_na", temporary = TRUE)
    expect_identical(
      dplyr::collect(summarize_with_margins(
        remote,
        units = sum(units, na.rm = TRUE),
        .grouping = rollup(region),
        .margin_label = NULL,
        .sort = "last"
      ))$region,
      expected
    )
  }

  if (backend_available("dtplyr")) {
    expect_identical(
      dplyr::collect(summarize_with_margins(
        dtplyr::lazy_dt(data),
        units = sum(units),
        .grouping = rollup(region),
        .margin_label = NULL,
        .sort = "last"
      ))$region,
      expected
    )
  }

  if (backend_available("arrow")) {
    expect_identical(
      dplyr::collect(summarize_with_margins(
        arrow::as_arrow_table(data),
        units = sum(units),
        .grouping = rollup(region),
        .margin_label = NULL,
        .sort = "last"
      ))$region,
      expected
    )
  }
})

test_that("dtplyr and Arrow agree with local Margin order", {
  data <- margin_order_data()
  local <- summarize_with_margins(
    data,
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )

  if (backend_available("dtplyr")) {
    lazy <- summarize_with_margins(
      dtplyr::lazy_dt(data),
      units = sum(units),
      .grouping = rollup(region, store),
      .sort = "last"
    )
    expect_s3_class(lazy, "dtplyr_step")
    collected <- dplyr::collect(lazy)
    expect_identical(collected$region, local$region)
    expect_identical(collected$store, local$store)
  }

  if (backend_available("arrow")) {
    lazy <- summarize_with_margins(
      arrow::as_arrow_table(data),
      units = sum(units),
      .grouping = rollup(region, store),
      .sort = "last"
    )
    expect_s3_class(lazy, "arrow_dplyr_query")
    collected <- dplyr::collect(lazy)
    expect_identical(collected$region, local$region)
    expect_identical(collected$store, local$store)
  }
})
