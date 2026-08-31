# A Margin order is asserted through the rows a public verb returns, not
# through the key builder. ADR 0018 deliberately leaves each adapter to resolve
# the key in whatever its query can name, so an assertion on the builder would
# freeze one of those mechanisms; the tests that render SQL carry only what rows
# cannot show, which is that the mechanism a backend used is the one it owes.

margin_order_data <- function() {
  # Deliberately out of order in both dimensions, so an unordered result and an
  # ordered one cannot coincide.
  data.frame(
    region = c("East", "East", "West", "West"),
    store = c("s2", "s1", "s4", "s3"),
    units = c(1, 2, 4, 8)
  )
}

margin_order_missing_data <- function() {
  data.frame(
    region = c("East", NA, "West"),
    units = c(1, 2, 4)
  )
}

margin_order_by_missing_data <- function() {
  data.frame(
    year = c(2026L, NA, 2025L),
    region = c("West", "East", "East"),
    units = c(1, 2, 4)
  )
}

margin_order_factor_data <- function() {
  data.frame(
    size = ordered(
      c("large", "small", "medium"),
      levels = c("small", "medium", "large")
    ),
    units = c(1, 2, 4)
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
      "`\\.sort` must be one of \"none\", \"last\", or \"first\"\\.",
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

test_that("a Margin order leaves no window ordering to inherit", {
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
  # The key reads its Grouping bits from a column the result does not expose,
  # so no ordering over the columns it does expose reproduces it. What survives
  # the projection is a truncated key that orders a margin row by where its
  # label falls, and replaying that is what `compute()` fails on (#102), so a
  # window function written over the result inherits nothing.
  windowed <- suppressWarnings(
    dbplyr::sql_render(dplyr::mutate(query, running = cumsum(units)))
  )
  expect_match(windowed, "OVER (ROWS", fixed = TRUE)
  expect_false(grepl("OVER (ORDER BY", windowed, fixed = TRUE))

  # The `ORDER BY` the rows arrive in is not what was cleared.
  expect_match(dbplyr::sql_render(query), "ORDER BY", fixed = TRUE)
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

test_that("missing values sort last within a Grouping bit group", {
  result <- summarize_with_margins(
    margin_order_missing_data(),
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region),
    .margin_label = NULL,
    .sort = "last"
  )

  expect_identical(result$region, c("East", "West", NA, NA))
  expect_identical(result$units, c(1, 4, 2, 7))
})

test_that("a fixed key sorts its missing values last", {
  # A `.by` column takes no Grouping bit, so its missingness is the only thing
  # separating it from the dialect's own default — last locally and on DuckDB,
  # first on SQLite.
  result <- summarize_with_margins(
    margin_order_by_missing_data(),
    units = sum(units, na.rm = TRUE),
    .by = year,
    .grouping = rollup(region),
    .sort = "last"
  )

  expect_identical(result$year, c(2025L, 2025L, 2026L, 2026L, NA, NA))
  expect_identical(
    result$region,
    c("East", "Total", "West", "Total", "East", "Total")
  )
  expect_identical(result$units, c(4, 4, 1, 1, 2, 2))
})

# Every spelling that compiles to a plan holding one grouping-set occurrence.
# The bug they shared was not in any of them: a one-occurrence plan makes every
# Grouping bit constant, so the key reads no Grouping set identifier, and the
# projection that dropped the staged one ran after the ordering and took the
# `ORDER BY` with it (#339).
#
# Expressions rather than values, because `.by` and `.grouping` are evaluated
# by the verb and neither survives being computed here. The dimension is
# selected by name for the reason the helper below gives: `codetools` reads
# this closure and cannot follow a bare symbol standing for a column.
margin_order_single_set_specs <- function() {
  list(
    by_only = rlang::exprs(.by = dplyr::all_of("region")),
    grouping_spec = rlang::exprs(
      .grouping = grouping_spec(dplyr::all_of("region"))
    ),
    grouping_sets = rlang::exprs(
      .grouping = grouping_sets(grouping_set(dplyr::all_of("region")))
    ),
    duplicates_dropped = rlang::exprs(
      .grouping = grouping_sets(
        grouping_set(dplyr::all_of("region")),
        grouping_set(dplyr::all_of("region"))
      ),
      .duplicates = "drop"
    ),
    # One occurrence holding two dimensions. Each Grouping bit is constant for
    # the same reason a single dimension's is, and neither reaches the key.
    two_dimensions = rlang::exprs(
      .grouping = grouping_spec(dplyr::all_of(c("region", "store")))
    ),
    # A fixed key alongside the dimension, which contributes its own terms and
    # no Grouping bit.
    by_and_grouping = rlang::exprs(
      .by = dplyr::all_of("store"),
      .grouping = grouping_spec(dplyr::all_of("region"))
    )
  )
}

test_that("a one-occurrence plan carries its `ORDER BY` on a lazy backend", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_sqlite()
  )
  specs <- margin_order_single_set_specs()

  for (name in names(specs)) {
    arguments <- specs[[name]]
    queries <- list(
      summary = rlang::inject(summarize_with_margins(
        remote,
        units = sum(units, na.rm = TRUE),
        !!!arguments,
        .sort = "last"
      )),
      expansion = rlang::inject(expand_with_margins(
        remote,
        !!!arguments,
        .sort = "last"
      ))
    )

    for (verb in names(queries)) {
      query <- queries[[verb]]
      sql <- dbplyr::sql_render(query)
      info <- paste(name, verb)
      expect_match(sql, "ORDER BY", fixed = TRUE, info = info)
      # The key reads no Grouping set identifier here, so the staged column is
      # dropped before the ordering and reaches no query level at all.
      expect_false(
        grepl("..marginplyr_sort_", sql, fixed = TRUE),
        info = info
      )
      expect_false(
        any(grepl(
          "..marginplyr_sort_",
          as.character(dplyr::tbl_vars(query)),
          fixed = TRUE
        )),
        info = info
      )
    }
  }
})

test_that("a one-occurrence plan keeps its native plan and its order", {
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_postgres()
  )
  specs <- margin_order_single_set_specs()

  for (name in names(specs)) {
    query <- rlang::inject(summarize_with_margins(
      remote,
      units = sum(units, na.rm = TRUE),
      !!!specs[[name]],
      .sort = "last"
    ))
    sql <- dbplyr::sql_render(query)
    expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE, info = name)
    expect_false(grepl("UNION ALL", sql, fixed = TRUE), info = name)
    expect_match(sql, "ORDER BY", fixed = TRUE, info = name)
    # The native adapter stages its identifier inside the aggregate query, so
    # the order costs the result no column here either.
    expect_false(
      any(grepl(
        "..marginplyr_sort_",
        as.character(dplyr::tbl_vars(query)),
        fixed = TRUE
      )),
      info = name
    )
  }
})

test_that("`.id` on a one-occurrence plan leaves the key what it was", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_sqlite()
  )
  summarized <- function(...) {
    summarize_with_margins(
      remote,
      units = sum(units, na.rm = TRUE),
      .grouping = grouping_spec(dplyr::all_of("region")),
      .sort = "last",
      ...
    )
  }
  order_by <- function(query) {
    sub(".*ORDER BY", "", dbplyr::sql_render(query))
  }

  identified <- summarized(.id = "set")
  expect_identical(
    as.character(dplyr::tbl_vars(identified)),
    c("region", "set", "units")
  )
  # The identifier the caller asked for is the result's own column, so nothing
  # is dropped after the ordering. The key is the same one either way, the
  # tiebreak having nothing to break.
  expect_identical(order_by(identified), order_by(summarized()))

  local <- summarize_with_margins(
    margin_order_data(),
    units = sum(units, na.rm = TRUE),
    .grouping = grouping_spec(dplyr::all_of("region")),
    .id = "set",
    .sort = "last"
  )
  expect_identical(local$region, c("East", "West"))
  expect_identical(local$set, c(1L, 1L))
})

test_that("`.sort = \"none\"` records no order on a one-occurrence plan", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    margin_order_data(),
    con = dbplyr::simulate_sqlite()
  )
  specs <- margin_order_single_set_specs()

  for (name in names(specs)) {
    arguments <- specs[[name]]
    queries <- list(
      summary = rlang::inject(summarize_with_margins(
        remote,
        units = sum(units, na.rm = TRUE),
        !!!arguments,
        .sort = "none"
      )),
      expansion = rlang::inject(expand_with_margins(
        remote,
        !!!arguments,
        .sort = "none"
      ))
    )
    for (verb in names(queries)) {
      expect_false(
        grepl("ORDER BY", dbplyr::sql_render(queries[[verb]]), fixed = TRUE),
        info = paste(name, verb)
      )
    }
  }
})

test_that("`.sort = \"none\"` clears no window ordering", {
  remote <- dbplyr::window_order(
    dbplyr::tbl_lazy(margin_order_data(), con = dbplyr::simulate_postgres()),
    region
  )

  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = grouping_spec(region),
    .sort = "none"
  )
  # Asking for no order reaches neither the ordering nor the clearing that
  # follows it, so a window ordering the input carried is still what a window
  # function written over the result orders by. Its complement is the test
  # named "a Margin order leaves no window ordering to inherit".
  windowed <- suppressWarnings(
    dbplyr::sql_render(dplyr::mutate(query, running = cumsum(units)))
  )
  expect_match(windowed, "OVER (ORDER BY", fixed = TRUE)
})

# The live backend contracts follow.
#
# Everything above proves the order against a local data frame or against a
# rendered query. Neither answers the question these tests exist for: whether a
# real DuckDB, SQLite, dtplyr, or Arrow run returns the rows in that order.
#
# Each backend gets its own tests. One test branching over several backends
# would not do, and the reason is that no job executes it: every `backend` job
# in `release-matrix.yaml` installs one optional backend and withholds the rest,
# so a test needing two skips in all of them while each job reports green. The
# whole suite is held to that rule now, by `verify-suite-coverage.R` rather than
# by this comment.

# Runs the scenarios a live backend has to reproduce and compares each with the
# local result for the same Grouping specification, so that a backend cannot
# pass by being self-consistently wrong. The literals pinning those local
# results are the assertions earlier in this file.
#
# `as_input()` takes a data frame and a table name and returns the backend's
# representation of it; the name is a parameter because the SQL backends need a
# distinct table per scenario.
#
# A scenario runs a verb rather than always `summarize_with_margins()`, because
# a one-occurrence plan reaches the same finalization from both verbs and #339
# broke it on the expansion of every dbplyr backend. Every one-occurrence
# scenario keys its rows uniquely, so the comparison is a comparison of order:
# the key leaves ties among rows a database is free to return either way.
#
# The dimensions are selected by name rather than as bare symbols, which the
# tests above can write because `test_that()` passes a block rather than defines
# a closure. `codetools` reads the closures below and cannot follow an NSE
# pronoun, so bare symbols here would need one `# nolint` each; `all_of()` is
# the same selection written in a form the linter can see through.
expect_margin_order_agrees <- function(as_input) {
  scenarios <- list(
    # A rollup over two dimensions: subtotals with the rows they summarize.
    rollup = list(
      data = margin_order_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .grouping = rollup(dplyr::all_of(c("region", "store"))),
          .sort = "last"
        )
      },
      columns = c("region", "store", "units")
    ),
    # A source missing value and a margin display alike under
    # `.margin_label = NULL`, and the Grouping bit is what separates them.
    missing = list(
      data = margin_order_missing_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .grouping = rollup(dplyr::all_of("region")),
          .margin_label = NULL,
          .sort = "last"
        )
      },
      columns = c("region", "units")
    ),
    # A fixed key takes no Grouping bit, so its missingness term is the only
    # thing standing between it and the dialect's own default.
    fixed_key = list(
      data = margin_order_by_missing_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .by = dplyr::all_of("year"),
          .grouping = rollup(dplyr::all_of("region")),
          .sort = "last"
        )
      },
      columns = c("year", "region", "units")
    ),
    # A plan holding one grouping-set occurrence, which every Grouping bit is
    # constant over: the order is the fixed key's and the dimension's alone.
    # The staged Grouping set identifier is dropped before the ordering there,
    # and dropping it afterwards is what cost the result its outermost
    # `ORDER BY` (#339).
    single_set = list(
      data = margin_order_missing_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .grouping = grouping_sets(grouping_set(dplyr::all_of("region"))),
          .sort = "last"
        )
      },
      columns = c("region", "units")
    ),
    # The everyday one-occurrence plan: `.by` with no grouping specification at
    # all, so the key holds no dimension term either.
    single_set_by = list(
      data = margin_order_by_missing_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .by = dplyr::all_of("year"),
          .sort = "last"
        )
      },
      columns = c("year", "units")
    ),
    # One occurrence holding two dimensions, which is the shape a `rollup()`
    # scenario comes closest to and still does not reach.
    single_set_dimensions = list(
      data = margin_order_data(),
      run = function(input) {
        summarize_with_margins(
          input,
          units = sum(units, na.rm = TRUE),
          .grouping = grouping_spec(dplyr::all_of(c("region", "store"))),
          .sort = "last"
        )
      },
      columns = c("region", "store", "units")
    ),
    # Expansion always takes the portable path, so a one-occurrence plan is
    # where it lost its order on every dbplyr backend rather than on the ones
    # without native `GROUPING SETS`.
    single_set_expansion = list(
      data = margin_order_data(),
      run = function(input) {
        expand_with_margins(
          input,
          .grouping = grouping_spec(dplyr::all_of(c("region", "store"))),
          .sort = "last"
        )
      },
      columns = c("region", "store", "units")
    )
  )

  for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    local <- scenario$run(scenario$data)
    query <- scenario$run(
      as_input(scenario$data, paste0("margin_order_", name))
    )
    # dbplyr reports a dropped `ORDER BY` as a warning naming `arrange()`, a
    # verb the caller never wrote, so a backend that stops carrying the order
    # is caught here as well as by the rows below (#339).
    remote <- expect_no_warning(dplyr::collect(query))
    for (column in scenario$columns) {
      expect_identical(
        remote[[column]],
        local[[column]],
        info = paste(name, column)
      )
    }
  }

  invisible(NULL)
}

# The `as_input()` a SQL backend needs: each scenario gets its own table on the
# connection the calling test opened, so that a scenario reads the data it named
# rather than whatever the previous one left behind.
copy_to_input <- function(con) {
  function(data, name) {
    dplyr::copy_to(con, data, name, overwrite = TRUE, temporary = TRUE)
  }
}

# Materializing a sorted Margin order, which `vignettes/database_backends.qmd`
# recommends for keeping a result in the database, and which #102 found no
# `.sort` but `"none"` could survive. ADR 0018's second amendment is what the
# rows below are owed.
#
# Every value of the option runs from one place, because what has to hold is
# that materializing keeps whichever order the option asked for. Each is checked
# against the local result for the same specification -- the literals pinning
# those are the assertions earlier in this file -- so that a backend cannot pass
# by materializing its own wrong order faithfully. `"none"` promises no order,
# so it is compared as a set.
expect_computed_margin_order <- function(as_input) {
  columns <- c("region", "store", "units")

  for (sort in margin_sort_choices) {
    summarize_input <- function(input) {
      summarize_with_margins(
        input,
        units = sum(units, na.rm = TRUE),
        .grouping = rollup(dplyr::all_of(c("region", "store"))),
        .sort = sort
      )
    }
    query <- summarize_input(
      as_input(margin_order_data(), paste0("margin_order_compute_", sort))
    )
    computed <- dplyr::compute(query)

    expect_s3_class(computed, "tbl_lazy")
    # The Grouping set identifier the order reads its Grouping bits from is
    # staged inside the query, so materializing must not surface it.
    expect_identical(
      as.character(dplyr::tbl_vars(computed)),
      columns,
      info = sort
    )

    local <- summarize_input(margin_order_data())
    materialized <- dplyr::collect(computed)
    if (identical(sort, "none")) {
      by_key <- function(result) {
        dplyr::arrange(
          result,
          dplyr::across(dplyr::all_of(c("region", "store")))
        )
      }
      local <- by_key(local)
      materialized <- by_key(materialized)
    }
    for (column in columns) {
      expect_identical(
        materialized[[column]],
        local[[column]],
        info = paste(sort, column)
      )
    }
  }

  invisible(NULL)
}

test_that("DuckDB executes a Margin order on its native plan", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_margin_order_agrees(copy_to_input(con))

  data <- margin_order_data()
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_order_native",
    overwrite = TRUE,
    temporary = TRUE
  )
  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  # Asking for a Margin order does not cost the native plan, which is the half
  # of the promise a collected result cannot show. The simulated connections
  # earlier in this file render the same claim; asserting it here too is what
  # ties it to the rows this job collected from a real DuckDB.
  sql <- dbplyr::sql_render(query)
  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_false(grepl("UNION ALL", sql, fixed = TRUE))
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("region", "store", "units")
  )

  # A fixed key's missingness term reaches the aggregate query too, rather than
  # costing the plan the way a staged sort column would.
  fixed_key <- dplyr::copy_to(
    con,
    margin_order_by_missing_data(),
    "margin_order_native_by",
    overwrite = TRUE,
    temporary = TRUE
  )
  expect_match(
    dbplyr::sql_render(summarize_with_margins(
      fixed_key,
      units = sum(units, na.rm = TRUE),
      .by = year,
      .grouping = rollup(region),
      .sort = "last"
    )),
    "GROUP BY GROUPING SETS",
    fixed = TRUE
  )

  # Keeping duplicates does not move the work off the native plan either,
  # because which adapter runs is decided before a Margin order asks for
  # anything.
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

test_that("DuckDB native and portable Margin orders agree", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- margin_order_data()
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_order_adapters",
    overwrite = TRUE,
    temporary = TRUE
  )
  spec <- rollup(region, store)
  summarized <- function(...) {
    summarize_with_margins(
      remote,
      units = sum(units, na.rm = TRUE),
      .grouping = spec,
      .id = "set",
      .sort = "last",
      ...
    )
  }

  # `.id` together with `.duplicates = "keep"` is what sends a DuckDB summary
  # through the portable adapter, and this plan holds no duplicate occurrence,
  # so the two adapters owe the same rows in the same order. Both sides go
  # through the public verb rather than calling `summarize_margin_union()` the
  # way the `.id` agreement test does: ordering happens in the shared finalizer,
  # which an adapter called directly never reaches, so that comparison would put
  # an unordered result against an ordered one.
  #
  # Which adapter each side actually took is therefore asserted rather than
  # assumed. Without this, a change to the native-plan guard would quietly turn
  # the comparison below into native against native.
  native_query <- summarized()
  portable_query <- summarized(.duplicates = "keep")
  expect_match(
    dbplyr::sql_render(native_query),
    "GROUP BY GROUPING SETS",
    fixed = TRUE
  )
  expect_match(
    dbplyr::sql_render(portable_query),
    "UNION ALL",
    fixed = TRUE
  )

  native <- dplyr::collect(native_query)
  portable <- dplyr::collect(portable_query)
  local <- summarize_with_margins(
    data,
    units = sum(units, na.rm = TRUE),
    .grouping = spec,
    .id = "set",
    .sort = "last"
  )

  expect_identical(native, portable)
  expect_identical(native$region, local$region)
  expect_identical(native$store, local$store)
  expect_identical(native$set, local$set)
  expect_identical(portable$region, local$region)
  expect_identical(portable$store, local$store)
  expect_identical(portable$set, local$set)
})

test_that("DuckDB materializes a sorted Margin result with `compute()`", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_computed_margin_order(copy_to_input(con))

  # A caller's own `.id` is the Grouping set identifier the order reads, so it
  # stays in the result and the ordering terms derived from it are the ones a
  # projection cannot prune. Materializing has to survive that too.
  remote <- dplyr::copy_to(
    con,
    margin_order_data(),
    "margin_order_compute_id",
    overwrite = TRUE,
    temporary = TRUE
  )
  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .id = "set",
    .sort = "last"
  )
  computed <- dplyr::compute(query)
  expect_identical(
    as.character(dplyr::tbl_vars(computed)),
    c("region", "store", "set", "units")
  )
  materialized <- dplyr::collect(computed)
  local <- summarize_with_margins(
    margin_order_data(),
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .id = "set",
    .sort = "last"
  )
  for (column in c("region", "store", "set", "units")) {
    expect_identical(materialized[[column]], local[[column]], info = column)
  }
})

test_that("DuckDB orders a factor dimension by its restored levels", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- margin_order_factor_data()
  DBI::dbWriteTable(con, "margin_order_factor", data)

  result <- dplyr::collect(summarize_with_margins(
    dplyr::tbl(con, "margin_order_factor"),
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(size),
    .sort = "last"
  ))

  # Level order, not the alphabetical order the rendered labels would give.
  expect_true(is.factor(result$size))
  expect_identical(
    as.character(result$size),
    c("small", "medium", "large", "Total")
  )
})

test_that("RSQLite executes a portable Margin order end to end", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_margin_order_agrees(copy_to_input(con))

  data <- margin_order_data()
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_order_portable",
    temporary = TRUE
  )
  query <- summarize_with_margins(
    remote,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE)
  # The per-branch identifier the order is derived from stays inside the query.
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("region", "store", "units")
  )

  # Level order is not asserted here: the SQLite backend cannot restore
  # factors, so the dimension arrives as the character values the branches
  # carried and orders by those.
  factors <- dplyr::copy_to(
    con,
    margin_order_factor_data(),
    "margin_order_sqlite_factor",
    temporary = TRUE
  )
  result <- dplyr::collect(summarize_with_margins(
    factors,
    units = sum(units, na.rm = TRUE),
    .grouping = rollup(size),
    .sort = "last"
  ))
  expect_identical(result$size, c("large", "medium", "small", "Total"))
})

test_that("RSQLite materializes a portable Margin order with `compute()`", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # The portable adapter carries its Grouping set identifier as a per-branch
  # literal inside a `UNION ALL`, so this is the other shape the order can take.
  expect_computed_margin_order(copy_to_input(con))
})

test_that("RSQLite places missing values where its own default would not", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- margin_order_missing_data()
  remote <- dplyr::copy_to(con, data, "margin_order_na", temporary = TRUE)

  # SQLite is the one backend whose own ordering puts missing values first,
  # which is the disagreement the promise exists to remove. The test above
  # already compared these rows with the local result; restating them as
  # literals is what makes the comparison evidence rather than a coincidence,
  # because a Margin order that had simply inherited the dialect's default would
  # agree with local everywhere except here.
  expect_identical(
    dplyr::collect(dplyr::arrange(remote, region))$region,
    c(NA, "East", "West")
  )

  expect_identical(
    dplyr::collect(summarize_with_margins(
      remote,
      units = sum(units, na.rm = TRUE),
      .grouping = rollup(region),
      .margin_label = NULL,
      .sort = "last"
    ))$region,
    c("East", "West", NA, NA)
  )

  # A fixed key takes no Grouping bit, so nothing but its own missingness term
  # separates it from that default.
  fixed <- dplyr::copy_to(
    con,
    margin_order_by_missing_data(),
    "margin_order_by_na",
    temporary = TRUE
  )
  result <- dplyr::collect(summarize_with_margins(
    fixed,
    units = sum(units, na.rm = TRUE),
    .by = year,
    .grouping = rollup(region),
    .sort = "last"
  ))
  expect_identical(result$year, c(2025L, 2025L, 2026L, 2026L, NA, NA))
  expect_identical(
    result$region,
    c("East", "Total", "West", "Total", "East", "Total")
  )
})

test_that("dtplyr executes a Margin order end to end", {
  skip_if_suggest_absent("dtplyr")

  expect_margin_order_agrees(function(data, name) dtplyr::lazy_dt(data))

  # Ordering does not collect the step.
  lazy <- summarize_with_margins(
    dtplyr::lazy_dt(margin_order_data()),
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  expect_s3_class(lazy, "dtplyr_step")
})

test_that("dtplyr orders a factor dimension by its restored levels", {
  skip_if_suggest_absent("dtplyr")

  data <- margin_order_factor_data()
  result <- dplyr::collect(summarize_with_margins(
    dtplyr::lazy_dt(data),
    units = sum(units),
    .grouping = rollup(size),
    .sort = "last"
  ))

  expect_true(is.ordered(result$size))
  expect_identical(
    as.character(result$size),
    c("small", "medium", "large", "Total")
  )
  expect_identical(
    as.character(result$size),
    as.character(summarize_with_margins(
      data,
      units = sum(units),
      .grouping = rollup(size),
      .sort = "last"
    )$size)
  )
})

test_that("Arrow executes a Margin order end to end", {
  skip_if_suggest_absent("arrow")

  expect_margin_order_agrees(function(data, name) arrow::as_arrow_table(data))

  # Ordering does not execute the query.
  lazy <- summarize_with_margins(
    arrow::as_arrow_table(margin_order_data()),
    units = sum(units),
    .grouping = rollup(region, store),
    .sort = "last"
  )
  expect_s3_class(lazy, "arrow_dplyr_query")

  # Level order is not asserted here: the Arrow backend cannot restore
  # factors, so an ordered dimension arrives as character and orders by those
  # values.
  result <- dplyr::collect(summarize_with_margins(
    arrow::as_arrow_table(margin_order_factor_data()),
    units = sum(units),
    .grouping = rollup(size),
    .sort = "last"
  ))
  expect_identical(result$size, c("large", "medium", "small", "Total"))
})
