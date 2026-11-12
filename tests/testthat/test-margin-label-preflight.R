test_that("known invalid Margin label names precede DuckDB metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(fixed = 1L, g = "a", h = "b", value = 1L),
    "label_preflight"
  )
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  cases <- list(
    list(label = c(g = "All", h = "All", ghost = "All"),
         message = "unknown dimension name:\ni `ghost`"),
    list(label = list(g = "All", h = NULL, fixed = "All"),
         message = "must not name fixed `\\.by` column:\ni `fixed`"),
    list(label = c(g = "All"),
         message = "must name every Margin dimension\\.\ni Missing `h`")
  )
  for (verb in list(summarize_with_margins, expand_with_margins)) {
    for (case in cases) {
      expect_error(
        verb(source, .by = fixed, .grouping = rollup(g, h),
             .margin_label = case$label),
        case$message,
        class = "marginplyr_error"
      )
      expect_identical(counter$queries, 0L)
      expect_identical(last_sent_queries()$purpose, character())
    }
  }
  expect_error(
    expand_with_margins(
      source, .grouping = rollup(g, h),
      .margin_label = c(g = "All", h = "All", ghost = "All")
    ),
    "unknown dimension name:\ni `ghost`",
    class = "marginplyr_error"
  )
  expect_identical(counter$queries, 0L)
  expect_identical(last_sent_queries()$purpose, character())
})

test_that("a grouped lazy input's fixed-key refusals send no query", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(fixed = 1L, g = "a", value = 1L), "grouped_preflight"
  )
  grouped <- dplyr::group_by(source, fixed)
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  for (verb in list(summarize_with_margins, expand_with_margins)) {
    expect_error(
      verb(grouped, .grouping = rollup(fixed, g)),
      "Fixed grouping column of `.data` cannot also appear in `.grouping`",
      fixed = TRUE,
      class = "marginplyr_error"
    )
    expect_error(
      verb(grouped, .grouping = rollup(g),
           .margin_label = c(g = "All", fixed = "All")),
      "must not name fixed grouping column of `.data`",
      fixed = TRUE,
      class = "marginplyr_error"
    )
    expect_identical(counter$queries, 0L)
    expect_identical(last_sent_queries()$purpose, character())
  }
})

test_that("static summary selections precede label names without metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", h = "b", value = 1L),
    "summary_label_preflight"
  )
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  expect_error(
    summarize_with_margins(
      source, dplyr::across(unknown, sum), .grouping = rollup(g, h),
      .margin_label = c(g = "All")
    ),
    "Column `unknown` doesn't exist",
    class = "vctrs_error_subscript_oob"
  )
  expect_identical(counter$queries, 0L)
  expect_identical(last_sent_queries()$purpose, character())
})

test_that("both Nest verbs diagnose known label names before unsafe dtplyr", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(
    data.frame(fixed = 1L, g = "a", h = "b", value = 1L)
  ) |>
    dplyr::mutate(value = mean(value))
  cases <- list(
    list(label = list(g = "All", h = NULL, ghost = "All"),
         message = "unknown dimension name:\ni `ghost`"),
    list(label = c(g = "All", h = "All", fixed = "All"),
         message = "must not name fixed `\\.by` column:\ni `fixed`"),
    list(label = list(g = NULL),
         message = "must name every Margin dimension\\.\ni Missing `h`")
  )
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (case in cases) {
      expect_error(
        verb(source, .by = fixed, .grouping = rollup(g, h),
             .margin_label = case$label),
        case$message,
        class = "marginplyr_error"
      )
    }
    expect_error(
      verb(source, .by = fixed, .grouping = rollup(g, h), .key = "g",
           .margin_label = c(g = "All", h = "All", ghost = "All")),
      "must not be a grouping column",
      class = "marginplyr_error"
    )
    expect_error(
      verb(source, .by = fixed, .grouping = rollup(g, h),
           .margin_label = c(g = "All", h = "All")),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
  }
})

test_that("unresolved typed selections defer Margin label name checks", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(fixed = 1L, g = "a", h = "b", value = 1L),
    "typed_label_preflight"
  )
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  expect_error(
    expand_with_margins(
      source, .by = fixed, .grouping = rollup(where(is.character)),
      .margin_label = c(g = "All")
    ),
    "must name every Margin dimension\\.\ni Missing `h`",
    class = "marginplyr_error"
  )
  expect_identical(counter$queries, 2L)
  expect_identical(last_sent_queries()$purpose,
                   c("relation_catalog", "selection_proxy"))

  counter$queries <- 0L
  expect_error(
    expand_with_margins(
      source, .by = where(is.integer), .grouping = rollup(g, h),
      .margin_label = c(g = "All", h = "All", ghost = "All")
    ),
    "unknown dimension name:\ni `ghost`",
    class = "marginplyr_error"
  )
  expect_identical(counter$queries, 2L)
  expect_identical(last_sent_queries()$purpose,
                   c("relation_catalog", "selection_proxy"))
})

test_that("valid named labels retain their mapping across plan selections", {
  source <- data.frame(fixed = 1L, g = "a", h = "b", value = 1L)
  for (verb in list(summarize_with_margins, expand_with_margins,
                    nest_with_margins, nest_by_with_margins)) {
    for (spec in list(rollup(g, h), rollup(where(is.character)),
                      rollup(g, where(is.character) & h))) {
      result <- verb(
        source, .by = fixed, .grouping = spec,
        .margin_label = list(h = NULL, g = "All"), .sort = "last"
      )
      expect_identical(result$g, c("a", "a", "All"))
      expect_identical(result$h, c("b", NA_character_, NA_character_))
    }
  }
})

test_that("partial dimensions defer label names and summary selections", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("payload evaluated")
  step <- dtplyr::lazy_dt(data.frame(g = "a", h = "b", value = 1L)) |>
    dplyr::mutate(value = probe(value))
  for (verb in list(summarize_with_margins, expand_with_margins,
                    nest_with_margins, nest_by_with_margins)) {
    for (labels in list(c(g = "All", h = "All"), c(g = "All"),
                        c(ghost = "All"))) {
      expect_error(
        verb(step, .grouping = rollup(g, where(is.character) & h),
             .margin_label = labels),
        "Can't safely determine column metadata", class = "marginplyr_error"
      )
    }
  }
  expect_error(
    summarize_with_margins(
      step, dplyr::across(unknown, sum),
      .grouping = rollup(g, where(is.character) & h)
    ),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
})
