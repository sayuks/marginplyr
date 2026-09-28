test_that("SQLite keeps .env as an ordinary summary output", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", v = 2L),
    "env_summary_output", temporary = TRUE
  )
  name <- ".env"
  query <- summarize_with_margins(
    source, !!name := sum(v), .grouping = grouping_set(g), .sort = "none"
  )

  expect_s3_class(query, "tbl_lazy")
  result <- dplyr::collect(query)
  expect_identical(names(result), c("g", ".env"))
  expect_identical(result[[".env"]], 2L)
})

test_that("SQLite .env summary output survives a rollup and materialization", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", v = 2L),
    "env_summary_rollup", temporary = TRUE
  )
  name <- ".env"
  query <- summarize_with_margins(
    source, !!name := dplyr::n(), .grouping = rollup(g),
    .margin_label = "Total", .sort = "last"
  )

  expect_identical(as.character(dplyr::tbl_vars(query)), c("g", ".env"))
  expected <- data.frame(g = c("east", "Total"), .env = c(1L, 1L),
                         check.names = FALSE)
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_summary_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
  expect_identical(DBI::dbListFields(con, "env_summary_result"),
                   c("g", ".env"))
})

test_that("SQLite keeps .env as a one-based Grouping set identifier", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", v = 2L),
    "env_identifier_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, rows = dplyr::n(), .grouping = rollup(g),
    .id = ".env", .sort = "last"
  )

  expect_identical(as.character(dplyr::tbl_vars(query)),
                   c("g", ".env", "rows"))
  expected <- data.frame(g = c("east", "Total"), .env = c(1L, 2L),
                         rows = c(1L, 1L), check.names = FALSE)
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_identifier_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
  expect_identical(DBI::dbListFields(con, "env_identifier_result"),
                   names(expected))
})

test_that("SQLite .env outputs retain values across plans, labels, and order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("east", "east"), v = c(2L, 3L)),
    "env_matrix_source", temporary = TRUE
  )
  case <- 0L
  output <- ".env"

  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) dplyr::filter(source, g == "absent") else source
    for (rollup_plan in c(FALSE, TRUE)) {
      for (label in list(NULL, NA_character_, "Total")) {
        for (sort in c("none", "first", "last")) {
          case <- case + 1L
          summary_query <- if (rollup_plan) {
            summarize_with_margins(
              input, !!output := dplyr::n(), .grouping = rollup(g),
              .margin_label = label, .sort = sort
            )
          } else {
            summarize_with_margins(
              input, !!output := dplyr::n(), .grouping = grouping_set(g),
              .margin_label = label, .sort = sort
            )
          }
          id_query <- if (rollup_plan) {
            summarize_with_margins(
              input, rows = dplyr::n(), .grouping = rollup(g),
              .id = ".env", .margin_label = label, .sort = sort
            )
          } else {
            summarize_with_margins(
              input, rows = dplyr::n(), .grouping = grouping_set(g),
              .id = ".env", .margin_label = label, .sort = sort
            )
          }
          info <- paste("case", case, "empty", empty, "rollup", rollup_plan,
                        "label", if (is.null(label)) "NULL" else
                          if (is.na(label)) "NA" else label,
                        "sort", sort)
          expect_s3_class(summary_query, "tbl_lazy")
          expect_s3_class(id_query, "tbl_lazy")

          g <- if (empty) character() else "east"
          rows <- if (empty) integer() else 2L
          ids <- if (empty) integer() else 1L
          if (rollup_plan) {
            g <- c(g, if (is.null(label) || is.na(label)) {
              NA_character_
            } else {
              label
            })
            rows <- c(rows, if (empty) 0L else 2L)
            ids <- c(ids, 2L)
          }
          if (rollup_plan && !empty && identical(sort, "first")) {
            g <- rev(g)
            rows <- rev(rows)
            ids <- rev(ids)
          }
          expected_summary <- data.frame(g = g, .env = rows,
                                         check.names = FALSE)
          expected_id <- data.frame(g = g, .env = ids, rows = rows,
                                    check.names = FALSE)
          for (kind in c("summary", "identifier")) {
            query <- if (identical(kind, "summary")) {
              summary_query
            } else {
              id_query
            }
            expected <- if (identical(kind, "summary")) {
              expected_summary
            } else {
              expected_id
            }
            destination <- paste0("env_matrix_", kind, "_", case)
            materialized <- dplyr::compute(query, name = destination)
            for (result in list(dplyr::collect(query),
                                dplyr::collect(materialized))) {
              actual <- as.data.frame(result)
              if (identical(sort, "none")) {
                actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
                expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
                rownames(actual) <- NULL
                rownames(expected) <- NULL
              }
              if (empty && !rollup_plan) {
                expect_identical(names(actual), names(expected), info = info)
                expect_identical(actual$g, character(), info = info)
                expect_identical(nrow(actual), 0L, info = info)
                expect_length(actual[[".env"]], 0L)
                if (identical(kind, "identifier")) {
                  expect_length(actual$rows, 0L)
                }
              } else {
                expect_identical(actual, expected,
                                 info = paste(info, kind))
              }
              if (identical(kind, "identifier")) {
                expect_identical(typeof(actual[[".env"]]), "integer",
                                 info = info)
              }
            }
            expect_identical(DBI::dbListFields(con, destination),
                             names(expected), info = paste(info, kind))
          }
        }
      }
    }
  }
})

test_that("SQLite .env pronoun and other output names keep their meanings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", v = 2L),
    "env_pronoun_source", temporary = TRUE
  )
  offset <- 4L
  name <- ".env"
  other <- ".data"
  query <- summarize_with_margins(
    source, "{name}" := dplyr::n() + .env$offset,
    "{other}" := sum(v, na.rm = TRUE),
    .grouping = rollup(g), .sort = "last"
  )
  expect_s3_class(query, "tbl_lazy")
  expected_names <- c("g", ".env", ".data")
  for (result in list(dplyr::collect(query),
                      dplyr::collect(dplyr::compute(
                        query, name = "env_pronoun_result"
                      )))) {
    expect_identical(names(result), expected_names)
    expect_identical(result$g, c("east", "Total"))
    expect_equal(result[[".env"]], c(5, 5))
    expect_equal(result[[".data"]], c(2, 2))
  }
  expect_identical(DBI::dbListFields(con, "env_pronoun_result"),
                   expected_names)
})

test_that("SQLite fixed keys materialize .env outputs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(p = "north", g = "east", v = 2L),
    "env_fixed_source", temporary = TRUE
  )
  output <- ".env"
  summary_query <- summarize_with_margins(
    source, !!output := sum(v, na.rm = TRUE),
    .by = p, .grouping = rollup(g), .sort = "first"
  )
  id_query <- summarize_with_margins(
    source, amount = sum(v, na.rm = TRUE),
    .by = p, .grouping = rollup(g), .id = ".env", .sort = "first"
  )
  expected_summary <- data.frame(
    p = c("north", "north"), g = c("Total", "east"),
    .env = c(2L, 2L), check.names = FALSE
  )
  expected_id <- data.frame(
    p = c("north", "north"), g = c("Total", "east"),
    .env = c(2L, 1L), amount = c(2L, 2L), check.names = FALSE
  )
  for (kind in c("summary", "identifier")) {
    query <- if (identical(kind, "summary")) summary_query else id_query
    expected <- if (identical(kind, "summary")) {
      expected_summary
    } else {
      expected_id
    }
    expect_identical(as.data.frame(dplyr::collect(query)), expected)
    destination <- paste0("env_fixed_", kind)
    computed <- dplyr::compute(
      query, name = destination,
      temporary = identical(kind, "identifier")
    )
    expect_identical(as.data.frame(dplyr::collect(computed)), expected)
    expect_identical(DBI::dbListFields(con, destination), names(expected))
  }
})

test_that("SQLite .env summaries read input when collected", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", v = 2L),
    "env_lazy_source", temporary = TRUE
  )
  output <- ".env"
  summary_query <- summarize_with_margins(
    source, !!output := dplyr::n(), .grouping = grouping_set(g)
  )
  id_query <- summarize_with_margins(
    source, rows = dplyr::n(), .grouping = grouping_set(g),
    .id = ".env"
  )
  expect_s3_class(summary_query, "tbl_lazy")
  expect_s3_class(id_query, "tbl_lazy")
  DBI::dbExecute(con, "INSERT INTO env_lazy_source (g, v) VALUES ('east', 3)")
  expect_identical(dplyr::collect(summary_query)[[".env"]], 2L)
  expect_identical(dplyr::collect(id_query)$rows, 2L)
  expect_identical(dplyr::collect(id_query)[[".env"]], 1L)
  expect_identical(dplyr::collect(dplyr::compute(summary_query))[[".env"]],
                   2L)
})
