test_that("SQLite preserves a .env Grouping dimension in a one-set summary", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(.env = c("east", "west"), value = c(2L, 3L),
               check.names = FALSE),
    "env_dimension_source", temporary = TRUE
  )

  query <- summarize_with_margins(
    input, amount = sum(value),
    .grouping = grouping_set(tidyselect::all_of(".env")),
    .margin_label = NULL, .sort = "none"
  )

  expected <- data.frame(.env = c("east", "west"), amount = c(2L, 3L),
                         check.names = FALSE)
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
})

test_that("SQLite .env rollups retain detail and Grand total keys", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(.env = c("east", "west"), value = c(2L, 3L),
               check.names = FALSE),
    "env_rollup_source", temporary = TRUE
  )

  query <- summarize_with_margins(
    input, amount = sum(value),
    .grouping = rollup(tidyselect::all_of(".env")),
    .margin_label = "Total", .id = "sid", .sort = "last"
  )

  expected <- data.frame(
    .env = c("east", "west", "Total"), sid = c(1L, 1L, 2L),
    amount = c(2L, 3L, 5L), check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_rollup_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
  fields <- DBI::dbGetQuery(con, "PRAGMA table_info(env_rollup_result)")
  expect_identical(fields$name, names(expected))
  expect_identical(fields$type[match(".env", fields$name)], "TEXT")
  expect_match(fields$type[match("sid", fields$name)], "INT")
})

test_that("SQLite .env fixed keys partition missing keys", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(.env = c("a", NA_character_, "a"),
               g = c("east", "east", "west"), value = c(2L, 3L, 4L),
               check.names = FALSE),
    "env_fixed_source", temporary = TRUE
  )

  query <- summarize_with_margins(
    input, amount = sum(value), .by = tidyselect::all_of(".env"),
    .grouping = rollup(g), .margin_label = NULL,
    .id = "sid", .sort = "last"
  )

  expected <- data.frame(
    .env = c("a", "a", "a", NA_character_, NA_character_),
    g = c("east", "west", NA_character_, "east", NA_character_),
    sid = c(1L, 1L, 2L, 1L, 2L), amount = c(2L, 4L, 6L, 3L, 3L),
    check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_fixed_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
})

test_that("SQLite .env inspection and expansion retain their source keys", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(.env = c("east", "west"), value = c(2L, 3L),
               check.names = FALSE),
    "env_controls_source", temporary = TRUE
  )
  spec <- rollup(tidyselect::all_of(".env"))

  plan <- inspect_grouping(input, .grouping = spec)
  expect_identical(plan$set_id, c(1L, 2L))
  expect_identical(plan$included, c("(.env)", "()"))
  expanded <- expand_with_margins(
    input, .grouping = spec, .margin_label = "Total", .id = "sid"
  )
  expected <- data.frame(
    .env = c("east", "west", "Total", "Total"),
    sid = c(1L, 1L, 2L, 2L), value = c(2L, 3L, 2L, 3L),
    check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(expanded)), expected)
})

test_that("SQLite .env fixed keys keep their shape across Margin orders", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(.env = c("a", NA_character_), g = c("east", "west"),
               check.names = FALSE),
    "env_fixed_matrix", temporary = TRUE
  )

  for (label in list(NULL, "Total")) {
    margin <- if (is.null(label)) NA_character_ else label
    for (sort in c("none", "first", "last")) {
      for (with_id in c(FALSE, TRUE)) {
        id <- if (with_id) "sid" else NULL
        query <- summarize_with_margins(
          input, rows = dplyr::n(),
          .by = tidyselect::all_of(".env"), .grouping = rollup(g),
          .margin_label = label, .id = id, .sort = sort
        )
        detail <- data.frame(
          .env = c("a", NA_character_), g = c("east", "west"),
          sid = 1L, rows = 1L, check.names = FALSE
        )
        totals <- data.frame(
          .env = c("a", NA_character_), g = margin,
          sid = 2L, rows = 1L, check.names = FALSE
        )
        expected <- if (identical(sort, "first")) {
          vctrs::vec_rbind(totals[1L, ], detail[1L, ],
                           totals[2L, ], detail[2L, ])
        } else {
          vctrs::vec_rbind(detail[1L, ], totals[1L, ],
                           detail[2L, ], totals[2L, ])
        }
        if (!with_id) {
          expected$sid <- NULL
        }
        actual <- as.data.frame(dplyr::collect(query))
        if (identical(sort, "none")) {
          actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
          expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
          rownames(actual) <- NULL
          rownames(expected) <- NULL
        }
        expect_identical(actual, as.data.frame(expected))
      }
    }
  }
})

test_that("SQLite .env dimensions survive labels, orders, and empty input", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(.env = c("east", "west"), check.names = FALSE),
    "env_dimension_matrix", temporary = TRUE
  )
  case <- 0L

  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) dplyr::filter(source, FALSE) else source
    for (rollup_plan in c(FALSE, TRUE)) {
      spec <- if (rollup_plan) {
        rollup(tidyselect::all_of(".env"))
      } else {
        grouping_set(tidyselect::all_of(".env"))
      }
      for (label in list(NULL, NA_character_, "Total")) {
        for (sort in c("none", "first", "last")) {
          for (with_id in c(FALSE, TRUE)) {
            case <- case + 1L
            id <- if (with_id) "sid" else NULL
            query <- summarize_with_margins(
              input, rows = dplyr::n(), .grouping = spec,
              .margin_label = label, .id = id, .sort = sort
            )
            keys <- if (empty) character() else c("east", "west")
            rows <- if (empty) integer() else c(1L, 1L)
            ids <- rep(1L, length(keys))
            if (rollup_plan) {
              margin <- if (is.null(label) || is.na(label)) {
                NA_character_
              } else {
                label
              }
              keys <- c(keys, margin)
              rows <- c(rows, if (empty) 0L else 2L)
              ids <- c(ids, 2L)
              if (identical(sort, "first")) {
                keys <- c(tail(keys, 1L), head(keys, -1L))
                rows <- c(tail(rows, 1L), head(rows, -1L))
                ids <- c(tail(ids, 1L), head(ids, -1L))
              }
            }
            expected <- if (with_id) {
              data.frame(.env = keys, sid = ids, rows = rows,
                         check.names = FALSE)
            } else {
              data.frame(.env = keys, rows = rows, check.names = FALSE)
            }
            materialized <- dplyr::compute(
              query, name = paste0("env_dimension_case_", case)
            )
            for (value in list(dplyr::collect(query),
                               dplyr::collect(materialized))) {
              actual <- as.data.frame(value)
              want <- expected
              if (identical(sort, "none")) {
                actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
                want <- want[vctrs::vec_order(want), , drop = FALSE]
                rownames(actual) <- NULL
                rownames(want) <- NULL
              }
              if (empty && !rollup_plan) {
                expect_identical(names(actual), names(want))
                expect_identical(actual[[".env"]], character())
                expect_identical(nrow(actual), 0L)
                expect_length(actual$rows, 0L)
                if (with_id) {
                  expect_identical(actual$sid, integer())
                }
              } else {
                expect_identical(actual, want, info = paste("case", case))
              }
            }
            schema <- DBI::dbGetQuery(
              con, paste0("PRAGMA table_info(env_dimension_case_", case, ")")
            )
            expect_identical(schema$name, names(expected))
            expect_identical(schema$type[match(".env", schema$name)], "TEXT")
            if (with_id) {
              expect_match(schema$type[match("sid", schema$name)], "INT")
            }
          }
        }
      }
    }
  }
})
