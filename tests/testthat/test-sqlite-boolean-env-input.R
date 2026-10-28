test_that("SQLite expands Boolean-word source names beside .env", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)

  for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    data <- stats::setNames(data.frame("a", 1L), c(word, ".env"))
    source <- dplyr::copy_to(con, data, "boolean_env_source", overwrite = TRUE)

    expect_identical(as.data.frame(dplyr::collect(source)), data, info = word)
    plan <- inspect_grouping(source, .format = "list")
    expect_identical(plan$set_id, 1L, info = word)
    expect_identical(plan$included, list(character()), info = word)
    query <- expand_with_margins(source)
    expect_identical(last_sent_queries()$purpose, "result", info = word)
    expect_identical(as.data.frame(dplyr::collect(query)), data, info = word)
    computed <- dplyr::compute(query, name = "boolean_env_identity",
                               overwrite = TRUE)
    expect_identical(as.data.frame(dplyr::collect(computed)), data, info = word)
    expect_identical(DBI::dbListFields(con, "boolean_env_identity"),
                     names(data), info = word)
  }
})

test_that("SQLite rollups preserve Boolean-word and .env keys and payloads", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)

  for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    data <- data.frame(key = c("b", "a"), .env = c("y", "x"),
                       value = c(2L, 3L), check.names = FALSE)
    names(data)[[1L]] <- word
    source <- dplyr::copy_to(con, data, "boolean_env_rollup", overwrite = TRUE)
    for (dimension in c(word, ".env")) {
      other <- if (identical(dimension, word)) ".env" else word
      id <- if (identical(dimension, word)) "sid" else
        if (tolower(word) == "true") "FALSE" else "TRUE"
      for (label in list(NULL, NA_character_, "Total")) {
        for (sort in c("none", "first", "last")) {
          info <- paste(word, dimension, label, sort, sep = "/")
          query <- expand_with_margins(
            source, .grouping = rollup(tidyselect::all_of(dimension)),
            .id = id, .margin_label = label, .sort = sort
          )
          expect_identical(last_sent_queries()$purpose, "result", info = info)
          margin <- if (is.null(label)) NA_character_ else label
          expected <- data.frame(
            key = c(data[[dimension]], margin, margin),
            sid = c(1L, 1L, 2L, 2L),
            payload = rep(data[[other]], 2L), value = c(2L, 3L, 2L, 3L)
          )
          names(expected) <- c(dimension, id, other, "value")
          expected <- expected[order(expected[[id]], expected$value), ]
          rownames(expected) <- NULL
          computed <- dplyr::compute(query, name = "boolean_env_result",
                                     overwrite = TRUE)
          schema <- DBI::dbGetQuery(
            con, "PRAGMA table_info(boolean_env_result)"
          )
          expect_identical(schema$name, names(expected), info = info)
          expect_identical(schema$type, c("TEXT", "INT", "TEXT", "INT"),
                           info = info)
          results <- list(dplyr::collect(query), dplyr::collect(computed))
          for (result in results) {
            actual <- as.data.frame(result)
            if (!identical(sort, "none")) {
              expected_ids <- if (identical(sort, "first")) {
                c(2L, 2L, 1L, 1L)
              } else {
                c(1L, 1L, 2L, 2L)
              }
              expect_identical(actual[[id]], expected_ids, info = info)
              expect_identical(actual$value[actual[[id]] == 1L], c(3L, 2L),
                               info = info)
            }
            actual <- actual[order(actual[[id]], actual$value), ]
            rownames(actual) <- NULL
            expect_identical(actual, expected, info = info)
          }
        }
      }
    }
  }
})

test_that("SQLite limited summaries keep Boolean-word and .env sources", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  data <- data.frame(`TRUE` = c("b", "a", "c"), .env = c(20L, 10L, 30L),
                     value = c(2L, 3L, 7L), check.names = FALSE)
  source <- dplyr::copy_to(con, data, "boolean_env_summary")
  offset <- 4L
  source <- dplyr::mutate(source, adjusted = value + .env$offset)
  source <- utils::head(dplyr::arrange(source, value), 2L)
  query <- summarize_with_margins(
    source, rows = dplyr::n(), amount = sum(adjusted) + .env$offset,
    .grouping = rollup(tidyselect::all_of(".env")), .id = "sid",
    .margin_label = NULL, .sort = "last"
  )
  expect_identical(last_sent_queries()$purpose, "result")
  expected <- data.frame(.env = c(10L, 20L, NA_integer_),
                         sid = c(1L, 1L, 2L), rows = c(1L, 1L, 2L),
                         amount = c(11L, 10L, 17L), check.names = FALSE)
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "boolean_env_summary_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
})

test_that("SQLite one-name inputs keep generated identifiers", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cases <- list(c("g", ".env", "TRUE"), c("TRUE", ".data", ".env"))
  for (case in cases) {
    data <- stats::setNames(data.frame("a", 1L), case[1:2])
    source <- dplyr::copy_to(con, data, "boolean_env_control", overwrite = TRUE)
    query <- expand_with_margins(source, .id = case[[3L]])
    expected <- data.frame(id = 1L, data, check.names = FALSE)
    names(expected)[[1L]] <- case[[3L]]
    expect_identical(as.data.frame(dplyr::collect(query)), expected)
    computed <- dplyr::compute(query, name = "boolean_env_control_result",
                               overwrite = TRUE)
    expect_identical(as.data.frame(dplyr::collect(computed)), expected)
  }
})

test_that("SQLite rollups can omit both Boolean-word and .env dimensions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(`TRUE` = c("b", "a"), .env = c("y", "x"),
                     value = c(2L, 3L), check.names = FALSE)
  source <- dplyr::copy_to(con, data, "boolean_env_dimensions")
  for (label in list(NULL, NA_character_, "Total")) {
    for (sort in c("none", "first", "last")) {
      query <- expand_with_margins(
        source, .grouping = rollup(tidyselect::all_of(c("TRUE", ".env"))),
        .id = "sid", .margin_label = label, .sort = sort
      )
      margin <- if (is.null(label)) NA_character_ else label
      expected <- data.frame(
        `TRUE` = c("b", "a", "b", "a", margin, margin),
        .env = c("y", "x", margin, margin, margin, margin),
        sid = c(1L, 1L, 2L, 2L, 3L, 3L), value = rep(c(2L, 3L), 3L),
        check.names = FALSE
      )
      expected <- expected[order(expected$sid, expected$value), ]
      rownames(expected) <- NULL
      computed <- dplyr::compute(query, name = "boolean_env_dimensions_result",
                                 overwrite = TRUE)
      for (result in list(dplyr::collect(query), dplyr::collect(computed))) {
        actual <- as.data.frame(result)
        if (!identical(sort, "none")) {
          ids <- if (identical(sort, "first")) c(3L, 3L, 2L, 1L, 2L, 1L) else
            c(1L, 2L, 1L, 2L, 3L, 3L)
          expect_identical(actual$sid, ids, info = paste(label, sort))
        }
        actual <- actual[order(actual$sid, actual$value), ]
        rownames(actual) <- NULL
        expect_identical(actual, expected, info = paste(label, sort))
      }
    }
  }
})

test_that("SQLite preserves types for empty Boolean-word and .env results", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(`TRUE` = NA_character_, .env = NA_integer_,
                     value = 2L, check.names = FALSE)
  input <- dplyr::copy_to(con, data, "boolean_env_types")
  for (empty in c(FALSE, TRUE)) {
    source <- if (empty) dplyr::filter(input, value < 0L) else input
    for (dimension in c("TRUE", ".env")) {
      for (sort in c("none", "first", "last")) {
        query <- expand_with_margins(
          source, .grouping = rollup(tidyselect::all_of(dimension)),
          .id = "sid", .margin_label = NULL, .sort = sort
        )
        computed <- dplyr::compute(query, name = "boolean_env_types_result",
                                   overwrite = TRUE)
        schema <- DBI::dbGetQuery(
          con, "PRAGMA table_info(boolean_env_types_result)"
        )
        positions <- match(c("TRUE", ".env", "sid"), schema$name)
        expect_identical(schema$type[positions], c("TEXT", "INT", "INT"))
        results <- list(dplyr::collect(query), dplyr::collect(computed),
                        dplyr::collect(query, n = 0L))
        for (i in seq_along(results)) {
          result <- results[[i]]
          expect_identical(typeof(result[["TRUE"]]), "character")
          expect_identical(typeof(result[[".env"]]), "integer")
          expect_identical(typeof(result$sid), "integer")
          expect_identical(nrow(result), if (empty || i == 3L) 0L else 2L)
        }
      }
    }
  }
})

test_that("SQLite fixed keys compose Boolean-word and .env columns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(`TRUE` = c("b", "a"), .env = c("y", "x"),
                     value = c(2L, 3L), check.names = FALSE)
  source <- dplyr::copy_to(con, data, "boolean_env_fixed")
  for (fixed in c("TRUE", ".env")) {
    dimension <- if (identical(fixed, "TRUE")) ".env" else "TRUE"
    query <- expand_with_margins(
      source, .by = tidyselect::all_of(fixed),
      .grouping = rollup(tidyselect::all_of(dimension)), .id = "sid",
      .margin_label = NULL, .sort = "last"
    )
    expected <- data.frame(
      fixed = rep(data[[fixed]][2:1], each = 2L),
      dimension = c(data[[dimension]][[2L]], NA_character_,
                    data[[dimension]][[1L]], NA_character_),
      sid = c(1L, 2L, 1L, 2L), value = c(3L, 3L, 2L, 2L)
    )
    names(expected)[1:2] <- c(fixed, dimension)
    expect_identical(as.data.frame(dplyr::collect(query)), expected)
    computed <- dplyr::compute(query, name = "boolean_env_fixed_result",
                               overwrite = TRUE)
    expect_identical(as.data.frame(dplyr::collect(computed)), expected)
  }
})

test_that("SQLite summaries can group a Boolean-word source beside .env", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  data <- data.frame(`TRUE` = c("b", "a"), .env = c(2L, 1L),
                     value = c(3L, 5L), check.names = FALSE)
  source <- dplyr::copy_to(con, data, "boolean_env_grouped_summary")
  offset <- 2L
  source <- dplyr::mutate(source, adjusted = value + .env$offset)
  for (input in c("plain", "limited", "ordered", "both")) {
    limited <- input %in% c("limited", "both")
    ordered <- input %in% c("ordered", "both")
    selected <- if (ordered) dplyr::arrange(source, value) else source
    if (limited) selected <- utils::head(selected, 1L)
    for (id in c("sid", "FALSE")) {
      for (sort in c("none", "first", "last")) {
        query <- summarize_with_margins(
          selected, total = sum(adjusted, na.rm = TRUE) + .env$offset,
          .grouping = rollup(tidyselect::all_of("TRUE")), .id = id,
          .margin_label = NULL, .sort = sort
        )
        expected <- if (limited) {
          data.frame(`TRUE` = c("b", NA_character_), sid = c(1L, 2L),
                     total = c(7L, 7L), check.names = FALSE)
        } else {
          data.frame(`TRUE` = c("a", "b", NA_character_),
                     sid = c(1L, 1L, 2L), total = c(9L, 7L, 14L),
                     check.names = FALSE)
        }
        names(expected)[[2L]] <- id
        info <- paste(input, id, sort)
        computed <- dplyr::compute(query, name = "boolean_env_grouped_result",
                                   overwrite = TRUE)
        results <- list(dplyr::collect(query), dplyr::collect(computed))
        for (result in results) {
          actual <- as.data.frame(result)
          if (identical(sort, "first")) {
            expect_identical(actual[[id]][[1L]], 2L, info = info)
          } else if (identical(sort, "last")) {
            expect_identical(tail(actual[[id]], 1L), 2L, info = info)
          }
          actual <- actual[order(actual[[id]], actual[["TRUE"]]), ]
          rownames(actual) <- NULL
          expect_identical(actual, expected, info = info)
        }
      }
    }
  }
})

test_that("SQLite limited counts can omit Boolean-word and .env columns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(`TRUE` = "a", .env = 1L, check.names = FALSE)
  source <- dplyr::copy_to(con, data, "boolean_env_limited_count")
  source <- utils::head(source, 1L)
  expect_identical(as.data.frame(dplyr::collect(source)), data)
  query <- summarize_with_margins(source, rows = dplyr::n())
  expect_identical(as.data.frame(dplyr::collect(query)), data.frame(rows = 1L))
})
