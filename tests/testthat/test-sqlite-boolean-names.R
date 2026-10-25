test_that("SQLite Boolean-word names survive Margin summary composition", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(p = c("north", "north", "south"),
                      g = c("a", "b", "a"), v = c(2L, 3L, 7L))
  case <- 0L
  for (word in c("TRUE", "FALSE", "true", "false", "TrUe", "FaLsE")) {
    for (role in c("dimension", "fixed", "summary", "identifier")) {
      data <- input
      dimension <- if (identical(role, "dimension")) word else "g"
      fixed <- if (identical(role, "fixed")) word else "p"
      output <- if (identical(role, "summary")) word else "amount"
      id <- if (identical(role, "identifier")) word else "set"
      names(data)[1:2] <- c(fixed, dimension)
      source <- dplyr::copy_to(con, data, paste0("source_", case))
      control <- dplyr::summarise(
        source, !!output := sum(v, na.rm = TRUE),
        .by = dplyr::all_of(c(fixed, dimension))
      )
      expect_identical(names(dplyr::collect(control)),
                       c(fixed, dimension, output))
      expect_equal(dplyr::collect(control)[[output]], c(2L, 3L, 7L))
      for (rollup_plan in c(FALSE, TRUE)) {
        for (label in list(NULL, NA_character_, "Total")) {
          for (sort in c("none", "first", "last")) {
            case <- case + 1L
            spec <- if (rollup_plan) rollup(!!rlang::sym(dimension)) else
              grouping_set(!!rlang::sym(dimension))
            query <- summarize_with_margins(
              source, !!output := sum(v, na.rm = TRUE),
              .by = dplyr::all_of(fixed), .grouping = spec,
              .id = id, .margin_label = label, .sort = sort
            )
            margin <- if (is.null(label) || is.na(label)) {
              NA_character_
            } else {
              label
            }
            expected <- data.frame(
              p = c("north", "north", "south"),
              g = c("a", "b", "a"), set = c(1L, 1L, 1L),
              amount = c(2L, 3L, 7L)
            )
            if (rollup_plan) {
              expected <- rbind(expected, data.frame(
                p = c("north", "south"), g = rep(margin, 2L),
                set = c(2L, 2L), amount = c(5L, 7L)
              ))
              if (!identical(sort, "none")) {
                order <- if (identical(sort, "first")) {
                  c(4L, 1L, 2L, 5L, 3L)
                } else {
                  c(1L, 2L, 4L, 3L, 5L)
                }
                expected <- expected[order, , drop = FALSE]
              }
            }
            names(expected) <- c(fixed, dimension, id, output)
            rownames(expected) <- NULL
            destination <- paste0("result_", case)
            computed <- dplyr::compute(query, name = destination)
            for (result in list(dplyr::collect(query),
                                dplyr::collect(computed))) {
              actual <- as.data.frame(result)
              if (identical(sort, "none")) {
                actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
                expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
                rownames(actual) <- rownames(expected) <- NULL
              }
              expect_identical(actual, expected,
                               info = paste(word, role, rollup_plan,
                                            sort, case))
            }
            expect_identical(DBI::dbListFields(con, destination),
                             names(expected))
          }
        }
      }
    }
  }
})

test_that("SQLite Boolean-word summaries keep values and compute options", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", v = 2L, ..MARGINPLYR_BOOLEAN_1 = 4L),
    "boolean_options_source"
  )
  offset <- 3L
  query <- summarize_with_margins(
    source, `TRUE` = sum(.data$v, na.rm = TRUE) + .env$offset,
    `FALSE` = "TRUE", .env = dplyr::n(), .grouping = grouping_set(g)
  )
  expected <- data.frame(g = "a", `TRUE` = 5L, `FALSE` = "TRUE", .env = 1L,
                         check.names = FALSE)
  expect_equal(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(
    query, name = "boolean_options_result", temporary = FALSE,
    indexes = list("true"), unique_indexes = list(c("g", "FALSE")),
    sql_options = dbplyr::sql_options(cte = TRUE)
  )
  expect_equal(as.data.frame(dplyr::collect(computed)), expected)
  index <- DBI::dbGetQuery(con, "PRAGMA index_list(boolean_options_result)")
  expect_identical(nrow(index), 2L)
  indexed <- lapply(index$name, function(name) {
    DBI::dbGetQuery(con, paste0(
      "PRAGMA index_info(", DBI::dbQuoteIdentifier(con, name), ")"
    ))$name
  })
  expect_true(any(vapply(indexed, identical, logical(1), "TRUE")))
  expect_true(any(vapply(indexed, identical, logical(1), c("g", "FALSE"))))
})

test_that("SQLite Boolean-word names retain empty declared types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (value in list(NA_integer_, NA_real_, NA_character_)) {
    source <- dplyr::copy_to(con, data.frame(`TRUE` = value, v = 2L,
                                             check.names = FALSE),
                             paste0("boolean_type_", typeof(value)))
    for (empty in c(FALSE, TRUE)) {
      input <- if (empty) dplyr::filter(source, v < 0L) else source
      for (sort in c("none", "first", "last")) {
        query <- summarize_with_margins(
          input, amount = sum(v, na.rm = TRUE), bit = grouping_bit(`TRUE`),
          .grouping = grouping_set(`TRUE`), .id = "FALSE",
          .margin_label = NULL, .sort = sort
        )
        destination <- paste("boolean_declared", typeof(value), empty, sort,
                             sep = "_")
        computed <- dplyr::compute(query, name = destination)
        for (result in list(dplyr::collect(query), dplyr::collect(computed),
                            dplyr::collect(query, n = 0L))) {
          expect_identical(names(result), c("TRUE", "FALSE", "amount", "bit"))
          expect_identical(typeof(result[["TRUE"]]), typeof(value))
          expect_identical(typeof(result[["FALSE"]]), "integer")
          expect_true(is.numeric(result$bit))
        }
        schema <- DBI::dbGetQuery(con, paste0(
          "PRAGMA table_info(",
          DBI::dbQuoteIdentifier(con, destination), ")"
        ))
        expected_type <- switch(typeof(value), integer = "INT", double = "REAL",
                                character = "TEXT")
        expect_identical(schema$type[1:2], c(expected_type, "INT"))
      }
    }
  }
})

test_that("SQLite Boolean-word names work without a requested identifier", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(`TRUE` = c("a", "b"), v = c(2L, 3L), check.names = FALSE),
    "boolean_no_id"
  )
  for (sort in c("none", "first", "last")) {
    query <- summarize_with_margins(
      source, `FALSE` = sum(v, na.rm = TRUE),
      .grouping = rollup(`TRUE`), .margin_label = NULL, .sort = sort
    )
    expected <- data.frame(`TRUE` = c("a", "b", NA_character_),
                           `FALSE` = c(2L, 3L, 5L), check.names = FALSE)
    if (identical(sort, "first")) expected <- expected[c(3L, 1L, 2L), ]
    rownames(expected) <- NULL
    for (result in list(dplyr::collect(query),
                        dplyr::collect(dplyr::compute(query)))) {
      expect_identical(as.data.frame(result), expected)
    }
    prefix <- dplyr::collect(query, n = 1L)
    expect_identical(as.data.frame(prefix), expected[1L, , drop = FALSE])
  }
})

test_that("SQLite Boolean-word construction leaves the source unread", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(`TRUE` = "a", v = 2L,
                                           check.names = FALSE), "boolean_lazy")
  build <- function() {
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) stop("unexpected source read"),
      .package = "DBI"
    )
    summarize_with_margins(
      dplyr::arrange(source, `TRUE`), `FALSE` = sum(v, na.rm = TRUE),
      .grouping = rollup(`TRUE`), .margin_label = NULL, .sort = "last"
    )
  }
  query <- build()
  expect_s3_class(query, "tbl_lazy")
  DBI::dbExecute(con, "INSERT INTO boolean_lazy VALUES ('a', 3)")
  expect_identical(dplyr::collect(query)[["FALSE"]], c(5L, 5L))
})

test_that("SQLite Boolean-word summary sources survive share joins", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(2L, 3L)),
                           "boolean_shares")
  query <- summarize_with_margins(
    source, `TRUE` = sum(v, na.rm = TRUE), `FALSE` = share_of_total(`TRUE`),
    .grouping = rollup(g), .sort = "last", .check_share_source = FALSE
  )
  for (result in list(dplyr::collect(query),
                      dplyr::collect(dplyr::compute(query)))) {
    expect_identical(names(result), c("g", "TRUE", "FALSE"))
    expect_identical(result[["TRUE"]], c(2L, 3L, 5L))
    expect_equal(result[["FALSE"]], c(0.4, 0.6, 1))
  }
})

test_that("failed Boolean-word materialization restores an overwritten table", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(2L, 2L)),
                           "boolean_rollback_source")
  dplyr::copy_to(con, data.frame(saved = 9L), "boolean_rollback_result")
  query <- summarize_with_margins(
    source, `TRUE` = sum(v, na.rm = TRUE), .grouping = grouping_set(g)
  )
  expect_error(dplyr::compute(
    query, name = "boolean_rollback_result", overwrite = TRUE,
    unique_indexes = list("TRUE")
  ), "UNIQUE constraint failed")
  expect_identical(DBI::dbReadTable(con, "boolean_rollback_result"),
                   data.frame(saved = 9L))
})
