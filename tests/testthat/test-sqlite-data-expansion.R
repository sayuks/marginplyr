test_that("SQLite directly materializes an expansion identifier named .data", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(g = c("east", "west"), value = c(2L, 3L)),
    "data_expansion_source", temporary = TRUE
  )
  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = ".data",
    .margin_label = NULL, .sort = "last"
  )
  expected <- data.frame(
    g = c("east", "west", NA_character_, NA_character_),
    .data = c(1L, 1L, 2L, 2L), value = c(2L, 3L, 2L, 3L),
    check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  saved <- dplyr::compute(query, name = "data_expansion_result")
  expect_identical(as.data.frame(dplyr::collect(saved)), expected)
})

test_that("SQLite expansion identifiers retain copies across names and order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (payload in c(FALSE, TRUE)) {
    data <- data.frame(g = c("west", "east"), value = c(2L, 3L))
    if (payload) data[[".env"]] <- c(5L, 7L)
    input <- dplyr::copy_to(con, data, "identifier_source", overwrite = TRUE)
    for (empty in c(FALSE, TRUE)) {
      source <- if (empty) dplyr::filter(input, FALSE) else input
      for (label in list(NULL, NA_character_, "Total")) {
        for (sort in c("none", "first", "last")) {
          for (id in c(".data", "sid", ".env")) {
            if (payload && identical(id, ".env")) next
            info <- paste(payload, empty, label, sort, id, collapse = "/")
            query <- expand_with_margins(
              source, .grouping = rollup(g), .id = id,
              .margin_label = label, .sort = sort
            )
            margin <- if (is.null(label)) NA_character_ else label
            expected <- data.frame(
              g = c("west", "east", margin, margin),
              sid = c(1L, 1L, 2L, 2L), value = c(2L, 3L, 2L, 3L)
            )
            names(expected)[[2L]] <- id
            if (payload) expected[[".env"]] <- c(5L, 7L, 5L, 7L)
            positions <- switch(sort, none = 1:4, first = c(3L, 4L, 2L, 1L),
                                last = c(2L, 1L, 3L, 4L))
            expected <- expected[if (empty) integer() else positions, ]
            rownames(expected) <- NULL
            saved <- dplyr::compute(query, name = "identifier_result",
                                    overwrite = TRUE)
            expect_identical(DBI::dbListFields(con, "identifier_result"),
                             names(expected), info = info)
            for (actual in list(dplyr::collect(query), dplyr::collect(saved))) {
              actual <- as.data.frame(actual)
              if (!identical(sort, "none")) {
                expect_identical(actual$g, expected$g, info = info)
                expect_identical(actual[[id]], expected[[id]], info = info)
              }
              # Margin order leaves ties between copies of omitted keys free.
              actual <- actual[order(actual[[id]], actual$value), ]
              target <- expected[order(expected[[id]], expected$value), ]
              rownames(actual) <- rownames(target) <- NULL
              expect_identical(actual, target, info = info)
            }
          }
        }
      }
    }
  }
})

test_that("SQLite owned expansions materialize source columns named .data", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("west", "east"), value = c(2L, 3L))
  for (role in c("payload", "dimension", "fixed")) {
    data[[".data"]] <- if (identical(role, "payload")) c(5L, 7L) else "key"
    input <- dplyr::copy_to(con, data, "data_column_source", overwrite = TRUE)
    for (empty in c(FALSE, TRUE)) {
      source <- if (empty) dplyr::filter(input, FALSE) else input
      for (label in list(NULL, NA_character_, "Total")) {
        for (sort in c("none", "first", "last")) {
          spec <- if (identical(role, "dimension")) {
            rollup(tidyselect::all_of(".data"))
          } else {
            rollup(g)
          }
          fixed <- if (identical(role, "fixed")) ".data" else character()
          query <- expand_with_margins(
            source, .by = tidyselect::all_of(fixed), .grouping = spec,
            .id = "sid", .margin_label = label, .sort = sort
          )
          expected <- as.data.frame(dplyr::collect(query))
          saved <- dplyr::compute(query, name = "data_column_result",
                                  overwrite = TRUE)
          actual <- as.data.frame(dplyr::collect(saved))
          expect_identical(DBI::dbListFields(con, "data_column_result"),
                           names(expected))
          expect_identical(actual, expected,
                           info = paste(role, empty, label, sort))
          ids <- if (identical(sort, "first")) {
            c(2L, 2L, 1L, 1L)
          } else {
            c(1L, 1L, 2L, 2L)
          }
          expect_identical(actual$sid, if (empty) integer() else ids)
          if (!identical(sort, "none")) {
            without_id <- expand_with_margins(
              source, .by = tidyselect::all_of(fixed), .grouping = spec,
              .margin_label = label, .sort = sort
            )
            saved <- dplyr::compute(without_id)
            expect_identical(as.data.frame(dplyr::collect(saved)),
                             expected[setdiff(names(expected), "sid")])
          }
        }
      }
    }
  }
})

test_that("SQLite .data expansion keeps caller transactions and cleanup", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(g = c("east", "west"), value = c(2L, 3L)),
    "data_transaction_source", temporary = TRUE
  )
  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = ".data", .sort = "last"
  )
  DBI::dbExecute(con, "CREATE TABLE main.report (old INT)")
  DBI::dbExecute(con, "INSERT INTO main.report VALUES (42)")
  DBI::dbExecute(con, "CREATE TABLE main.marker (value TEXT)")
  for (flag in c(FALSE, TRUE)) {
    for (finish in c("commit", "rollback")) {
      before <- DBI::dbListTables(con)
      DBI::dbBegin(con)
      DBI::dbExecute(con, "INSERT INTO main.marker VALUES ('caller')")
      for (destination in c("report", "new_report")) {
        expect_error(dplyr::compute(
          query, name = destination, temporary = FALSE, overwrite = TRUE,
          unique_indexes = list("value"), in_transaction = flag
        ), "UNIQUE constraint failed")
        expect_true(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbListTables(con), before)
        expect_identical(DBI::dbReadTable(con, "report")$old, 42L)
      }
      saved <- dplyr::compute(
        query, name = "new_report", temporary = FALSE, in_transaction = flag
      )
      expect_true(RSQLite::sqliteIsTransacting(con))
      expect_identical(dplyr::collect(saved)[[".data"]], c(1L, 1L, 2L, 2L))
      expect_identical(DBI::dbReadTable(con, "marker")$value, "caller")
      if (identical(finish, "commit")) {
        DBI::dbCommit(con)
        expect_true(DBI::dbExistsTable(con, "new_report"))
        expect_identical(DBI::dbReadTable(con, "marker")$value, "caller")
        DBI::dbRemoveTable(con, "new_report")
        DBI::dbExecute(con, "DELETE FROM main.marker")
      } else {
        DBI::dbRollback(con)
        expect_false(DBI::dbExistsTable(con, "new_report"))
        expect_identical(DBI::dbReadTable(con, "marker")$value, character())
      }
    }
  }
})

test_that("SQLite .data expansion compute preserves option handling", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(con, data.frame(g = "east"), "data_option_source")
  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = ".data", .sort = "last"
  )
  expected <- as.data.frame(dplyr::collect(query))
  saved <- dplyr::compute(query)
  expect_identical(as.data.frame(dplyr::collect(saved)), expected)
  saved <- dplyr::compute(
    query, name = "data_options", sql_options = dbplyr::sql_options(cte = TRUE)
  )
  expect_identical(as.data.frame(dplyr::collect(saved)), expected)
  expect_warning(saved <- dplyr::compute(query, cte = TRUE), "deprecated")
  expect_identical(as.data.frame(dplyr::collect(saved)), expected)
  before <- DBI::dbListTables(con)
  expect_error(dplyr::compute(query, temporary = FALSE), "must be provided")
  expect_error(dplyr::compute(
    query, name = "data_exclusive", cte = TRUE,
    sql_options = dbplyr::sql_options(cte = TRUE)
  ), "Exactly one")
  expect_identical(DBI::dbListTables(con), before)
})
