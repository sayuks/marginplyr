test_that("derived SQLite Boolean results keep rows under a shadowing name", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(g = c("b", "a"), payload = c(11L, 22L))
  DBI::dbWriteTable(con, "facts", input)
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .id = "TRUE",
    .margin_label = NULL, .sort = "last"
  )
  derived <- dplyr::filter(query, payload > 0L)
  expected <- data.frame(
    g = c("a", "b", NA, NA), `TRUE` = c(1L, 1L, 2L, 2L),
    payload = c(22L, 11L, 11L, 22L), check.names = FALSE
  )
  canonical <- function(x) {
    x <- as.data.frame(x)
    x <- x[vctrs::vec_order(x), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  expect_identical(canonical(dplyr::collect(derived)), canonical(expected))
  saved <- dplyr::compute(derived, name = "facts", analyze = FALSE)
  expect_identical(canonical(dplyr::collect(saved)), canonical(expected))
  expect_identical(DBI::dbListFields(
    con, DBI::Id(schema = "temp", table = "facts")
  ), names(expected))
  expect_identical(DBI::dbReadTable(
    con, DBI::Id(schema = "main", table = "facts")
  ), input)
})

test_that("derived SQLite qualified sources and other destinations work", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  temp <- DBI::Id(schema = "temp", table = "facts")
  input <- data.frame(g = c("b", "a"), v = c(11L, 22L))
  DBI::dbWriteTable(con, main, input)
  DBI::dbExecute(con, "CREATE TEMP TABLE facts AS SELECT 'keep' AS g, 99 AS v")
  expected <- data.frame(g = c("a", "b", NA, NA),
                         `FALSE` = c(1L, 1L, 2L, 2L),
                         v = c(22L, 11L, 11L, 22L), check.names = FALSE)
  query <- dplyr::filter(expand_with_margins(
    dplyr::tbl(con, main), .grouping = rollup(g), .id = "FALSE",
    .margin_label = NULL, .sort = "last"
  ), v > 0L)
  rows <- function(x) {
    dplyr::arrange(as.data.frame(dplyr::collect(x)), g, v)
  }
  expected <- dplyr::arrange(expected, g, v)
  for (name in list("saved", temp, NULL)) {
    saved <- dplyr::compute(query, name = name, overwrite = !is.null(name),
                            indexes = if (inherits(name, "Id")) list() else
                              list("FALSE"), analyze = TRUE)
    expect_identical(rows(saved), expected)
    expect_identical(DBI::dbReadTable(con, main), input)
    expect_identical(rows(query), expected)
    expect_identical(names(dplyr::collect(saved)), names(expected))
  }
})

test_that("derived SQLite refuses when safe publication is unavailable", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(g = c("b", "a"), v = c(11L, 22L))
  DBI::dbWriteTable(con, "facts", input)
  query <- dplyr::filter(expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .id = "TrUe",
    .margin_label = NULL, .sort = "last"
  ), v > 0L)
  expected <- dplyr::collect(query)
  get_query <- DBI::dbGetQuery
  execute <- DBI::dbExecute
  writes <- character()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (identical(statement, "PRAGMA legacy_alter_table")) {
        return(data.frame())
      }
      get_query(conn, statement, ...)
    },
    dbExecute = function(conn, statement, ...) {
      if (grepl("^(CREATE|DROP|ALTER|INSERT|DELETE|UPDATE)", statement)) {
        writes <<- c(writes, statement)
      }
      execute(conn, statement, ...)
    },
    .package = "DBI"
  )
  expect_error(
    dplyr::compute(query, name = "facts"),
    "Choose a different destination name", class = "marginplyr_error"
  )
  expect_identical(writes, character())
  expect_identical(DBI::dbGetQuery(
    con, "SELECT name FROM temp.sqlite_master"
  )$name, character())
  expect_identical(DBI::dbReadTable(con, "facts"), input)
  expect_identical(dplyr::collect(query), expected)
  expect_identical(as.data.frame(dplyr::collect(dplyr::compute(
    query, name = "retry", analyze = FALSE
  ))), as.data.frame(expected))
})

test_that("SQLite Boolean construction and ordinary derivation stay lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "facts", data.frame(g = "a", v = 11L))
  source <- dplyr::tbl(con, "facts")
  build <- function() {
    unexpected <- function(...) stop("unexpected database execution")
    local_mocked_bindings(
      dbSendQuery = unexpected, dbSendStatement = unexpected,
      dbExecute = unexpected, dbGetQuery = unexpected,
      dbExistsTable = unexpected, .package = "DBI"
    )
    query <- expand_with_margins(
      source, .grouping = rollup(g), .id = "FaLsE", .sort = "last",
      .margin_label = NULL
    )
    derived <- list(
      dplyr::filter(query, v > 0L), dplyr::mutate(query, v = v + 1L),
      dplyr::select(query, g, v, dplyr::all_of("FaLsE")),
      dplyr::arrange(query, g), utils::head(query, 3L), dplyr::collapse(query)
    )
    for (result in derived) expect_s3_class(dbplyr::sql_render(result), "sql")
    derived
  }
  derived <- build()
  expect_false(RSQLite::sqliteIsTransacting(con))
  expect_identical(DBI::dbListTables(con), "facts")
  DBI::dbExecute(con, "INSERT INTO facts VALUES ('b', 22)")
  expect_identical(nrow(dplyr::collect(derived[[1L]])), 4L)
  expect_identical(sort(dplyr::collect(derived[[2L]])$v), c(12L, 12L, 23L, 23L))
})

test_that("SQLite derived destination safety spans Boolean roles and verbs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  temp <- DBI::Id(schema = "temp", table = "facts")
  canonical <- function(x) {
    x <- as.data.frame(x)
    x <- x[vctrs::vec_order(x), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  case <- 0L
  for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    for (role in c("identifier", "payload")) {
      payload <- if (role == "payload") word else "v"
      id <- if (role == "identifier") word else "sid"
      input <- data.frame(g = c("b", "a"), v = c(11L, 22L))
      names(input)[[2L]] <- payload
      DBI::dbWriteTable(con, main, input, overwrite = TRUE)
      transforms <- list(
        filter = function(x) dplyr::filter(x, .data[[payload]] > 0L),
        mutate = function(x) dplyr::mutate(x, extra = .data[[payload]] + 100L),
        select = function(x) {
          dplyr::select(x, dplyr::all_of(c(payload, id, "g")))
        },
        arrange = function(x) {
          dplyr::arrange(x, dplyr::desc(.data[[payload]]), g)
        },
        head = function(x) {
          utils::head(dplyr::arrange(
            x, dplyr::desc(.data[[payload]]), dplyr::desc(g)
          ), 3L)
        }
      )
      for (sort in c("none", "first", "last")) {
        expected <- data.frame(
          g = c("b", "a", NA, NA), sid = c(1L, 1L, 2L, 2L),
          v = c(11L, 22L, 11L, 22L)
        )
        names(expected) <- c("g", id, payload)
        if (sort == "last") expected <- expected[c(2L, 1L, 3L, 4L), ]
        if (sort == "first") expected <- expected[c(3L, 4L, 2L, 1L), ]
        for (verb in names(transforms)) {
          case <- case + 1L
          name <- list("facts", "FACTS", temp,
                       dbplyr::in_schema("temp", "FACTS"))[[
            (case - 1L) %% 4L + 1L
          ]]
          info <- paste(word, role, sort, verb, case)
          reference <- canonical(transforms[[verb]](expected))
          for (existing in c(FALSE, TRUE)) {
            if (existing) DBI::dbWriteTable(con, temp, input)
            query <- expand_with_margins(
              dplyr::tbl(con, "facts"), .grouping = rollup(g),
              .id = id, .margin_label = NULL, .sort = sort
            )
            derived <- transforms[[verb]](query)
            expect_identical(canonical(dplyr::collect(derived)), reference,
                             info = info)
            if (existing) {
              expect_error(
                dplyr::compute(derived, name = name, overwrite = TRUE,
                               analyze = FALSE),
                "Choose a different destination name",
                class = "marginplyr_error"
              )
              expect_identical(DBI::dbReadTable(con, temp, check.names = FALSE),
                               input, info = info)
              expect_identical(canonical(dplyr::collect(derived)), reference,
                               info = info)
              saved <- dplyr::compute(derived, name = "retry", analyze = FALSE)
              expect_identical(canonical(dplyr::collect(saved)), reference,
                               info = info)
              DBI::dbRemoveTable(con, "retry")
            } else {
              saved <- dplyr::compute(derived, name = name, analyze = FALSE)
              expect_identical(canonical(dplyr::collect(saved)), reference,
                               info = info)
              expect_identical(DBI::dbListFields(con, temp), names(reference),
                               info = info)
            }
            expect_identical(DBI::dbReadTable(con, main, check.names = FALSE),
                             input, info = info)
            DBI::dbRemoveTable(con, temp)
          }
        }
      }
    }
  }
})

test_that("derived SQLite compute preserves caller transaction ownership", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  temp <- DBI::Id(schema = "temp", table = "facts")
  input <- data.frame(g = c("b", "a"), v = c(11L, 22L))
  DBI::dbWriteTable(con, main, input)
  DBI::dbExecute(con, "CREATE TABLE sentinel (v INT)")
  for (flag in c(FALSE, TRUE)) {
    for (commit in c(FALSE, TRUE)) {
      for (outcome in c("success", "refusal", "failure")) {
        existing <- outcome == "refusal"
        if (existing) DBI::dbWriteTable(con, temp, input)
        query <- dplyr::filter(expand_with_margins(
          dplyr::tbl(con, "facts"), .grouping = rollup(g), .id = "TRUE",
          .margin_label = NULL, .sort = "last"
        ), v > 0L)
        expected <- as.data.frame(dplyr::collect(query))
        catalog <- DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_master")
        DBI::dbBegin(con)
        DBI::dbExecute(con, "INSERT INTO sentinel VALUES (1)")
        DBI::dbExecute(con, "SAVEPOINT caller_checkpoint")
        DBI::dbExecute(con, "INSERT INTO sentinel VALUES (2)")
        result <- tryCatch(dplyr::compute(
          query, name = "facts", overwrite = existing, analyze = FALSE,
          unique_indexes = if (outcome == "failure") list("TRUE") else list(),
          in_transaction = flag
        ), error = identity)
        if (outcome == "success") {
          expect_identical(as.data.frame(dplyr::collect(result)), expected)
        } else {
          if (existing) {
            expect_s3_class(result, "marginplyr_error")
            expect_identical(DBI::dbReadTable(con, temp), input)
          } else {
            expect_s3_class(result, "error")
            expect_false(inherits(result, "marginplyr_error"))
            expect_match(conditionMessage(result$parent),
                         "UNIQUE constraint failed", fixed = TRUE)
          }
          expect_identical(DBI::dbGetQuery(
            con, "SELECT * FROM temp.sqlite_master"
          ), catalog)
          expect_identical(as.data.frame(dplyr::collect(query)), expected)
        }
        expect_identical(DBI::dbReadTable(con, main), input)
        expect_true(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbReadTable(con, "sentinel")$v, c(1L, 2L))
        DBI::dbExecute(con, "ROLLBACK TO caller_checkpoint")
        DBI::dbExecute(con, "RELEASE caller_checkpoint")
        expect_identical(DBI::dbReadTable(con, "sentinel")$v, 1L)
        if (commit) DBI::dbCommit(con) else DBI::dbRollback(con)
        expect_false(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbReadTable(con, "sentinel")$v,
                         if (commit) 1L else integer())
        expect_identical(DBI::dbGetQuery(
          con, "SELECT * FROM temp.sqlite_master"
        ), catalog)
        if (existing) DBI::dbRemoveTable(con, temp)
        DBI::dbExecute(con, "DELETE FROM sentinel")
      }
    }
  }
})

test_that("derived SQLite self-overwrite refuses before changing the input", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(g = c("b", "a"), payload = c(11L, 22L))
  DBI::dbWriteTable(con, "facts", input)
  DBI::dbExecute(con, "CREATE TEMP TABLE facts AS SELECT * FROM main.facts")
  query <- dplyr::filter(expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .id = "FaLsE",
    .margin_label = NULL, .sort = "last"
  ), payload > 0L)
  expected <- dplyr::collect(query)
  catalog <- DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_master")
  execute <- DBI::dbExecute
  writes <- character()
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      if (grepl("^(CREATE|DROP|ALTER|INSERT|DELETE|UPDATE)", statement)) {
        writes <<- c(writes, statement)
      }
      execute(conn, statement, ...)
    },
    .package = "DBI"
  )
  for (name in list("facts", "FACTS", DBI::Id(schema = "temp",
                                              table = "facts"),
                    dbplyr::in_schema("temp", "FACTS"))) {
    expect_error(
      dplyr::compute(query, name = name, overwrite = TRUE, analyze = FALSE),
      "Choose a different destination name", class = "marginplyr_error"
    )
    expect_identical(writes, character())
    expect_identical(DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_master"),
                     catalog)
    expect_identical(DBI::dbReadTable(
      con, DBI::Id(schema = "temp", table = "facts")
    ), input)
    expect_identical(dplyr::collect(query), expected)
  }
  saved <- dplyr::compute(query, name = "retry", analyze = FALSE)
  expect_identical(as.data.frame(dplyr::collect(saved)),
                   as.data.frame(expected))
  expect_identical(DBI::dbReadTable(
    con, DBI::Id(schema = "main", table = "facts")
  ), input)
})
