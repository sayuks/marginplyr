test_that("SQLite Boolean-word expansions support limiting and filtering", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", `TRUE` = 7L, check.names = FALSE), "facts"
  )
  query <- expand_with_margins(source, .grouping = rollup(g))
  expected <- data.frame(g = c("a", "Total"), `TRUE` = c(7L, 7L),
                         check.names = FALSE)
  expect_identical(as.data.frame(dplyr::collect(head(query, 1L))),
                   expected[1L, , drop = FALSE])
  expect_identical(as.data.frame(dplyr::collect(dplyr::filter(query, TRUE))),
                   expected)
  computed <- dplyr::compute(query, name = "saved", analyze = FALSE)
  for (control in list(source, computed)) {
    expect_identical(as.data.frame(dplyr::collect(head(control, 1L))),
                     expected[1L, , drop = FALSE])
  }
  # Ordinary SQLite unions can normalize TRUE to column2. This control checks
  # execution and values; the expansion above also promises the public name.
  ordinary <- dplyr::union_all(source, dplyr::mutate(source, g = "Total"))
  for (derived in list(head(ordinary, 1L), dplyr::filter(ordinary, TRUE))) {
    actual <- dplyr::collect(derived)
    expect_identical(actual[[2L]], rep(7L, nrow(actual)))
  }
})

test_that("SQLite Boolean-word expansion schemas compose across shapes", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(p = c("north", "north", "south"),
                      g = c("b", "a", "a"), v = c(7L, 19L, 29L),
                      ..MARGINPLYR_BOOLEAN_1 = c(3L, 5L, 11L))
  sort_rows <- function(data) {
    dplyr::arrange(data, dplyr::across(dplyr::everything()))
  }
  for (word in c("TRUE", "FALSE", "true", "false", "TrUe", "FaLsE")) {
    for (role in c("dimension", "fixed", "payload", "identifier")) {
      fixed <- if (identical(role, "fixed")) word else "p"
      dimension <- if (identical(role, "dimension")) word else "g"
      payload <- if (identical(role, "payload")) word else "v"
      identifier <- if (identical(role, "identifier")) word else "set"
      data <- input
      names(data)[1:3] <- c(fixed, dimension, payload)
      source <- dplyr::copy_to(con, data, "schema_source", overwrite = TRUE)
      for (empty in c(FALSE, TRUE)) {
        selected <- if (empty) head(source, 0L) else source
        for (label in list(NULL, "Total")) {
          for (sort in c("none", "first", "last")) {
            ids <- if (identical(role, "identifier")) {
              list(identifier)
            } else {
              list(NULL, identifier)
            }
            for (id in ids) {
              info <- paste(word, role, empty, label, sort, id)
              query <- expand_with_margins(
                selected, .by = dplyr::all_of(fixed),
                .grouping = rollup(dplyr::all_of(dimension)),
                .margin_label = label, .id = id, .sort = sort
              )
              margin <- if (is.null(label)) NA_character_ else label
              expected <- data.frame(
                p = rep(input$p, 2L), g = c(input$g, rep(margin, 3L)),
                set = rep(1:2, each = 3L), v = rep(input$v, 2L),
                ..MARGINPLYR_BOOLEAN_1 = rep(input$..MARGINPLYR_BOOLEAN_1, 2L)
              )
              names(expected)[1:4] <- c(fixed, dimension, identifier, payload)
              if (identical(sort, "first")) {
                expected <- expected[c(4L, 5L, 2L, 1L, 6L, 3L), ]
              } else if (identical(sort, "last")) {
                expected <- expected[c(2L, 1L, 4L, 5L, 3L, 6L), ]
              }
              if (is.null(id)) expected[[identifier]] <- NULL
              if (empty) expected <- expected[FALSE, , drop = FALSE]
              rownames(expected) <- NULL
              computed <- dplyr::compute(
                query, name = "schema_result", overwrite = TRUE,
                analyze = FALSE
              )
              for (direct in list(query, computed)) {
                actual <- as.data.frame(dplyr::collect(direct))
                if (!identical(sort, "none")) {
                  keys <- c(fixed, dimension, id)
                  expect_identical(actual[keys], expected[keys], info = info)
                }
                expect_identical(sort_rows(actual), sort_rows(expected),
                                 info = info)
              }
              for (derived in list(query, computed)) {
                filtered <- as.data.frame(dplyr::collect(
                  dplyr::filter(derived, TRUE)
                ))
                expect_identical(names(filtered), names(expected), info = info)
                expect_identical(nrow(filtered), nrow(expected), info = info)
                if (!empty) {
                  expect_equal(sort_rows(filtered), sort_rows(expected),
                               info = info)
                }
                limited <- as.data.frame(dplyr::collect(head(derived, 1L)))
                expect_identical(names(limited), names(expected), info = info)
                expect_identical(nrow(limited), if (empty) 0L else 1L,
                                 info = info)
                if (!empty) {
                  expect_identical(nrow(dplyr::anti_join(
                    limited, expected, by = names(expected)
                  )), 0L, info = info)
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("SQLite Boolean-word composition beside .env stays lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(`TRUE` = "a", `FALSE` = 7L, .env = "x",
                    check.names = FALSE), "lazy_source"
  )
  tables <- DBI::dbListTables(con)
  build <- function() {
    unexpected <- function(...) stop("unexpected database execution")
    testthat::local_mocked_bindings(
      dbSendQuery = unexpected, dbSendStatement = unexpected,
      dbExecute = unexpected, dbGetQuery = unexpected,
      dbExistsTable = unexpected, .package = "DBI"
    )
    query <- expand_with_margins(
      source, .grouping = rollup(dplyr::all_of("TRUE")),
      .id = "set", .sort = "last"
    )
    derived <- dplyr::filter(query, TRUE)
    expect_s3_class(dbplyr::sql_render(head(derived, 1L)), "sql")
    derived
  }
  query <- build()
  expect_identical(DBI::dbListTables(con), tables)
  DBI::dbExecute(con, "INSERT INTO lazy_source VALUES ('b', 19, 'y')")
  actual <- as.data.frame(dplyr::collect(query))
  expected <- data.frame(
    `TRUE` = c("a", "b", "Total", "Total"), set = c(1L, 1L, 2L, 2L),
    `FALSE` = c(7L, 19L, 7L, 19L), .env = c("x", "y", "x", "y"),
    check.names = FALSE
  )
  expect_identical(names(actual), names(expected))
  expect_identical(
    dplyr::arrange(actual, .data$set, .data[["FALSE"]]), expected
  )
})

test_that("SQLite Boolean-word aliases survive repeated query layers", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(`TRUE` = c("b", "a"), `FALSE` = c(7L, 19L),
                    .env = c("x", "y"), check.names = FALSE), "layer_source"
  )
  query <- expand_with_margins(
    source, .grouping = rollup(dplyr::all_of("TRUE")), .id = "set"
  )
  derived <- query |>
    dplyr::mutate(..MARGINPLYR_BOOLEAN_1 = 99L) |>
    dplyr::mutate(changed = .data[["FALSE"]] + .data$..MARGINPLYR_BOOLEAN_1) |>
    dplyr::select(-dplyr::all_of("..MARGINPLYR_BOOLEAN_1")) |>
    dplyr::filter(.data$changed > 110L)
  expected <- data.frame(
    `TRUE` = c("a", "Total"), set = c(1L, 2L), `FALSE` = c(19L, 19L),
    .env = c("y", "y"), changed = c(118L, 118L), check.names = FALSE
  )
  for (cte in c(FALSE, TRUE)) {
    actual <- as.data.frame(dplyr::collect(
      derived, sql_options = dbplyr::sql_options(cte = cte)
    ))
    expect_identical(dplyr::arrange(actual, .data$set), expected)
    limited <- as.data.frame(dplyr::collect(derived, n = 1L))
    expect_identical(names(limited), names(expected))
    expect_identical(nrow(limited), 1L)
    expect_identical(limited[["FALSE"]], 19L)
    expect_identical(limited$changed, 118L)
  }
  expect_identical(dplyr::collect(query)[["FALSE"]], c(7L, 19L, 7L, 19L))
})
