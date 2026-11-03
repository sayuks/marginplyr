test_that("SQLite collapse exposes only the sorted Margin public schema", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = 1L), "collapse_source")
  control <- dplyr::collapse(dplyr::arrange(source, g))
  expect_identical(as.data.frame(dplyr::collect(control)), data.frame(g = 1L))
  query <- expand_with_margins(
    source, .grouping = grouping_set(g), .margin_label = NULL, .sort = "last"
  )
  collapsed <- dplyr::collapse(query)
  expect_identical(as.character(dplyr::tbl_vars(collapsed)), "g")
  expect_identical(as.data.frame(dplyr::collect(collapsed)), data.frame(g = 1L))
})

test_that("SQLite collapsed Margin schemas compose across result shapes", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  canonical <- function(x) {
    x <- as.data.frame(lapply(x, as.character), check.names = FALSE)
    x <- x[vctrs::vec_order(x), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  special <- c("g", "TRUE", "FaLsE", "...1", "odd name",
               "..MARGINPLYR_ORDER_1")
  case <- 0L
  for (empty in c(FALSE, TRUE)) {
    for (multiple in c(FALSE, TRUE)) {
      for (label in list(NULL, "Total")) {
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid")) {
            case <- case + 1L
            dimension <- special[[(case - 1L) %% length(special) + 1L]]
            data <- data.frame(g = c("b", "a", "b"), v = c(7L, 11L, 7L))
            names(data)[[1L]] <- dimension
            source <- dplyr::copy_to(con, data, "collapse_shapes",
                                     overwrite = TRUE)
            if (empty) source <- dplyr::filter(source, FALSE)
            spec <- if (multiple) rollup(tidyselect::all_of(dimension)) else
              grouping_set(tidyselect::all_of(dimension))
            query <- expand_with_margins(
              source, .grouping = spec, .id = id,
              .margin_label = label, .sort = sort
            )
            expected <- data
            if (!is.null(id)) expected <- data.frame(
              data[dimension], sid = 1L, data["v"], check.names = FALSE
            )
            if (multiple) {
              margin <- expected
              margin[[dimension]] <- label %||% NA_character_
              if (!is.null(id)) margin$sid <- 2L
              expected <- rbind(expected, margin)
            }
            if (empty) expected <- expected[FALSE, , drop = FALSE]
            collapsed <- dplyr::collapse(query)
            info <- paste(dimension, empty, multiple, sort, id, label)
            expect_identical(as.character(dplyr::tbl_vars(collapsed)),
                             names(expected), info = info)
            selected <- dplyr::select(collapsed, dplyr::everything())
            materializable <- dplyr::rename(
              collapsed, key = tidyselect::all_of(dimension)
            )
            boolean <- tolower(dimension) %in% c("true", "false")
            if (boolean) materializable <- collapsed
            saved <- dplyr::compute(materializable, name = "collapsed_result",
                                    overwrite = TRUE)
            for (result in list(collapsed, selected)) {
              expect_identical(canonical(dplyr::collect(result)),
                               canonical(expected), info = info)
            }
            saved_expected <- expected
            if (!boolean) names(saved_expected)[[1L]] <- "key"
            expect_identical(canonical(dplyr::collect(saved)),
                             canonical(saved_expected), info = info)
            mutated <- dplyr::mutate(collapsed, extra = v + 1L)
            expected$extra <- expected$v + 1L
            expect_identical(canonical(dplyr::collect(mutated)),
                             canonical(expected), info = info)
          }
        }
      }
    }
  }
})

test_that("SQLite direct CTE collection preserves Boolean-word payload names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = "a", `TRUE` = 7L, check.names = FALSE)
  source <- dplyr::copy_to(con, data, "cte_source")
  options <- dbplyr::sql_options(cte = TRUE)
  expect_identical(as.data.frame(dplyr::collect(
    dplyr::filter(source, g == "a"), sql_options = options
  )), data)
  for (sort in c("none", "first", "last")) {
    query <- expand_with_margins(source, .grouping = rollup(g), .sort = sort)
    expected <- data.frame(g = c("a", "Total"), `TRUE` = c(7L, 7L),
                           check.names = FALSE)
    if (identical(sort, "first")) expected <- expected[2:1, ]
    rownames(expected) <- NULL
    expect_identical(as.data.frame(dplyr::collect(query)), expected)
    expect_identical(as.data.frame(dplyr::collect(
      query, sql_options = options
    )), expected)
    expect_match(as.character(dbplyr::sql_render(query, sql_options = options)),
                 "WITH", fixed = TRUE)
  }
})

test_that("SQLite collapsed Boolean names survive ordinary materialization", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", `TRUE` = 7L, check.names = FALSE),
    "collapse_boolean"
  )
  query <- expand_with_margins(source, .grouping = rollup(g), .sort = "last")
  collapsed <- dplyr::collapse(query)
  expected <- data.frame(g = c("a", "Total"), `TRUE` = c(7L, 7L),
                         check.names = FALSE)
  for (cte in c(FALSE, TRUE)) {
    for (input in list(collapsed, dplyr::filter(collapsed, TRUE))) {
      saved <- dplyr::compute(
        input, name = "collapsed_boolean_result", overwrite = TRUE,
        indexes = list("TRUE"), sql_options = dbplyr::sql_options(cte = cte)
      )
      expect_identical(as.data.frame(dplyr::collect(saved)), expected)
    }
  }
  expect_error(dplyr::compute(
    collapsed, name = "collapsed_boolean_result", overwrite = TRUE,
    unique_indexes = list("TRUE")
  ), "UNIQUE constraint failed", fixed = TRUE)
  expect_identical(as.data.frame(dplyr::collect(saved)), expected)
})

test_that("SQLite direct CTE execution keeps public names, copies and types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  options <- dbplyr::sql_options(cte = TRUE)
  canonical <- function(x) {
    x <- as.data.frame(x)
    x <- x[vctrs::vec_order(x), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  case <- 0L
  for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    for (role in c("dimension", "fixed", "payload", "identifier")) {
      fixed <- if (identical(role, "fixed")) word else "p"
      dimension <- if (identical(role, "dimension")) word else "g"
      payload <- if (identical(role, "payload")) word else "v"
      id <- if (identical(role, "identifier")) word else "sid"
      data <- data.frame(p = c("x", "x"), g = c("b", "a"), v = c(7L, 11L),
                         ..MARGINPLYR_BOOLEAN_1 = c(13L, 17L))
      names(data)[1:3] <- c(fixed, dimension, payload)
      source <- dplyr::copy_to(con, data, "cte_shapes", overwrite = TRUE)
      for (empty in c(FALSE, TRUE)) {
        input <- if (empty) dplyr::filter(source, FALSE) else source
        for (multiple in c(FALSE, TRUE)) {
          for (sort in c("none", "first", "last")) {
            case <- case + 1L
            label <- if (case %% 2L == 0L) NULL else "Total"
            spec <- if (multiple) rollup(tidyselect::all_of(dimension)) else
              grouping_set(tidyselect::all_of(dimension))
            query <- expand_with_margins(
              input, .by = tidyselect::all_of(fixed), .grouping = spec,
              .id = id, .margin_label = label, .sort = sort
            )
            expected <- data.frame(data[c(fixed, dimension)], sid = 1L,
                                   data[c(payload, "..MARGINPLYR_BOOLEAN_1")],
                                   check.names = FALSE)
            names(expected)[[3L]] <- id
            if (multiple) {
              margin <- expected
              margin[[dimension]] <- label %||% NA_character_
              margin[[id]] <- 2L
              expected <- rbind(expected, margin)
            }
            if (empty) expected <- expected[FALSE, , drop = FALSE]
            info <- paste(word, role, empty, multiple, sort, label)
            ordinary <- dplyr::collect(dplyr::filter(input, TRUE),
                                       sql_options = options)
            expect_identical(canonical(ordinary), canonical(data[!empty, ]),
                             info = info)
            actual <- as.data.frame(dplyr::collect(
              query, sql_options = options
            ))
            expect_identical(canonical(actual), canonical(expected),
                             info = info)
            expect_identical(actual, as.data.frame(dplyr::collect(query)),
                             info = info)
            saved <- dplyr::compute(
              query, name = "cte_result", overwrite = TRUE, analyze = FALSE,
              sql_options = options
            )
            expect_identical(as.data.frame(dplyr::collect(saved)), actual,
                             info = info)
            for (n in c(0L, 1L)) {
              prefix <- as.data.frame(dplyr::collect(
                query, n = n, sql_options = options
              ))
              expect_identical(prefix, utils::head(actual, n), info = info)
            }
          }
        }
      }
    }
  }
})

test_that("SQLite public and direct CTE boundaries remain lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", `TRUE` = 7L, check.names = FALSE),
    "lazy_boundaries"
  )
  build <- function() {
    unexpected <- function(...) stop("unexpected database execution")
    testthat::local_mocked_bindings(
      dbSendQuery = unexpected, dbSendStatement = unexpected,
      dbExecute = unexpected, dbGetQuery = unexpected,
      dbExistsTable = unexpected, dbListFields = unexpected, .package = "DBI"
    )
    query <- expand_with_margins(source, .grouping = rollup(g), .sort = "first")
    collapsed <- dplyr::collapse(query)
    for (input in list(query, collapsed, dplyr::collapse(collapsed))) {
      expect_s3_class(dbplyr::sql_render(
        input, sql_options = dbplyr::sql_options(cte = TRUE)
      ), "sql")
    }
    list(query, collapsed)
  }
  queries <- build()
  DBI::dbExecute(con, "INSERT INTO lazy_boundaries VALUES ('b', 11)")
  expected <- data.frame(g = c("a", "b", "Total", "Total"),
                         `TRUE` = c(7L, 11L, 7L, 11L), check.names = FALSE)
  for (query in queries) {
    actual <- dplyr::collect(
      query, sql_options = dbplyr::sql_options(cte = TRUE)
    )
    expect_identical(dplyr::arrange(as.data.frame(actual), g, .data[["TRUE"]]),
                     dplyr::arrange(expected, g, .data[["TRUE"]]))
  }
})

test_that("SQLite CTE summaries retain missing and declared output types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  options <- dbplyr::sql_options(cte = TRUE, use_star = FALSE,
                                 qualify_all_columns = TRUE)
  for (value in list(NA_integer_, NA_real_, NA_character_)) {
    source <- dplyr::copy_to(
      con, data.frame(`TrUe` = value, v = 7L, check.names = FALSE),
      "cte_missing", overwrite = TRUE
    )
    for (empty in c(FALSE, TRUE)) {
      input <- if (empty) dplyr::filter(source, FALSE) else source
      for (sort in c("none", "first", "last")) {
        query <- summarize_with_margins(
          input, amount = sum(v, na.rm = TRUE), bit = grouping_bit(TrUe),
          .grouping = grouping_set(TrUe), .id = "FaLsE",
          .margin_label = NULL, .sort = sort
        )
        expected <- as.data.frame(dplyr::collect(query))
        saved <- dplyr::compute(query, name = "cte_missing_result",
                                overwrite = TRUE, sql_options = options)
        results <- list(dplyr::collect(query, sql_options = options),
                        dplyr::collect(saved))
        for (result in results) {
          actual <- as.data.frame(result)
          expect_identical(actual, expected)
          expect_identical(nrow(actual), if (empty) 0L else 1L)
          expect_identical(actual$TrUe, rep(value, nrow(actual)))
          expect_identical(actual$FaLsE, rep(1L, nrow(actual)))
          expect_true(is.numeric(actual$bit))
        }
        prefix <- dplyr::collect(query, n = 0L, sql_options = options)
        expect_type(prefix$TrUe, typeof(value))
        expect_type(prefix$FaLsE, "integer")
      }
    }
  }
})
