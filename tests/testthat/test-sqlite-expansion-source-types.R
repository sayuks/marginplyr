test_that("SQLite unsorted finite expansion keeps missing source types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_), "finite_missing_dimension"
  )
  query <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL
  )

  expect_identical(dplyr::collect(query, n = 1L)$g, NA_character_)
})

test_that("SQLite finite expansion keeps source schema and row copies", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  plans <- list(
    rollup = rollup(grouping_set(text, integer, double)),
    detail_first = grouping_sets(
      grouping_set(text, integer, double), grouping_set()
    ),
    total_first = grouping_sets(
      grouping_set(), grouping_set(text, integer, double)
    )
  )
  for (missing in c(TRUE, FALSE)) {
    values <- if (missing) {
      data.frame(text = NA_character_, integer = NA_integer_, double = NA_real_,
                 payload = 7L)
    } else {
      data.frame(text = "a", integer = 2L, double = 3.5, payload = 7L)
    }
    source <- dplyr::copy_to(
      con, values, "finite_source_schema", overwrite = TRUE
    )
    # The unexpanded source is an independent driver-level type oracle.
    oracle <- dplyr::collect(source)
    expect_identical(as.data.frame(oracle), values)
    margin <- oracle
    for (column in c("text", "integer", "double")) {
      margin[[column]] <- oracle[[column]][NA_integer_]
    }
    for (kind in names(plans)) {
      for (label in list(NULL, NA_character_)) {
        for (id in list(NULL, "sid")) {
          for (sort in c("none", "first", "last")) {
            info <- paste(missing, kind, label, id, sort, collapse = " ")
            detail <- oracle
            total <- margin
            if (!is.null(id)) {
              detail$sid <- if (identical(kind, "total_first")) 2L else 1L
              total$sid <- 3L - detail$sid
            }
            expected <- if (identical(sort, "first")) {
              dplyr::bind_rows(total, detail)
            } else {
              dplyr::bind_rows(detail, total)
            }
            columns <- c("text", "integer", "double", id, "payload")
            expected <- expected[columns]
            query <- expand_with_margins(
              source, .grouping = plans[[kind]], .margin_label = label,
              .id = id, .sort = sort
            )
            for (n in c(0L, 1L, 2L, Inf)) {
              result <- dplyr::collect(query, n = n)
              rows <- seq_len(min(n, 2L))
              expect_identical(names(result), columns, info = info)
              expect_identical(nrow(result), length(rows), info = info)
              if (identical(sort, "none")) {
                expect_identical(class(result), class(oracle), info = info)
              }
              for (column in c("text", "integer", "double", "payload")) {
                expect_identical(typeof(result[[column]]),
                                 typeof(oracle[[column]]), info = info)
              }
              if (!is.null(id)) {
                expect_type(result$sid, "integer")
              }
              if (identical(sort, "none") && length(rows) == 1L) {
                # An unsorted prefix may choose either grouping-set copy.
                expect_true(any(vapply(1:2, function(i) {
                  identical(as.list(result), as.list(expected[i, ]))
                }, logical(1))), info = info)
              } else if (identical(sort, "none")) {
                expect_identical(
                  as.data.frame(dplyr::arrange(
                    result, dplyr::across(dplyr::everything())
                  )),
                  as.data.frame(dplyr::arrange(
                    expected[rows, ], dplyr::across(dplyr::everything())
                  )), info = info
                )
              } else {
                expect_identical(as.list(result), as.list(expected[rows, ]),
                                 info = info)
              }
            }
            materialized <- dplyr::collect(dplyr::compute(query))
            expect_identical(names(materialized), columns, info = info)
            expect_identical(
              as.data.frame(dplyr::arrange(
                materialized, dplyr::across(dplyr::everything())
              )),
              as.data.frame(dplyr::arrange(
                expected, dplyr::across(dplyr::everything())
              )), info = info
            )
          }
        }
      }
    }
    expect_identical(dplyr::collect(source), oracle)
  }
})

test_that("SQLite finite expansion keeps limit validation and warnings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_), "finite_expansion_limits"
  )
  for (id in list(NULL, "sid")) {
    query <- expand_with_margins(
      source, .grouping = rollup(g), .margin_label = NULL, .id = id
    )
    for (n in list(-1L, "1", c(1L, 2L))) {
      expect_error(dplyr::collect(query, n = n), "non-negative integer")
    }
    expect_error(dplyr::collect(query, n = 1.5), "datatype mismatch")
    for (warn in c(FALSE, TRUE)) {
      for (n in c(0L, 1L, 2L, Inf)) {
        expect_no_warning(result <- dplyr::collect(
          query, n = n, warn_incomplete = warn
        ))
        expect_identical(result$g, rep(NA_character_, min(n, 2L)))
      }
    }
  }
})

test_that("SQLite finite expansion preserves construction laziness and audit", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_), "finite_expansion_audit"
  )
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  build <- function(id, label) {
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) stop("unexpected source query"),
      dbSendStatement = function(...) stop("unexpected source statement"),
      .package = "DBI"
    )
    expand_with_margins(
      source, .grouping = rollup(g), .margin_label = label, .id = id
    )
  }
  first <- build(NULL, NULL)
  first_record <- last_sent_queries()
  expect_identical(first_record$purpose, "result")
  expect_identical(first_record$sql, as.character(dbplyr::sql_render(first)))
  second <- build("sid", NA_character_)
  second_record <- last_sent_queries()
  expect_identical(second_record$purpose, "result")
  expect_identical(second_record$sql, as.character(dbplyr::sql_render(second)))
  expect_false(identical(first_record$sql, second_record$sql))

  DBI::dbExecute(con, "INSERT INTO finite_expansion_audit VALUES (NULL)")
  expect_identical(dplyr::collect(first)$g, rep(NA_character_, 4L))
  expect_identical(dplyr::collect(first, n = 1L)$g, NA_character_)
  expect_identical(last_sent_queries(), second_record)
  materialized <- dplyr::compute(first)
  expect_identical(dplyr::collect(materialized)$g, rep(NA_character_, 4L))
  expect_identical(last_sent_queries(), second_record)
})

test_that("SQLite expansion copies each evaluated SQL payload", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TEMPORARY TABLE MARGINPLYR_EXPAND_SOURCE_1 AS",
    "SELECT 'detail' AS g, 7 AS v"
  ))
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT g, v, hex(randomblob(16)) AS token",
    "FROM MARGINPLYR_EXPAND_SOURCE_1"
  )), vars = c("g", "v", "token"))
  plan <- grouping_sets(grouping_set(g), grouping_set(), grouping_set(g))

  query <- expand_with_margins(
    source, .grouping = plan, .duplicates = "keep", .id = "set"
  )
  # Collection alone cannot distinguish the caller's connection from another
  # connection with equivalent input.
  expect_identical(dbplyr::remote_con(query), con)
  result <- dplyr::collect(query) |>
    dplyr::arrange(.data$set)

  expect_identical(names(result), c("g", "set", "v", "token"))
  expect_identical(result$g, c("detail", "Total", "detail"))
  expect_identical(result$set, 1:3)
  expect_identical(result$v, c(7L, 7L, 7L))
  expect_true(all(grepl("^[0-9A-F]{32}$", result$token)))
  # SQLite 3.35 introduced the materialization hint for a shared source.
  if (numeric_version(RSQLite::rsqliteVersion()[["library"]]) >= "3.35.0") {
    expect_identical(result$token, rep(result$token[[1L]], 3L))
  }
})

test_that("SQLite sorted one-set expansion keeps unchanged source types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a"), fixed = rep(NA_real_, 2L),
                    payload = rep(NA_integer_, 2L)),
    "expansion_source_types", temporary = TRUE
  )
  query <- expand_with_margins(
    source, .by = fixed, .grouping = grouping_set(g), .sort = "last"
  )
  result <- dplyr::collect(query)
  expect_identical(names(result), c("fixed", "g", "payload"))
  expect_identical(result$fixed, rep(NA_real_, 2L))
  expect_identical(result$g, c("a", "b"))
  expect_identical(result$payload, rep(NA_integer_, 2L))
})

test_that("SQLite expansion orders dimensions by their source collation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con, "CREATE TABLE expansion_collation (g TEXT COLLATE NOCASE, v TEXT)"
  )
  DBI::dbExecute(con, paste(
    "INSERT INTO expansion_collation VALUES ('a', 'a'), ('B', 'B'),",
    "(NULL, NULL)"
  ))
  source <- dplyr::tbl(con, "expansion_collation")
  plan <- grouping_sets(grouping_set(), grouping_set(g))

  query <- expand_with_margins(
    source, .grouping = plan, .margin_label = NULL, .sort = "last", .id = "set"
  )
  expected <- c("a", "B", rep(NA_character_, 4L))
  expect_identical(dplyr::collect(query)$g, expected)
  computed <- dplyr::compute(query, name = "expansion_collation_result")
  expect_identical(dplyr::collect(computed)$g, expected)
})

test_that("SQLite text Margin displays retain source collation in filters", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE facts (g TEXT COLLATE NOCASE)")
  DBI::dbExecute(con, "INSERT INTO facts VALUES ('a')")
  query <- expand_with_margins(dplyr::tbl(con, "facts"), .grouping = rollup(g))

  result <- dplyr::collect(dplyr::filter(query, .data$g == "A"))
  expect_identical(result$g, "a")
})

test_that("SQLite expansion collects Date displays with a text Margin label", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con,
    tibble::tibble(day = as.Date(c("2020-01-01", NA)), payload = 7:8),
    "date_display"
  )
  expected <- tibble::tibble(
    day = c("18262", NA_character_, "Total", "Total"),
    sid = rep(1:2, each = 2L),
    payload = rep(7:8, 2L)
  )

  for (checked in c(FALSE, TRUE)) {
    query <- expand_with_margins(
      source, .grouping = rollup(day), .id = "sid",
      .check_margin_label = checked
    )
    expect_identical(as.character(dplyr::tbl_vars(query)), names(expected))
    expect_no_warning(result <- dplyr::collect(query))
    expect_identical(dplyr::arrange(result, .data$sid, .data$payload), expected)

    sorted <- dplyr::collect(expand_with_margins(
      source, .grouping = rollup(day), .id = "sid", .sort = "last",
      .check_margin_label = checked
    ))
    expect_identical(sorted$sid, expected$sid)
    expect_identical(
      as.data.frame(dplyr::arrange(sorted, .data$sid, .data$payload)),
      as.data.frame(expected)
    )

    materialized <- dplyr::collect(dplyr::compute(query))
    expect_identical(
      dplyr::arrange(materialized, .data$sid, .data$payload), expected
    )
  }

  expected$day <- as.Date(c("2020-01-01", rep(NA_character_, 3L)))
  for (label in list(NULL, NA_character_)) {
    for (sort in c("none", "last")) {
      query <- expand_with_margins(
        source, .grouping = rollup(day), .id = "sid",
        .margin_label = label, .sort = sort
      )
      expect_no_warning(result <- dplyr::collect(query))
      expect_identical(
        as.data.frame(dplyr::arrange(result, .data$sid, .data$payload)),
        as.data.frame(expected)
      )
    }
  }
})

test_that("SQLite expansion keeps source types at direct boundaries", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a", "a"), fixed = rep(NA_real_, 3L),
                    payload = rep(NA_integer_, 3L)),
    "expansion_source_matrix", temporary = TRUE
  )
  plans <- list(one = grouping_set(g), rollup = rollup(g))
  case <- 0L
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) dplyr::filter(source, FALSE) else source
    for (kind in names(plans)) {
      multiple <- identical(kind, "rollup")
      for (label in list("Total", NULL, NA_character_)) {
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid")) {
            info <- paste(empty, multiple, label, sort, id, collapse = " ")
            query <- expand_with_margins(
              input, .by = fixed, .grouping = plans[[kind]],
              .margin_label = label, .sort = sort, .id = id
            )
            expected_names <- c("fixed", "g", id, "payload")
            expect_identical(as.character(dplyr::tbl_vars(query)),
                             expected_names, info = info)
            expected_g <- if (identical(sort, "none")) {
              c("b", "a", "a")
            } else {
              c("a", "a", "b")
            }
            expected_id <- rep(1L, 3L)
            if (multiple) {
              margin <- rep(if (is.null(label)) NA_character_ else label, 3L)
              if (identical(sort, "first")) {
                expected_g <- c(margin, expected_g)
                expected_id <- c(rep(2L, 3L), expected_id)
              } else {
                expected_g <- c(expected_g, margin)
                expected_id <- c(expected_id, rep(2L, 3L))
              }
            }
            if (empty) {
              expected_g <- character()
              expected_id <- integer()
            }
            case <- case + 1L
            name <- paste0("expansion_source_result_", case)
            computed <- dplyr::compute(query, name = name)
            expect_identical(as.character(dplyr::tbl_vars(computed)),
                             expected_names, info = info)
            # The physical schema is the public materialization boundary;
            # tbl_vars() alone cannot reveal hidden columns in its table.
            expect_identical(DBI::dbListFields(con, name), expected_names,
                             info = info)
            for (endpoint in list(query, computed)) {
              for (n in c(Inf, 0L, 1L)) {
                result <- dplyr::collect(endpoint, n = n)
                rows <- seq_len(min(n, length(expected_g)))
                expect_identical(names(result), expected_names, info = info)
                if (identical(sort, "none")) {
                  if (is.infinite(n)) {
                    expect_identical(sort(result$g, na.last = TRUE),
                                     sort(expected_g, na.last = TRUE),
                                     info = info)
                  } else {
                    expect_true(all(result$g %in% expected_g), info = info)
                  }
                } else {
                  expect_identical(result$g, expected_g[rows], info = info)
                }
                expect_identical(result$fixed, rep(NA_real_, length(rows)),
                                 info = info)
                expect_identical(result$payload,
                                 rep(NA_integer_, length(rows)), info = info)
                if (!is.null(id)) {
                  if (identical(sort, "none")) {
                    if (is.infinite(n)) {
                      expect_identical(sort(result$sid), sort(expected_id),
                                       info = info)
                      set <- 2L - as.integer(result$g %in% c("a", "b"))
                      expect_identical(result$sid, set, info = info)
                    } else {
                      expect_true(all(result$sid %in% expected_id), info = info)
                    }
                  } else {
                    expect_identical(result$sid, expected_id[rows], info = info)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("SQLite source-type preservation keeps construction lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", fixed = NA_real_, payload = NA_integer_),
    "expansion_lazy_source", temporary = TRUE
  )
  build <- function(sort) {
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) stop("unexpected source read"),
      .package = "DBI"
    )
    expand_with_margins(
      source, .by = fixed, .grouping = grouping_set(g), .sort = sort
    )
  }
  queries <- lapply(c("none", "first", "last"), build)
  DBI::dbExecute(
    con, "INSERT INTO expansion_lazy_source VALUES ('b', NULL, NULL)"
  )
  for (query in queries) {
    result <- dplyr::collect(query)
    expect_identical(result$g, c("a", "b"))
    expect_identical(result$fixed, rep(NA_real_, 2L))
    expect_identical(result$payload, rep(NA_integer_, 2L))
  }
})
