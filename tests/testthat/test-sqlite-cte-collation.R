test_that("SQLite unsorted CTE expansion keeps source types and row copies", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (missing in c(TRUE, FALSE)) {
    values <- if (missing) {
      data.frame(text = NA_character_, integer = NA_integer_, double = NA_real_,
                 day = as.Date(NA_character_), payload = 7L)
    } else {
      data.frame(text = "a", integer = 2L, double = 3.5,
                 day = as.Date("2020-01-01"), payload = 7L)
    }
    source <- dplyr::copy_to(con, values, "cte_source_types", overwrite = TRUE)
    oracle <- dplyr::collect(source)
    expect_identical(as.data.frame(oracle), values)
    for (label in list(NULL, NA_character_)) {
      for (id in list(NULL, "sid")) {
        query <- expand_with_margins(
          source, .grouping = rollup(grouping_set(text, integer, double, day)),
          .margin_label = label, .id = id
        )
        detail <- oracle
        total <- oracle
        for (column in c("text", "integer", "double", "day")) {
          total[[column]] <- oracle[[column]][NA_integer_]
        }
        if (!is.null(id)) {
          detail$sid <- 1L
          total$sid <- 2L
        }
        columns <- c("text", "integer", "double", "day", id, "payload")
        expected <- dplyr::bind_rows(detail, total)[columns]
        for (cte in c(FALSE, TRUE)) {
          for (n in c(Inf, 0L, 1L, 2L)) {
            info <- paste(missing, label, id, cte, n, collapse = " ")
            result <- dplyr::collect(
              query, n = n, sql_options = dbplyr::sql_options(cte = cte)
            )
            expect_identical(names(result), columns, info = info)
            expect_identical(nrow(result), as.integer(min(n, 2L)), info = info)
            for (column in names(oracle)) {
              expect_identical(class(result[[column]]),
                               class(oracle[[column]]), info = info)
              expect_identical(typeof(result[[column]]),
                               typeof(oracle[[column]]), info = info)
            }
            if (!is.null(id)) expect_type(result$sid, "integer")
            if (n == 1L) {
              # An unsorted prefix may choose either grouping-set copy.
              expect_true(any(vapply(1:2, function(i) {
                identical(as.list(result), as.list(expected[i, ]))
              }, logical(1))), info = info)
            } else {
              target <- if (n == 0L) expected[FALSE, ] else expected
              expect_identical(
                dplyr::arrange(result, dplyr::across(dplyr::everything())),
                dplyr::arrange(target, dplyr::across(dplyr::everything())),
                info = info
              )
            }
          }
        }
      }
    }
    expect_identical(dplyr::collect(source), oracle)
  }
})

test_that("SQLite direct CTE collection preserves source collation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE facts (g TEXT COLLATE NOCASE)")
  DBI::dbExecute(con, "INSERT INTO facts VALUES ('B'), ('a')")
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = grouping_set(g), .sort = "last"
  )
  for (cte in c(FALSE, TRUE)) {
    actual <- dplyr::collect(
      query, sql_options = dbplyr::sql_options(cte = cte)
    )
    expect_identical(actual$g, c("a", "B"), info = paste("cte", cte))
    first <- dplyr::collect(
      query, n = 1L, sql_options = dbplyr::sql_options(cte = cte)
    )
    expect_identical(first$g, "a", info = paste("cte", cte))
  }
})

test_that("SQLite CTE Margin order retains keys, copies and public names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  profiles <- list(c("p", "g", "sid", "value"),
                   c("TrUe", "FaLsE", "sid", "value"),
                   c("p", "g", "TrUe", "FaLsE"))
  case <- 0L
  for (fixed_collation in c("NOCASE", "BINARY")) {
    for (dimension_collation in c("NOCASE", "BINARY")) {
      for (columns in profiles) {
        case <- case + 1L
        table <- paste0("facts_", case)
        quoted <- DBI::dbQuoteIdentifier(con, columns)
        DBI::dbExecute(con, paste0(
          "CREATE TABLE ", table, " (", quoted[[1L]], " TEXT COLLATE ",
          fixed_collation, ", ", quoted[[2L]], " TEXT COLLATE ",
          dimension_collation, ", ", quoted[[4L]], " INTEGER)"
        ))
        DBI::dbExecute(con, paste0(
          "INSERT INTO ", table,
          " VALUES ('B', 'B', 7), ('B', 'a', 7), ('B', 'a', 7),",
          " ('a', 'B', 7), ('a', 'a', 7), ('a', 'a', 7)"
        ))
        source <- dplyr::tbl(con, table)
        fixed <- if (identical(fixed_collation, "NOCASE")) {
          c("a", "B")
        } else {
          c("B", "a")
        }
        dimension <- if (identical(dimension_collation, "NOCASE")) {
          c("a", "a", "B")
        } else {
          c("B", "a", "a")
        }
        for (omitted in c(FALSE, TRUE)) {
          spec <- if (omitted) rollup(!!rlang::sym(columns[[2L]])) else
            grouping_set(!!rlang::sym(columns[[2L]]))
          for (sort in c("first", "last")) {
            query <- expand_with_margins(
              source, .by = dplyr::all_of(columns[[1L]]), .grouping = spec,
              .id = columns[[3L]], .margin_label = NULL, .sort = sort
            )
            g <- dimension
            id <- rep(1L, 3L)
            if (omitted) {
              g <- c(g, rep(NA_character_, 3L))
              id <- c(id, rep(2L, 3L))
              if (identical(sort, "first")) {
                g <- g[c(4:6, 1:3)]
                id <- id[c(4:6, 1:3)]
              }
            }
            expected <- data.frame(
              p = rep(fixed, each = length(g)), g = rep(g, 2L),
              sid = rep(id, 2L), value = 7L
            )
            names(expected) <- columns
            expect_identical(as.character(dplyr::tbl_vars(query)), columns)
            for (cte in c(FALSE, TRUE)) {
              for (n in c(Inf, 1L)) {
                info <- paste(fixed_collation, dimension_collation, case,
                              omitted, sort, cte, n)
                actual <- dplyr::collect(
                  query, n = n, sql_options = dbplyr::sql_options(cte = cte)
                )
                target <- if (is.infinite(n)) expected else expected[1L, ]
                expect_identical(as.data.frame(actual), target, info = info)
              }
            }
          }
        }
      }
    }
  }
})
