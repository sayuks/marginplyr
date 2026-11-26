# A generic SQL backend takes the portable UNION ALL path, whose first operand
# is a zero-row type anchor over the input (#1643).

# The outer clauses of every UNION ALL operand that some later operand follows,
# at whatever subquery depth the compound sits. Text inside an operand's own
# parenthesised subqueries is left out, since it is not that operand's clause.
non_final_union_operands <- function(query) {
  sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
  sql <- gsub(" UNION ALL ", "\001", sql, fixed = TRUE)
  tokens <- regmatches(sql, gregexpr("\\(|\\)|\001|[^()\001]+", sql))[[1L]]
  buffers <- ""
  operands <- character()
  for (token in tokens) {
    depth <- length(buffers)
    if (identical(token, "(")) {
      buffers <- c(buffers, "")
    } else if (identical(token, ")")) {
      buffers <- buffers[-depth]
    } else if (identical(token, "\001")) {
      operands <- c(operands, buffers[[depth]])
      buffers[[depth]] <- ""
    } else {
      buffers[[depth]] <- paste0(buffers[[depth]], token)
    }
  }
  operands
}

limited_generic_inputs <- function(source) {
  list(
    head = head(source, 3L),
    arranged = head(dplyr::arrange(source, .data$a), 3L)
  )
}

test_that("a limited input's row limit stays off non-final operands", {
  for (simulate in c("simulate_mssql", "simulate_mysql", "simulate_oracle",
                     "simulate_dbi")) {
    con <- getExportedValue("dbplyr", simulate)()
    source <- dbplyr::lazy_frame(
      k = 1L, a = "x", v = 1, con = con, .name = "df"
    )
    for (input in limited_generic_inputs(source)) {
      for (sort in c("none", "last", "first")) {
        query <- suppressWarnings(summarize_with_margins(
          input, n = sum(v, na.rm = TRUE), .by = k,
          .grouping = rollup(a), .sort = sort
        ))
        operands <- non_final_union_operands(query)
        expect_gt(length(operands), 0L)
        for (operand in operands) {
          expect_no_match(
            operand, "ORDER BY|LIMIT|\\bTOP\\b|FETCH FIRST",
            info = paste(simulate, sort)
          )
        }
      }
    }
  }
})

test_that("SQL Server keeps no unbounded ORDER BY in a derived table", {
  source <- dbplyr::lazy_frame(
    k = 1L, a = "x", v = 1, con = dbplyr::simulate_mssql(), .name = "df"
  )
  query <- summarize_with_margins(
    head(dplyr::arrange(source, .data$a), 3L), n = sum(v, na.rm = TRUE),
    .by = k, .grouping = rollup(a)
  )
  sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
  # Every ORDER BY in a subquery is bounded by the TOP of the same SELECT.
  selects <- strsplit(sql, "SELECT ", fixed = TRUE)[[1L]]
  ordered <- grep("ORDER BY", selects, value = TRUE)
  expect_true(all(grepl("^TOP ", ordered)))
})

test_that("a generic SQL connection collects a summary over a limited input", {
  skip_if_suggest_absent("duckdb", "DBI")
  duck <- duckdb_test_connection(dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    k = c(1L, 1L, 2L, 2L, 2L), a = c("x", "y", "x", "y", "z"),
    b = c("p", "q", "p", "q", "p"), v = c(1, 2, 3, 4, 5)
  )
  DBI::dbWriteTable(duck, "df", data)
  con <- generic_forwarding_connection(duck)
  source <- dplyr::tbl(con, "df")
  specs <- list(
    rollup = rollup(a, b),
    cube = cube(a, b),
    sets = grouping_sets(
      marginplyr::grouping_set(a), marginplyr::grouping_set()
    )
  )
  summarize_both <- function(data, spec, by, sort, share, id) {
    shares <- if (share) rlang::exprs(s = share_of_parent(n)) else list()
    summarize_with_margins(
      data, n = sum(v, na.rm = TRUE), !!!shares,
      .by = dplyr::all_of(by), .grouping = spec, .sort = sort, .id = id
    )
  }
  for (input in limited_generic_inputs(source)) {
    local <- dplyr::collect(input)
    for (spec in names(specs)) {
      # A Parent share is defined for a pure rollup() only.
      for (share in c(FALSE, if (spec == "rollup") TRUE)) {
        for (by in list(NULL, "k")) {
          for (id in list(NULL, "set")) {
            for (sort in c("none", "last", "first")) {
              run <- function(data) {
                summarize_both(data, specs[[spec]], by, sort, share, id)
              }
              expected <- run(local)
              result <- dplyr::collect(run(input))
              key <- intersect(c(by, "a", "b", id), names(expected))
              expect_equal(
                as.data.frame(
                  dplyr::arrange(result, dplyr::across(all_of(key)))
                ),
                as.data.frame(
                  dplyr::arrange(expected, dplyr::across(all_of(key)))
                ),
                ignore_attr = TRUE, info = paste(spec, share, sort, id)
              )
            }
          }
        }
      }
    }
  }
})

# A sorted share summary with a typed-missing Margin label orders by the
# private set identifier, which the final type anchor's operand does not
# select (#1768).
sorted_typed_missing_share <- function(data, share = "total", sort = "last",
                                       label = list(NULL), grouping = NULL,
                                       by = NULL, id = NULL) {
  grouping <- grouping %||% rollup(dplyr::all_of("a"))
  share_call <- rlang::call2(paste0("share_of_", share), rlang::sym("n"))
  summarize_with_margins(
    data, n = sum(.data$v, na.rm = TRUE), p = !!share_call,
    .by = dplyr::all_of(by), .grouping = grouping,
    .check_share_source = FALSE, .margin_label = label[[1L]], .sort = sort,
    .id = id
  )
}

test_that("a sorted typed-missing share collects in Margin order", {
  skip_if_suggest_absent("duckdb", "DBI")
  duck <- duckdb_test_connection(dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    k = c(1L, 1L, 1L, 2L, 2L, 2L), a = c("x", "y", "x", "y", "z", "z"),
    b = c("p", "q", "p", "q", "p", "q"), v = c(1, 2, 3, 4, 5, 6)
  )
  DBI::dbWriteTable(duck, "df", data)
  source <- dplyr::tbl(generic_forwarding_connection(duck), "df")

  # Per-grain sums divided by the grand total by hand, in Margin order.
  result <- dplyr::collect(sorted_typed_missing_share(source))
  expect_equal(
    as.data.frame(result),
    data.frame(
      a = c("x", "y", "z", NA), n = c(4, 6, 11, 21),
      p = c(4, 6, 11, 21) / 21
    )
  )
  first <- dplyr::collect(
    sorted_typed_missing_share(source, share = "parent", sort = "first",
                               label = list(NA_character_))
  )
  expect_equal(first$a, c(NA, "x", "y", "z"))
  expect_equal(first$p, c(1, 4 / 21, 6 / 21, 11 / 21))

  # The data frame computes the same rows in the same Margin order.
  specs <- list(rollup = rollup(a, b), cube = cube(a, b))
  labels <- list(
    null = list(NULL), na = list(NA_character_),
    mixed = list(list(a = NULL, b = "All"))
  )
  for (spec in names(specs)) {
    # A Parent share is defined for a pure rollup() only.
    for (share in c("total", if (spec == "rollup") "parent")) {
      for (label in names(labels)) {
        for (by in list(NULL, "k")) {
          for (id in list(NULL, "set")) {
            for (sort in c("last", "first")) {
              run <- function(input) {
                sorted_typed_missing_share(
                  input, share = share, sort = sort, label = labels[[label]],
                  grouping = specs[[spec]], by = by, id = id
                )
              }
              expect_equal(
                as.data.frame(dplyr::collect(run(source))),
                as.data.frame(run(data)),
                ignore_attr = TRUE,
                info = paste(spec, share, label, by, id, sort)
              )
            }
          }
        }
      }
    }
  }
})

test_that("a sorted typed-missing share computes and takes later verbs", {
  skip_if_suggest_absent("duckdb", "DBI")
  duck <- duckdb_test_connection(dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    a = c("x", "y", "x", "y", "z", "z"), v = c(1, 2, 3, 4, 5, 6)
  )
  DBI::dbWriteTable(duck, "df", data)
  source <- dplyr::tbl(generic_forwarding_connection(duck), "df")
  for (sort in c("none", "last")) {
    query <- sorted_typed_missing_share(source, sort = sort)
    computed <- dplyr::collect(dplyr::arrange(
      dplyr::compute(query), is.na(.data$a), .data$a
    ))
    expect_equal(computed$n, c(4, 6, 11, 21), info = sort)
    later <- dplyr::collect(dplyr::filter(query, .data$n > 5))
    expect_setequal(later$n, c(6, 11, 21))
  }
})

test_that("a typed-missing dimension keeps its source type under a sort", {
  skip_if_suggest_absent("duckdb", "DBI")
  duck <- duckdb_test_connection(dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = c(2L, 1L, 2L, 3L), v = c(1, 2, 3, 4))
  DBI::dbWriteTable(duck, "df", data)
  source <- dplyr::tbl(generic_forwarding_connection(duck), "df")
  result <- dplyr::collect(sorted_typed_missing_share(source))
  expect_identical(result$a, c(1L, 2L, 3L, NA))
})

# The clause text of a rendered query outside every parenthesised subquery.
top_level_sql <- function(query) {
  sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
  repeat {
    stripped <- gsub("\\([^()]*\\)", "", sql)
    if (identical(stripped, sql)) break
    sql <- stripped
  }
  sql
}

test_that("a sorted compound orders only by columns its operands select", {
  for (simulate in c("simulate_dbi", "simulate_mssql", "simulate_mysql",
                     "simulate_oracle", "simulate_snowflake",
                     "simulate_redshift")) {
    con <- getExportedValue("dbplyr", simulate)()
    source <- dbplyr::lazy_frame(a = "x", v = 1, con = con, .name = "df")
    for (sort in c("last", "first")) {
      sql <- top_level_sql(sorted_typed_missing_share(source, sort = sort))
      order_by <- sub(".*ORDER BY", "", sql)
      expect_match(sql, "ORDER BY", info = simulate)
      if (grepl("UNION ALL", sql, fixed = TRUE)) {
        expect_no_match(order_by, "marginplyr", info = paste(simulate, sort))
      }
    }
  }
  source <- dbplyr::lazy_frame(
    a = "x", v = 1, con = dbplyr::simulate_mssql(), .name = "df"
  )
  sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(
    sorted_typed_missing_share(source)
  )))
  # Every ORDER BY in a subquery is bounded by its SELECT's TOP (#994).
  selects <- strsplit(sql, "SELECT ", fixed = TRUE)[[1L]]
  ordered <- grep("ORDER BY", selects[-length(selects)], value = TRUE)
  expect_true(all(grepl("^TOP ", ordered)))
})
