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
