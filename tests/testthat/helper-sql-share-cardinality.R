expect_sql_share_cardinality <- function(con, backend) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1L, 3L)), "share_cardinality_facts"
  )
  array <- function(elements) {
    if (backend == "duckdb") {
      paste0("CAST([", elements, "] AS INTEGER[])")
    } else {
      paste0("ARRAY[", elements, "]::INTEGER[]")
    }
  }
  requests <- list(
    parent = rlang::exprs(parent = share_of_parent(value)),
    total = rlang::exprs(total = share_of_total(value))
  )
  summarize <- function(value, share, portable) {
    summarize_with_margins(
      source, value = !!value, !!!requests[[share]],
      .grouping = if (portable) rollup("g", "g") else rollup("g"),
      .duplicates = if (portable) "keep" else "error",
      .id = "set", .margin_label = NULL
    )
  }

  for (share in names(requests)) {
    for (portable in c(FALSE, TRUE)) {
      for (members in c("", "10, 20", "10")) {
        value <- dbplyr::sql(paste0("SUM(x) + unnest(", array(members), ")"))
        query <- summarize(value, share, portable)
        testthat::expect_s3_class(query, "tbl_lazy")
        testthat::expect_match(
          dbplyr::sql_render(query),
          if (portable) "UNION ALL" else "GROUPING SETS",
          fixed = TRUE
        )
        if (members != "10") {
          testthat::expect_error(dplyr::collect(query), "exactly one value")
        } else {
          result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$g)
          values <- if (portable) c(11, 13, 11, 13, 14) else c(11, 13, 14)
          ids <- if (portable) c(1, 1, 2, 2, 3) else c(1, 1, 2)
          testthat::expect_equal(as.double(result$set), ids)
          testthat::expect_equal(as.double(result$value), values)
          testthat::expect_equal(result[[share]], values / 14)
        }
      }

      query <- summarize(quote(sum(x, na.rm = TRUE)), share, portable)
      result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$g)
      values <- if (portable) c(1, 3, 1, 3, 4) else c(1, 3, 4)
      testthat::expect_equal(as.double(result$value), values)
      testthat::expect_equal(result[[share]], values / 4)
    }
  }

  for (portable in c(FALSE, TRUE)) {
    sequence <- if (portable) "portable_share_rows" else "native_share_rows"
    DBI::dbExecute(con, paste("CREATE TEMP SEQUENCE", sequence))
    current <- paste0("SELECT currval('", sequence, "')")
    next_value <- paste0("nextval('", sequence, "')")
    value <- dbplyr::sql(paste0(
      "SUM(x) + unnest(", array(next_value), ")"
    ))
    query <- summarize(value, "total", portable)
    testthat::expect_match(
      dbplyr::sql_render(query),
      if (portable) "UNION ALL" else "GROUPING SETS",
      fixed = TRUE
    )
    testthat::expect_error(DBI::dbGetQuery(con, current), "not yet defined")
    result <- dplyr::collect(query)
    root <- if (portable) 3L else 2L
    expected_rows <- if (portable) 5L else 3L
    testthat::expect_equal(nrow(result), expected_rows)
    testthat::expect_equal(as.double(DBI::dbGetQuery(con, current)[[1L]]),
                           expected_rows)
    values <- as.double(result$value)
    testthat::expect_equal(result$total, values / values[result$set == root])
  }

  value <- dbplyr::sql("((SUM(x) + unnest(array_agg(x))))")
  query <- summarize(value, "total", FALSE)
  testthat::expect_error(dplyr::collect(query), "exactly one value")

  value <- dbplyr::sql(paste0(
    "SUM(x) + (SELECT SUM(v) FROM unnest(", array("10, 20"),
    ") AS inner_values(v)) /* unnest is part of a scalar subquery */"
  ))
  query <- summarize(value, "total", FALSE)
  result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$g)
  testthat::expect_equal(as.double(result$value), c(31, 33, 34))
  testthat::expect_equal(result$total, c(31, 33, 34) / 34)

  value <- dbplyr::sql(paste0(
    "(SELECT SUM(v) FROM unnest(", array("10, 20"), ") AS inner_values(v))"
  ))
  result <- dplyr::collect(summarize(value, "total", FALSE))
  testthat::expect_equal(as.double(result$value), rep(30, 3L))
  testthat::expect_equal(result$total, rep(1, 3L))

  if (backend == "duckdb") {
    value <- dbplyr::sql("SUM(x) + unnest([[10]], recursive := true)")
    query <- summarize(value, "total", FALSE)
    result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$g)
    testthat::expect_equal(as.double(result$value), c(11, 13, 14))
    testthat::expect_equal(result$total, c(11, 13, 14) / 14)
  }
}
