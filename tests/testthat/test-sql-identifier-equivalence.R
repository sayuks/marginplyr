test_that("SQLite refuses equivalent public result names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("a", "b"), v = c(1, 2))
  remote <- dplyr::copy_to(con, data, "case_names", temporary = TRUE)

  error <- expect_error(summarize_with_margins(
    remote, G = sum(v), .grouping = rollup(g)
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "g", fixed = TRUE)
  expect_match(conditionMessage(error), "G", fixed = TRUE)
  expect_match(conditionMessage(error), "rename", ignore.case = TRUE)
})

test_that("local result names remain case sensitive", {
  result <- summarize_with_margins(
    data.frame(g = c("a", "b"), v = c(1, 2)),
    G = sum(v), .grouping = rollup(g)
  )
  expect_identical(names(result), c("g", "G"))
  expect_equal(result$G[result$g == "Total"], 3)
})

test_that("SQLite internal keys cannot read an equivalent source column", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("a", "b"), v = c(1, 1))
  data[["..MARGINPLYR_KEY_1"]] <- "WRONG"
  remote <- dplyr::copy_to(con, data, "private_names", temporary = TRUE)

  result <- dplyr::collect(summarize_with_margins(
    remote, n = dplyr::n(), .grouping = grouping_set(g),
    .margin_label = NULL
  ))
  expect_setequal(result$g, c("a", "b"))
  expect_setequal(result$n, c(1L, 1L))
})

test_that("SQLite validates identifiers only where public columns coexist", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)),
    "case_public", temporary = TRUE
  )

  cases <- list(
    function() {
      summarize_with_margins(remote, s = sum(v), .by = g, .id = "G")
    },
    function() {
      summarize_with_margins(remote, s = sum(v), .by = g, .id = "S")
    },
    function() {
      expand_with_margins(remote, .grouping = rollup(g), .id = "V")
    },
    function() {
      summarize_with_margins(
        remote, dplyr::across(v, sum, .names = "G"),
        .grouping = rollup(g)
      )
    }
  )
  for (make in cases) {
    error <- expect_error(make())
    expect_s3_class(error, "marginplyr_error")
    expect_match(conditionMessage(error), "rename", ignore.case = TRUE)
  }

  replacement <- dplyr::collect(summarize_with_margins(
    remote, V = sum(v), .grouping = grouping_set(g),
    .margin_label = NULL
  ))
  expect_identical(names(replacement), c("g", "V"))
  expect_setequal(replacement$V, c(1, 2))
})

test_that("SQLite keeps distinct non-ASCII public spellings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(v = c(1, 2))
  data[["Ä"]] <- c("a", "b")
  data[["ä"]] <- c("x", "y")
  remote <- dplyr::copy_to(con, data, "unicode_names", temporary = TRUE)
  result <- dplyr::collect(expand_with_margins(
    remote, .grouping = grouping_set(Ä, ä), .margin_label = NULL
  ))
  expect_identical(names(result), c("Ä", "ä", "v"))
  expect_setequal(result[["Ä"]], c("a", "b"))
  expect_setequal(result[["ä"]], c("x", "y"))
})

test_that("SQL name checks do not re-evaluate an across names expression", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)),
    "names_once", temporary = TRUE
  )
  evaluations <- 0L
  result <- summarize_with_margins(
    remote,
    dplyr::across(v, sum, .names = {
      evaluations <<- evaluations + 1L
      "s"
    }),
    .grouping = grouping_set(g)
  )
  expect_identical(evaluations, 1L)
  expect_identical(as.character(dplyr::tbl_vars(result)), c("g", "s"))
})

test_that("DuckDB protects names on native and portable plans", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(g = c("a", "b"), v = c(1, 1))
  data[["..MARGINPLYR_KEY_1"]] <- "WRONG"
  remote <- dplyr::copy_to(con, data, "duck_case", temporary = TRUE)

  for (sort in c("none", "last", "first")) {
    native_error <- expect_error(summarize_with_margins(
      remote, G = sum(v), .grouping = rollup(g), .sort = sort
    ))
    expect_s3_class(native_error, "marginplyr_error")
    expect_match(conditionMessage(native_error), "g", fixed = TRUE)
    expect_match(conditionMessage(native_error), "G", fixed = TRUE)
  }

  portable <- dplyr::collect(summarize_with_margins(
    remote, n = dplyr::n(),
    .grouping = grouping_sets(grouping_set(g), grouping_set(g)),
    .duplicates = "keep", .id = "set", .margin_label = NULL
  ))
  expect_identical(names(portable), c("g", "set", "n"))
  expect_setequal(portable$g, c("a", "b", "a", "b"))
  expect_equal(portable$n, rep(1L, 4L))
})

test_that("SQLite refuses a share output equivalent to a dimension", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)),
    "share_case", temporary = TRUE
  )
  error <- expect_error(summarize_with_margins(
    remote, z = sum(v), G = share_of_total(z),
    .grouping = rollup(g), .check_share_source = FALSE
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "g", fixed = TRUE)
  expect_match(conditionMessage(error), "G", fixed = TRUE)
})

test_that("SQLite sorted direct and computed results keep public values", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("a", "b"), v = c(1, 2))
  data[["..MARGINPLYR_KEY_1"]] <- "WRONG"
  remote <- dplyr::copy_to(con, data, "sorted_case", temporary = TRUE)

  for (sort in c("none", "last", "first")) {
    result <- summarize_with_margins(
      remote, n = dplyr::n(), .grouping = rollup(g),
      .sort = sort, .id = "set", .margin_label = NULL
    )
    direct <- dplyr::collect(result)
    computed <- dplyr::collect(dplyr::compute(result, temporary = TRUE))
    expect_identical(names(direct), c("g", "set", "n"), info = sort)
    expect_identical(names(computed), names(direct), info = sort)
    expect_equal(as.data.frame(direct), as.data.frame(computed), info = sort)
    expect_setequal(direct$n[direct$set == 1L], c(1L, 1L))
  }
})
