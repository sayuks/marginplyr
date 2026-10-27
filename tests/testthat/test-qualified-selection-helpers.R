with_foreign_selection_package <- function(code) {
  root <- tempfile()
  dir.create(root)
  pkg <- file.path(root, "foreignselect")
  dir.create(pkg)
  dir.create(file.path(pkg, "R"))
  helpers <- c(
    "all_of", "any_of", "starts_with", "ends_with", "contains", "matches",
    "num_range", "everything", "last_col", "one_of", "character_columns"
  )
  writeLines(c(
    "Package: foreignselect", "Version: 0.0.1", "Title: Fixture",
    "Description: Fixture package.", "License: MIT",
    "Authors@R: person('A', 'B', email = 'a@example.com', role = c('aut', 'cre'))"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(paste0("export(", helpers, ")"), file.path(pkg, "NAMESPACE"))
  writeLines(c(
    "character_columns <- function(predicate = FALSE, ...) {",
    "  if (predicate) return(tidyselect::where(is.character))",
    "  data <- tidyselect::peek_data()",
    "  names(data)[vapply(data, is.character, logical(1))]",
    "}",
    paste0(helpers[-length(helpers)], " <- character_columns")
  ), file.path(pkg, "R", "helpers.R"))
  lib <- file.path(root, "lib")
  dir.create(lib)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-byte-compile", "-l", shQuote(lib), shQuote(pkg)),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(status, "status"))) {
    stop(paste(status, collapse = "\n"))
  }
  old <- .libPaths()
  .libPaths(c(lib, old))
  on.exit({
    unloadNamespace("foreignselect")
    .libPaths(old)
    unlink(root, recursive = TRUE)
  }, add = TRUE)
  code()
}

test_that("foreign qualified helpers preserve fixed partitions and rollups", {
  with_foreign_selection_package(function() {
    data <- tibble::tibble(key = c("x", "y"), value = 1:2)
    alias <- getExportedValue("foreignselect", "character_columns")
    expected <- inspect_grouping(data, .by = alias())
    for (helper in c(
      "all_of", "any_of", "starts_with", "ends_with", "contains", "matches",
      "num_range", "everything", "last_col", "one_of", "character_columns"
    )) {
      selection <- rlang::call2(helper, .ns = "foreignselect")
      expect_identical(inspect_grouping(data, .by = !!selection), expected,
                       info = helper)
      expect_identical(expected$fixed, "(key)")
      plan <- inspect_grouping(data, .grouping = rollup(!!selection))
      expect_identical(plan$included, c("(key)", "()"), info = helper)
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        result <- verb(data, .by = !!selection)
        expect_identical(result$key, c("x", "y"), info = helper)
        expect_identical(lapply(result$data, function(x) x$value),
                         list(1L, 2L), info = helper)
        rolled <- verb(data, .grouping = rollup(!!selection),
                       .margin_label = NULL, .id = "set")
        expect_identical(rolled$set, c(1L, 1L, 2L), info = helper)
        expect_identical(lapply(rolled$data, function(x) x$value),
                         list(1L, 2L, 1:2), info = helper)
      }
    }
  })
})

test_that("qualified genuine helpers retain early validation", {
  data <- data.frame(key = "x", value = 1)
  reads <- 0L
  local_mocked_bindings(grouping_selection_proxy = function(...) {
    reads <<- reads + 1L
    stop("unexpected metadata read")
  })
  for (ns in c("tidyselect", "dplyr")) {
    selection <- rlang::call2("all_of", "absent", .ns = ns)
    expect_error(inspect_grouping(data, .by = !!selection),
                 class = "vctrs_error_subscript_oob")
    expect_identical(reads, 0L)
  }
})

test_that("classification does not evaluate caller functions", {
  env <- rlang::env()
  calls <- 0L
  makeActiveBinding("starts_with", function() {
    calls <<- calls + 1L
    stop("classification forced caller binding")
  }, env)
  expect_true(is_name_only_expr(quote(starts_with("k")), env, "key"))
  expect_false(is_name_only_expr(rlang::call2("starts_with", .ns = "foreignselect"), env, "key"))
  expect_identical(calls, 0L)
  for (helper in c(
    "all_of", "any_of", "starts_with", "ends_with", "contains", "matches",
    "num_range", "everything", "last_col", "one_of"
  )) {
    for (ns in c("tidyselect", "dplyr")) {
      expect_true(is_name_only_expr(rlang::call2(helper, .ns = ns), env, "key"),
                  info = paste(ns, helper))
    }
  }
})

test_that("foreign helpers select the same typed keys on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  with_foreign_selection_package(function() {
    data <- tibble::tibble(key = c("x", "y"), value = 1:2)
    remote <- dtplyr::lazy_dt(data)
    selection <- rlang::call2("starts_with", .ns = "foreignselect")
    expect_identical(inspect_grouping(remote, .by = !!selection),
                     inspect_grouping(data, .by = !!selection))
    expect_identical(inspect_grouping(remote, .grouping = rollup(!!selection)),
                     inspect_grouping(data, .grouping = rollup(!!selection)))
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(remote, .by = !!selection)
      if (inherits(result, "dtplyr_step")) result <- dplyr::collect(result)
      expect_identical(result$key, c("x", "y"))
      expect_identical(lapply(result$data, function(x) x$value), list(1L, 2L))
    }
  })
})

test_that("foreign helpers select the same typed keys on DuckDB", {
  skip_if_suggest_absent("duckdb")
  with_foreign_selection_package(function() {
    data <- tibble::tibble(key = c("x", "y"), value = 1:2)
    con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    remote <- dplyr::copy_to(con, data, "selection_fixture", temporary = TRUE)
    selection <- rlang::call2("starts_with", .ns = "foreignselect")
    expect_identical(inspect_grouping(remote, .by = !!selection),
                     inspect_grouping(data, .by = !!selection))
    expect_identical(inspect_grouping(remote, .grouping = rollup(!!selection)),
                     inspect_grouping(data, .grouping = rollup(!!selection)))
  })
})

test_that("opaque backends do not acquire source rows for foreign helpers", {
  with_foreign_selection_package(function() {
    data <- data.frame(key = c("x", "y"), value = 1:2)
    remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_dbi())
    error <- expect_error(inspect_grouping(
      remote, .by = !!rlang::call2("starts_with", predicate = TRUE,
                                    .ns = "foreignselect")
    ), class = "marginplyr_error")
    expect_s3_class(error$parent, "tidyselect_error_predicates_unsupported")
  })
})
