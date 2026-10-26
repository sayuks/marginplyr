# Compare the caller-owned condition inside any wrapper the backend adds.
expect_across_naming_failure <- function(data) {
  for (persistent in c(FALSE, TRUE)) {
    cause <- rlang::error_cnd("naming_cause", message = "naming cause")
    failure <- rlang::error_cnd(
      "naming_failure", message = "template failed", parent = cause,
      call = quote(template())
    )
    run <- function(mode) {
      state <- new.env(parent = emptyenv())
      state$reads <- 0L
      template <- function() {
        state$reads <- state$reads + 1L
        if (persistent || state$reads == 1L) {
          stop(failure)
        }
        "total"
      }
      # These column names are resolved in the summary's data mask.
      # nolint start: object_usage_linter.
      error <- tryCatch(
        switch(mode,
          ordinary = dplyr::summarise(
            data, dplyr::across(x, sum, .names = template())
          ),
          margin = summarize_with_margins(
            data, dplyr::across(x, sum, .names = template())
          ),
          share = summarize_with_margins(
            data, dplyr::across(x, sum, .names = template()),
            p = share_of_total(total)
          ),
          unrelated = summarize_with_margins(
            data, dplyr::across(x, sum, .names = template()),
            ordinary = sum(x), p = share_of_total(ordinary)
          )
        ),
        error = identity
      )
      # nolint end
      reads <- state$reads
      expect_s3_class(error, "error")
      expect_identical(reads, 1L)
      original <- error
      while (inherits(original, "error") &&
               !inherits(original, "naming_failure") &&
               !is.null(original$parent)) {
        original <- original$parent
      }
      expect_identical(original, failure)
      list(error = error, reads = reads, original = original)
    }
    baseline <- run("ordinary")
    for (mode in c("margin", "share", "unrelated")) {
      actual <- run(mode)
      expect_identical(actual$reads, baseline$reads)
      expect_identical(actual$original, baseline$original)
    }
  }
}

# Naming belongs to the captured caller environment, once per construction.
expect_across_naming_success <- function(data) {
  run <- function(mode) {
    state <- new.env(parent = emptyenv())
    state$reads <- 0L
    output <- "total"
    template <- function() {
      state$reads <- state$reads + 1L
      output
    }
    # These names are resolved by dplyr or as a contextual share source.
    # nolint start: object_usage_linter.
    result <- switch(mode,
      ordinary = dplyr::summarise(
        data, dplyr::across(x, sum, .names = template())
      ),
      margin = summarize_with_margins(
        data, dplyr::across(x, sum, .names = template())
      ),
      share = summarize_with_margins(
        data, dplyr::across(x, sum, .names = template()),
        p = share_of_total(total), .check_share_source = FALSE
      )
    )
    # nolint end
    list(columns = as.character(dplyr::tbl_vars(result)), reads = state$reads)
  }
  baseline <- run("ordinary")
  expect_identical(baseline$reads, 1L)
  expect_identical(baseline$columns, "total")
  for (mode in c("margin", "share")) {
    actual <- run(mode)
    expect_identical(actual$reads, baseline$reads)
    expect_identical(setdiff(actual$columns, "p"), baseline$columns)
  }
}

test_that("local shares preserve the first across naming failure", {
  data <- tibble::tibble(x = 1:2)
  expect_across_naming_failure(data)
  expect_across_naming_success(data)
})

test_that("dtplyr construction preserves the first across naming failure", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(tibble::tibble(x = 1:2))
  expect_across_naming_failure(data)
  expect_across_naming_success(data)
})

test_that("SQL construction preserves the first across naming failure", {
  data <- dbplyr::lazy_frame(x = 1:2)
  expect_across_naming_failure(data)
  expect_across_naming_success(data)
})

test_that("DuckDB construction preserves the first across naming failure", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- dplyr::copy_to(con, tibble::tibble(x = 1:2), "naming_input")
  expect_across_naming_failure(data)
  expect_across_naming_success(data)
})

test_that("unrelated local shares leave first naming in its dplyr context", {
  run <- function(mode) {
    reads <- 0L
    template <- function() {
      reads <<- reads + 1L
      paste0("out_", dplyr::n())
    }
    data <- tibble::tibble(x = 1:2)
    result <- switch(mode,
      ordinary = dplyr::summarise(
        data, dplyr::across(x, sum, .names = template()), total = sum(x)
      ),
      margin = summarize_with_margins(
        data, dplyr::across(x, sum, .names = template()), total = sum(x)
      ),
      share = summarize_with_margins(
        data, dplyr::across(x, sum, .names = template()), total = sum(x),
        p = share_of_total(total)
      )
    )
    list(result = result, reads = reads)
  }
  baseline <- run("ordinary")
  expect_identical(baseline$reads, 1L)
  for (mode in c("margin", "share")) {
    actual <- run(mode)
    expect_identical(actual$reads, baseline$reads)
    expect_identical(actual$result[names(baseline$result)], baseline$result)
  }
})

test_that("deferred first naming cannot duplicate an explicit share source", {
  data <- tibble::tibble(x = 1:2)
  reads <- 0L
  template <- function() {
    reads <<- reads + 1L
    "total"
  }
  expect_error(
    summarize_with_margins(
      data, dplyr::across(x, sum, .names = template()),
      total = sum(x), p = share_of_total(total)
    ),
    "defined exactly once", class = "marginplyr_error"
  )
  expect_identical(reads, 1L)
})
