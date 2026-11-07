test_that("zero-column expansion ignores late caller repetition bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; only that backend is required here.
  source <- data.table::data.table()
  before <- serialize(source, NULL)
  root <- new.env(parent = globalenv())
  root$source <- source
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- expand_with_margins(input, .grouping = grouping_set(), .id = "set")
  expect_s3_class(query, "dtplyr_step")
  expected <- tibble::tibble(set = integer())
  expect_identical(dplyr::collect(query), expected)

  calls <- 0L
  root$rep <- function(...) {
    calls <<- calls + 1L
    999L
  }
  expect_identical(dplyr::collect(query), expected)
  root$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(c(package, name), c("base", "rep"))) {
      return(root$rep)
    }
    getExportedValue(package, name)
  }
  expect_identical(dplyr::collect(query), expected)
  expect_identical(calls, 0L)
  expect_identical(dim(dplyr::collect(input)), c(0L, 0L))
  expect_identical(serialize(source, NULL), before)
})

test_that("zero-column nesting ignores caller repetition bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; only that backend is required here.
  for (late in c(FALSE, TRUE)) {
    for (id in list(NULL, "set")) {
      source <- data.table::data.table()
      before <- serialize(source, NULL)
      root <- new.env(parent = globalenv())
      root$source <- source
      calls <- 0L
      caller_rep <- function(...) {
        calls <<- calls + 1L
        stop("caller repetition ran")
      }
      if (!late) root$rep <- caller_rep
      input <- evalq(dtplyr::lazy_dt(source), root)
      expanded <- expand_with_margins(input, .id = id)
      nested <- nest_with_margins(input, .id = id)
      expect_s3_class(expanded, "dtplyr_step")
      expect_s3_class(nested, "dtplyr_step")
      if (late) root$rep <- caller_rep

      for (attempt in seq_len(2L)) {
        expect_identical(
          dplyr::collect(expanded),
          tibble::as_tibble(expand_with_margins(data.frame(), .id = id))
        )
        expect_identical(
          dplyr::collect(nested),
          tibble::as_tibble(nest_with_margins(data.frame(), .id = id))
        )
      }
      rowwise <- nest_by_with_margins(input, .id = id)
      expect_s3_class(rowwise, "rowwise_df")
      expect_identical(nrow(rowwise), 1L)
      expect_identical(dim(rowwise$data[[1L]]), c(0L, 0L))
      if (!is.null(id)) expect_identical(rowwise[[id]], 1L)
      expect_identical(calls, 0L)
      expect_identical(dim(dplyr::collect(input)), c(0L, 0L))
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("identifier isolation preserves caller lookup and lazy execution", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(value = 7L)
  reads <- 0L
  root$observe <- function(value) {
    reads <<- reads + base::length(value)
    base::rep(TRUE, base::length(value))
  }
  root$rep <- function(...) 777L
  root$input <- evalq(
    dplyr::filter(dtplyr::lazy_dt(source), observe(value)), root
  )
  expanded <- expand_with_margins(root$input, .id = "set")
  nested <- nest_with_margins(root$input, .id = "set")
  expect_s3_class(expanded, "dtplyr_step")
  expect_s3_class(nested, "dtplyr_step")
  expect_identical(reads, 0L)
  expect_identical(
    dplyr::collect(expanded), tibble::tibble(set = 1L, value = 7L)
  )
  expect_gt(reads, 0L)
  reads <- 0L
  expect_identical(dplyr::collect(nested)$data[[1L]]$value, 7L)
  expect_gt(reads, 0L)
  reads <- 0L
  rowwise <- nest_by_with_margins(root$input, .id = "set")
  expect_s3_class(rowwise, "rowwise_df")
  expect_identical(rowwise$data[[1L]]$value, 7L)
  expect_gt(reads, 0L)

  summary <- evalq(summarize_with_margins(
    input, repeated = rep(1L, 1L), .id = "set"
  ), root)
  result <- dplyr::collect(summary)
  expect_identical(result$repeated, 777L)
  expect_identical(result$set, 1L)
})
