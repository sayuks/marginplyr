# A column written where `.grouping` expects a Grouping specification (#1638).

bare_column_data <- function() {
  data.frame(region = "East", store = "A", value = 1)
}

bare_column_lines <- function(columns, written) {
  c(
    paste0(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`."
    ),
    paste0(
      "i ", columns, " must be wrapped in a constructor, for example ",
      "`rollup(", written, ")` or `grouping_sets(", written, ")`."
    )
  )
}

# The failed lookup of the column is what the refusal explains, so it is not
# carried as a cause that would repeat it (#1703).
expect_bare_column_refusal <- function(call, columns, written) {
  error <- expect_error(call, class = "marginplyr_error")
  lines <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]]
  expect_identical(lines, bare_column_lines(columns, written))
  expect_null(error$parent)
  invisible(error)
}

test_that("a bare column as .grouping is refused on every entry point", {
  data <- bare_column_data()
  for (verb in names(forwarded_verbs)) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](data, NULL, region),
      "Column `region`", "region"
    )
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](data, NULL, c(region, store)),
      "Columns `region` and `store`", "region, store"
    )
  }
})

test_that("the bare-column refusal names the call", {
  data <- bare_column_data()
  expect_snapshot(error = TRUE, {
    summarize_with_margins(data, n = dplyr::n(), .grouping = region)
    inspect_grouping(data, .grouping = c(region, store))
  })
})

# `tbl_lazy()` has no connection to send a query to, so a refusal reached by
# reading data would be a different error rather than this one. The nesting
# verbs refuse a lazy input before reading `.grouping`, so they are not asked.
test_that("a lazy input is refused from its metadata alone", {
  remote <- dbplyr::tbl_lazy(
    bare_column_data(),
    con = dbplyr::simulate_postgres()
  )
  lazy_verbs <- setdiff(
    names(forwarded_verbs), c("nest_with_margins", "nest_by_with_margins")
  )
  for (verb in lazy_verbs) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](remote, NULL, c(region, store)),
      "Columns `region` and `store`", "region, store"
    )
  }
})

test_that("a dtplyr input is refused the same way", {
  skip_if_suggest_absent("dtplyr")
  lazy <- dtplyr::lazy_dt(bare_column_data())
  for (verb in names(forwarded_verbs)) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](lazy, NULL, region),
      "Column `region`", "region"
    )
  }
})

test_that("an Arrow input is refused the same way", {
  skip_if_suggest_absent("arrow")
  table <- arrow::arrow_table(bare_column_data())
  for (verb in c("summarize_with_margins", "inspect_grouping")) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](table, NULL, region),
      "Column `region`", "region"
    )
  }
})

test_that("a specification bound to a column's name is still read", {
  data <- bare_column_data()
  region <- rollup(store)
  expect_identical(
    inspect_grouping(data, .grouping = region)$included,
    inspect_grouping(data, .grouping = rollup(store))$included
  )
})

test_that("a column passed as a quosure is refused the same way", {
  data <- bare_column_data()
  column <- rlang::quo(region)
  # A quosure handed to `.grouping` as a value, and one wrapped in another.
  for (grouping in list(column, rlang::new_quosure(column))) {
    expect_bare_column_refusal(
      rlang::inject(inspect_grouping(data, .grouping = !!grouping)),
      "Column `region`", "region"
    )
  }
})

test_that("other failures in .grouping propagate unchanged", {
  data <- bare_column_data()
  build <- function() stop("builder failed")
  error <- expect_error(inspect_grouping(data, .grouping = build()))
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(conditionMessage(error), "builder failed")

  # A name that is not a column, and a `c()` mixing one in, are not a column
  # written in place of a specification.
  for (written in list(quote(regoin), quote(c(region, regoin)))) {
    error <- expect_error(
      inspect_grouping(data, .grouping = !!written),
      "object '(regoin|region)' not found"
    )
    expect_false(inherits(error, "marginplyr_error"))
  }
})

# A string names a column as `all_of()` reads it, so the refusal echoes it and
# gives the constructor spelling that selects it (#1703).
test_that("a string as .grouping is answered with an all_of() rewrite", {
  data <- bare_column_data()
  cases <- list(
    list(
      grouping = "region",
      supplied = "i A character vector was supplied: \"region\".",
      rewrite = "`rollup(all_of(\"region\"))`"
    ),
    list(
      grouping = c("region", "store"),
      supplied = "i A character vector was supplied: \"region\" and \"store\".",
      rewrite = "`rollup(all_of(c(\"region\", \"store\")))`"
    )
  )
  for (verb in names(forwarded_verbs)) {
    for (case in cases) {
      error <- expect_error(
        forwarded_verbs[[verb]](data, NULL, case$grouping),
        class = "marginplyr_error"
      )
      lines <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]]
      expect_identical(lines[[1L]], bare_column_lines("", "")[[1L]])
      expect_identical(lines[[2L]], case$supplied, info = verb)
      expect_identical(
        lines[[3L]],
        paste0(
          "i Select the columns it names inside a constructor, for example ",
          case$rewrite, "."
        ),
        info = verb
      )
    }
  }

  rewritten <- inspect_grouping(
    data, .grouping = rollup(all_of(c("region", "store")))
  )
  expect_identical(
    rewritten$included,
    inspect_grouping(data, .grouping = rollup(region, store))$included
  )
})

# A bare name bound where `.grouping` is evaluated was not a failed lookup, so
# whatever its evaluation raised is the caller's to read, even when the input
# has a column of that name.
test_that("a bound name's own failure propagates unchanged", {
  data <- bare_column_data()
  make_spec <- function(dim) {
    if (!identical(dim, "region")) stop("unknown dimension: ", dim)
    rollup(region)
  }
  report <- function(data, store) {
    inspect_grouping(data, .grouping = store)
  }
  error <- expect_error(report(data, make_spec("zone")))
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(conditionMessage(error), "unknown dimension: zone")
  expect_identical(conditionCall(error), quote(make_spec("zone")))

  forwarded <- function(data, region, store) {
    summarize_with_margins(data, n = dplyr::n(), .grouping = c(region, store))
  }
  error <- expect_error(forwarded(data, stop("caller failed"), 1))
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(conditionMessage(error), "caller failed")

  # A missing argument, and an active binding that raises when read.
  missing_store <- function(data, store) {
    inspect_grouping(data, .grouping = store)
  }
  error <- expect_error(missing_store(data), "is missing")
  expect_false(inherits(error, "marginplyr_error"))
  env <- new.env()
  reads <- 0L
  makeActiveBinding("region", function() {
    reads <<- reads + 1L
    stop("binding failed")
  }, env)
  error <- expect_error(
    inspect_grouping(data, .grouping = !!rlang::new_quosure(
      quote(region), env
    ))
  )
  expect_identical(conditionMessage(error), "binding failed")
  expect_identical(reads, 1L)

  # One unbound name among bound ones is still the failed lookup.
  region <- "East"
  expect_bare_column_refusal(
    inspect_grouping(data, .grouping = c(region, store)),
    "Columns `region` and `store`", "region, store"
  )
})

test_that("a time limit while a bound .grouping evaluates ends the call", {
  data <- bare_column_data()
  report <- function(data, store) {
    inspect_grouping(data, .grouping = store)
  }
  slow <- function() {
    deadline <- Sys.time() + 2
    while (Sys.time() < deadline) NULL
    rollup(region)
  }
  withr::defer(setTimeLimit(elapsed = Inf))
  error <- expect_error({
    setTimeLimit(elapsed = 0.2, transient = TRUE)
    report(data, slow())
  })
  setTimeLimit(elapsed = Inf)
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "time limit")
})
