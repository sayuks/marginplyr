test_that("assert_logical_scalar_scalar() works", {
  expect_no_error(assert_logical_scalar(TRUE))
  expect_no_error(assert_logical_scalar(FALSE))
  expect_error(assert_logical_scalar())
  expect_error(assert_logical_scalar(logical()))
  expect_error(assert_logical_scalar(c(TRUE, TRUE)))
  expect_error(assert_logical_scalar(list(TRUE)))
  expect_error(assert_logical_scalar(1))
  expect_error(assert_logical_scalar("a"))
  expect_error(assert_logical_scalar(NULL))
  expect_error(assert_logical_scalar(new.env()))
})

test_that("assert_sring_scalar() works", {
  expect_no_error(assert_string_scalar("a"))
  expect_no_error(assert_string_scalar(""))
  expect_no_error(assert_string_scalar(NA_character_))
  expect_error(assert_string_scalar())
  expect_error(assert_string_scalar(character()))
  expect_error(assert_string_scalar(c("1", "a")))
  expect_error(assert_string_scalar(list("a")))
  expect_error(assert_string_scalar(1))
  expect_error(assert_string_scalar(TRUE))
  expect_error(assert_string_scalar(NULL))
  expect_error(assert_string_scalar(new.env()))
})

test_that("assert_nest_possible() works", {
  skip_if_backend_absent("dtplyr")

  expect_no_error(assert_nest_possible(data.frame()))
  expect_no_error(assert_nest_possible(dtplyr::lazy_dt(data.frame())))
  expect_error(assert_nest_possible())
  expect_error(assert_nest_possible(character()))
  expect_error(assert_nest_possible(list(data.frame())))
  expect_error(assert_nest_possible(1))
  expect_error(assert_nest_possible(TRUE))
  expect_error(assert_nest_possible(NULL))
  expect_error(assert_nest_possible(new.env()))
})

test_that("assert_lazy_table() works", {
  skip_if_backend_absent("arrow")

  expect_error(
    assert_lazy_table(
      arrow::as_record_batch_reader(data.frame())
    ),
    "must not be an object of the following class"
  )
  expect_no_error(assert_lazy_table(data.frame()))
  expect_no_error(assert_lazy_table(arrow::arrow_table(x = 1)))
  expect_no_error(assert_lazy_table(arrow::record_batch(x = 1)))

  # arrow "Dataset" class
  tmp <- tempfile("test", fileext = ".parquet")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE))

  arrow::write_parquet(
    x = data.frame(),
    sink = tmp
  )

  expect_no_error(
    assert_lazy_table(
      arrow::open_dataset(tmp)
    )
  )
})

test_that("shared argument assertions use the package condition seam", {
  x <- 1

  logical_error <- expect_error(
    assert_logical_scalar(x),
    "`x` must be a logical scalar"
  )
  string_error <- expect_error(
    assert_string_scalar(x),
    "`x` must be a character vector of length 1"
  )
  nest_error <- expect_error(
    assert_nest_possible(x),
    "`x` must be one of the following classes"
  )

  expect_s3_class(logical_error, "marginplyr_error")
  expect_s3_class(string_error, "marginplyr_error")
  expect_s3_class(nest_error, "marginplyr_error")
})

# The rule `static_call_args()` states: its elements can be R's empty argument,
# so a walk reads them by subscript and never binds one with `for`. #168 is what
# happens when a walk forgets -- an untyped `missingArgError` naming the loop
# variable, from a summary expression as ordinary as `sum(value[])`.
#
# Scanned rather than listed, for the reason every other gate here derives: a
# list of walks is a list the next walk is not on. It reads the namespace rather
# than `R/`, so it holds under `R CMD check` too, where the sources sit beside
# the `.Rcheck` directory rather than inside it -- and so it needs no
# `skip_if()` that `verify-backend.R` would then read as a job skipping for a
# reason other than a withheld backend.
package_functions <- function() {
  namespace <- asNamespace("marginplyr")
  names <- ls(namespace, all.names = TRUE)
  objects <- lapply(names, get, envir = namespace)
  keep <- vapply(objects, is.function, logical(1))
  stats::setNames(objects[keep], names[keep])
}

# By subscript throughout, since the code being scanned is exactly the code that
# may hold an empty argument.
binds_call_parts_with_for <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  sequence <- if (identical(expr[[1L]], quote(`for`)) && length(expr) >= 3L) {
    expr[[3L]]
  }
  if (
    is.call(sequence) && identical(sequence[[1L]], quote(static_call_args))
  ) {
    return(TRUE)
  }
  for (index in seq_along(expr)) {
    if (binds_call_parts_with_for(expr[[index]])) {
      return(TRUE)
    }
  }
  FALSE
}

test_that("no walk binds a call's parts with `for`", {
  functions <- package_functions()
  # The scan iterates over this set, so a set that arrived empty is a set that
  # passes. `static_call_args()` itself is the cheapest witness that the
  # namespace was read.
  expect_true("static_call_args" %in% names(functions))

  offenders <- vapply(
    functions,
    function(f) binds_call_parts_with_for(body(f)),
    logical(1)
  )

  # Named rather than counted, so the failure says which walk to rewrite.
  expect_equal(
    names(functions)[offenders],
    character(),
    info = paste(
      "Read the parts by subscript instead --",
      "`parts <- static_call_args(expr)`, then `parts[[index]]`."
    )
  )
})

test_that("the walk scan detects the shape it is written to forbid", {
  # Asserted before the scan's verdict means anything, for the reason
  # `verify-suite-coverage.R` asserts its own mechanism: a scan that stopped
  # matching reports exactly what a clean package reports.
  offending <- function(expr) {
    for (part in static_call_args(expr)) {
      part
    }
  }
  nested <- function(expr) {
    if (TRUE) {
      lapply(seq_len(2L), function(i) {
        for (part in static_call_args(expr)) {
          part
        }
      })
    }
  }
  compliant <- function(expr) {
    parts <- static_call_args(expr)
    for (index in seq_along(parts)) {
      parts[[index]]
    }
  }

  expect_true(binds_call_parts_with_for(body(offending)))
  expect_true(binds_call_parts_with_for(body(nested)))
  expect_false(binds_call_parts_with_for(body(compliant)))
  # An empty argument in the scanned code must not abort the scan, which is the
  # bug one level up.
  expect_false(binds_call_parts_with_for(quote(sum(value[]))))
})

test_that("lazy-table assertions use the package condition seam", {
  skip_if_backend_absent("arrow")

  error <- expect_error(
    assert_lazy_table(arrow::as_record_batch_reader(data.frame())),
    "must not be an object of the following class"
  )

  expect_s3_class(error, "marginplyr_error")
})
