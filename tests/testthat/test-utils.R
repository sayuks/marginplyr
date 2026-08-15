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
# so a walk reads them by subscript and never binds one to a name. #168 is what
# happens when a walk forgets -- an untyped `missingArgError` naming the loop
# variable, from a summary expression as ordinary as `sum(value[])`.
#
# `for` is one spelling of that binding and `<-` is the other, which is how the
# same condition reached an `across()` rebuild (#174): a local bound to one
# element of a parsed call's arguments raises it on the first read of that
# local. Both spellings are scanned below.
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

# Where a list of a call's arguments comes from: the shared reader, the rlang
# function it wraps, or the `call_args` element of a parse this package builds
# out of one, in either spelling of the element read.
#
# One matcher serves both scans below, because the hazard is one hazard: a list
# reached this way holds elements that may be empty, whichever construct then
# binds one. Written separately, the `for` scan matched a direct
# `static_call_args()` call alone, so the same loop over a local -- the shape
# `removal_retained_bound()` sits one line away from -- passed both gates while
# the prose above claimed otherwise.
reads_call_arguments <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  head <- expr[[1L]]
  named_reader <-
    identical(head, quote(static_call_args)) ||
    identical(head, quote(rlang::call_args))
  if (named_reader) {
    return(TRUE)
  }
  element <- identical(head, quote(`$`)) || identical(head, quote(`[[`))
  element &&
    length(expr) == 3L &&
    (identical(expr[[3L]], quote(call_args)) ||
       identical(expr[[3L]], "call_args"))
}

# The names a body binds to such a list, so that a loop over one, or a
# subscript of one, is read as reaching the arguments themselves.
call_argument_locals <- function(expr, found = character()) {
  if (!is.call(expr)) {
    return(found)
  }
  if (
    identical(expr[[1L]], quote(`<-`)) &&
      length(expr) == 3L &&
      is.symbol(expr[[2L]]) &&
      reads_call_arguments(expr[[3L]])
  ) {
    found <- unique(c(found, as.character(expr[[2L]])))
  }
  for (index in seq_along(expr)) {
    found <- call_argument_locals(expr[[index]], found)
  }
  found
}

is_call_arguments <- function(expr, locals) {
  reads_call_arguments(expr) ||
    (is.symbol(expr) && as.character(expr) %in% locals)
}

# By subscript throughout, since the code being scanned is exactly the code that
# may hold an empty argument.
binds_call_parts_with_for <- function(body) {
  locals <- call_argument_locals(body)
  scan <- function(expr) {
    if (!is.call(expr)) {
      return(FALSE)
    }
    if (
      identical(expr[[1L]], quote(`for`)) &&
        length(expr) >= 3L &&
        is_call_arguments(expr[[3L]], locals)
    ) {
      return(TRUE)
    }
    for (index in seq_along(expr)) {
      if (scan(expr[[index]])) {
        return(TRUE)
      }
    }
    FALSE
  }
  scan(body)
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
  # The same loop over a local, and over the rlang function the reader wraps:
  # a scan that matched only a direct `static_call_args()` call passed both,
  # which is the shape half this package's argument walks are written in.
  through_local <- function(expr) {
    args <- rlang::call_args(expr)
    for (arg in args) {
      arg
    }
  }
  through_parsed <- function(expr) {
    for (part in parse_across_arguments(expr)$call_args) {
      part
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
  expect_true(binds_call_parts_with_for(body(through_local)))
  expect_true(binds_call_parts_with_for(body(through_parsed)))
  expect_false(binds_call_parts_with_for(body(compliant)))
  # An empty argument in the scanned code must not abort the scan, which is the
  # bug one level up.
  expect_false(binds_call_parts_with_for(quote(sum(value[]))))
})

# The `<-` spelling, over the same matcher: `name <- arguments[[index]]` binds
# one element of the list the `for` scan above binds every element of.
#
# What this cannot see is a part read straight off the call node --
# `column <- expr[[3L]]` -- because the scan would then have to flag every
# `x <- y[[i]]` in the package, most of which read an ordinary list. Those sites
# are covered by behaviour instead, in `test-static-expression-analysis.R`,
# where each shape is compared against what `dplyr::summarise()` does with it.
binds_call_parts_by_assign <- function(body) {
  locals <- call_argument_locals(body)
  scan <- function(expr) {
    if (!is.call(expr)) {
      return(FALSE)
    }
    if (
      identical(expr[[1L]], quote(`<-`)) &&
        length(expr) == 3L &&
        is.symbol(expr[[2L]]) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], quote(`[[`)) &&
        length(expr[[3L]]) >= 2L
    ) {
      if (is_call_arguments(expr[[3L]][[2L]], locals)) {
        return(TRUE)
      }
    }
    for (index in seq_along(expr)) {
      if (scan(expr[[index]])) {
        return(TRUE)
      }
    }
    FALSE
  }
  scan(body)
}

test_that("no walk binds a call's parts with `<-`", {
  functions <- package_functions()
  expect_true("parse_across_arguments" %in% names(functions))

  offenders <- vapply(
    functions,
    function(f) binds_call_parts_by_assign(body(f)),
    logical(1)
  )

  expect_equal(
    names(functions)[offenders],
    character(),
    info = paste(
      "Read the argument at the point of use --",
      "`parts[[index]]` rather than `part <- parts[[index]]`."
    )
  )
})

test_that("the assignment scan detects the shape it is written to forbid", {
  # The two shapes #174 found, and the two the package uses instead. Asserted
  # before the scan's verdict means anything, as above.
  through_parse <- function(expr) {
    parsed <- parse_across_arguments(expr)
    fns_expr <- parsed$call_args[[parsed$fns_index]]
    fns_expr
  }
  through_local <- function(expr) {
    args <- rlang::call_args(expr)
    arg <- args[[1L]]
    arg
  }
  through_parsed_local <- function(expr) {
    call_args <- parse_across_arguments(expr)$call_args
    fns <- call_args[[1L]]
    fns
  }
  # The element read by string rather than by name, which is one respelling
  # away from the shape above and answers the same.
  through_string_element <- function(expr) {
    fns <- parse_across_arguments(expr)[["call_args"]][[1L]]
    fns
  }
  through_reader <- function(expr) {
    part <- static_call_args(expr)[[1L]]
    part
  }
  compliant_local <- function(expr) {
    parts <- static_call_args(expr)
    rlang::is_symbol(parts[[1L]])
  }
  compliant_list <- function(labels) {
    label <- labels[[1L]]
    label
  }

  expect_true(binds_call_parts_by_assign(body(through_parse)))
  expect_true(binds_call_parts_by_assign(body(through_local)))
  expect_true(binds_call_parts_by_assign(body(through_parsed_local)))
  expect_true(binds_call_parts_by_assign(body(through_string_element)))
  expect_true(binds_call_parts_by_assign(body(through_reader)))
  expect_false(binds_call_parts_by_assign(body(compliant_local)))
  # An ordinary list read is not the shape, or the scan would name most of the
  # package and mean nothing.
  expect_false(binds_call_parts_by_assign(body(compliant_list)))
  expect_false(binds_call_parts_by_assign(quote(sum(value[]))))
})

test_that("every shared reader answers an empty call part", {
  # The scans above forbid binding a call part to a local; this is the same
  # rule read from the other end, at the readers a walk hands one to. An empty
  # argument is a call part like any other -- `x[, "col"]` is everyday R that
  # `dplyr::summarize()` accepts -- so each of these has to answer it rather
  # than raise base R's untyped `missingArgError` from inside itself (#168,
  # #174). The name readers are the ones this can be lost at: both unwrap the
  # parentheses #178 made transparent, and unwrapping before the shape test
  # binds the missing marker.
  #
  # Read by subscript throughout, since the parts being read are exactly the
  # parts that may be empty.
  parts <- static_call_args(quote(f(x, )))
  expect_true(rlang::is_missing(parts[[2L]]))
  expect_null(static_call_name(parts[[2L]]))
  expect_null(static_call_ns(parts[[2L]]))
  expect_null(static_spelling_name(parts[[2L]], "selection"))
  expect_null(static_spelling_reference_name(parts[[2L]], "share"))
  expect_null(static_character_value(parts[[2L]]))
  expect_null(static_callee_name(parts[[2L]]))
  expect_false(is_parenthesized(parts[[2L]]))
  expect_identical(expression_data_symbols(parts[[2L]]), character())
})

test_that("lazy-table assertions use the package condition seam", {
  skip_if_backend_absent("arrow")

  error <- expect_error(
    assert_lazy_table(arrow::as_record_batch_reader(data.frame())),
    "must not be an object of the following class"
  )

  expect_s3_class(error, "marginplyr_error")
})
