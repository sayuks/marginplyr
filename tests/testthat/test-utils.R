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
  skip_if_suggest_absent("dtplyr")

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
  skip_if_suggest_absent("arrow")

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
# reason other than a withheld backend. `namespace_functions()` and
# `visit_calls()` are that reading, shared with every other structural gate from
# `helper-namespace-walk.R`.

# Where a list of a call's arguments comes from: the shared reader, the rlang
# function it wraps, the reader that unwraps each of them through an injected
# quosure, or the `call_args` element of a parse this package builds out of one,
# in either spelling of the element read.
#
# One matcher serves both scans below, because the hazard is one hazard: a list
# reached this way holds elements that may be empty, whichever construct then
# binds one. Written separately, the `for` scan matched a direct
# `static_call_args()` call alone, so the same loop over a local -- the shape
# `removal_retained_bound()` sits one line away from -- passed both gates while
# the prose above claimed otherwise.
#
# `unwrap_injected_args()` is here rather than recognized as an `lapply()` over
# one of the others, which is what it is (#169): a mapping spelled out at each
# call site would have to be matched by the function being mapped, and
# `grouping_helper_vars()` is handed its arguments as a parameter, so nothing in
# its body says the list it maps over is a call's arguments at all. A reader
# with a name is what this list is written to hold, and adding one to it is how
# the package says a list of parts came from somewhere new.
reads_call_arguments <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  head <- expr[[1L]]
  named_reader <-
    identical(head, quote(static_call_args)) ||
    identical(head, quote(rlang::call_args)) ||
    identical(head, quote(unwrap_injected_args))
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
call_argument_locals <- function(expr) {
  found <- character()
  visit_calls(expr, function(node) {
    if (
      identical(node[[1L]], quote(`<-`)) &&
        length(node) == 3L &&
        is.symbol(node[[2L]]) &&
        reads_call_arguments(node[[3L]])
    ) {
      found <<- unique(c(found, as.character(node[[2L]])))
    }
  })
  found
}

is_call_arguments <- function(expr, locals) {
  reads_call_arguments(expr) ||
    (is.symbol(expr) && as.character(expr) %in% locals)
}

# The scan both gates below are, once the predicate is taken out of it: of the
# functions a namespace binds, the names of those whose body the predicate
# matches. Every predicate handed to it here matches a walk, which is what the
# name says; what it returns is a set of names, named rather than counted so a
# failure says which walk to rewrite.
#
# The predicate is the only thing it takes for itself. The two it deliberately
# does not take are the witness and the message: each gate's witness names what
# that gate reads -- `static_call_args` for the `for` scan,
# `parse_across_arguments` for the `<-` scan -- and each remedy is written for
# its own spelling, so an argument for either would exist only to be different,
# and would make the two look interchangeable. They stay in the `test_that()`
# block that means them.
#
# The enumeration and the namespace come from the caller rather than from
# defaults here. A gate then asserts its witness against the very set this
# iterates, the two arguments cannot come to describe different namespaces, and
# `namespace_functions()` keeps being passed the namespace its caller reads
# bodies from -- which is what that function's own default is written around.
#
# Local to this source rather than in `helper-namespace-walk.R`: that file
# exists because testthat gives each test file its own environment, so one
# file's gate cannot see a reader another defined, and both gates here are in
# one file. The other scan of this shape, in `test-grouping-plan.R`, decides by
# deparsing rather than by walking and says so where it stands, so a shared home
# would be a home for one caller.
walks_matching <- function(predicate, functions, ns) {
  matched <- vapply(
    functions,
    function(name) predicate(body(get(name, envir = ns))),
    logical(1)
  )
  functions[matched]
}

# By subscript throughout, since the code being scanned is exactly the code that
# may hold an empty argument.
binds_call_parts_with_for <- function(expr) {
  locals <- call_argument_locals(expr)
  found <- FALSE
  visit_calls(expr, function(node) {
    if (
      identical(node[[1L]], quote(`for`)) &&
        length(node) >= 3L &&
        is_call_arguments(node[[3L]], locals)
    ) {
      found <<- TRUE
    }
  })
  found
}

test_that("no walk binds a call's parts with `for`", {
  ns <- asNamespace("marginplyr")
  functions <- namespace_functions(ns)
  # The scan iterates over this set, so a set that arrived empty is a set that
  # passes. `static_call_args()` itself is the cheapest witness that the
  # namespace was read.
  expect_true("static_call_args" %in% functions)

  expect_equal(
    walks_matching(binds_call_parts_with_for, functions, ns),
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
  # And over the reader that unwraps each argument through an injected quosure,
  # whose result is a list of parts like any other: the empty argument goes into
  # the unwrapping and comes out of it (#169). Once through a list the same body
  # read, and once in a function handed its arguments -- the shape
  # `grouping_helper_vars()` has, and the one the reader has to be recognized by
  # its name for, since nothing else in such a body says where the list came
  # from.
  through_derived <- function(expr) {
    args <- static_call_args(expr)
    carried <- unwrap_injected_args(args)
    for (part in carried) {
      part
    }
  }
  through_derived_parameter <- function(args) {
    carried <- unwrap_injected_args(args)
    for (part in carried) {
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
  expect_true(binds_call_parts_with_for(body(through_derived)))
  expect_true(binds_call_parts_with_for(body(through_derived_parameter)))
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
binds_call_parts_by_assign <- function(expr) {
  locals <- call_argument_locals(expr)
  found <- FALSE
  visit_calls(expr, function(node) {
    assigns_element <- identical(node[[1L]], quote(`<-`)) &&
      length(node) == 3L &&
      is.symbol(node[[2L]]) &&
      is.call(node[[3L]]) &&
      identical(node[[3L]][[1L]], quote(`[[`)) &&
      length(node[[3L]]) >= 2L
    if (assigns_element && is_call_arguments(node[[3L]][[2L]], locals)) {
      found <<- TRUE
    }
  })
  found
}

test_that("no walk binds a call's parts with `<-`", {
  ns <- asNamespace("marginplyr")
  functions <- namespace_functions(ns)
  expect_true("parse_across_arguments" %in% functions)

  expect_equal(
    walks_matching(binds_call_parts_by_assign, functions, ns),
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
  # The same subscript, one list along: what the unwrapping reader collected
  # from a call's arguments holds an empty one wherever the arguments did
  # (#169). Read from a local and from a parameter, as above.
  through_derived <- function(expr) {
    args <- static_call_args(expr)
    carried <- unwrap_injected_args(args)
    part <- carried[[1L]]
    part
  }
  through_derived_parameter <- function(args) {
    carried <- unwrap_injected_args(args)
    part <- carried[[1L]]
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
  expect_true(binds_call_parts_by_assign(body(through_derived)))
  expect_true(binds_call_parts_by_assign(body(through_derived_parameter)))
  expect_false(binds_call_parts_by_assign(body(compliant_local)))
  # An ordinary list read is not the shape, or the scan would name most of the
  # package and mean nothing.
  expect_false(binds_call_parts_by_assign(body(compliant_list)))
  expect_false(binds_call_parts_by_assign(quote(sum(value[]))))
})

# The third gate over the same namespace, and the one that reads a reader
# rather than a walk over a call's parts. A pair of parentheses is read through
# by restarting on what it wraps, and `unparenthesized_value()` stops at a pair
# holding R's empty argument rather than unwrapping to it -- so a restart
# guarded by `is_redundant_parens()` re-enters on the node it was called with
# and never terminates. `is_parenthesized()` is the guard that cannot: it asks
# whether unwrapping answers a different node, which is `FALSE` for exactly the
# pair the peeler stops at.
#
# Scanned rather than listed, for the reason the gates above are. Two readers
# were written under the weaker guard and both recursed without bound, one
# reached from a share expression and one from a summary holding no share
# helper at all (#349) -- so a list would be a list the third reader is not on.
#
# What it matches is the combination and not either half. Both readers in the
# pair vocabulary -- `unparenthesized_call()` and `unparenthesized_name()` --
# test `is_redundant_parens()` and read the peeler beside it, and both are
# correct, because what they do with the answer is bind it and return: no
# re-entry, so no node to shrink. Handing it to another call is the re-entry,
# and `captured_call_parts()` tests the pair without reading the peeler at all.
reads_unparenthesized_value <- function(node, index) {
  part <- node[[index]]
  is.call(part) && identical(part[[1L]], quote(unparenthesized_value))
}

# Whether a body hands what the peeler answers straight to another call, which
# is the shape of a reader restarting on the unwrapped node. An assignment is
# not one: `spelled <- unparenthesized_value(expr)` is the binding both readers
# in the pair vocabulary make, and what follows it re-enters nothing.
hands_on_unparenthesized_value <- function(expr) {
  found <- FALSE
  visit_calls(expr, function(node) {
    head <- node[[1L]]
    passes_on <- !identical(head, quote(unparenthesized_value)) &&
      !identical(head, quote(`<-`)) &&
      !identical(head, quote(`=`))
    if (!passes_on) {
      return(invisible(NULL))
    }
    # By subscript, since a scanned body may hold an empty argument of its own.
    for (index in seq_along(node)[-1L]) {
      if (reads_unparenthesized_value(node, index)) {
        found <<- TRUE
      }
    }
  })
  found
}

tests_redundant_parens <- function(expr) {
  found <- FALSE
  visit_calls(expr, function(node) {
    if (identical(node[[1L]], quote(is_redundant_parens))) {
      found <<- TRUE
    }
  })
  found
}

restarts_on_an_unshrunk_pair <- function(expr) {
  tests_redundant_parens(expr) && hands_on_unparenthesized_value(expr)
}

test_that("no reader restarts on a pair of parentheses it cannot shrink", {
  ns <- asNamespace("marginplyr")
  functions <- namespace_functions(ns)
  # The scan iterates over this set, so a set that arrived empty is a set that
  # passes.
  expect_true("unparenthesized_value" %in% functions)

  expect_equal(
    walks_matching(restarts_on_an_unshrunk_pair, functions, ns),
    character(),
    info = paste(
      "Guard the restart with `is_parenthesized()` instead --",
      "it answers `FALSE` for the pair `unparenthesized_value()` stops at,",
      "so the reader cannot re-enter on the node it was called with."
    )
  )
})

test_that("the restart scan detects the shape it is written to forbid", {
  # Asserted before the scan's verdict means anything, for the reason the two
  # gates above assert their own: a scan that stopped matching reports exactly
  # what a clean package reports.
  offending <- function(expr) {
    if (is_redundant_parens(expr)) {
      return(offending(unparenthesized_value(expr)))
    }
    expr
  }
  # The same restart one call deeper, and one written as a plain statement
  # rather than a `return()`: neither is a shape a reader may be written in.
  nested <- function(expr) {
    if (TRUE) {
      lapply(seq_len(2L), function(i) {
        if (is_redundant_parens(expr)) {
          nested(unparenthesized_value(expr))
        }
      })
    }
  }
  # Correct, and the two shapes the gate must not name: the pair vocabulary's
  # own readers, which bind the peeler's answer and return without re-entering,
  # and a reader that restarts under the guard that answers `FALSE` for the
  # pair the peeler stops at.
  compliant_binding <- function(expr) {
    if (!is_redundant_parens(expr)) {
      return(expr)
    }
    spelled <- unparenthesized_value(expr)
    if (is_nameable_call(spelled)) {
      return(spelled)
    }
    expr
  }
  compliant_restart <- function(expr) {
    if (is_parenthesized(expr)) {
      return(compliant_restart(unparenthesized_value(expr)))
    }
    expr
  }
  compliant_test <- function(expr) {
    if (is_redundant_parens(expr)) {
      return(NULL)
    }
    expr
  }

  expect_true(restarts_on_an_unshrunk_pair(body(offending)))
  expect_true(restarts_on_an_unshrunk_pair(body(nested)))
  expect_false(restarts_on_an_unshrunk_pair(body(compliant_binding)))
  expect_false(restarts_on_an_unshrunk_pair(body(compliant_restart)))
  expect_false(restarts_on_an_unshrunk_pair(body(compliant_test)))
  # An empty argument in the scanned code must not abort the scan, which is the
  # bug the gates in this file exist for.
  expect_false(restarts_on_an_unshrunk_pair(quote(sum(value[]))))
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
  skip_if_suggest_absent("arrow")

  error <- expect_error(
    assert_lazy_table(arrow::as_record_batch_reader(data.frame())),
    "must not be an object of the following class"
  )

  expect_s3_class(error, "marginplyr_error")
})
