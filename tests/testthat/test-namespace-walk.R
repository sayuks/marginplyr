# `helper-namespace-walk.R` holds the two readings every structural gate in
# this suite shares, and this file is what keeps them there. A gate that
# enumerated the namespace itself would be a second copy of the reading, and
# the copy is what drifts: the walk over a call's elements was independently
# re-derived three times before it was shared, and two of the three carried
# defensive code the third did not need.
#
# The enumeration is what is scanned for, rather than the recursion. It has one
# spelling -- a function test over a listing that includes the hidden names --
# so a source holding it is visible from the outside, while a recursion can be
# written any number of ways and is only recognizable once read, which is why
# the markers below are named rather than quoted. A gate that enumerates is a
# gate that will walk, so holding the enumeration in one place is what puts
# every walker in front of the shared visitor.
#
# It reads the test sources rather than the loaded namespace, because a test
# helper is not bound in it. Two things follow from that, and both have
# precedent here. The markers are assembled rather than written out, as
# `test-grouping-plan.R` assembles the plan compiler's name for its own source
# scan, or this gate would report itself. And the self-witness below runs the
# predicate over synthetic lines rather than over an offending file written
# beside this one, which the gate would find and fail on.

# Whether these lines hold the namespace enumeration: the function test and the
# hidden-name listing that together name every function a namespace binds.
# Both, because either alone is ordinary -- `test-optional-backends.R` asserts
# a guard is a function, and `test-share-backends.R` lists a recorded-verdicts
# environment -- and neither of those is walking a namespace.
enumerates_namespace_functions <- function(lines) {
  markers <- c(paste0("all", ".names"), paste0("is", ".function"))
  all(vapply(
    markers,
    function(marker) any(grepl(marker, lines, fixed = TRUE)),
    logical(1)
  ))
}

sources_enumerating_functions <- function(sources) {
  Filter(
    function(path) enumerates_namespace_functions(readLines(path)),
    sources
  )
}

test_that("the namespace enumeration is written in one place", {
  # The sources sit beside this file whenever the suite runs, so the scan needs
  # no installed copy; the assertion refuses a scan that found nothing rather
  # than passing on one, since every verdict below is a Filter over this set.
  sources <- list.files(".", pattern = "^(test|helper)-.*\\.R$")
  expect_gt(length(sources), 1L)

  expect_identical(
    sources_enumerating_functions(sources),
    "helper-namespace-walk.R",
    info = paste(
      "Take the enumeration from `namespace_functions()` instead --",
      "it is in `helper-namespace-walk.R`, beside the shared visitor."
    )
  )
})

test_that("the reuse scan detects the shape it is written to forbid", {
  # Assembled for the reason the markers are: a copy of the enumeration written
  # out here would be a copy of the enumeration in a test source, which is
  # exactly what the gate above reports.
  listing <- paste0("  ls(ns, all", ".names = TRUE)")
  tested <- paste0("  function(binding) is", ".function(get(binding, ns)),")
  offending <- c("  Filter(", tested, listing, "  )")

  expect_true(enumerates_namespace_functions(offending))
  # Either marker alone is not the enumeration, which is what keeps the two
  # sources holding one of them correctly outside the gate.
  expect_false(enumerates_namespace_functions(listing))
  expect_false(enumerates_namespace_functions(tested))
  expect_false(enumerates_namespace_functions(character()))
})
