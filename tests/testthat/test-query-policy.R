# ADR 0020 says marginplyr sends no query that reads a lazy input's data
# unless the caller asked for one, with two enumerated exemptions. That rule
# is not readable from any one file -- it is a property of the call graph
# from the public verbs down to whatever function actually executes a query
# -- so this file asserts it structurally instead of describing it.
#
# `R/` is not installed, so scanning the shipped package would find nothing
# to grep. The helpers below walk `body()` of every function bound in the
# loaded marginplyr namespace instead, which is why every test here needs the
# package loaded via `pkgload::load_all()` or an installed dev build, not a
# CRAN-style installed copy of a release.

# A parsed call can hold the missing-argument placeholder as one of its own
# elements -- a `switch()` fallthrough branch such as
# `switch(x, a = , b = foo())`, or a subsetting call such as `x[i, ]` both
# produce one. Reading that placeholder through an ordinary `for` loop raises
# "argument is missing, with no default" the moment the loop variable is
# looked up, even though nothing about the walk is actually missing an
# argument: a `for` loop's own assignment preserves the internal missing flag,
# and normal symbol lookup checks for it. Binding the same value through a
# function argument does not carry the flag forward, which is why the
# recursion below goes through `lapply()` and never through a bare `for` over
# a call's elements.
call_targets <- function(expr) {
  targets <- character()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    head <- e[[1]]
    if (is.name(head)) {
      head_chr <- as.character(head)
      if (
        (identical(head_chr, "::") || identical(head_chr, ":::")) &&
          length(e) == 3
      ) {
        targets[[length(targets) + 1L]] <<- paste0(
          as.character(e[[2]]), "::", as.character(e[[3]])
        )
      } else {
        targets[[length(targets) + 1L]] <<- head_chr
      }
    } else {
      walk(head)
    }
    lapply(as.list(e)[-1], walk)
    invisible(NULL)
  }
  walk(expr)
  unique(targets)
}

# `backend_capabilities()` (`R/grouping-backend.R`) is the one place the
# backend-kind universe and the `collect_selection_proxy` grant are recorded,
# in a local `enabled <- list(...)` assignment. It is a self-contained
# literal, so finding that assignment's right-hand side and evaluating it
# reads the same table the package reads, rather than a copy of it kept here
# by hand.
find_local_assignment <- function(fn, var_name) {
  found <- NULL
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (
      identical(as.character(e[[1]]), "<-") &&
        length(e) == 3 &&
        is.symbol(e[[2]]) &&
        identical(as.character(e[[2]]), var_name)
    ) {
      found <<- e[[3]]
      return(invisible(NULL))
    }
    lapply(as.list(e), walk)
    invisible(NULL)
  }
  walk(body(fn))
  found
}

marginplyr_internal_functions <- function(ns = asNamespace("marginplyr")) {
  all_names <- ls(ns, all.names = TRUE)
  Filter(function(name) is.function(get(name, envir = ns)), all_names)
}

# Enumerated rather than derived from a shape, exactly as ADR 0020 enumerates
# its two exemptions rather than deriving them: an execution entry point is
# whatever a vendor's client library exposes for running a query and reading
# a result, and no static property of a function distinguishes one from an
# ordinary query-building call. `dplyr::show_query()` is deliberately absent
# -- ADR 0020 states that it runs nothing.
lazy_execution_entry_points <- function() {
  c(
    "dplyr::collect",
    "dplyr::compute",
    "dplyr::pull",
    "as.data.frame",
    "DBI::dbGetQuery",
    "DBI::dbSendQuery",
    "DBI::dbSendStatement",
    "DBI::dbFetch",
    "DBI::dbReadTable"
  )
}

# The functions reaching an entry point directly, plus every function calling
# one of those, to a fixed point -- the call graph is not asserted to be
# acyclic, so growing the reachable set once is not enough.
functions_reaching_entry_point <- function(call_graph, entry_points) {
  reaches <- function(targets, allowed) any(targets %in% allowed)
  reach <- names(call_graph)[
    vapply(call_graph, reaches, logical(1), allowed = entry_points)
  ]
  repeat {
    grown <- names(call_graph)[
      vapply(call_graph, reaches, logical(1), allowed = reach)
    ]
    grown <- union(reach, grown)
    if (setequal(grown, reach)) {
      break
    }
    reach <- grown
  }
  sort(reach)
}

test_that("the scanned entry-point set is the ADR 0020 execution catalog", {
  expect_snapshot(lazy_execution_entry_points())
})

test_that("marginplyr functions reaching an execution entry point", {
  ns <- asNamespace("marginplyr")
  internal_functions <- marginplyr_internal_functions(ns)
  call_graph <- stats::setNames(
    lapply(internal_functions, function(name) {
      call_targets(body(get(name, envir = ns)))
    }),
    internal_functions
  )

  reach <- functions_reaching_entry_point(
    call_graph,
    lazy_execution_entry_points()
  )

  expect_snapshot(reach)
})

test_that("backend kinds granted the collect_selection_proxy capability", {
  ns <- asNamespace("marginplyr")
  enabled_expr <- find_local_assignment(
    get("backend_capabilities", envir = ns),
    "enabled"
  )
  enabled <- eval(enabled_expr, envir = baseenv())

  kinds_with_proxy <- sort(names(enabled)[
    vapply(enabled, function(capabilities) {
      "collect_selection_proxy" %in% capabilities
    }, logical(1))
  ])

  expect_snapshot(kinds_with_proxy)
})
