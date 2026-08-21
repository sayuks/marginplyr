# ADR 0020 says marginplyr sends no query that reads a lazy input's data
# unless the caller asked for one, with two enumerated exemptions. That rule
# is not readable from any one file -- it is a property of the call graph
# from the public verbs down to whatever function actually executes a query
# -- so this file asserts it structurally instead of describing it.
#
# `R/` is not installed, so scanning the shipped package would find nothing
# to grep. The readers below walk `body()` of every function bound in the
# loaded marginplyr namespace instead, which is why every test here needs the
# package loaded via `pkgload::load_all()` or an installed dev build, not a
# CRAN-style installed copy of a release. The enumeration of those functions
# and the recursion over one body are shared with every other structural gate
# and come from `helper-namespace-walk.R`.

# The name each call below `expr` is to. A head that is not a symbol names no
# target of its own, and the shared visitor reaches the call it holds anyway.
call_targets <- function(expr) {
  targets <- character()
  visit_calls(expr, function(node) {
    head <- node[[1]]
    if (!is.name(head)) {
      return(invisible(NULL))
    }
    head_chr <- as.character(head)
    if (
      (identical(head_chr, "::") || identical(head_chr, ":::")) &&
        length(node) == 3
    ) {
      targets[[length(targets) + 1L]] <<- paste0(
        as.character(node[[2]]), "::", as.character(node[[3]])
      )
    } else {
      targets[[length(targets) + 1L]] <<- head_chr
    }
  })
  unique(targets)
}

# `backend_capabilities()` (`R/grouping-backend.R`) is the one place the
# backend-kind universe and the `collect_selection_proxy` grant are recorded,
# in a local `enabled <- list(...)` assignment. It is a self-contained
# literal, so finding that assignment's right-hand side and evaluating it
# reads the same table the package reads, rather than a copy of it kept here
# by hand.
#
# The last such assignment in the body, since the visitor reaches every one of
# them. A body assigning the name twice would be a body with two tables in it,
# which is the state this reading exists to make impossible to have quietly:
# the assertion below evaluates what it returns, so the wrong one of two would
# show up as a changed snapshot rather than as a silent pass.
find_local_assignment <- function(fn, var_name) {
  found <- NULL
  visit_calls(body(fn), function(node) {
    if (
      identical(as.character(node[[1]]), "<-") &&
        length(node) == 3 &&
        is.symbol(node[[2]]) &&
        identical(as.character(node[[2]]), var_name)
    ) {
      found <<- node[[3]]
    }
  })
  found
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
  internal_functions <- namespace_functions(ns)
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
