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
# Whichever such assignment the walk reaches last, the visitor stopping at
# none of them. `backend_capabilities()` holds exactly one, and a body that
# held two would be a body with two tables in it -- a state this reading does
# not have to resolve, because the assertion below evaluates what it returns,
# so the wrong one of two shows up as a changed snapshot and not as a pass.
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

# ADR 0020's subject is a read marginplyr *causes*, not one it issues. Every
# reading above walks `R/`, so all of them are structurally blind to a read
# another package performs on marginplyr's behalf -- and one such read exists:
# an Arrow input absorbs an expression its own engine cannot evaluate by
# collecting the whole input while the verb runs. The scans above reported a
# clean result throughout, which reads exactly like a package that causes no
# such read, and is the failure mode this gate closes (#254).
#
# Tracing Arrow's own methods is what the question needs. Nothing marginplyr
# holds afterwards says whether a row was read: an absorbed summary answers
# with a local frame, and so does a summary the caller collected themselves.
# The trace is installed around one expression and removed on exit, so a
# failure here leaves nothing behind for the rest of the file.
# Filtered against the namespace rather than assumed: `DESCRIPTION` admits
# `arrow (>= 13.0.0)`, and `trace()` errors on a name a version does not bind,
# which would fail this gate for a reason that is not a read. What matters is
# that whichever of them the installed Arrow has are watched -- a version
# missing one cannot reach it either.
arrow_collect_methods <- function() {
  candidates <- c(
    "collect.ArrowTabular",
    "collect.arrow_dplyr_query",
    "collect.Dataset"
  )
  intersect(candidates, ls(asNamespace("arrow"), all.names = TRUE))
}

# A function tracer rather than `quote()`, which is the difference between
# counting and silently counting nothing: an expression tracer is evaluated in
# the traced function's own frame, so its `<<-` walks Arrow's namespace and
# assigns into the global environment instead of into this counter.
count_arrow_collects <- function(expr) {
  count <- 0L
  ns <- asNamespace("arrow")
  for (method in arrow_collect_methods()) {
    suppressMessages(trace(
      method,
      where = ns,
      tracer = function() count <<- count + 1L,
      print = FALSE
    ))
  }
  on.exit(
    for (method in arrow_collect_methods()) {
      suppressMessages(untrace(method, where = ns))
    },
    add = TRUE
  )
  force(expr)
  count
}

test_that("no Arrow read happens while a Margin verb runs", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(
    k = c("E", "E", "W"),
    v = c(1, 2, 3),
    s = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  table <- arrow::Table$create(data)

  # A summary Arrow evaluates itself: the verb builds a query and returns it.
  expect_identical(
    count_arrow_collects(summarize_with_margins(
      table,
      total = sum(v),
      .grouping = rollup(k)
    )),
    0L
  )

  # An expansion carries no caller expression at all, so it cannot reach an
  # absorbed one. Asserted rather than assumed, since that is a property of
  # the signature and signatures change.
  expect_identical(
    count_arrow_collects(expand_with_margins(table, .grouping = rollup(k))),
    0L
  )

  # And the refusing path reads nothing, which is the criterion the refuse
  # disposition of #254 is held to. `try()` keeps the refusal from leaving
  # before the count is read.
  expect_identical(
    count_arrow_collects(try(
      summarize_with_margins(
        table,
        joined = paste(s, collapse = ","),
        .grouping = rollup(k)
      ),
      silent = TRUE
    )),
    0L
  )
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
