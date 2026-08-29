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
#
# `tibble::as_tibble` is a member because it is the generic that receives the
# caller's lazy object on a conversion path where `as.data.frame` receives only
# a local one: converting a dtplyr step evaluates the step first and converts
# the resulting `data.table`, so the read goes uncounted (#301).
# `as.data.frame()` written directly against that same input is counted, dtplyr
# registering a method the base generic dispatches from, so what this entry
# covers is the conversion and not the backend. It is traced at the generic
# rather than at `as_tibble.dtplyr_step`, for the reason given below for
# `dplyr::collect`.
#
# `subject_test` says whether counting an invocation requires knowing what the
# call was applied to, and it is enumerated for the same reason the membership
# is. `as.data.frame` needs one because its name is also how unrelated code
# converts unrelated objects -- a verb reads its input's schema through it
# repeatedly, and no such conversion reaches a backend. `as_tibble` needs one
# for the same reason, and reaches it on the path this entry exists for:
# converting a dtplyr step invokes the generic twice, once on the step and once
# on the local table it produced. No `DBI::` entry takes one: invoking it is
# itself a read, and its first argument is a connection rather than the caller's
# data, so a subject test there would count nothing at all.
lazy_execution_entry_points <- function() {
  data.frame(
    package = c(
      "dplyr", "dplyr", "dplyr", "base", "tibble",
      "DBI", "DBI", "DBI", "DBI", "DBI"
    ),
    name = c(
      "collect", "compute", "pull", "as.data.frame", "as_tibble",
      "dbGetQuery", "dbSendQuery", "dbSendStatement", "dbFetch", "dbReadTable"
    ),
    subject_test = c(
      FALSE, FALSE, FALSE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Both spellings of every entry, because a call target is whatever `R/` wrote.
# `as.data.frame` is spelled bare there and `dplyr::collect` qualified, so a
# derivation producing one form would stop matching the other -- and which form
# a call site uses is not a property this catalog knows.
entry_point_targets <- function() {
  catalog <- lazy_execution_entry_points()
  unique(c(catalog$name, paste0(catalog$package, "::", catalog$name)))
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
    entry_point_targets()
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
# Watching the reads as they happen is what the question needs. Nothing
# marginplyr holds afterwards says whether a row was read: an absorbed summary
# answers with a local frame, and so does a summary the caller collected
# themselves. The trace is installed around one expression and removed on exit,
# so a failure here leaves nothing behind for the rest of the file.
#
# Derived from the catalog above rather than listed again, and traced at the
# *generic* rather than at a backend's methods. `trace()` rewrites a namespace
# binding, while S3 dispatch reaches the copy `registerS3method()` left in the
# methods table, so tracing `arrow:::collect.ArrowTabular` sees only Arrow's own
# by-name call and misses every `dplyr::collect()` a caller or `R/` makes --
# which is the read this gate exists to see. Measured on arrow 25.0.1: a plain
# `dplyr::collect()` of an Arrow table counted 0 that way and counts 1 this way.
#
# Every entry the catalog names is traced, including the base one; what varies
# is whether its tracer counts unconditionally or asks what the call was applied
# to. The subject is read from the traced function's signature rather than
# recorded beside it, so a formal is written down once, in the package that
# defines it. An entry asking for a subject test on a function with no formals
# is an error: it would otherwise install a tracer that counts nothing and reads
# as one that found nothing.
traced_execution_entry_points <- function() {
  catalog <- lazy_execution_entry_points()
  reachable <- vapply(
    seq_len(nrow(catalog)),
    function(row) {
      package <- catalog$package[[row]]
      isNamespaceLoaded(package) &&
        exists(catalog$name[[row]], envir = asNamespace(package))
    },
    logical(1)
  )
  available <- catalog[reachable, , drop = FALSE]
  # The flag is resolved into a subject here and not carried onto the record:
  # `$subject` partially matches `subject_test`, so a record holding both sends
  # an unfiltered entry down the filtered branch with `FALSE` for a name.
  lapply(seq_len(nrow(available)), function(row) {
    package <- available$package[[row]]
    name <- available$name[[row]]
    subject <- NULL
    if (available$subject_test[[row]]) {
      formal_names <- names(formals(get(name, envir = asNamespace(package))))
      if (length(formal_names) == 0L) {
        stop("No subject to test on ", package, "::", name, call. = FALSE)
      }
      subject <- formal_names[[1L]]
    }
    list(package = package, name = name, subject = subject)
  })
}

# Whether an object is a lazy input marginplyr accepts, asked of the package's
# classifier rather than of the classes it reads.
#
# `local` is excluded because no external system is involved, and `other`
# because it is where every unrelated conversion lands -- an `arrow::schema()`,
# a list, an integer. Both kind names are spelled here rather than derived, and
# the two negative controls below hold them: renaming either starts counting
# what it excludes. An unrecognised lazy object materialised this way goes
# uncounted, which is a bound this gate states rather than one it hides.
#
# Nothing on this path is caught, here or at the subject read below. Every
# assertion either serves expects a zero, so answering "not a backend" for an
# object that could not be read or classified is the one wrong answer
# available: it turns a read this gate exists to see into a passing test.
is_lazy_backend_input <- function(x) {
  !grouping_backend(x)$kind %in% c("local", "other")
}

# One installation per entry, in a frame of its own.
#
# The tracer has to be a function *literal* written at the `trace()` call, and a
# variable holding the same closure is not the same thing. `trace()` substitutes
# this argument: a literal is inserted as the expression it is and resolves
# where it was installed, while a variable is inserted as its bare name and
# looked up in the traced function's own namespace, where it does not exist.
#
# A literal cannot be parameterized, so per-entry state comes from this frame
# rather than from a loop variable, which every tracer would read after the loop
# had stopped on its last entry. This frame outlives the call because the
# installed literal holds it.
install_read_tracer <- function(entry, counter) {
  subject <- entry$subject
  if (is.null(subject)) {
    suppressMessages(trace(
      entry$name,
      where = asNamespace(entry$package),
      tracer = function() counter$count <- counter$count + 1L,
      print = FALSE
    ))
    return(invisible(NULL))
  }
  suppressMessages(trace(
    entry$name,
    where = asNamespace(entry$package),
    tracer = function() {
      # Read into a name of its own first. `parent.frame()` answers from the
      # call stack, so writing it as an argument to a call this frame does not
      # make would answer for whichever frame forced the promise.
      frame <- parent.frame()
      value <- get(subject, envir = frame, inherits = FALSE)
      if (is_lazy_backend_input(value)) {
        counter$count <- counter$count + 1L
      }
    },
    print = FALSE
  ))
  invisible(NULL)
}

# An environment rather than a local integer, because the count is written from
# a frame that is not this one.
count_backend_reads <- function(expr) {
  counter <- new.env(parent = emptyenv())
  counter$count <- 0L
  entries <- traced_execution_entry_points()
  # Registered before the first trace is installed rather than after the last.
  # `trace()` failing part-way through the loop would otherwise leave every
  # trace already installed in place for the rest of the run -- a whole suite,
  # not a file -- each one incrementing a counter in a frame that has gone.
  # `untrace()` on a function that was never traced is a no-op, so removing
  # more than was installed is safe and removing less is not.
  on.exit(
    for (entry in entries) {
      suppressMessages(untrace(entry$name, where = asNamespace(entry$package)))
    },
    add = TRUE
  )
  for (entry in entries) {
    install_read_tracer(entry, counter)
  }
  force(expr)
  counter$count
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

  # The mechanism, asserted before anything is concluded from it. Every
  # expectation below is a zero, so a counter that counted nothing would report
  # exactly what a package that reads nothing reports -- which is the shape
  # `AGENTS.md` rules out for every derived gate, and the shape this counter
  # had while it traced Arrow's methods.
  expect_gt(count_backend_reads(dplyr::collect(table)), 0L)

  # The same mechanism for the one entry counted by what it was applied to. Both
  # directions are asserted, because a subject test that answered `TRUE` for
  # everything and one that answered `FALSE` for everything would each leave the
  # zeroes below reading exactly as they do now. The negative control converts
  # what `grouping_selection_proxy()` converts, so the call it must not count is
  # one the package actually makes.
  expect_gt(count_backend_reads(as.data.frame(table)), 0L)
  expect_identical(count_backend_reads(as.data.frame(arrow::schema(table))), 0L)
  expect_identical(count_backend_reads(as.data.frame(data)), 0L)

  # A summary Arrow evaluates itself: the verb builds a query and returns it.
  expect_identical(
    count_backend_reads(summarize_with_margins(
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
    count_backend_reads(expand_with_margins(table, .grouping = rollup(k))),
    0L
  )

  # And the refusing path reads nothing, which is the criterion the refuse
  # disposition of #254 is held to. Over every input class that absorbs rather
  # than over the one: what raises the refusal before the read is a class check,
  # so a class it stopped recognising would be read by Arrow and caught only
  # afterwards by the guard -- a difference invisible in the condition the
  # caller receives, both arms raising the same refusal, and visible only here.
  # `try()` keeps the refusal from leaving before the count is read.
  batch <- arrow::record_batch(data)
  absorbing <- list(
    table = table,
    record_batch = batch,
    table_query = dplyr::select(table, dplyr::all_of(names(data))),
    batch_query = dplyr::select(batch, dplyr::all_of(names(data)))
  )
  for (shape in names(absorbing)) {
    expect_identical(
      count_backend_reads(try(
        summarize_with_margins(
          absorbing[[shape]],
          joined = paste(s, collapse = ","),
          .grouping = rollup(k)
        ),
        silent = TRUE
      )),
      0L,
      info = shape
    )
  }
})

# The conversion path the catalog's `tibble::as_tibble` entry exists for, and
# the backend that takes it.
#
# Every expectation here names the package it calls into. `trace()` rewrites a
# namespace binding and the copy an importing package's imports environment
# holds, but not the one the attached `package:tibble` environment holds, which
# is where a bare `as_tibble()` written here would resolve -- so that spelling
# counts nothing while reading exactly as these do (#303).
#
# The two deprecated spellings are asserted because one entry covering all three
# is the reason there is one entry: both delegate to the generic from inside
# tibble's own namespace, so the caller's spelling does not decide whether the
# traced binding is reached.
test_that("a dtplyr materialization through the as_tibble family is counted", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    k = c("E", "E", "W"),
    v = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_gt(count_backend_reads(tibble::as_tibble(dtplyr::lazy_dt(data))), 0L)
  expect_gt(count_backend_reads(dplyr::as_tibble(dtplyr::lazy_dt(data))), 0L)
  expect_gt(
    count_backend_reads(
      suppressWarnings(tibble::as.tibble(dtplyr::lazy_dt(data)))
    ),
    0L
  )
  expect_gt(
    count_backend_reads(
      suppressWarnings(tibble::as_data_frame(dtplyr::lazy_dt(data)))
    ),
    0L
  )

  # The other direction, for the reason the `as.data.frame` controls give
  # above: a subject test answering `TRUE` for everything leaves every zero in
  # this file reading exactly as it does now.
  expect_identical(count_backend_reads(tibble::as_tibble(data)), 0L)
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
