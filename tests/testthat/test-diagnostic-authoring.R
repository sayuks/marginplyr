# ADR 0023 states how every Package condition is authored. Two of its rules are
# gated, and one structural reading catches both: walk every
# `abort_marginplyr()` call and fail any whose message argument is not a literal
# in the source.
#
# The two rules are one violation seen from two sides. *Caller text is a value,
# never part of the template* fails when a template is assembled -- a
# `paste0()` splicing a column name produces a template a caller's braces can
# reach into, which is why a column named `a{b}` would otherwise be answered
# with `Could not evaluate cli {} expression` instead of the refusal. *Every
# singular/plural choice goes through `{?}`* fails when an `if` spells a noun.
# Neither can be written without computing the argument, so neither survives the
# reading below.
#
# The line-length conditions deliberately get no gate here; ADR 0023 says why,
# and `.github/scripts/verify-site.R` is where their failure mode shows up.
#
# This file is in the shape of `test-query-policy.R` and for the same reason:
# the property is about every call site rather than about any one file, so it is
# asserted over the loaded namespace rather than described in prose. It needs
# the package loaded through `pkgload::load_all()` or an installed dev build.

# Every call to `name` below `expr`, as the expression each passed as its
# message argument -- the first argument, or `message =` where a site names it.
#
# The recursion goes through `lapply()` and never a bare `for` over a call's
# elements, for the reason `test-query-policy.R` records: a parsed call can hold
# the missing-argument placeholder as one of its own elements, and a `for`
# loop's assignment preserves the internal missing flag, so reading it raises
# "argument is missing, with no default" the moment the loop variable is looked
# up.
diagnostic_message_arguments <- function(expr, name) {
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    head <- e[[1]]
    if (is.name(head) && identical(as.character(head), name)) {
      args <- as.list(e)[-1]
      arg_names <- names(args)
      # R's own matching for `abort_marginplyr(message, ..., class, call)`:
      # `message =` by name, otherwise the first argument supplied without one.
      # Reading `args[[1]]` instead would take `class` for the message at a
      # site that named every argument it passed, and report that site clean
      # because a class is a literal too.
      positional <- if (is.null(arg_names)) args else args[!nzchar(arg_names)]
      message <- if (!is.null(arg_names) && "message" %in% arg_names) {
        args[["message"]]
      } else if (length(positional) > 0L) {
        positional[[1]]
      } else {
        NULL
      }
      # `c()` rather than `found[[length(found) + 1L]] <-`, which deletes
      # instead of appending when the value is `NULL`. A call written with no
      # message at all -- `abort_marginplyr(class = "x")` -- is exactly the
      # site the gate must report, and that spelling would drop it.
      found <<- c(found, list(message))
    }
    lapply(as.list(e)[-1], walk)
    invisible(NULL)
  }
  walk(expr)
  found
}

# Whether every string a message argument contributes is written in the source.
#
# A bare character literal is the common case. `c()` is admitted because it is
# how a cli message vector is spelled -- `c("Refusal.", i = "Bullet.")` -- and
# admitting it costs nothing: the recursion requires each of its arguments to be
# authored too, so `c(paste0(...), i = "...")` fails exactly as a bare
# `paste0()` does. Nothing else is admitted, because everything else computes
# text, and computed text is what both gated rules forbid. That includes a
# constant bound elsewhere in the package: a template has to be readable beside
# the call that raises it, or the injection rule cannot be reviewed at the site.
authored_template <- function(expr) {
  if (is.character(expr)) {
    return(TRUE)
  }
  if (
    is.call(expr) &&
      is.name(expr[[1]]) &&
      identical(as.character(expr[[1]]), "c")
  ) {
    return(all(vapply(as.list(expr)[-1], authored_template, logical(1))))
  }
  FALSE
}

# Every function bound in the namespace, which is where the call sites are:
# `R/` is not installed, so a scan of the shipped package would find no source
# to read.
marginplyr_diagnostic_sites <- function(name, ns = asNamespace("marginplyr")) {
  functions <- Filter(
    function(binding) is.function(get(binding, envir = ns)),
    ls(ns, all.names = TRUE)
  )
  sites <- lapply(functions, function(binding) {
    diagnostic_message_arguments(body(get(binding, envir = ns)), name)
  })
  stats::setNames(sites, functions)
}

# The reading, driven over source `R/` does not contain. Both halves of it are
# asserted here rather than left to the scan below, because the scan's verdict
# over a corpus that satisfies the rule is the same verdict a scan that read
# nothing returns -- which is the objection this repository makes to any gate
# whose failing branch nothing executes. While #223's phase 3 is unfinished the
# point is sharper still: `abort_marginplyr()` has one call site in the
# namespace, so without these fixtures neither the descent nor
# `authored_template()`'s refusal would ever run.
#
# The fixtures are functions that are parsed and never called. Each one is read
# through `body()`, exactly as the namespace scan reads a real function.
test_that("the reading finds a site wherever it is written", {
  fixture <- function(n) {
    if (n > 0L) {
      abort_marginplyr("A refusal.")
    }
    inner <- function() {
      abort_marginplyr(message = c("A refusal.", i = "A bullet."))
    }
    inner()
  }

  found <- diagnostic_message_arguments(body(fixture), "abort_marginplyr")

  # One nested inside an `if`, one inside a closure and named `message =`. A
  # walk that stopped descending, or one that only read the first argument,
  # loses one of the two.
  expect_length(found, 2L)
  expect_identical(found[[1]], "A refusal.")
  expect_true(all(vapply(found, authored_template, logical(1))))
})

test_that("the reading refuses every shape ADR 0023's gated rules forbid", {
  fixture <- function(columns, n) {
    template <- "A refusal."
    abort_marginplyr(paste0("Unknown column `", columns, "`."))
    abort_marginplyr(sprintf("Unknown column `%s`.", columns))
    abort_marginplyr(if (n == 1L) "One column." else "Several columns.")
    abort_marginplyr(c("A refusal.", i = paste0("Drop ", columns, ".")))
    abort_marginplyr(template)
    abort_marginplyr(class = "marginplyr_nothing")
  }

  found <- diagnostic_message_arguments(body(fixture), "abort_marginplyr")

  # An assembled template, twice; an `if` spelling a noun; a `c()` whose bullet
  # is assembled, which is the case admitting `c()` has to keep refusing; a
  # template bound elsewhere, which is readable nowhere near the refusal; and a
  # call carrying no message at all, which reads as `NULL` and is the site an
  # appending idiom that deletes on `NULL` would drop.
  expect_length(found, 6L)
  expect_false(any(vapply(found, authored_template, logical(1))))
})

test_that("every `abort_marginplyr()` message is a literal in the source", {
  sites <- marginplyr_diagnostic_sites("abort_marginplyr")

  # That the namespace was read at all. The reading itself is asserted by the
  # fixtures above; this only rules out the scan finding no call site --
  # a renamed constructor, or a run against a package that is not loaded --
  # since a scan that read nothing reports exactly what a package with no
  # violation reports.
  expect_gt(sum(lengths(sites)), 0L)

  # Named by the function that holds it and quoted as it was written, so a
  # failure says which site to re-author rather than that some site exists.
  assembled <- unlist(
    lapply(names(sites), function(binding) {
      arguments <- sites[[binding]]
      authored <- vapply(arguments, authored_template, logical(1))
      vapply(
        arguments[!authored],
        function(argument) {
          sprintf("%s(): %s", binding, paste(deparse(argument), collapse = " "))
        },
        character(1)
      )
    }),
    use.names = FALSE
  )

  expect_identical(as.character(assembled), character())
})

# What is left of #223's phase 3, as the functions still handing an assembled
# string to `abort_marginplyr_flat()`. The counts are part of the record because
# a function can hold more than one site, and a snapshot of names alone would
# accept a second one appearing inside a function already listed.
#
# This only ever shrinks. Each phase-3 pull request re-authors one file and
# removes its entries; the last one empties this and deletes
# `abort_marginplyr_flat()` with it. A new entry is a site written in the idiom
# the gate above exists to retire, which the gate itself cannot see -- what it
# reads is `abort_marginplyr()`'s own argument, and inside the transitional
# sibling that argument is a literal.
#
# One line per function rather than a printed vector: this record is read as a
# diff across every phase-3 pull request, and a named vector's column layout
# reflows on any change, which would show a file's worth of removals as a
# rewrite of the whole snapshot.
#
# The gate above reads the source and this reads the namespace, so between them
# they cover what a reviewer would check by eye. Neither one runs a diagnostic,
# which is what the test below is for.
test_that("the sites still assembling their own diagnostic", {
  sites <- marginplyr_diagnostic_sites("abort_marginplyr_flat")
  counts <- lengths(sites)
  counts <- counts[counts > 0L]
  expect_snapshot(
    cat(sprintf("%s(): %d", names(counts), counts), sep = "\n")
  )
})

# The runtime half of *Caller text is a value*, which the source-level gate
# above can only assert a proxy for. It reads whether a template was computed,
# and computing one is how the property gets lost from this side -- but the
# property itself is cli's: a value it interpolates is not re-read as a
# template. `cli (>= 3.4.0)` carries no ceiling, so nothing in DESCRIPTION
# would stop a cli whose value semantics changed, and the source gate would
# report clean while every diagnostic naming a brace-bearing subject answered
# `Could not evaluate cli {} expression` instead of the refusal.
#
# A column name is the subject chosen because it is the one a caller most
# obviously controls, and `a{b}` is a legal name that a template would try to
# evaluate. The refusal is matched by the name it carries rather than by its
# wording, so this outlives #223's phase 3 re-authoring the sentence around it
# -- and outlives the transitional sibling, which is the point: what it asserts
# is not the shim's behaviour but the rule the shim exists to keep true.
test_that("a caller's braces reach a diagnostic as text", {
  data <- data.frame(region = "E", n = 1)

  raised <- expect_error(expand_with_margins(
    data,
    .grouping = rollup(region),
    .margin_label = c(region = "A", "a{b}" = "A")
  ))

  expect_s3_class(raised, "marginplyr_error")
  expect_match(conditionMessage(raised), "a{b}", fixed = TRUE)
  expect_no_match(
    conditionMessage(raised),
    "Could not evaluate cli",
    fixed = TRUE
  )
})

# The promise `?marginplyr` makes about a subject the caller supplied: a Package
# condition names it as the caller spelled it. ADR 0024 decides it and records
# why `abort_marginplyr()` expands its template when the condition is raised
# rather than when it is read -- `cli::cli_abort()` collapses a run of
# whitespace inside an interpolated value at retrieval, which named a column
# `a b` that the caller had named `a  b`.
#
# Both directions are pinned, in the shape `test-diagnostic-pluralization.R`
# uses for a pluralizing diagnostic's two arms. Only asserting the preserved
# spellings would pass a package that stopped expanding at raise time in some
# future where nothing wrapped anyway; only asserting the residue would pass one
# that lost the promise entirely. `?marginplyr` states the residue rather than
# claiming more than it keeps, so a spelling moving from one list to the other
# is a documentation change and fails here first.
#
# The subject is a `.margin_label` dimension name, because that is a name the
# caller writes directly into the call and the shortest path to a refusal that
# quotes it.
unknown_dimension_message <- function(name) {
  data <- data.frame(region = "E", n = 1)
  # `region` is a column of the frame, which codetools reads as an undefined
  # global wherever a verb's arguments are written inside a function.
  # nolint start: object_usage_linter.
  conditionMessage(expect_error(
    expand_with_margins(
      data,
      .grouping = rollup(region),
      .margin_label = stats::setNames(c("A", "A"), c("region", name))
    ),
    # The class, because the tests below read the message for a name they put
    # into the call: an unrelated error quoting that name back would satisfy
    # them, and the promise is about a Package condition rather than about any
    # error that happens to mention the subject.
    class = "marginplyr_error"
  ))
  # nolint end
}

test_that("a Package condition spells a subject as the caller spelled it", {
  preserved <- c(
    "two  spaces",
    "a\ttab",
    "a\rcarriage return",
    " leading",
    "trailing ",
    "an\u3000ideographic space",
    "a\u2009thin space",
    "a{brace}",
    "a`backtick"
  )

  for (name in preserved) {
    expect_true(
      grepl(name, unknown_dimension_message(name), fixed = TRUE),
      label = sprintf("the refusal quotes %s", encodeString(name, quote = '"'))
    )
  }
})

test_that("the two spellings a Package condition cannot keep", {
  # cli's glue pass turns both of these into an ordinary space before any
  # marginplyr code sees the result, so the refusal names a subject one
  # character different from the one the caller wrote. `?marginplyr` says so
  # rather than promising more than it keeps.
  newline_and_nbsp <- c("\n", "\u00a0")
  rewritten <- paste0("a", newline_and_nbsp, "spelling")
  newline_and_nbsp <- paste(newline_and_nbsp, collapse = "")

  for (name in rewritten) {
    quoted <- unknown_dimension_message(name)
    as_spaces <- chartr(newline_and_nbsp, "  ", name)
    expect_false(grepl(name, quoted, fixed = TRUE))
    expect_true(grepl(as_spaces, quoted, fixed = TRUE))
  }
})
