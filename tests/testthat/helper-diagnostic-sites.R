# The readers two gates share, over the source `R/` does not ship: the
# structural gate in `test-diagnostic-authoring.R`, which asks whether every
# `abort_marginplyr()` template is authored in the source, and the coverage
# gate in `test-diagnostic-pluralization.R`, which asks whether every diagnostic
# this package pluralizes has both of its arms pinned.
#
# They live here rather than in either file because testthat gives each test
# file its own environment, so the second gate could not see a walker the first
# one defined -- and a copy is the thing that drifts. `helper-` is where
# testthat looks before it runs anything, which is also what lets both gates be
# read against a package loaded with `pkgload::load_all()` or an installed dev
# build.

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

# Every function bound in the namespace, which is where the call sites are:
# `R/` is not installed, so a scan of the shipped package would find no source
# to read.
marginplyr_namespace_functions <- function(ns = asNamespace("marginplyr")) {
  Filter(
    function(binding) is.function(get(binding, envir = ns)),
    ls(ns, all.names = TRUE)
  )
}

marginplyr_diagnostic_sites <- function(name, ns = asNamespace("marginplyr")) {
  sites <- lapply(marginplyr_namespace_functions(ns), function(binding) {
    diagnostic_message_arguments(body(get(binding, envir = ns)), name)
  })
  stats::setNames(sites, marginplyr_namespace_functions(ns))
}

# Whether a call is to `name`, however the source spells the head: bare, or
# qualified with `::`. `cli::pluralize()` is written qualified and
# `abort_marginplyr()` is not, and a reader that recognized only one of the two
# spellings would answer a clean corpus for whichever it could not see.
is_call_to <- function(expr, name) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  head <- expr[[1]]
  if (is.name(head)) {
    return(identical(as.character(head), name))
  }
  is.call(head) &&
    is.name(head[[1]]) &&
    as.character(head[[1]]) %in% c("::", ":::") &&
    identical(as.character(head[[3]]), name)
}

# Whether any part of this expression is an `if` choosing between two string
# literals. That is how a diagnostic outside ADR 0023 spells a plural: the two
# bare `stop()` invariants that suffix a noun write
# `if (length(x) == 1L) " " else "s "`, and nothing else in the namespace has
# the shape. It is deliberately not a test for the word `s`, because what makes
# a branch a pluralization is that the two arms are fixed text the count picks
# between, which is the same property ADR 0023's `{?}` rule is stated over.
has_literal_branch <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  chosen <- is_call_to(expr, "if") &&
    length(expr) == 4L &&
    is.character(expr[[3]]) &&
    is.character(expr[[4]])
  chosen ||
    any(vapply(as.list(expr)[-1], has_literal_branch, logical(1)))
}

# Whether a template pluralizes through cli, which is `{?}` wherever it sits --
# `{?s}`, `{?is/are}`, and `{?a/b}` alike, and behind a `cli::qty()` or not.
# Read off the deparsed expression rather than off a literal, because a
# re-authored template is a `paste()` or `paste0()` of literals split at a space
# (ADR 0023's second amendment) and the `{?}` can fall either side of the split.
has_cli_plural <- function(expr) {
  if (is.null(expr)) {
    return(FALSE)
  }
  any(grepl("{?", paste(deparse(expr), collapse = " "), fixed = TRUE))
}

# Every diagnostic this package pluralizes, as a count per namespace binding.
#
# Two constructions reach it and both are derived rather than listed. A Package
# condition pluralizes through cli, so it is an `abort_marginplyr()` template
# holding `{?}`; `report_branch_warnings()` writes the one sentence that
# pluralizes outside a Package condition, through `cli::pluralize()` inside
# `rlang::format_error_bullets()`, and is found by the same read of a different
# call. An invariant pluralizes with an `if`, ADR 0023 excluding a bare
# `stop()` from the idiom, and is found by `has_literal_branch()` above.
#
# A count rather than a set of names, because one binding can hold more than
# one: `compile_grouping_spec_impl()` raises the duplicate-grouping-set refusal
# and the unknown-`.by` invariant, which are pinned in different places, and
# the two label-collision constructors hold two arms each.
marginplyr_pluralizing_sites <- function(ns = asNamespace("marginplyr")) {
  counts <- vapply(
    marginplyr_namespace_functions(ns),
    function(binding) {
      expr <- body(get(binding, envir = ns))
      templates <- c(
        diagnostic_message_arguments(expr, "abort_marginplyr"),
        pluralize_templates(expr)
      )
      cli_sites <- sum(vapply(templates, has_cli_plural, logical(1)))
      cli_sites + invariant_plural_sites(expr)
    },
    integer(1)
  )
  counts[counts > 0L]
}

# The templates handed to `cli::pluralize()`, which takes its first argument
# positionally at the one site that calls it.
pluralize_templates <- function(expr) {
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (is_call_to(e, "pluralize") && length(e) > 1L) {
      found <<- c(found, list(e[[2]]))
    }
    lapply(as.list(e)[-1], walk)
    invisible(NULL)
  }
  walk(expr)
  found
}

# How many bare `stop()` calls below this expression spell a plural with an
# `if`. Counted at the `stop()` rather than at the `if`, so that a diagnostic
# inflecting a noun and a verb in one message is one site and not two.
invariant_plural_sites <- function(expr) {
  found <- 0L
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (is_call_to(e, "stop") && has_literal_branch(e)) {
      found <<- found + 1L
    }
    lapply(as.list(e)[-1], walk)
    invisible(NULL)
  }
  walk(expr)
  found
}
