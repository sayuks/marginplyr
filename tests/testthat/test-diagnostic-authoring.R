# ADR 0023 states how every Package condition is authored. Two of its rules are
# gated, and one structural reading catches both: walk every
# `abort_marginplyr()` call and fail any whose message argument is not authored
# in the source -- a literal, or a call over arguments that are each authored in
# turn, which `authored_template()` below defines exactly.
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
#
# The reading of the message arguments is in `helper-diagnostic-sites.R`, which
# `test-diagnostic-pluralization.R`'s coverage gate reads too, over the
# enumeration and the recursion in `helper-namespace-walk.R` that every
# structural gate shares. Only the predicate below is this gate's own.

# Whether every string a message argument contributes is written in the source.
#
# A bare character literal is the common case. `c()` is admitted because it is
# how a cli message vector is spelled -- `c("Refusal.", i = "Bullet.")` -- and
# admitting it costs nothing: the recursion requires each of its arguments to be
# authored too, so `c(paste0(x), i = "...")` fails exactly as a bare `paste0(x)`
# does. Nothing else is admitted, because everything else computes text, and
# computed text is what both gated rules forbid. That includes a constant bound
# elsewhere in the package: a template has to be readable beside the call that
# raises it, or the injection rule cannot be reviewed at the site.
#
# `paste()` and `paste0()` are admitted on the same terms, and for a reason that
# only appeared once #223's phase 3 began re-authoring a file. A template has to
# be one string literal per message element, because `abort_marginplyr()`
# expands it with `cli::format_inline()`, whose `keep_whitespace = TRUE` is what
# ADR 0024 keeps a caller's spelling with -- so a source line break inside a
# template is a line break in the refusal, and glue's `\` continuation is part
# of the trimming that flag turns off. Meanwhile ADR 0023's amendment demoted
# its 80-column condition to style advice, so the shipped sentences are 83 to
# 119 characters before markup. Those two facts leave a re-authored element with
# no spelling that fits `line_length_linter()`, and the alternatives were a
# `.lintr`, which is repository-wide and would stop measuring every line of
# `R/` and `tests/` for a property only diagnostics have, a `# nolint` at about
# half the message elements in the package, or rewording the sentences #223
# says to preserve.
#
# Nothing about either gated rule is weaker for it, because the recursion is
# what enforces them and not the name of the call. Caller-derived text is a
# symbol rather than a literal wherever it appears, so
# `paste0("Unknown column `", columns, "`.")` is refused exactly as before, and
# an `if` spelling a noun is refused wherever it sits, `paste0()` included. What
# is admitted is a sentence the author wrote, split at a space to fit the
# margin, and still readable beside the call that raises it -- which is the
# property the paragraph above asks a template for.
authored_template <- function(expr) {
  if (is.character(expr)) {
    return(TRUE)
  }
  if (
    is.call(expr) &&
      is.name(expr[[1]]) &&
      as.character(expr[[1]]) %in% c("c", "paste", "paste0")
  ) {
    return(all(vapply(as.list(expr)[-1], authored_template, logical(1))))
  }
  FALSE
}

# The reading, driven over source `R/` does not contain. Both halves of it are
# asserted here rather than left to the scan below, because the scan's verdict
# over a corpus that satisfies the rule is the same verdict a scan that read
# nothing returns -- which is the objection this repository makes to any gate
# whose failing branch nothing executes.
#
# That is sharper now than it was while #223's phase 3 ran, not softer. The
# transitional sibling is gone, so the scan reads every diagnostic this package
# raises -- and every one of them satisfies the rule, which is precisely why
# `authored_template()`'s refusing branch is executed by nothing but the
# fixtures below. A corpus that stopped violating the rule and a predicate that
# stopped refusing anything are the same green run without them.
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
    abort_marginplyr(c(
      paste0("A refusal too long to ", "spell on one source line."),
      i = paste0("A bullet in the same ", "position.")
    ))
  }

  found <- diagnostic_message_arguments(body(fixture), "abort_marginplyr")

  # One nested inside an `if`, one inside a closure and named `message =`. A
  # walk that stopped descending, or one that only read the first argument,
  # loses one of the two. The third is the shape a re-authored site actually
  # takes, and it is here so that the admitting branch for `paste0()` is
  # executed by something other than the package -- every fixture in the test
  # below is a refusal, so without this one only that branch would run.
  expect_length(found, 3L)
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
    abort_marginplyr(paste0("One column", if (n == 1L) "" else "s", "."))
    abort_marginplyr(template)
    abort_marginplyr(class = "marginplyr_nothing")
  }

  found <- diagnostic_message_arguments(body(fixture), "abort_marginplyr")

  # An assembled template, twice; an `if` spelling a noun; a `c()` whose bullet
  # is assembled, which is the case admitting `c()` has to keep refusing; an
  # `if` spelling a noun inside a `paste0()`, which is the case admitting
  # `paste0()` has to keep refusing, and the one both gated rules would be lost
  # through if the recursion stopped at the admitted call; a template bound
  # elsewhere, which is readable nowhere near the refusal; and a call carrying
  # no message at all, which reads as `NULL` and is the site an appending idiom
  # that deletes on `NULL` would drop.
  expect_length(found, 7L)
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
