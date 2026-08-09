# Verifies the `must_error` chunk option in `inst/vignette-hooks/must-error.R`.
#
# The option is what makes a vignette's claim that a call is rejected checkable:
# the chunk runs the call, and the render halts if the call succeeds or fails
# for the wrong reason. Nothing else in the repository can fail when the option
# stops working -- a broken option reports nothing, which reads exactly like a
# vignette whose rejected calls are all still rejected -- so the option needs a
# gate of its own.
#
# It cannot be a testthat test. `release-matrix.yaml`'s `backend` jobs install
# the hard dependencies plus one optional backend, so knitr is absent there, and
# `verify-backend.R` fails a job for any skip that does not name a backend the
# job withheld. A knitr-gated test would therefore turn every `backend` job red.
# This script runs where knitr is provisioned instead: `altdoc.yaml`, alongside
# `verify-site.R`, and locally with
#
#     Rscript .github/scripts/verify-must-error.R
#
# It knits fixture documents rather than calling the hooks directly, because
# every property below is a property of a render: which chunks knitr evaluates,
# what it puts in the result objects, and what it restores on the way out.

source(".github/scripts/ci-helpers.R")

definition <- "inst/vignette-hooks/must-error.R"
if (!file.exists(definition)) {
  stop(
    call. = FALSE,
    "Cannot find ", definition, ". Run this from the repository root."
  )
}

# The fixtures raise their own conditions rather than calling marginplyr, so
# this checks the option and not the package's error classes. `rlang` is an
# Import, so it is present wherever the package is.
setup_chunk <- c(
  "```{r}",
  "#| include: false",
  sprintf("source(%s)", encodeString(normalizePath(definition), quote = "\"")),
  "```",
  ""
)

# Most fixtures are knitted with a sentinel `evaluate` hook already installed.
# That serves two purposes: it is what the definition must put back, so the
# restoration assertion is about the previous hook rather than about knitr's
# default, and it makes `knit()` see non-default hooks on entry, which turns off
# knitr's own hook restoration. What restores the hooks is then the definition
# alone.
#
# A vignette render takes the other path -- default hooks on entry, where knitr
# registers its own `knit_hooks$set(.default.hooks)` on exit ahead of the
# `after.knit` call -- so one fixture below asks for `sentinel = FALSE` and
# covers it. There the `evaluate` assertion is weak, because knitr would restore
# that hook whatever the definition did; the `opts_hooks` entry is the one that
# still means something, since knitr never restores those at all.
baseline_evaluate <- knitr::knit_hooks$get("evaluate")
sentinel_evaluate <- function(...) baseline_evaluate(...)

# What the fixtures accumulate. An environment rather than a pair of variables
# reached with `<<-`: every fixture below runs at top level, so a cascading
# assignment here would be writing to the global environment.
found <- new.env(parent = emptyenv())
found$failures <- character()
found$report <- character()

fail <- function(...) {
  found$failures <- c(found$failures, paste0(...))
}

# Renders one fixture and reports what the render did, without stopping: a
# script that aborted on the first surprise would check one property per run.
knit_fixture <- function(body, sentinel) {
  input <- tempfile(fileext = ".Rmd")
  output <- tempfile(fileext = ".md")
  on.exit(unlink(c(input, output)), add = TRUE)
  writeLines(c(setup_chunk, body), input)

  knitr::knit_hooks$restore()
  knitr::opts_hooks$restore()
  if (sentinel) {
    knitr::knit_hooks$set(evaluate = sentinel_evaluate)
  }

  error <- NULL
  text <- tryCatch(
    {
      knitr::knit(input, output, quiet = TRUE)
      paste(readLines(output, warn = FALSE), collapse = "\n")
    },
    error = function(cnd) {
      error <<- cnd
      NA_character_
    }
  )

  state <- list(
    must_error_hook = knitr::opts_hooks$get("must_error"),
    evaluate_hook = knitr::knit_hooks$get("evaluate"),
    after_knit_hook = knitr::knit_hooks$get("after.knit")
  )

  knitr::knit_hooks$restore()
  knitr::opts_hooks$restore()

  list(
    halted = !is.null(error),
    message = if (is.null(error)) "" else conditionMessage(error),
    text = text,
    state = state,
    sentinel = sentinel
  )
}

# Every fixture, whatever it asserts, has to leave knitr as it found it. A
# render halted by the option is the case that matters most and the one an
# `on.exit()`-free implementation gets wrong, so this runs for the failing
# fixtures too.
check_restored <- function(name, result) {
  state <- result$state
  if (!is.null(state$must_error_hook)) {
    fail(name, ": the `must_error` entry in `opts_hooks` outlived the render.")
  }
  expected_evaluate <- if (result$sentinel) {
    sentinel_evaluate
  } else {
    baseline_evaluate
  }
  if (!identical(state$evaluate_hook, expected_evaluate)) {
    fail(name, ": the `evaluate` knit hook was not put back after the render.")
  }
  if (!is.null(state$after_knit_hook)) {
    fail(name, ": the `after.knit` knit hook outlived the render.")
  }
}

each_present <- function(text, needles) {
  vapply(needles, grepl, logical(1), x = text, fixed = TRUE)
}

check <- function(name,
                  body,
                  halted,
                  expect = character(),
                  forbid = character(),
                  sentinel = TRUE) {
  result <- knit_fixture(body, sentinel = sentinel)
  # The one text a fixture can assert against, whichever way the render went:
  # the halt message when the option stopped it, the rendered page otherwise.
  observed <- if (result$halted) result$message else result$text
  subject <- if (halted) "the halt message" else "the rendered page"
  if (result$halted != halted) {
    fail(
      name,
      if (halted) {
        ": the render completed, but it had to halt. Output: "
      } else {
        ": the render halted, but it had to complete. Error: "
      },
      observed
    )
  } else {
    wrong <- list(
      list(
        strings = expect[!each_present(observed, expect)],
        complaint = " is missing "
      ),
      list(
        strings = forbid[each_present(observed, forbid)],
        complaint = " must not hold "
      )
    )
    for (one in wrong) {
      if (length(one$strings) > 0L) {
        fail(
          name, ": ", subject, one$complaint,
          paste(sprintf("\"%s\"", one$strings), collapse = ", "),
          ". Got: ", observed
        )
      }
    }
  }
  check_restored(name, result)
  found$report <- c(found$report, sprintf(
    "- %s %s",
    if (name %in% sub(":.*$", "", found$failures)) "FAILED" else "OK",
    name
  ))
  invisible(result)
}

# `must_error: true` keeps the meaning it had before class assertion existed.
check(
  "true accepts any error",
  c(
    "```{r}",
    "#| label: any-error",
    "#| must_error: true",
    "stop(\"refused\")",
    "```"
  ),
  halted = FALSE,
  expect = "refused"
)

check(
  "true halts when nothing is raised",
  c(
    "```{r}",
    "#| label: quiet-chunk",
    "#| must_error: true",
    "1 + 1",
    "```"
  ),
  halted = TRUE,
  expect = c("quiet-chunk", "must_error: true", "without raising an error")
)

# The class form. The halt message has to name the chunk, what was expected, and
# what was raised, because the first thing a reader of a failed render asks is
# whether the prose or the call moved.
check(
  "a class halts on the wrong condition",
  c(
    "```{r}",
    "#| label: wrong-class",
    "#| must_error: fixture_error",
    "rlang::abort(\"refused\", class = \"other_error\")",
    "```"
  ),
  halted = TRUE,
  expect = c(
    "wrong-class",
    "must_error: fixture_error",
    "`fixture_error`",
    "other_error"
  )
)

check(
  "a class accepts that condition",
  c(
    "```{r}",
    "#| label: right-class",
    "#| must_error: fixture_error",
    "rlang::abort(\"refused\", class = \"fixture_error\")",
    "```"
  ),
  halted = FALSE,
  expect = "refused"
)

# The form a vignette actually needs: a Package condition is usually wrapped by
# the verb the reader called, and it is the wrapped class the prose names.
check(
  "a class accepts a wrapped condition",
  c(
    "```{r}",
    "#| label: wrapped-class",
    "#| must_error: fixture_error",
    "inner <- rlang::catch_cnd(",
    "  rlang::abort(\"refused\", class = \"fixture_error\")",
    ")",
    "rlang::abort(\"in argument\", parent = inner)",
    "```"
  ),
  halted = FALSE,
  expect = "in argument"
)

# The property `_R_CHECK_DEPENDS_ONLY_` builds depend on. knitr never calls the
# `evaluate` hook for a chunk it does not evaluate, so a chunk withheld because
# its package is absent must pass through unreported -- not be counted as a
# chunk that stopped failing.
#
# The guard is written the way `vignettes/recipes.qmd` writes it, on a name no
# library holds, because what this fixture can vary is whether knitr evaluated
# the chunk and not why. A genuinely withheld Suggest is checked where one can
# be withheld: `release-matrix.yaml`'s `depends-only` job rebuilds the vignettes
# with every Suggest absent, and `recipes.qmd`'s `nested-aggregate` chunk is a
# `must_error` chunk behind `has_duckdb`. An option that reported it would fail
# that job.
check(
  "a withheld chunk is not reported",
  c(
    "```{r}",
    "#| include: false",
    "has_backend <- requireNamespace(\"notapackage\", quietly = TRUE)",
    "```",
    "",
    "```{r}",
    "#| label: guarded",
    "#| must_error: true",
    "#| eval: !expr has_backend",
    "1 + 1",
    "```"
  ),
  halted = FALSE,
  forbid = "guarded"
)

# The path a vignette render actually takes: default hooks when `knit()` starts,
# so knitr registers its own `knit_hooks` reset ahead of the `after.knit` call
# that undoes the option. `after.knit` is not among `.default.hooks`, so it
# survives that reset and still runs -- and the `opts_hooks` entry, which knitr
# never restores, is the one this fixture proves gone.
check(
  "the option undoes itself from default hooks",
  c(
    "```{r}",
    "#| label: default-hooks",
    "#| must_error: fixture_error",
    "rlang::abort(\"refused\", class = \"fixture_error\")",
    "```"
  ),
  halted = FALSE,
  expect = "refused",
  sentinel = FALSE
)

# A value that is neither `true` nor a class name is a mistake in the chunk
# header, and a mistake that silently disabled the assertion would leave the
# prose unchecked.
check(
  "a malformed value is refused",
  c(
    "```{r}",
    "#| label: malformed",
    "#| must_error: 3",
    "stop(\"refused\")",
    "```"
  ),
  halted = TRUE,
  expect = "one condition class name"
)

write_step_summary(c(
  "## `must_error` chunk option",
  "",
  sprintf(
    "Verified `%s` against %d rendered fixture(s).",
    definition,
    length(found$report)
  ),
  "",
  found$report
))

if (length(found$failures) > 0L) {
  stop(
    call. = FALSE,
    sprintf(
      "The `must_error` option is not behaving as documented:\n%s",
      paste0("- ", found$failures, collapse = "\n")
    )
  )
}

message(sprintf(
  "Verified the `must_error` option against %d rendered fixture(s).",
  length(found$report)
))
