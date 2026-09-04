# The `must_error` chunk option, shared by every vignette.
#
# Two forms:
#
#     must_error: true               any error will do
#     must_error: marginplyr_error   the error must carry that condition class
#
# `AGENTS.md` is authoritative for what the option is for and when to mark a
# chunk. What a rewrite of this file must keep is stated at the sites that hold
# it, and not here.

local({
  # Sourcing this twice -- two setup chunks in one document, or a second
  # vignette rendered in a session that already rendered one -- must not stack a
  # second wrapper on the first. Each wrapper saves the hook it replaced, so a
  # stack of two would restore the other one and leave the option installed.
  if (!is.null(knitr::opts_hooks$get("must_error"))) {
    return(invisible(NULL))
  }

  # What a chunk's option value asks for. `NULL` means the chunk is not marked,
  # so every hook below leaves it alone. `NA_character_` means any error will
  # do, which is what `must_error: true` has always meant. A string names the
  # condition class the raised error has to carry.
  #
  # A value that is neither is refused rather than ignored: a chunk header this
  # cannot read is one whose assertion would silently stop happening, which is
  # the failure the option exists to prevent.
  expected_class <- function(value) {
    if (is.null(value) || identical(value, FALSE)) {
      return(NULL)
    }
    if (isTRUE(value)) {
      return(NA_character_)
    }
    if (is.character(value) && length(value) == 1L && nzchar(value)) {
      return(value)
    }
    stop(
      "`must_error` must be `true` or one condition class name.",
      call. = FALSE
    )
  }

  # How the chunk header was written, so a diagnostic quotes the option the
  # author set rather than paraphrasing it.
  option_text <- function(expected) {
    if (is.na(expected)) "true" else expected
  }

  # rlang chains a wrapped condition onto `parent`, and the errors a vignette
  # shows are usually wrapped: a rejected summary helper reaches the reader as a
  # dplyr error carrying the Package condition beneath it. Reading the chain is
  # what lets `must_error: marginplyr_error` mean "marginplyr refused this call"
  # for a call made through another verb.
  chain <- function(cnd) {
    out <- list()
    while (inherits(cnd, "condition")) {
      out[[length(out) + 1L]] <- cnd
      cnd <- cnd$parent
    }
    out
  }

  carries <- function(cnd, expected) {
    if (is.na(expected)) {
      return(TRUE)
    }
    any(vapply(chain(cnd), inherits, logical(1), what = expected))
  }

  describe <- function(cnd) {
    paste(
      vapply(
        chain(cnd),
        function(one) paste(class(one), collapse = "/"),
        character(1)
      ),
      collapse = " caused by "
    )
  }

  # `must_error` implies `error: true`, so the two can never be set
  # inconsistently on one chunk.
  knitr::opts_hooks$set(must_error = function(options) {
    if (!is.null(expected_class(options$must_error))) {
      options$error <- TRUE
    }
    options
  })

  previous_evaluate <- knitr::knit_hooks$get("evaluate")

  knitr::knit_hooks$set(evaluate = function(...) {
    # knitr does not call this hook for a chunk it does not evaluate, so a chunk
    # withheld by an availability guard -- `eval: !expr has_duckdb` -- is passed
    # over here with no special case. Losing that would report a guarded chunk
    # as a chunk that stopped failing, and break `_R_CHECK_DEPENDS_ONLY_`
    # builds.
    results <- previous_evaluate(...)
    options <- knitr::opts_current$get()
    expected <- expected_class(options$must_error)
    if (is.null(expected)) {
      return(results)
    }
    # Inspecting the result objects rather than catching the condition here:
    # this leaves knitr's own error rendering in place, whereas catching it
    # would print an `<error/rlang_error>` header and a backtrace through this
    # function, which no reader would see in their own session.
    raised <- Filter(function(one) inherits(one, "error"), results)
    if (length(raised) == 0L) {
      stop(
        sprintf(
          paste0(
            "Chunk `%s` is marked `must_error: %s` but completed without ",
            "raising an error."
          ),
          options$label,
          option_text(expected)
        ),
        call. = FALSE
      )
    }
    if (!any(vapply(raised, carries, logical(1), expected = expected))) {
      stop(
        sprintf(
          paste0(
            "Chunk `%s` is marked `must_error: %s` but raised no error ",
            "carrying `%s`. Raised: %s."
          ),
          options$label,
          option_text(expected),
          expected,
          paste(vapply(raised, describe, character(1)), collapse = "; ")
        ),
        call. = FALSE
      )
    }
    results
  })

  # knitr restores neither of the two things above: `opts_hooks` it never
  # restores, and `knit_hooks` only when the hooks were untouched at the moment
  # `knit()` started, which says nothing about a hook installed while it runs.
  # So the definition undoes itself.
  #
  # `after.knit` is where, because `knit()` runs it from `on.exit()`: a render
  # halted by a chunk that failed the assertion above restores just as one that
  # finished does. The child-mode guard is what keeps a child document's own
  # `knit()` from restoring while the parent still has chunks to run; a hook
  # this one displaced is still called there, because skipping the restoration
  # is not a reason to swallow someone else's hook. The `document` hook was the
  # alternative weighed
  # (`investigation/restoring-knitr-hooks-a-vignette-installs.md`).
  previous_after_knit <- knitr::knit_hooks$get("after.knit")

  knitr::knit_hooks$set(after.knit = function(...) {
    if (!isTRUE(knitr::opts_knit$get("child"))) {
      knitr::opts_hooks$delete("must_error")
      knitr::knit_hooks$set(evaluate = previous_evaluate)
      if (is.null(previous_after_knit)) {
        knitr::knit_hooks$delete("after.knit")
      } else {
        knitr::knit_hooks$set(after.knit = previous_after_knit)
      }
    }
    if (!is.null(previous_after_knit)) {
      previous_after_knit(...)
    }
    invisible(NULL)
  })
})
