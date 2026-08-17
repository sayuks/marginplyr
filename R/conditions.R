# The only constructor for Package conditions. Every error a caller can avoid by
# rewriting the call within the documented public interface goes through here;
# unreachable invariants and upstream defects use bare `stop()` or `stopifnot()`
# instead. The rule and the reasoning behind it are recorded in
# design/adr/0015-separate-package-conditions-from-internal-invariants.md, and
# the contract it promises is documented in `?marginplyr`.
#
# `class` adds a narrower subclass for handlers marginplyr itself needs. Those
# subclasses stay implementation details; `marginplyr_error` is the promised
# public class.
abort_marginplyr <- function(message,
                             ...,
                             class = NULL,
                             call = rlang::caller_call()) {
  rlang::abort(
    message = message,
    ...,
    class = c(class, "marginplyr_error"),
    call = call
  )
}

# The conditions a chain holds, outermost first: the condition given, then each
# `parent` that is a condition itself.
#
# The one reading of a condition another package raised, for the refusals that
# have to look past the condition they caught. tidyselect wraps a failure
# raised inside a selection helper, so what carries the refused subscript sits
# at a depth the caught condition does not say: `all_of(s)` reports it from a
# condition one level in, under a wrapper holding no `i` at all, while a bare
# `my_spec(region)` reports it at the top. A reader of the caught condition
# alone would answer the first shape as though nothing had been refused, which
# is why two modules walked this for themselves before it was named once
# (#193).
#
# What to ask of these conditions stays with the caller, because the chain is
# the only part such readers share: `share_selection_missing_names()` collects
# the character subscripts in `i`, and `is_grouping_spec_subscript()` tests a
# class against an argument's own label. This one decides nothing, which is
# what separates it from the rest of this module -- everything else here raises
# a Package condition or restates one a branch raised.
condition_chain <- function(cnd) {
  if (!inherits(cnd, "condition")) {
    return(list())
  }
  c(list(cnd), condition_chain(cnd$parent))
}

# What an External condition raised while one grouping-set branch runs is
# reported with. `keys` maps each `..marginplyr_key_N` column the branch
# grouped by to the column the caller named, and `call` is the Margin verb the
# caller wrote -- `NULL` where there is no verb to name, which is how a direct
# call to an adapter leaves the blamed call as it found it.
#
# The buffer is what makes a Repeated condition one report: a branch warning is
# withheld as it is raised and every branch's warnings are replayed together,
# which cannot be decided one branch at a time. It is an environment because
# the branches run inside `Map()`, so what they write to has to outlive each
# call. The decision is recorded in
# design/adr/0021-report-a-repeated-execution-condition-once.md, and the terms
# it is stated in -- *Condition context* and *Repeated condition* -- are
# CONTEXT.md's.
new_branch_conditions <- function(keys, call = NULL) {
  stopifnot(
    is.character(keys),
    length(keys) == 0L || !is.null(names(keys))
  )
  conditions <- new.env(parent = emptyenv())
  conditions$keys <- keys
  conditions$call <- call
  conditions$warnings <- list()
  conditions
}

# Runs one branch, withholding the warnings it raises and restating the context
# of the error that aborts it. Only the caller's own summary expressions are
# evaluated inside `expr`: a Package condition is raised by the branch builders
# around it, which is what keeps this from reaching one.
#
# `arguments` is this branch's own map from what it handed dplyr to what the
# caller wrote, and sits here rather than in `conditions` because a branch
# rewrites a Grouping helper to its own constant, so no two branches
# necessarily hand dplyr the same expressions.
with_branch_conditions <- function(expr,
                                   conditions,
                                   arguments = character()) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(cnd) {
        buffer_branch_warning(cnd, conditions, arguments)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      stop(restate_branch_error(cnd, conditions, arguments))
    }
  )
}

# An error arrives with its context in addressable fields: `$message` holds the
# argument bullet, `$body` the grouping-value bullet, and `$call` the internal
# `dplyr::summarize()` the adapter issued. `$parent` holds the caller's own
# condition and is never touched, which is what keeps the propagation faithful.
#
# Errors are not deduplicated, and there is nothing to deduplicate: branches
# run in sequence, so the first error aborts the operation and no second
# occurrence is ever raised.
restate_branch_error <- function(cnd, conditions, arguments) {
  cnd <- restate_condition_arguments(cnd, arguments)
  cnd <- restate_condition_names(cnd, conditions$keys)
  if (!is.null(conditions$call)) {
    cnd$call <- conditions$call
  }
  cnd
}

# The grouping-value bullet is `$body` for an error and part of the rendered
# `$message` for a warning, so both are rewritten in place rather than one
# being rendered into the other: replacing a structured condition's `$message`
# with everything `conditionMessage()` rendered would print its body and its
# cause twice.
restate_condition_names <- function(cnd, keys) {
  if (is.character(cnd$message)) {
    cnd$message <- restate_margin_keys(cnd$message, keys)
  }
  # `$body` is a character vector of bullets here, but rlang also admits a
  # function that renders them, which nothing can substitute into.
  if (is.character(cnd$body)) {
    cnd$body <- restate_margin_keys(cnd$body, keys)
  }
  cnd
}

# A warning arrives as one condition per branch whose message dplyr has already
# aggregated, flattened, and rendered: `$parent` is `NULL` and there is no
# `$body`, so the text is all there is to compare.
buffer_branch_warning <- function(cnd, conditions, arguments) {
  # The spelling is restored before the identity is read, so the identity is
  # over the argument the caller wrote: a Grouping helper renders as a different
  # constant in every branch, which split one written expression into one report
  # per branch (ADR 0022).
  cnd <- restate_condition_arguments(cnd, arguments)
  cnd <- restate_condition_names(cnd, conditions$keys)
  key <- branch_warning_identity(cnd)

  buffered <- conditions$warnings
  seen <- buffered[[key]]
  buffered[[key]] <- if (is.null(seen)) {
    list(condition = cnd, count = 1L)
  } else {
    list(condition = seen$condition, count = seen$count + 1L)
  }
  conditions$warnings <- buffered
  invisible(NULL)
}

# Two occurrences are repetitions of one condition when they agree on identity:
# the class, the diagnostic, and the argument the warning is attributed to. The
# class is read from the condition; the other two are what is left of the
# rendered message once the parts that necessarily differ between grouping sets
# are removed -- which groups raised the warning, how many of that branch's
# groups did, and dplyr's pointer at the store holding the rest.
#
# What identifies a part as dplyr's is where it sits, not what it says, because
# everything dplyr's aggregation says a caller's own diagnostic can say too: a
# diagnostic can open `There were 3 warnings in ...`, and cli renders its second
# line at column zero exactly as it renders a bullet. So the header is what
# precedes the first bullet, a grouping-value bullet is one written before the
# `Caused by` line that introduces the caller's own diagnostic, and the pointer
# at the store is looked for only in a message whose header said there was more
# than one warning to point at.
#
# Each part is matched as the line it was written as -- `message_line_runs()`
# below -- so that the console width decides nothing, and removed as the lines
# it was rendered onto. Every line kept is kept as it arrived: nothing a caller
# wrote is rewritten into what another branch wrote.
#
# What is left reads a format dplyr does not promise, and is chosen for which
# way it fails: wording dplyr changes stops matching, the identities stay
# distinct, and every occurrence is reported, which is what happens today. It
# can never collapse a warning that genuinely differs.
branch_warning_identity <- function(cnd) {
  lines <- strsplit(conditionMessage(cnd), "\n", fixed = TRUE)[[1L]]
  runs <- message_line_runs(lines)
  written <- vapply(
    runs,
    function(run) paste(sub("^ +", "", lines[run]), collapse = " "),
    character(1)
  )
  # `!` opens the caller's own diagnostic, so a message dplyr attributed to no
  # argument still has a bullet where its header ends. Both spellings of the
  # informational one are matched because the symbol is cli's and follows
  # `cli.unicode`.
  first_bullet <- match(TRUE, grepl("^[i\u2139!] ", written))
  cause <- match(TRUE, grepl("^Caused by ", written))
  removed <- rep(FALSE, length(runs))

  aggregated <- !is.na(first_bullet) &&
    first_bullet > 1L &&
    grepl(
      "^There (were|was) [0-9]+ warnings? in ",
      paste(written[seq_len(first_bullet - 1L)], collapse = " ")
    )
  if (aggregated) {
    removed[seq_len(first_bullet - 1L)] <- TRUE
  }
  if (!is.na(cause)) {
    removed <- removed |
      (grepl("^[i\u2139] In group ", written) & seq_along(written) < cause)
  }
  # The pointer runs to the end of the message, so everything after it goes
  # with it.
  pointer <- which(
    grepl("^[i\u2139] Run `dplyr::last_dplyr_warnings\\(\\)`", written)
  )
  if (aggregated && length(pointer) > 0L) {
    removed[seq(max(pointer), length(runs))] <- TRUE
  }

  paste(
    c(class(cnd), lines[unlist(runs[!removed])]),
    collapse = "\n"
  )
}

# The message as the lines it was written as, each one the indices of the lines
# cli rendered it onto: a line it cannot fit is wrapped onto continuations it
# indents by two spaces. Reading a part off the rendered lines instead would
# make the console width and the length of the grouping values decide what gets
# matched -- the ticket's own reproduction wrapped its grouping-value bullet at
# 60 columns and dplyr's opening sentence at 24, and reported three and two
# conditions for a plan that raises one.
message_line_runs <- function(lines) {
  wrapped <- grepl("^  ", lines) & seq_along(lines) > 1L
  unname(split(seq_along(lines), cumsum(!wrapped)))
}

# Replays what the branches withheld: one report per distinct identity, each
# saying how many further grouping sets raised it. The conditions are replayed
# in the order the branches raised them, and the reported occurrence is the
# first, so a plan that raises nothing new reads as one branch's report.
report_branch_warnings <- function(conditions) {
  for (entry in conditions$warnings) {
    cnd <- entry$condition
    if (entry$count > 1L) {
      cnd$message <- paste0(
        cnd$message,
        "\n",
        rlang::format_error_bullets(c(i = sprintf(
          "%d further grouping %s raised this warning.",
          entry$count - 1L,
          if (entry$count == 2L) "set" else "sets"
        )))
      )
    }
    warning(cnd)
  }
  invisible(NULL)
}

# The label dplyr quotes an argument by, reproduced so that the expression
# marginplyr handed it can be recognised in a context it rendered. dplyr writes
# `paste0(name, " = ", expr_as_label(expr))` for a named argument
# (`error_label_named()`), and its `expr_as_label()` is `rlang::as_label()` with
# rlang's infix labelling suppressed through an option neither package
# documents. Plain `as_label()` is what this uses, so the two disagree exactly
# where dplyr abbreviates a long infix expression -- `total = +...` -- and there
# the caller's own label renders that same string. ADR 0022 reproduces the
# convention and not the option for that reason: what the option covers is a
# substitution nothing could observe.
summary_argument_labels <- function(dots) {
  arg_names <- names(dots)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(dots))
  }
  vapply(
    seq_along(dots),
    function(i) {
      dot <- dots[[i]]
      expr <- if (rlang::is_quosure(dot)) rlang::quo_get_expr(dot) else dot
      label <- rlang::as_label(expr)
      if (nzchar(arg_names[[i]])) {
        paste0(arg_names[[i]], " = ", label)
      } else {
        label
      }
    },
    character(1)
  )
}

# What one branch hands dplyr, mapped to what the caller wrote. `origins` holds
# the caller's label for each dot and is carried from
# `plan_summary_expressions()`, because nothing between there and here preserves
# position: share planning drops a dot and expands a placeholder into one dot
# per output. A length that stops agreeing with the dots is an invariant rather
# than a Package condition (ADR 0015) -- no call can produce it, and a map built
# from a misaligned pair would quote one argument's expression under another.
#
# A label several dots share is kept only where the callers' own labels agree,
# since the replacement is then the same whichever dot dplyr meant; where they
# differ the entry is dropped and the quotation stays as dplyr wrote it. An
# entry no rewrite changed is dropped as well, having nothing to restate.
branch_argument_map <- function(dots, origins) {
  if (length(origins) == 0L) {
    return(character())
  }
  stopifnot(length(dots) == length(origins))

  labels <- summary_argument_labels(dots)
  written <- unique(labels)
  restored <- vapply(
    written,
    function(label) {
      candidates <- unique(origins[labels == label])
      if (length(candidates) == 1L) candidates else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
  keep <- !is.na(restored) & restored != written
  stats::setNames(restored[keep], written[keep])
}

# The argument bullet, restated to quote the expression the caller wrote. The
# span inside the backticks is compared with marginplyr's own rendering of what
# it handed dplyr, so a span equal to one is replaced by the caller's label,
# and a span equal to none is left where it was -- which is every span in a
# message whose format changed, and the whole of how this degrades (ADR 0022).
# Only the span moves: the sentence around it is dplyr's, and rewriting a
# sentence is what ADR 0021 refused for the blamed call.
restate_condition_arguments <- function(cnd, arguments) {
  if (length(arguments) == 0L || !is.character(cnd$message)) {
    return(cnd)
  }
  restated <- vapply(
    cnd$message,
    restate_argument_lines,
    character(1),
    arguments = arguments,
    USE.NAMES = FALSE
  )
  # The names carry the bullet markers of a structured condition, so they are
  # put back rather than left to `vapply()`, which would otherwise name each
  # element after the text it was rendered from.
  cnd$message <- stats::setNames(restated, names(cnd$message))
  cnd
}

# An error's `$message` is the argument bullet and nothing else, while a
# warning's is the whole text dplyr rendered before signalling, so both are read
# as the lines they were written as -- `message_line_runs()`, the same reading
# the identity uses, and for the same reason: cli wraps a bullet it cannot fit,
# and a span read off the line a bullet opens is a prefix of the label at any
# narrow width. Reading one line at a time restored the spelling at 80 columns
# and not at 40, which made the console width decide how many conditions a
# caller receives.
#
# A run that is restated collapses to the one line it was written as, since what
# replaces it is a line of a different length and cli is no longer there to wrap
# it. Every other run is given back exactly as it arrived.
restate_argument_lines <- function(text, arguments) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (length(lines) == 0L) {
    return(text)
  }
  restated <- lapply(
    message_line_runs(lines),
    function(run) {
      written <- paste(sub("^ +", "", lines[run]), collapse = " ")
      line <- restate_argument_line(written, arguments)
      if (identical(line, written)) lines[run] else line
    }
  )
  paste(unlist(restated, use.names = FALSE), collapse = "\n")
}

# `In argument:` opens the bullet, carrying cli's informational marker in a
# rendered warning and no marker at all in a structured error, where the `i` is
# the name of the message vector rather than part of its text. The span is read
# to the last backtick on the line, because a non-syntactic name the caller
# wrote puts backticks inside it.
restate_argument_line <- function(line, arguments) {
  found <- regmatches(
    line,
    regexec("^([i\u2139] )?In argument: `(.*)`\\.$", line)
  )[[1L]]
  if (length(found) == 0L) {
    return(line)
  }
  restored <- unname(arguments[found[[3L]]])
  if (is.na(restored)) {
    return(line)
  }
  paste0(found[[2L]], "In argument: `", restored, "`.")
}

# Rewrites the internal grouping-column names dplyr built the context from into
# the names the caller wrote. Finding one is a search for a literal marginplyr
# planted rather than a parse of dplyr's format, which is why this half carries
# none of the fragility `branch_warning_identity()` above does.
#
# One pass over the text rather than one `gsub()` per key, so that no token can
# be found inside another: a fixed replacement of `..marginplyr_key_1` corrupts
# the `..marginplyr_key_10` a plan of ten grouping columns allocates. Longest
# token first, because a Perl-compatible alternation matches the first branch
# that fits rather than the longest.
restate_margin_keys <- function(text, keys) {
  if (length(keys) == 0L || length(text) == 0L) {
    return(text)
  }
  keys <- keys[order(nchar(names(keys)), decreasing = TRUE)]
  pattern <- paste0(
    "(?:",
    paste0(gsub("([^[:alnum:]_])", "\\\\\\1", names(keys)), collapse = "|"),
    ")"
  )
  matches <- gregexpr(pattern, text, perl = TRUE)
  regmatches(text, matches) <- lapply(
    regmatches(text, matches),
    function(found) unname(keys[found])
  )
  text
}
