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
#
# It is also the interpolating entry point: `message` is a cli template a call
# site passes unexpanded, and `env` is what lets cli evaluate the template's
# `{}` expressions in that site's own frame. ADR 0023 is authoritative for the
# idiom every template is authored in.
#
# The template is expanded here, as the condition is raised, rather than when
# the condition is read, which is what keeps a subject spelled the way the
# caller spelled it. ADR 0024 is authoritative for that decision and its costs.
#
# `format_inline()` is the inline half of cli and does not consult the width, so
# every element comes back as the one line it was written as. Names carry the
# bullet markers and are reattached, `vapply()` having dropped them.
abort_marginplyr <- function(message,
                             ...,
                             class = NULL,
                             call = rlang::caller_call()) {
  env <- rlang::caller_env()
  expanded <- vapply(
    message,
    cli::format_inline,
    character(1),
    .envir = env,
    USE.NAMES = FALSE
  )
  names(expanded) <- names(message)
  rlang::abort(
    message = expanded,
    ...,
    class = c(class, "marginplyr_error"),
    call = call,
    # Spelled although `rlang::abort()` has a default, because the default is
    # this function's own caller as seen from `abort()` -- one frame deeper than
    # the site -- which leaves the raising call in the backtrace that
    # `cli_abort(.envir =)` trimmed.
    .frame = env
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
# `restatements` is this branch's own map from the label of what it handed
# dplyr to the label of what the caller wrote. It is a map and not the vector
# of labels `new_summary_arguments()` carries -- the two are one word apart and
# hold different things -- and it sits here rather than in `conditions` because
# a branch rewrites `grouping_bit()` and `grouping_id()` to its own constants,
# so no two branches necessarily hand dplyr the same expressions.
with_branch_conditions <- function(expr,
                                   conditions,
                                   restatements = character()) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(cnd) {
        buffer_branch_warning(cnd, conditions, restatements)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      stop(restate_branch_error(cnd, conditions, restatements))
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
restate_branch_error <- function(cnd, conditions, restatements) {
  cnd <- restate_condition_arguments(cnd, restatements)
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
buffer_branch_warning <- function(cnd, conditions, restatements) {
  # The spelling is restored before the identity is read, so the identity is
  # over the argument the caller wrote: `grouping_bit()` renders as a different
  # constant in every branch, which split one written expression into one
  # report per branch (ADR 0022).
  cnd <- restate_condition_arguments(cnd, restatements)
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
# at the store is the last line of a message whose header said there was more
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
  written <- written_message_lines(lines, runs)
  # `!` opens the caller's own diagnostic, so a message dplyr attributed to no
  # argument still has a bullet where its header ends. Both spellings of the
  # informational one are matched because the symbol is cli's and follows
  # `cli.unicode`.
  first_bullet <- match(TRUE, grepl("^[i\u2139!] ", written))
  cause <- match(TRUE, grepl("^Caused by ", written))
  removed <- rep(FALSE, length(runs))

  header <- if (is.na(first_bullet) || first_bullet == 1L) {
    ""
  } else {
    paste(written[seq_len(first_bullet - 1L)], collapse = " ")
  }
  header_match <- regmatches(
    header,
    regexec("^There (were|was) ([0-9]+) warnings? in ", header)
  )[[1L]]
  aggregated <- length(header_match) > 0L
  # `regmatches()` returns the whole match ahead of the two groups, so the
  # count is the third element.
  warning_count <- if (aggregated) {
    as.integer(header_match[[3L]])
  } else {
    NA_integer_
  }
  if (aggregated) {
    removed[seq_len(first_bullet - 1L)] <- TRUE
  }
  if (!is.na(cause)) {
    removed <- removed |
      (grepl("^[i\u2139] In group ", written) & seq_along(written) < cause)
  }
  # The pointer's backticks are optional because cli writes the call as an
  # `x-r-run` hyperlink where the terminal takes one, and the backticks are what
  # the link replaces -- so removing the escapes is necessary for this line and
  # not sufficient (#217).
  last <- length(written)
  appended <- aggregated &&
    warning_count > 1L &&
    grepl(
      "^[i\u2139] Run `?dplyr::last_dplyr_warnings\\(\\)`?",
      written[[last]]
    )
  if (appended) {
    removed[[last]] <- TRUE
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

# Each run rejoined to the one line it was written as, with cli's rendering
# taken back off it. The identity and the argument restatement both read
# messages through this, which is what keeps the two readings one: a message the
# identity reads as three written lines is a message the restatement reads as
# the same three.
#
# Every pattern either reader matches is anchored at the start of a line, so
# cli's rendering has to be undone before either reads. The wrapping is undone
# by the runs above; the styling and the links are undone here. ADR 0021's *No
# rendering decision takes part in the identity* is authoritative for why all
# three are excluded. `link` is spelled although it is the default, because
# stripping the hyperlink is half of what this call is here for rather than
# incidental to it.
#
# This is a *reading*. `branch_warning_identity()` assembles its key from the
# lines as they arrived, so nothing here can make two diagnostics that differ
# only by an escape sequence into one identity. The restatement is the one
# reader that puts a line it read back into a message, and a restated line is
# therefore rendered plain -- accepted rather than worked around, and recorded
# in ADR 0022 beside the wrapping such a line already loses.
#
# cli needs no availability guard: it is an Import of this package, and every
# error path crosses it, `abort_marginplyr()` above expanding its template
# through `cli::format_inline()` (ADR 0023). `ansi_strip()` learned `link` in
# cli 3.3.0, so this reader is satisfied by anything DESCRIPTION's
# `cli (>= 3.4.0)` admits.
written_message_lines <- function(lines, runs) {
  lines <- cli::ansi_strip(lines, link = TRUE)
  vapply(
    runs,
    function(run) paste(sub("^ +", "", lines[run]), collapse = " "),
    character(1)
  )
}

# Replays what the branches withheld: one report per distinct identity, each
# saying how many further grouping sets raised it. The conditions are replayed
# in the order the branches raised them, and the reported occurrence is the
# first, so a plan that raises nothing new reads as one branch's report.
#
# The count line is marginplyr's own sentence and is inside ADR 0023's rule,
# where everything else this module writes is outside it: the only value
# interpolated is an integer marginplyr counted. `format_error_bullets()` stays
# around it because this is appended to a rendered message rather than raised,
# so the `i` has to be rendered here.
#
# ADR 0021's contract is untouched. The warning's identity is computed when a
# branch buffers it, before this line exists.
report_branch_warnings <- function(conditions) {
  for (entry in conditions$warnings) {
    cnd <- entry$condition
    if (entry$count > 1L) {
      cnd$message <- paste0(
        cnd$message,
        "\n",
        rlang::format_error_bullets(c(i = cli::pluralize(
          "{entry$count - 1L} further grouping set{?s} raised this warning."
        )))
      )
    }
    warning(cnd)
  }
  invisible(NULL)
}

# The label dplyr quotes an argument by, reproduced so that the expression
# marginplyr handed it can be recognised in a context it rendered. Plain
# `rlang::as_label()` under dplyr's `name = ` convention is what this uses,
# rather than dplyr's `expr_as_label()`. What this site writes is a condition
# label, and that is what ADR 0022 decides; a name a caller reads back falls
# the other way, under the same ADR's amendment for #439.
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

# What one branch hands dplyr, mapped to what the caller wrote. `caller_labels`
# is the label half of what `new_summary_arguments()` built: the caller's label
# for each dot, kept beside the dots since planning because the rewrites do not
# preserve position (ADR 0022). The lengths agree here or stop the operation,
# as an invariant rather than a Package condition (ADR 0015): a map built from
# a misaligned pair would quote one argument's expression under another. `NULL`
# is a caller with no spelling to restore, and maps nothing.
#
# A label several dots share is kept only where the callers' own labels agree,
# since the replacement is then the same whichever dot dplyr meant; where they
# differ the entry is dropped and the quotation stays as dplyr wrote it. An
# entry no rewrite changed is dropped as well, having nothing to restate.
branch_argument_map <- function(dots, caller_labels) {
  stopifnot(length(dots) == length(caller_labels))

  labels <- summary_argument_labels(dots)
  written <- unique(labels)
  restored <- vapply(
    written,
    function(label) {
      candidates <- unique(caller_labels[labels == label])
      if (length(candidates) == 1L) candidates else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
  keep <- !is.na(restored) & restored != written
  stats::setNames(restored[keep], written[keep])
}

# The argument bullet, restated to quote the expression the caller wrote. The
# span inside the backticks is replaced where it equals exactly one branch
# dot's label, and left where it is otherwise; nothing around it moves.
# ADR 0022 is authoritative for what the span is compared against and for how
# the restatement degrades.
#
# `rendered` says which shape the message has rather than what to do with it: a
# warning carries the text dplyr rendered before signalling, and an error's
# `$message` is the structured bullet alone. What each shape means for the
# restatement is `dplyr_message_runs()` below, in one piece.
restate_condition_arguments <- function(cnd, restatements) {
  if (length(restatements) == 0L || !is.character(cnd$message)) {
    return(cnd)
  }
  restated <- vapply(
    cnd$message,
    restate_argument_lines,
    character(1),
    restatements = restatements,
    rendered = inherits(cnd, "warning"),
    USE.NAMES = FALSE
  )
  # The names carry the bullet markers of a structured condition, so they are
  # put back rather than left to `vapply()`, which would otherwise name each
  # element after the text it was rendered from.
  cnd$message <- stats::setNames(restated, names(cnd$message))
  cnd
}

# Which written lines of a message are dplyr's to restate, which is the whole
# of the bound and is therefore in one place.
#
# A rendered warning carries the caller's own diagnostic after its `Caused by`
# line, and a diagnostic can spell anything -- including dplyr's bullet over a
# label a branch really handed dplyr -- so only what precedes that line is
# dplyr's. A rendered message with no such line is left whole: that is dplyr's
# aggregation of a caller diagnostic that rendered empty, among everything that
# is not an aggregation at all, and telling those apart would need the reading
# of dplyr's format this declines to do. An error's `$message` is dplyr's
# bullet alone, the caller's diagnostic being `$parent`, so nothing bounds it.
dplyr_message_runs <- function(written, rendered) {
  if (!rendered) {
    return(seq_along(written))
  }
  cause <- match(TRUE, grepl("^Caused by ", written))
  if (is.na(cause)) {
    return(integer())
  }
  seq_len(cause - 1L)
}

# The message is read as the lines it was written as --
# `written_message_lines()`, the same reading the identity uses, and for the
# same reason: cli wraps a bullet it cannot fit, and a span read off the line
# a bullet opens is a prefix of the label at any narrow width. Reading one
# line at a time restored the spelling at 80 columns and not at 40, which made
# the console width decide how many conditions a caller receives.
#
# A run that is restated collapses to the one line it was written as, since
# what replaces it is a line of a different length and cli is no longer there
# to wrap it. A message in which nothing is restated is returned as the object
# that arrived, not rebuilt from its lines: rebuilding dropped a trailing
# newline, which is a byte the degradation contract says this may not touch.
# The same guard restores a restated message's trailing newline, of which
# splitting keeps all but one.
restate_argument_lines <- function(text, restatements, rendered) {
  # An empty message needs no guard of its own: it splits into no lines, so
  # there is no run to restate and the unchanged path below returns it.
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  runs <- message_line_runs(lines)
  written <- written_message_lines(lines, runs)
  restatable <- dplyr_message_runs(written, rendered)

  changed <- FALSE
  pieces <- vector("list", length(runs))
  for (i in seq_along(runs)) {
    restated <- if (i %in% restatable) {
      restate_argument_bullet(written[[i]], restatements)
    } else {
      written[[i]]
    }
    if (identical(restated, written[[i]])) {
      pieces[[i]] <- lines[runs[[i]]]
    } else {
      pieces[[i]] <- restated
      changed <- TRUE
    }
  }
  if (!changed) {
    return(text)
  }

  restated <- paste(unlist(pieces, use.names = FALSE), collapse = "\n")
  if (endsWith(text, "\n")) {
    restated <- paste0(restated, "\n")
  }
  restated
}

# `In argument:` opens the bullet, carrying cli's informational marker in a
# rendered warning and no marker at all in a structured error, where the `i` is
# the name of the message vector rather than part of its text. The span is read
# to the last backtick on the line, because a non-syntactic name the caller
# wrote puts backticks inside it.
#
# The trailing period is captured rather than required, and put back as it was
# found: eager dplyr ends the sentence with one and dbplyr does not, and the
# sentence around the span belongs to whichever raised the condition
# (ADR 0022).
restate_argument_bullet <- function(line, restatements) {
  found <- regmatches(
    line,
    regexec("^([i\u2139] )?In argument: `(.*)`(\\.)?$", line)
  )[[1L]]
  if (length(found) == 0L) {
    return(line)
  }
  restored <- unname(restatements[found[[3L]]])
  if (is.na(restored)) {
    return(line)
  }
  paste0(found[[2L]], "In argument: `", restored, "`", found[[4L]])
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
