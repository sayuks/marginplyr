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
with_branch_conditions <- function(expr, conditions) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(cnd) {
        buffer_branch_warning(cnd, conditions)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      stop(restate_branch_error(cnd, conditions))
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
restate_branch_error <- function(cnd, conditions) {
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
buffer_branch_warning <- function(cnd, conditions) {
  cnd <- restate_condition_names(cnd, conditions$keys)
  key <- branch_warning_identity(cnd)

  buffered <- conditions$warnings
  seen <- match(key, names(buffered))
  if (is.na(seen)) {
    buffered[[key]] <- list(condition = cnd, count = 1L)
  } else {
    buffered[[seen]]$count <- buffered[[seen]]$count + 1L
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
# Removal is confined to where dplyr writes those parts, and not merely to what
# they say, because everything they say is also something a caller's own
# diagnostic can say. A marker is only a marker at the start of a line, so an
# unanchored match would find one inside a grouping value -- a region named
# `Hawaii Region` carries `i `. A line is only dplyr's before its `Caused by`
# line, since what follows that is the caller's diagnostic, whose second line
# cli renders at column zero exactly as it renders a bullet. And the pointer at
# the store is only dplyr's as the last line of the message.
#
# What is left reads a format dplyr does not promise, and is chosen for which
# way it fails: wording dplyr changes stops matching, the identities stay
# distinct, and every occurrence is reported, which is what happens today. It
# can never collapse a warning that genuinely differs. Both bullet spellings
# are matched because the leading symbol is cli's and follows `cli.unicode`.
branch_warning_identity <- function(cnd) {
  lines <- drop_aggregation_preamble(unwrap_message_lines(
    strsplit(conditionMessage(cnd), "\n", fixed = TRUE)[[1L]]
  ))
  bullet <- "^[i\u2139] "
  causes <- which(grepl("^Caused by ", lines))
  # No `Caused by` line is a message this cannot locate dplyr's own context in,
  # so nothing is removed and the message is compared whole -- the reading that
  # reports every occurrence rather than the one that collapses them.
  attributed <- if (length(causes) == 0L) {
    rep(FALSE, length(lines))
  } else {
    seq_along(lines) < causes[[1L]]
  }
  groups <- attributed & grepl(paste0(bullet, "In group "), lines)
  pointer <- seq_along(lines) == length(lines) &
    grepl(paste0(bullet, "Run `dplyr::last_dplyr_warnings\\(\\)`"), lines)
  paste(c(class(cnd), lines[!(groups | pointer)]), collapse = "\n")
}

# Every bullet as the one line it was written as. cli wraps a bullet it cannot
# fit onto continuation lines indented by two spaces, so which lines a message
# holds is a function of the console width and of how long the grouping values
# are -- and a group bullet matched line by line would then survive the removal
# above from its second line on. Measured on the ticket's own reproduction,
# whose values are one and four characters long: three reports of one condition
# at 60 columns, and four at 40.
#
# Rejoining can only lose text that cli indented under the line above it, which
# is what a wrap is; a caller's own indented line is rejoined into the
# diagnostic it follows and stays part of the identity.
unwrap_message_lines <- function(lines) {
  wrapped <- grepl("^  ", lines) & seq_along(lines) > 1L
  if (!any(wrapped)) {
    return(lines)
  }
  unname(vapply(
    split(sub("^ +", "", lines), cumsum(!wrapped)),
    paste,
    character(1),
    collapse = " "
  ))
}

# dplyr's aggregation header: how many warnings that branch raised, in which
# call, and -- where there was more than one -- that what follows is the first
# of them. All of it precedes the first bullet, and the count is the part that
# necessarily differs between grouping sets.
#
# It is dropped whole rather than edited because it is the one part of the
# message cli wraps onto lines it does not indent, so unwrapping cannot restore
# it and there is no reliable line to edit: at any width from 15 to 24 columns
# the ticket's own reproduction wrapped it into `There were 3` against `There
# was 1`, leaving `warnings in` against `in`, and reported two conditions for a
# plan that raises one. What goes with the count is the call, which every
# branch names identically.
#
# A message that does not open with the header, or that holds no bullet to stop
# at, is left alone and compared whole -- the reading that reports every
# occurrence rather than the one that collapses them.
drop_aggregation_preamble <- function(lines) {
  bullets <- which(grepl("^[i\u2139!] ", lines))
  if (length(bullets) == 0L || bullets[[1L]] == 1L) {
    return(lines)
  }
  # Read as one sentence however it was broken up, so that the match does not
  # become another thing the width decides.
  preamble <- paste(
    sub("^ +", "", lines[seq_len(bullets[[1L]] - 1L)]),
    collapse = " "
  )
  if (!grepl("^There (were|was) [0-9]+ warnings? in ", preamble)) {
    return(lines)
  }
  lines[seq(bullets[[1L]], length(lines))]
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
