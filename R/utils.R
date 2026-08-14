# rlang soft-deprecated unquoting inside the `.data` pronoun, and signals a
# lifecycle condition when `expr()` sees it — twice per Margin operation, into
# whatever condition handler the caller has installed. Building the call
# directly produces the identical expression, `.data[["name"]]`, without the
# signal. The name must stay a literal: a bare symbol would be resolved
# against the data mask when the generated expression runs.
margin_column_pronoun <- function(name) {
  # An invariant, not a Package condition (ADR-0015): every call site passes a
  # column name it already holds as a string, so no rewrite of a public call
  # reaches this. A symbol arriving here would build the mask-resolving
  # reference the comment above warns about, silently.
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  rlang::call2("[[", rlang::sym(".data"), name)
}

assert_logical_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(isTRUE(x) || isFALSE(x))) {
    abort_marginplyr(
      sprintf("`%s` must be a logical scalar (`TRUE` or `FALSE`).", nm)
    )
  }
}

# NA_character_ is allowed
assert_string_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(is.character(x) && length(x) == 1)) {
    abort_marginplyr(
      sprintf("`%s` must be a character vector of length 1.", nm)
    )
  }
}

assert_nest_possible <- function(x) {
  nm <- deparse(substitute(x))
  valid_classes <- c("data.frame", "dtplyr_step")
  if (!inherits(x, valid_classes)) {
    abort_marginplyr(
      sprintf(
        "`%s` must be one of the following classes, which can be nested: %s",
        nm,
        toString(valid_classes)
      )
    )
  }
}

# Admission is duck-typed rather than a class whitelist: marginplyr supports
# any input the dplyr verbs it calls can handle, and a whitelist would reject
# a backend that works. `group_vars()` is the first dplyr generic every Margin
# operation reaches, so an input without a method for it cannot proceed, and
# saying so here keeps the diagnostic in the caller's terms instead of
# surfacing "no applicable method for 'group_vars'".
#
# Admissibility is decided by looking for a method rather than by calling the
# generic and catching whatever comes back. A backend's `group_vars()` can
# fail for its own reasons, and catching every error would report that failure
# as the caller having supplied the wrong kind of object. dplyr registers no
# default method, so an object no registered method matches is exactly the
# object the generic cannot dispatch on.
assert_margin_input <- function(x) {
  nm <- deparse(substitute(x))
  dispatches <- any(vapply(
    class(x),
    function(cls) {
      # `envir` names the namespace that owns the generic. The default is the
      # caller's frame, where `group_vars` is not visible from inside this
      # package, so every class would look unsupported.
      !is.null(utils::getS3method(
        "group_vars",
        cls,
        optional = TRUE,
        envir = asNamespace("dplyr")
      ))
    },
    logical(1L)
  ))
  if (dispatches) {
    return(invisible(NULL))
  }

  abort_marginplyr(
    sprintf(
      paste0(
        "`%s` must be a data frame or a lazy table that supports dplyr ",
        "verbs; %s was supplied. Convert it with `as.data.frame()` or ",
        "`dplyr::tbl()` first."
      ),
      nm,
      if (is.null(x)) "`NULL`" else sprintf("a <%s>", class(x)[[1L]])
    )
  )
}

assert_lazy_table <- function(x) {
  nm <- deparse(substitute(x))
  invalid_lazy_table_names <- "RecordBatchReader"
  if (inherits(x, invalid_lazy_table_names)) {
    abort_marginplyr(
      sprintf(
        "`%s` must not be an object of the following classes: %s",
        nm,
        toString(invalid_lazy_table_names)
      )
    )
  }
}

# The name and namespace a static analysis reads from a call, and `NULL` when
# there is none to read. Anything that is not a call answers `NULL` too, so a
# site may ask without a guard of its own; the sites that keep one keep it for
# a read it makes beside this, such as walking `as.list(expr)[-1L]`.
#
# `rlang::call_name()` and `rlang::call_ns()` do not answer a formula as the
# call to `~` that it is: both unwrap a one-sided formula to its right-hand
# side. When that side is a bare symbol they raise "`call` must be a defused
# call, not a symbol" -- an untyped condition of the class ADR-0015
# separates -- and when it is not they answer a different call, so
# `~ .data$share` reads as a `$` call and the analysis enters a branch written
# for another shape, carrying the formula as the expression that branch then
# reads `[[2L]]` of (#163). `rlang::call_args()` unwraps the same way, which is
# how a formula reached the direct-share path and was computed as the share it
# is not -- measured on 5f078ea, and the one shape of this whose misread raised
# nothing, which is what the ticket's severity note does not cover.
#
# Every analysis that reads a name recognizes a call by that name, none of them
# recognizes `~`, and each already has an answer for a name it does not know:
# walk the call's parts, or report the shape as one it does not handle. A
# formula is therefore a call with no name, and the analysis treats it as the
# `~` call it is rather than as its right-hand side.
#
# An injected quosure is a call to `~` as well, and gets the same answer for a
# stronger reason. Every site reads operands from the node it has just named --
# `expr[[2L]]`, `as.list(expr)[-1L]`, `length(expr)` -- and a quosure answers
# none of those as the call it carries: `rlang::quo(.data$share)[[2L]]` is
# `.data$share` rather than `.data`, and warns that subsetting a quosure is
# deprecated, while `length()` of any quosure is 2. Naming a quosure for the
# call inside it would split the name from the operands, which is the defect
# this removes rather than a fix for it. Falling through costs nothing:
# `as.list()` of a quosure yields the expression it carries, so a walk reaches
# that expression as a part and analyses it there, where the operands are its
# own.
static_call_name <- function(expr) {
  if (!is_nameable_call(expr)) {
    return(NULL)
  }
  rlang::call_name(expr)
}

static_call_ns <- function(expr) {
  if (!is_nameable_call(expr)) {
    return(NULL)
  }
  rlang::call_ns(expr)
}

# Whether `rlang::call_name()` and `rlang::call_ns()` may be asked about this
# node at all: it is a call, and not a call to `~`. It does not promise a name
# -- a call whose head is itself a call has none, and both answer `NULL` for
# it, which every site is already written to handle since #100 made the name
# read NULL-safe.
is_nameable_call <- function(expr) {
  rlang::is_call(expr) && !rlang::is_call(expr, "~")
}

# The head and the arguments of a call a walk descends into, and the node
# rebuilt around rewritten arguments. `static_call_name()` above answers what a
# walk asks of a node it does not descend into; these three are what it asks of
# one it does.
#
# All three exist for the same node the name read exists for. A quosure is a
# call to `~`, so a walk descends into it, and the spellings a walk reaches for
# -- `expr[[1L]]`, `as.list(expr)[-1L]` -- are the two rlang soft-deprecated on
# a quosure in 0.4.0. Walking one therefore signals a lifecycle condition into
# whatever handler the caller has installed, which is the class of signal
# `margin_column_pronoun()` above exists to avoid producing. `as.list()` is the
# subtler half: it keeps the quosure's class on the list it returns, so it is
# the `[-1L]` after it that dispatches to rlang's deprecated method (#165).
#
# The rebuild is the half that changed an answer rather than only a signal.
# `rlang::call2()` and `as.call()` build a plain call, so the environment and
# the class a quosure carries were dropped and dplyr was handed a one-sided
# formula where the caller injected a quosure: `sum(!!rlang::quo(dplyr::n()))`
# reached the summary as `sum(~dplyr::n())` and `sum()` was given a language
# object. A formula object a caller injects loses the same two attributes, and
# nothing tests for it -- `rlang::is_formula()` reads the call's shape rather
# than its class -- so a lambda came out resolving against whatever mask it
# landed in rather than where it was written. Carrying the attributes across
# covers both, and covers a plain call for free, which has none.
#
# A `~` written in source is untouched by any of this: the verb captures it
# unevaluated, so the node is a bare call with no attributes to lose. The
# exposure is injection, for a quosure and a formula alike.
#
# Every site reading a node's parts goes through these, including the many a
# name match has already told cannot hold a quosure -- a `list()` of `.fns`, a
# `{` block, a `pick()` call. Those sites are safe as they stand, and routing
# them here anyway is what makes the rule checkable rather than remembered:
# neither deprecated spelling survives anywhere in `R/` outside this block, so
# a walk written the old way is one grep away rather than a signal nobody sees
# until a caller installs a handler.
static_call_head <- function(expr) {
  if (rlang::is_quosure(expr)) {
    return(quote(`~`))
  }
  expr[[1L]]
}

# An element of what this returns can be R's empty argument, so a caller reads
# the parts by subscript -- `lapply()`, `vapply()`, or `parts[[i]]` under a
# `seq_along()` loop -- and never as `for (part in static_call_args(expr))`.
# All three of the first form pass the empty symbol as a value, which forces
# without error; `for` binds the missing marker to a variable instead, and the
# first read of that variable raises base R's untyped `missingArgError` naming
# the loop variable rather than anything the caller wrote (#168). This is not a
# contrived shape: every empty-index spelling has one, and `x[, "col"]` is
# everyday R that `dplyr::summarise()` accepts.
#
# What `for` does is bind, so `part <- parts[[index]]` does it too: the rule is
# about the binding rather than about the loop, and lifting one part into a
# local is how the same condition reached an `across()` rebuild (#174). Read
# the part where it is used, and ask `is_name_part()` below what it is.
#
# `test-utils.R` scans the namespace for both spellings.
static_call_args <- function(expr) {
  if (rlang::is_quosure(expr)) {
    return(list(rlang::quo_get_expr(expr)))
  }
  as.list(expr)[-1L]
}

# Whether a call part names something: a symbol, and not the empty argument.
# The second half is what `rlang::is_symbol()` cannot answer on its own, and
# every site that reads a name out of a part needs it -- the empty argument is
# a symbol whose name is `""`, so a bare symbol test reports a column, a
# binding, or an output called `""`, which nothing the caller wrote can be
# (#174). Asked of a part read by subscript, never of one bound to a name,
# which is the rule above.
is_name_part <- function(part) {
  !rlang::is_missing(part) && rlang::is_symbol(part)
}

rebuild_static_call <- function(expr, args) {
  # An invariant, not a Package condition (ADR-0015): a quosure carries exactly
  # one expression, so rebuilding one around any other number of arguments
  # would attach its class to a `~` call that is not a quosure at all -- an
  # object rlang's own accessors would then misread. No rewrite of a public
  # call reaches this: the two walks that hand a quosure here map the
  # arguments `static_call_args()` gave them one to one, and the ones that
  # change an argument count are named `across()` and `pick()` calls.
  stopifnot(!rlang::is_quosure(expr) || length(args) == 1L)
  rebuilt <- rlang::call2(static_call_head(expr), !!!args)
  # Argument names live in the call's own pairlist tags rather than in an
  # attribute, so this replaces nothing `call2()` just set.
  attributes(rebuilt) <- attributes(expr)
  rebuilt
}

# The name each of a call's arguments was given, empty where it was passed
# positionally. An unnamed call carries no names at all rather than empty ones,
# which is the case every site reading both name and position has to fill in.
argument_names <- function(args) {
  arg_names <- names(args)
  if (is.null(arg_names)) {
    return(rep("", length(args)))
  }
  arg_names
}

# The argument a call gives one formal, matched by name and then by position,
# which is how R matches the primitives the analyses here read. Returned as a
# list of length one or an empty list, so that an argument written as `NULL` is
# not read as an absent one.
call_formal_argument <- function(expr, formal, positional = TRUE) {
  args <- rlang::call_args(expr)
  args[call_formal_index(args, formal, positional = positional)]
}

# Where that argument sits, and `0L` when the call supplies none. The two
# answers are one matching rule read two ways: a site reading the argument
# takes the value, and one that has to put something back where the caller
# wrote it takes the position.
call_formal_index <- function(args, formal, positional = TRUE) {
  arg_names <- argument_names(args)
  index <- match(formal, arg_names, nomatch = 0L)
  if (index == 0L && positional) {
    index <- match("", arg_names, nomatch = 0L)
  }
  index
}

# Whether a call supplies an argument beyond the one it takes in `formal`,
# either by naming one of `other_names` or by leaving one more argument unnamed
# than `formal` accounts for. Both primitives that ask this ask it about an
# environment: `get()` and its siblings search the one they are given instead of
# the mask, and `substitute()` substitutes from it.
call_supplies_other_argument <- function(expr, formal, other_names) {
  args <- rlang::call_args(expr)
  arg_names <- argument_names(args)
  if (any(arg_names %in% other_names)) {
    return(TRUE)
  }
  sum(arg_names == "") > as.integer(!(formal %in% arg_names))
}

# The formal a language-capturing primitive takes its captured argument in,
# `NA_character_` where it captures every argument it is given, and `NULL` for
# a call that captures nothing. R evaluates a captured argument nowhere:
# `quote()` answers it unchanged, `substitute()` answers it with names replaced
# from an environment, and `expression()` collects however many it is given.
# Until something evaluates what one of them returns, the expression under it
# is data the caller is carrying rather than code the data mask runs, so no
# analysis here may read a helper, a selection, or a column reference out of
# it, and no rewrite may replace what is inside it (#179).
#
# `expression()` is here because `static_language_values()` already recovers
# it: the two halves of one boundary would otherwise disagree about the same
# call, reading it as a language object where `eval()` runs it and as ordinary
# code everywhere else.
#
# `evalq()` and `bquote()` are deliberately absent. `evalq()` captures its
# first argument and then evaluates it, so the symbols under it are read;
# `bquote()` evaluates whatever `.()` wraps in the enclosing frame, which under
# a data mask is the mask, so `bquote(f(.(share)))` reads the share. Both stay
# analyzed, which over-reports the parts of them that really are data.
#
# rlang's `expr()` and `quo()` are absent for a different reason, and it is the
# asymmetry below rather than a claim about what they do. Recognizing a name
# here is a decision to stop reporting what sits under it, and `expr` is a name
# callers bind -- this package binds it in nearly every function -- so a bare
# call to one is not evidence of rlang's. Reading them as ordinary code keeps
# the pre-#179 answer, which is a false refusal at worst.
language_capture_formal <- function(call_name) {
  if (is.null(call_name)) {
    return(NULL)
  }
  switch(call_name,
    quote = ,
    substitute = "expr",
    expression = NA_character_,
    NULL
  )
}

# Which of a call's arguments are captured that way, as a logical vector over
# `static_call_args()`. Everything else the call holds is evaluated:
# `substitute()`'s `env` is an ordinary operand, and so is every argument of a
# call that captures nothing.
#
# A capture is recognized only where it is written plainly -- `quote()`, or
# `base::quote()`. A qualifier naming another namespace is another package's
# function, and a computed head names nothing this can read, so both fall
# through to the walk that reports the symbols under them. That asymmetry is
# the point: reading a capture where there is none costs a diagnostic about a
# column the caller did write, while missing one is the silent miss #130 fixed
# this walk to prevent, and no static reading tells `pkg::quote()` from
# `base::quote()` except the qualifier itself.
#
# What the name alone cannot rule out is a caller who binds a function of their
# own to it. A binding this analysis can see is answered where the bound names
# are known, in `call_part_symbols()`; one made outside the expression --
# `quote <- function(e) e` in the calling environment -- is the undecidable
# case `is_reflective_lookup()` records for `f <- get`, and it resolves the
# same way, by reading what the name says.
#
# A call carrying more arguments than the primitive takes is not the shape this
# recognizes either. R refuses `quote(a, b)` when it runs, so the argument
# beyond the captured one is reported rather than protected, which is the same
# direction the length tests in `expression_data_symbols()` resolve toward.
captured_call_parts <- function(expr, call_name = static_call_name(expr)) {
  args <- static_call_args(expr)
  captured <- rep(FALSE, length(args))
  formal <- language_capture_formal(call_name)
  if (is.null(formal)) {
    return(captured)
  }
  namespace <- static_call_ns(expr)
  if (!is.null(namespace) && !identical(namespace, "base")) {
    return(captured)
  }
  if (is.na(formal)) {
    return(rep(TRUE, length(args)))
  }
  index <- call_formal_index(args, formal)
  if (index == 0L) {
    return(captured)
  }
  captured[[index]] <- TRUE
  captured
}

# The arguments of a call a walk analyzes: everything the mask evaluates, and
# nothing it captures. Every search that descends into a call reaches its parts
# through this rather than through `static_call_args()` directly, which is what
# keeps one reading of the boundary from being four.
evaluated_call_args <- function(expr, call_name = static_call_name(expr)) {
  static_call_args(expr)[!captured_call_parts(expr, call_name)]
}

# The call a rewrite gives back once it has descended into the same parts:
# each evaluated argument replaced by what `rewrite` makes of it, each captured
# argument left as the caller wrote it.
#
# The arguments are read by index rather than mapped over, because a rewrite
# has to put its replacements back in the positions they came from, and the
# names have to be carried across with them: they live in the call's pairlist
# tags, which `rebuild_static_call()` takes from this list, so an index map that
# dropped them would turn `across(value, .names = "{.col}")` into a call whose
# template is a positional argument.
rewrite_evaluated_call_parts <- function(expr, rewrite) {
  parts <- static_call_args(expr)
  captured <- captured_call_parts(expr)
  language_index <- evaluated_language_index(expr)
  rewritten <- lapply(
    seq_along(parts),
    function(index) {
      if (captured[[index]]) {
        return(parts[[index]])
      }
      if (index == language_index) {
        return(rewrite_evaluated_language(parts[[index]], rewrite))
      }
      rewrite(parts[[index]])
    }
  )
  rebuild_static_call(expr, stats::setNames(rewritten, names(parts)))
}

# The argument an `eval()` runs, rewritten as the code it becomes. A capture
# written there is opened, because the mask evaluates what it holds after all:
# `eval(quote(grouping_bit(region)))` compiled to its branch constant before
# the boundary was drawn, and leaving the capture closed sent the helper itself
# to the caller, which reports that it can only be used inside the verb they
# are already inside (#179).
#
# Only a capture written out can be opened. Language built at run time --
# `eval(str2lang("grouping_bit(region)"))` -- has nothing in the source to
# rewrite, and it reached the helper before this ticket exactly as it does
# after: what a search can recover by parsing, a rewrite cannot put back.
rewrite_evaluated_language <- function(expr, rewrite) {
  captured <- captured_call_parts(expr)
  if (!any(captured)) {
    return(rewrite(expr))
  }
  parts <- static_call_args(expr)
  rewritten <- lapply(
    seq_along(parts),
    function(index) rewrite(parts[[index]])
  )
  rebuild_static_call(expr, stats::setNames(rewritten, names(parts)))
}

# Required because dtplyr is an optional Suggest rather than an Import: it
# brings data.table, which reads this flag from the calling namespace.
# data.table fixes the spelling of this name, so it cannot follow the package's
# object naming convention.
.datatable.aware <- TRUE # nolint: object_name_linter

utils::globalVariables(".data")
