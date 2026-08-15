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
  named <- nameable_call(expr)
  if (is.null(named)) {
    return(NULL)
  }
  rlang::call_name(named)
}

static_call_ns <- function(expr) {
  named <- nameable_call(expr)
  if (is.null(named)) {
    return(NULL)
  }
  rlang::call_ns(named)
}

# Whether `rlang::call_name()` and `rlang::call_ns()` may be asked about this
# node at all: it is a call, and not a call to `~`. It does not promise a name
# -- a call whose head is itself a call has none, and both answer `NULL` for
# it, which every site is already written to handle since #100 made the name
# read NULL-safe.
is_nameable_call <- function(expr) {
  rlang::is_call(expr) && !rlang::is_call(expr, "~")
}

# The node those two read a name from: this call with its redundant parentheses
# removed, and `NULL` where there is no name to read. Where the parentheses were
# around the head, the node is rebuilt without them, because that is the only
# way to ask rlang about a head it would otherwise refuse to read -- and the
# rebuilt node is used for naming alone, never handed back to a rewrite, so the
# head a walk puts back is still the head the caller wrote.
#
# The shape test comes before anything is bound to a local, which is the rule
# `static_call_args()` states, applied to the two readers every walk asks first.
# `expr` here is a call part like any other, so it can be R's empty argument,
# and `rlang::is_call()` answers that as the `FALSE` it always did while
# `expr <- ...` would bind the missing marker and raise `missingArgError` on the
# next read of it (#168, #174).
nameable_call <- function(expr) {
  if (!is_nameable_call(expr)) {
    return(NULL)
  }
  unwrapped <- unparenthesized_call(expr)
  head <- static_call_head(unwrapped)
  spelled <- unparenthesized_name(head)
  if (identical(spelled, head)) {
    return(unwrapped)
  }
  rebuild_static_call(unwrapped, static_call_args(unwrapped), head = spelled)
}

# `(` is the identity function, so a redundant pair of parentheses changes
# nothing about what a call calls or what it is given: `(grouping_id)(region)`,
# `(grouping_id(region))`, and `grouping_id(region)` are one call written three
# ways. The three readers below see through them, which is what gives every
# analysis in the package the same answer for the three at once, rather than
# each family recognizing whichever spellings someone thought to enumerate
# (#178, ADR 0019).
#
# Identity here stays syntactic. `(get("grouping_id"))(region)` strips to a
# call rather than to a name, so the head is unresolved and stays that way,
# which is the conservative #130 policy these three are written not to weaken.
#
# Everything a pair of parentheses wraps, however many pairs deep. This is the
# reading for a position holding a value -- a `.fns` argument, a name given to
# `get()` -- where `(x)` is the value of `x` and nothing else.
#
# It stops at R's empty argument rather than unwrapping to it, so that nothing
# below can bind the missing marker to a local: the first read of such a local
# raises base R's untyped `missingArgError`, naming this frame's variable rather
# than anything the caller wrote (#168, #174). A constructed `(` call holding
# one wraps no value to read, which is the answer stopping gives.
unparenthesized_value <- function(expr) {
  while (is_redundant_parens(expr) && !rlang::is_missing(expr[[2L]])) {
    expr <- expr[[2L]]
  }
  expr
}

# The name a head or a function reference spells. A head is unwrapped only down
# to a name, because that is the only thing a head can be unwrapped to without
# changing what R does with it: `("sum")(1)` is not a call to `sum` but the
# error R raises for applying a non-function, and `(function(x) x)(1)` names
# nothing either way. Both keep the answer they have always had.
#
# The early return is the other half of the missing-marker rule above. This is
# asked of an argument a caller may have left empty -- an `across()` `.fns` is
# one -- and an argument that is not a pair of parentheses is given straight
# back rather than reaching the local below.
unparenthesized_name <- function(expr) {
  if (!is_redundant_parens(expr)) {
    return(expr)
  }
  spelled <- unparenthesized_value(expr)
  if (is_name_part(spelled) || rlang::is_call(spelled, c("::", ":::"))) {
    return(spelled)
  }
  expr
}

# The call a node is, through parentheses wrapped around the whole of it. A
# pair wrapping anything but a call is kept: `(share)` wraps a bare symbol, and
# that symbol is a genuine data-mask read which the walks report by descending
# into the `(` call as they descend into any other, so unwrapping it would hand
# a walk a symbol where it has just tested for a call.
#
# `is_nameable_call()` rather than `rlang::is_call()` is what keeps an injected
# quosure or formula wrapped. Both are calls to `~` carrying an environment and
# a class, and a node reached through this is the node `rebuild_static_call()`
# rebuilds around -- from the outer `(` call's attributes, which are none. That
# is the identity loss #165 removed, arriving one pair of parentheses later.
unparenthesized_call <- function(expr) {
  if (!is_redundant_parens(expr)) {
    return(expr)
  }
  spelled <- unparenthesized_value(expr)
  if (is_nameable_call(spelled)) {
    return(spelled)
  }
  expr
}

# A pair of parentheses R evaluates as itself: the call `(` really is, holding
# the one argument the parser gives it. A constructed call to `(` holding any
# other number is not a pair a reader may drop.
is_redundant_parens <- function(expr) {
  rlang::is_call(expr, "(") && length(expr) == 2L
}

# Whether unwrapping answers a different node, which is what a reader that
# restarts on what the parentheses wrap has to know before it restarts. Asked
# here rather than compared at each site so that the three walks that restart
# share one test, and so that none of them binds the answer to a local: a call
# part may be R's empty argument, and `unparenthesized_value()` stops at one
# rather than answering it.
is_parenthesized <- function(expr) {
  !identical(unparenthesized_value(expr), expr)
}

# The head and the arguments of a call a walk descends into, and the node
# rebuilt around rewritten arguments. `static_call_name()` above answers what a
# walk asks of a node it does not descend into; these three are what it asks of
# one it does.
#
# All three read through the parentheses the name read reads through, so a node
# has one name and one set of operands rather than a name taken from
# `(f(x))`'s content and operands taken from its wrapper. That split is the
# defect the quosure paragraph below describes, and it is the same defect
# whichever node the two readings disagree about.
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
  unparenthesized_call(expr)[[1L]]
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
  as.list(unparenthesized_call(expr))[-1L]
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

# `head` defaults to the head the node already has, which is what every walk
# wants: a rewrite of a call's arguments is not a rewrite of what it calls.
# Passing one is how a recognized spelling is written back qualified
# (`qualify_static_spelling()`), and it goes through this rather than through
# `expr[[1L]] <-` so that the quosure and formula attributes are carried across
# by the one function that knows to carry them.
rebuild_static_call <- function(expr, args, head = static_call_head(expr)) {
  # An invariant, not a Package condition (ADR-0015): a quosure carries exactly
  # one expression, so rebuilding one around any other number of arguments
  # would attach its class to a `~` call that is not a quosure at all -- an
  # object rlang's own accessors would then misread. No rewrite of a public
  # call reaches this: the two walks that hand a quosure here map the
  # arguments `static_call_args()` gave them one to one, and the ones that
  # change an argument count are named `across()` and `pick()` calls.
  stopifnot(!rlang::is_quosure(expr) || length(args) == 1L)
  rebuilt <- rlang::call2(head, !!!args)
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
#
# Through the shared reader rather than `rlang::call_args()`, because every
# caller asks this of a call it has just named: reading the arguments any other
# way is what splits a name taken from `(quote(x))`'s content off the operands
# of its wrapper.
call_formal_argument <- function(expr, formal, positional = TRUE) {
  args <- static_call_args(expr)
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
  args <- static_call_args(expr)
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
#
# `bound` is what the caller knows about the names in scope where this call
# sits, and a bare head naming one of them is not claimed as a capture: a block
# that binds `quote` to a function of its own and then calls it evaluates the
# argument. R's function lookup skips a non-function binding, so the name may
# still reach `base::quote()` and the parts be data after all -- undecidable
# here, and answered in the direction that analyses rather than the one that
# hides. A qualified head is out of reach of any binding and keeps its capture.
#
# A head written inside parentheses is refused a capture outright, whatever
# `bound` holds, and it is the one place a parenthesized head is not read as the
# name it spells. `(quote)(x)` evaluates its head as a value, so R's function
# lookup never runs and *any* binding wins -- a column, a preceding summary, or
# a caller's own function, none of which this can see. Claiming the capture
# would stop reporting `x`, which is the silent miss #130 fixed this walk to
# prevent, so the undecidable head resolves toward analysing as every other one
# does. This is the asymmetry `R/contextual-helpers.R` records: a capture
# answers to the environment, and a Contextual helper never does, which is why
# recognizing one through the same parentheses is right there and wrong here.
#
# Only the share dependency walk passes a set, because it is the only analysis
# that tracks bindings and the only one whose wrong answer is silence about a
# wrong number (#130, #162). The searches and the rewrites pass none, and that
# is the reading they already give every name they match: a locally bound
# `across` is resolved as a selection today, and a locally bound `cur_group` is
# refused as the branch-local helper. Their wrong answer is a diagnostic or an
# uncompiled helper, both of which the caller sees.
captured_call_parts <- function(expr,
                                call_name = static_call_name(expr),
                                bound = character()) {
  args <- static_call_args(expr)
  captured <- rep(FALSE, length(args))
  formal <- language_capture_formal(call_name)
  if (is.null(formal)) {
    return(captured)
  }
  call_head <- static_call_head(expr)
  if (is_redundant_parens(call_head)) {
    return(captured)
  }
  if (rlang::is_symbol(call_head) && rlang::as_name(call_head) %in% bound) {
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
evaluated_call_args <- function(expr,
                                call_name = static_call_name(expr),
                                bound = character()) {
  static_call_args(expr)[!captured_call_parts(expr, call_name, bound = bound)]
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
  captured <- captured_call_parts(expr)
  language_index <- readable_language_index(expr)
  map_call_parts(
    expr,
    function(part, index) {
      if (captured[[index]]) {
        return(part)
      }
      if (index == language_index) {
        return(rewrite_evaluated_language(part, rewrite))
      }
      rewrite(part)
    }
  )
}

# The call rebuilt around parts a walk mapped one to one. The arguments are
# read by index rather than mapped over, because a rewrite has to put its
# replacements back in the positions they came from, and the names have to be
# carried across with them: they live in the call's pairlist tags, which
# `rebuild_static_call()` takes from this list, so an index map that dropped
# them would turn `across(value, .names = "{.col}")` into a call whose template
# is a positional argument.
map_call_parts <- function(expr, map) {
  parts <- static_call_args(expr)
  mapped <- lapply(
    seq_along(parts),
    function(index) map(parts[[index]], index)
  )
  rebuild_static_call(expr, stats::setNames(mapped, names(parts)))
}

# Where a rewrite may open a capture: the argument an `eval()` runs, and only
# where what it runs is statically readable. `eval(substitute(x, env))`
# substitutes from an environment this analysis cannot read, so what reaches
# the mask is unknown -- and a rewrite that opened it anyway would compile a
# helper the searches beside it contribute nothing about, which is the same
# index read three ways instead of one.
readable_language_index <- function(expr, call_name = static_call_name(expr)) {
  if (is.null(evaluated_language_parts(expr, call_name = call_name))) {
    return(0L)
  }
  evaluated_language_index(expr, call_name = call_name)
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
  if (!any(captured_call_parts(expr))) {
    return(rewrite(expr))
  }
  map_call_parts(expr, function(part, index) rewrite(part))
}

# The readers below say what a call names and what language it holds. They
# answer questions about an expression and decide nothing about it: which
# function a head names however it is spelled, which primitives resolve a name
# or evaluate language, and which language object a call is statically known to
# build. Every walk in the package reads through them -- the share dependency
# walk in `R/share.R`, the two rewrites, and the three searches -- and each
# decides for itself what to do with the answer.
#
# They live here rather than beside the walk that first needed them, in #173
# and then #179, so that the dependency runs one way. `R/share.R` owns every
# contextual share decision, as design/architecture.md records, and a shared
# reader that lived there would be a deep module the grouping-context rewrite
# and the summary-selection searches had to reach into for a fact that is not
# about shares at all.
#
# The function an expression statically names, however it spells it: a symbol,
# a namespace qualifier, redundant parentheses, or a call to a primitive whose
# purpose is to name a function. `get("get")("share")`,
# `match.fun("get")("share")` and `getFunction("get")("share")` all invoke the
# same primitive as `get("share")`, and a branch matching on the call's own
# name sees none of them, because a call whose head is a call has no name.
#
# What none of those spellings change is that the head is a read: `(f)`,
# `match.fun("f")` and the rest are evaluated in the mask as values rather than
# through R's function lookup. `call_part_symbols()` reports that read, and
# every caller of this unions the two answers.
#
# A literal string is here for `do.call()`, which takes its function that way.
static_callee_name <- function(callee) {
  # `is_name_part()` rather than `rlang::is_symbol()`, for the reason that
  # function gives: the empty argument is a symbol whose name is `""`, so a
  # bare symbol test answers a callee named `""`, which nothing the caller
  # wrote can be (#174). It answers no name instead, which is what an
  # unreadable head already answers.
  if (is_name_part(callee)) {
    return(rlang::as_name(callee))
  }
  if (!rlang::is_call(callee)) {
    named <- static_character_value(callee)
    if (is.null(named) || length(named) != 1L) {
      return(NULL)
    }
    return(named)
  }
  # Through the shared reading of a redundant pair rather than a test of its
  # own. This branch is where #130 first recorded the shape, and it predates
  # #178; leaving it written out kept two rules for one pair of parentheses,
  # which disagreed about a `(` call built by hand with two arguments -- a node
  # this dropped and every other reader kept.
  if (is_parenthesized(callee)) {
    return(static_callee_name(unparenthesized_value(callee)))
  }
  if (rlang::is_call(callee, c("::", ":::")) && length(callee) >= 3L) {
    return(static_callee_name(callee[[3L]]))
  }
  call_name <- static_call_name(callee)
  if (is.null(call_name)) {
    return(NULL)
  }
  formal <- function_naming_formal(call_name)
  if (is.null(formal)) {
    return(NULL)
  }
  static_callee_name_from(callee, formal)
}

# The name a function-naming primitive was given, where that name is statically
# knowable. One that is not leaves the head unnamed, which is the answer for
# any other computed head: the walk reports the head's own reads and treats the
# call as the ordinary call it cannot recognize.
static_callee_name_from <- function(callee, formal) {
  argument <- call_formal_argument(callee, formal)
  if (length(argument) == 0L) {
    return(NULL)
  }
  named <- static_character_value(argument[[1L]])
  if (is.null(named) || length(named) != 1L) {
    return(NULL)
  }
  named
}

# The formal each function-naming primitive takes its name in, and `NULL` for a
# call that names no function. `mget()` and `exists()` are absent because
# neither can answer a function to call: one answers a list and the other a
# flag.
function_naming_formal <- function(call_name) {
  switch(call_name,
    match.fun = "FUN",
    getFunction = "name",
    get = ,
    get0 = "x",
    NULL
  )
}

# The primitives that resolve a name given as a string. Each searches ordinary
# lexical scope from an environment that, under a data mask, is the mask, so a
# name one of them is handed is a mask read exactly as the symbol is.
#
# `dynGet()` is deliberately absent: it searches the calling frames rather than
# the lexical scope, so it does not reach the mask -- measured, not assumed.
#
# One of these reached as a *value* rather than as a callee is out of reach of
# any static walk, and is safe for the same measured reason: in
# `sapply(c("share"), get)` the environment `get()` searches from is the frame
# that called it, inside `sapply()`, so the call raises rather than reading a
# column. Which function a value holds is undecidable in general -- `f <- get`
# is the smallest case -- so a walk answering it would have to over-report
# every call it cannot name, and that is nearly all of them.
is_reflective_lookup <- function(callee_name) {
  !is.null(callee_name) &&
    callee_name %in% c("get", "get0", "mget", "exists")
}

# The primitives that evaluate a language object. `evalq()` is absent because
# it quotes its first argument, so the general walk already reports the symbols
# under it -- adding it here would answer the same thing twice.
is_reflective_evaluation <- function(callee_name) {
  !is.null(callee_name) &&
    callee_name %in% c("eval", "eval_tidy", "eval_bare")
}

# The function a call reaches, read from its own name where it has one and from
# its head where it does not. `(get)("share")` and `match.fun("get")("share")`
# name the primitive through a head that is a call, so a site matching on the
# call's name alone sees neither (#130, #173).
#
# `call_name` is taken rather than read again because every caller has read it
# already, and reading a node's name once is what the analysis sites here were
# folded down to (#163). That is also why the readers below thread it: one
# search hands its own read all the way through.
resolved_callee_name <- function(expr, call_name = static_call_name(expr)) {
  if (!is.null(call_name)) {
    return(call_name)
  }
  static_callee_name(static_call_head(expr))
}

# Where in a call's arguments the language it evaluates sits, and `0L` for a
# call that evaluates none. The searches, the rewrites, and the dependency walk
# open the same argument, so its position is read once here.
evaluated_language_index <- function(expr, call_name = static_call_name(expr)) {
  if (!is_reflective_evaluation(resolved_callee_name(expr, call_name))) {
    return(0L)
  }
  call_formal_index(static_call_args(expr), "expr")
}

# The language a call is statically known to evaluate: `list()` for a call that
# evaluates none, and `NULL` where what it evaluates is not knowable. The two
# empty answers are the ones `static_language_values()` separates, kept apart
# because each reader resolves them differently -- the walk turns `NULL` into
# the marker, a search into nothing, and a rewrite into a capture it may not
# open.
#
# An `envir` argument is not an exemption, here or at the walk that used to ask
# this question alone. `eval()`'s `enclos` defaults to `parent.frame()`, which
# under a data mask is the mask, so a supplied `envir` that is a list or a data
# frame leaves the mask on the lookup path -- `eval(as.name("share"),
# list(a = 1))` reads the share, measured rather than assumed. Which of the two
# an argument evaluates to is not decidable here.
evaluated_language_parts <- function(expr, call_name = static_call_name(expr)) {
  index <- evaluated_language_index(expr, call_name = call_name)
  if (index == 0L) {
    return(list())
  }
  static_language_values(static_call_args(expr)[[index]])
}

# Everything a search descends into: the arguments the mask evaluates, and the
# language the call hands `eval()`. Both halves are the boundary #179 draws,
# read in one place because a search that took only the first half would let
# `eval(quote(cur_group_id()))` run and answer a branch-local identifier, which
# is the value that guard exists to refuse.
#
# Language that is not statically readable contributes nothing rather than a
# marker. The dependency walk has an alias set to compare an over-report
# against and turns an unreadable one into a read of every alias; a search has
# nothing to compare, so refusing on one would reject `eval(built)` wherever it
# appears, which is legal code and always was.
searched_call_parts <- function(expr, call_name = static_call_name(expr)) {
  language <- evaluated_language_parts(expr, call_name = call_name)
  if (is.null(language)) {
    language <- list()
  }
  c(evaluated_call_args(expr, call_name = call_name), language)
}

# The language objects an expression is statically known to hand `eval()`, and
# `NULL` where it is not knowable. An empty list is the third answer and is not
# the same as `NULL`: a constant evaluates to itself and looks nothing up.
#
# The constructors that hide a name from the walk need a case, and so do the
# two that capture one. A name written as a string was never reported by the
# walk of the call's parts, and a name written under `quote()` or
# `substitute()` no longer is, since the mask evaluates neither (#179) -- so
# this is the only thing standing between `eval(quote(share))` and the silence
# that #173 removed.
#
# A capture evaluated in place, as `evalq()` evaluates its argument, needs no
# case of its own: the walk of the call's parts reports the symbol under it as
# it always did.
#
# `bquote()` is not among them because `.()` substitutes an expression this
# walk cannot see, which leaves it with the answer it gives any other
# unrecognized shape.
static_language_values <- function(expr) {
  if (rlang::is_symbol(expr)) {
    # The symbol names a value, and which language object that value holds is
    # not visible here.
    return(NULL)
  }
  if (!rlang::is_call(expr)) {
    return(list())
  }
  call_name <- static_call_name(expr)
  if (is.null(call_name)) {
    return(NULL)
  }
  if (identical(call_name, "quote")) {
    return(call_formal_argument(expr, "expr"))
  }
  if (identical(call_name, "substitute")) {
    # What `substitute()` answers is its captured argument with names replaced
    # from `env`. A call supplying one is a substitution this walk cannot read,
    # so the language reaching `eval()` is unknown; a call supplying none
    # substitutes from the mask, which leaves a column and an earlier summary
    # alias alone -- measured under dplyr -- so the expression arrives as
    # written.
    if (call_supplies_other_argument(expr, "expr", "env")) {
      return(NULL)
    }
    return(call_formal_argument(expr, "expr"))
  }
  if (identical(call_name, "expression")) {
    return(static_call_args(expr))
  }
  if (call_name %in% c("as.name", "as.symbol")) {
    return(recovered_name_values(call_formal_argument(expr, "x")))
  }
  if (identical(call_name, "str2lang")) {
    return(parsed_language_values(call_formal_argument(expr, "s")))
  }
  if (identical(call_name, "str2expression")) {
    return(parsed_language_values(call_formal_argument(expr, "text")))
  }
  if (identical(call_name, "parse")) {
    # Matched by name alone: `parse()`'s first positional argument is a
    # connection, and what a connection holds is not knowable here.
    return(parsed_language_values(
      call_formal_argument(expr, "text", positional = FALSE)
    ))
  }
  NULL
}

# The symbols a statically known string names. An empty string is dropped
# rather than turned into a symbol, because `as.name("")` is an error in R and
# the walk must raise none of its own.
recovered_name_values <- function(argument) {
  if (length(argument) == 0L) {
    return(NULL)
  }
  text <- static_character_value(argument[[1L]])
  if (is.null(text)) {
    return(NULL)
  }
  lapply(text[nzchar(text)], as.name)
}

# The language a statically known string parses to. Parsing is not evaluation:
# nothing the caller wrote runs here, which is what lets the recovery happen
# during planning at all. Text that does not parse is reported as unknown
# rather than raised, since the call itself raises R's own condition when it
# runs.
parsed_language_values <- function(argument) {
  if (length(argument) == 0L) {
    return(NULL)
  }
  text <- static_character_value(argument[[1L]])
  if (is.null(text)) {
    return(NULL)
  }
  parsed <- tryCatch(
    str2expression(paste(text, collapse = "\n")),
    error = function(cnd) NULL
  )
  if (is.null(parsed)) {
    return(NULL)
  }
  as.list(parsed)
}

# The character vector an expression is statically known to be, and `NULL`
# where it is not. A literal is one, and so is a `c()` of literals, which is
# how a caller writes the vector `mget()` takes; anything else names a value
# this walk cannot see without running it.
static_character_value <- function(expr) {
  # A literal is a value, so the parentheses a caller may have written around
  # one are read through as they are everywhere else: `get(("share"))` names
  # what `get("share")` names. Read by recursion rather than by rebinding
  # `expr`, which is an argument a `c()` above may have left empty (#174).
  if (is_redundant_parens(expr)) {
    return(static_character_value(unparenthesized_value(expr)))
  }
  if (is.character(expr)) {
    if (anyNA(expr)) {
      return(NULL)
    }
    return(expr)
  }
  if (!rlang::is_call(expr, "c")) {
    return(NULL)
  }
  values <- lapply(static_call_args(expr), static_character_value)
  if (any(vapply(values, is.null, logical(1)))) {
    return(NULL)
  }
  recovered <- unlist(values, use.names = FALSE)
  if (is.null(recovered)) {
    # `c()` of nothing, which names nothing.
    return(character())
  }
  recovered
}

# Required because dtplyr is an optional Suggest rather than an Import: it
# brings data.table, which reads this flag from the calling namespace.
# data.table fixes the spelling of this name, so it cannot follow the package's
# object naming convention.
.datatable.aware <- TRUE # nolint: object_name_linter

utils::globalVariables(".data")
