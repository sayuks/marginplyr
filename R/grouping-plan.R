validate_grouping_spec_early <- function(grouping_spec) {
  if (is.null(grouping_spec)) {
    return(invisible(NULL))
  }
  if (!inherits(grouping_spec, "margin_grouping_spec")) {
    abort_marginplyr(paste0(
      "{.arg .grouping} must be created with ",
      "{.or {.fun {grouping_constructor_names()}}}."
    ))
  }

  # Classified where it is read, so that what this guard holds from here on is
  # a name and not the value it was read off (#280).
  kind <- grouping_kind_name(grouping_spec_kind(grouping_spec))
  # A catch of its own, because the two fields are not read together: an object
  # can refuse one while answering the other, and this is the only reader of
  # `args` before the guard below has run.
  args <- tryCatch(grouping_spec$args, error = function(cnd) NULL)
  if (is.null(kind) || !is.list(args)) {
    abort_invalid_grouping_spec()
  }

  rule <- find_grouping_kind_rule(kind)
  if (is.null(rule)) {
    abort_invalid_grouping_spec()
  }
  rule$validate_empty(grouping_spec)

  invisible(NULL)
}

abort_invalid_grouping_spec <- function() {
  abort_marginplyr("Invalid grouping specification.")
}

# The kind an object carrying the specification class says it is, or `NULL`
# where it cannot be asked for one at all. Reading a field is a question not
# every object a class can sit on answers: `$` is invalid for an atomic vector,
# a closure is not subsettable, and an S4 class that defines no method for it
# refuses it. Asking before establishing that much is what answered a forged
# `.grouping` with base R's own untyped error, in place of the refusal the
# guard above is written for (#262).
#
# The read is `$` itself and the catch is the whole of what is new, so nothing
# here decides what a specification may be stored as. Two narrower readings
# look equivalent and are not: `is.list()` refuses an environment whose fields
# read, and `[[` with `exact = FALSE` bypasses a `$` method the object defines.
#
# Every reader that can be handed an object nothing has validated shares this
# one, because they ask the same question of the same field, and the colliding
# and the non-colliding spelling answering that question differently is the
# asymmetry #262 was found through. Every other reader of a kind in this file
# sits behind the guard above, which is what establishes there is one to read.
# The third sharer is `print.margin_grouping_spec()`, in `R/grouping-spec.R`,
# which sits behind no guard because a print method has none to sit behind: it
# joined the two this comment was written for in #264.
#
# The catch is narrow in what it decides and not in what it swallows, exactly
# as the evaluation catch in `check_ambiguous_nested_name()` is: an object that
# cannot produce a kind is not a specification of a readable one, whatever
# stopped it producing one. Deciding that is not deciding what the object is,
# which is where ADR 0015 puts the line -- the guard above says what it is, in
# marginplyr's words.
grouping_spec_kind <- function(spec) {
  tryCatch(spec$type, error = function(cnd) NULL)
}

normalize_grouping_input <- function(.data, by_quo) {
  stopifnot(rlang::is_quosure(by_quo))

  if (inherits(.data, "rowwise_df")) {
    abort_marginplyr(c(
      "{.fun rowwise} input is not supported.",
      i = "Call {.fun dplyr::ungroup} first."
    ))
  }

  if (!dplyr::group_by_drop_default(.data)) {
    abort_marginplyr(c(
      "Grouped input created with {.code .drop = FALSE} is not supported.",
      i = "Call {.fun dplyr::ungroup} first."
    ))
  }

  input_groups <- dplyr::group_vars(.data)
  if (length(input_groups) > 0L && !rlang::quo_is_null(by_quo)) {
    abort_marginplyr(c(
      "Can't supply {.arg .by} when {.arg .data} is grouped.",
      i = "Call {.fun dplyr::ungroup} first."
    ))
  }

  .data <- dplyr::ungroup(.data)

  list(data = .data, groups = input_groups)
}

# The fixed keys, wherever column names alone settle them: the grouping columns
# of a grouped input, an absent `.by`, or a selection that names columns.
# `NULL` says the selection carries a predicate, which no set of names can
# answer; that one is resolved against the typed snapshot instead, exactly as a
# Grouping dimension carrying a predicate is.
resolve_fixed_keys <- function(by_quo, group_vars, data_vars) {
  if (length(group_vars) > 0L) {
    # Grouping columns are names dplyr resolved, so they name columns of the
    # input and cannot rename one.
    return(group_vars)
  }
  # An empty `.by` selects no columns, which is dplyr's answer to it. Asked
  # before `is_name_only_selection()` below, which reads the expression rather
  # than binding it: the empty argument is a symbol whose name is `""`, and
  # `rlang::env_has()` raises for a zero-length variable name (#340).
  #
  # `normalize_grouping_input()` has already refused a grouped input carrying
  # any `.by`, which keeps dplyr's other reading of an empty one: supplied
  # there, no columns here.
  if (rlang::quo_is_null(by_quo) || is_empty_argument(by_quo)) {
    return(character())
  }
  if (!is_name_only_selection(by_quo, data_vars)) {
    return(NULL)
  }
  resolve_by_selection(by_quo, grouping_name_proxy(data_vars))
}

# A fixed key is a column of the input, exactly as a grouping dimension is, so
# the rule #103 established for `.grouping` holds here too. `.by` was resolved
# by selecting it and reading the names back with `get_col_names()`, which are
# the names the caller wrote, so `.by = c(area = region)` was accepted two ways:
# as a plan rejected for `area`, a column the caller never wrote, and — where
# `area` is another column — as a plan silently fixed on that other column,
# partitioning every verb's result by a column the selection did not name
# (#134). Resolving through the selection is what keeps both names in hand;
# `get_col_names()`'s other callers read back names dplyr assigned on purpose,
# so the check belongs on this resolution rather than in that helper.
resolve_by_selection <- function(by_quo, data_proxy) {
  resolve_column_selection(
    by_quo,
    data_proxy,
    on_rename = abort_by_rename
  )
}

prepare_grouping_plan <- function(.data,
                                  by_quo,
                                  grouping_quo,
                                  .duplicates,
                                  duplicates_choices,
                                  validate_grouping = NULL,
                                  validate_names = NULL,
                                  call = rlang::caller_call()) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))
  stopifnot(is.null(validate_grouping) || is.function(validate_grouping))
  stopifnot(is.null(validate_names) || is.function(validate_names))
  # Already matched against the calling verb's own vocabulary, which may be
  # narrower than the Margin one. Re-matching here against the wider list is
  # what made the nesting verbs' rejection message offer a value they refuse
  # (#110), so this asserts the precondition instead of re-deciding it. The
  # vocabulary travels on because the duplicate-set diagnostic names the
  # policies the caller could have asked for instead.
  stopifnot(
    rlang::is_string(.duplicates),
    .duplicates %in% duplicates_choices
  )

  with_margin_error_call(
    {
      # An empty `.grouping` is the argument left absent, which is what
      # `.grouping = ` already means: R matches a named formal's empty argument
      # to that formal's default. Only injection carries the missing marker
      # this far, and `rlang::eval_tidy()` evaluates it -- `object '' not
      # found`, naming nothing the caller wrote (#340).
      grouping_spec <- if (is_empty_argument(grouping_quo)) {
        NULL
      } else {
        rlang::eval_tidy(grouping_quo)
      }
      validate_grouping_spec_early(grouping_spec)
      if (!is.null(validate_grouping)) {
        validate_grouping(grouping_spec)
      }

      input <- normalize_grouping_input(.data, by_quo)
      data <- input$data
      backend <- grouping_backend(data)
      data_vars <- get_col_names(data, dplyr::everything())
      by <- resolve_fixed_keys(by_quo, input$groups, data_vars)
      if (!is.null(validate_names)) {
        validate_names(data_vars)
      }
      # Preflighted once and handed to every compilation pass -- both of them
      # where the plan is settled by names alone, and the one below otherwise.
      # This is the call's only reading of a nested argument, so a recognized
      # one is evaluated once whichever passes run (ADR-0008).
      preflight <- preflight_grouping_spec(grouping_spec, data_vars)
      if (preflight$name_only) {
        # Reject name-only plan errors before acquiring typed metadata. The
        # canonical plan is compiled from the typed snapshot below. Unresolved
        # fixed keys stand in as none: a `.by` carrying a predicate is not known
        # until that snapshot, and the two checks that read the keys — an
        # unknown fixed key and one overlapping a dimension — are made again
        # against the resolved keys below. Withholding the pass instead would
        # make a `.grouping` error determinable from names alone wait for a
        # backend read, which ADR-0005 forbids.
        # Discarded with the plan it compiles, so a warning it signals is one
        # the canonical pass signals again from the same names. Suppressing
        # them is what keeps this pass invisible but for the failure it exists
        # to raise; `one_of()` is where that is observable.
        #
        # A deprecation warning is the one kind suppressing is not enough for.
        # lifecycle signals one once per session, so a pass that signals it
        # and muffles it spends the only signal the caller had coming and the
        # canonical pass then reports nothing. The option withholds the signal
        # instead of hiding it; `.data$region` is where that is observable.
        #
        # `"error"` is the one verbosity it does not replace, because under it
        # the deprecation is a failure the names decide and quieting it would
        # put the read below in front of it (ADR 0005).
        rlang::with_options(
          suppressWarnings(compile_grouping_spec(
            grouping_spec,
            data_vars = data_vars,
            data_proxy = grouping_name_proxy(data_vars),
            .by = if (is.null(by)) character() else by,
            .duplicates = .duplicates,
            duplicates_choices = duplicates_choices,
            preflight = preflight
          )),
          lifecycle_verbosity = discarded_pass_verbosity()
        )
      }
      data_proxy <- grouping_selection_proxy(data, backend = backend)
      if (is.null(by)) {
        by <- resolve_by_selection(by_quo, data_proxy)
      }
      plan <- compile_grouping_spec(
        grouping_spec,
        data_vars = data_vars,
        data_proxy = data_proxy,
        .by = by,
        .duplicates = .duplicates,
        duplicates_choices = duplicates_choices,
        preflight = preflight
      )

      list(
        data = data,
        backend = backend,
        data_vars = data_vars,
        data_proxy = data_proxy,
        plan = plan
      )
    },
    call = call
  )
}

abort_empty_grouping_units <- function(kind) {
  abort_marginplyr("{.fun {kind}} requires at least one dimension.")
}

# The refusal of an argument the caller left empty. `position` is the
# argument's index in the specification's `args`, which is the position the
# caller wrote it at: every constructor takes `...` alone, so
# `rlang::enquos(...)` captures one element per written argument. The index is
# what the diagnostic points with because the argument has no spelling for
# ADR 0024's rule to quote (#261).
abort_empty_grouping_arg <- function(constructor, position) {
  abort_marginplyr(c(
    "Argument {position} of {.fun {constructor}} is empty.",
    i = "Remove the comma, or write the columns that position selects."
  ))
}

abort_empty_composite <- function() {
  abort_marginplyr(
    "An empty {.fun grouping_set} cannot be a composite dimension."
  )
}

allow_empty_grouping <- function(spec) {
  invisible(NULL)
}

validate_empty_grouping_sets <- function(spec) {
  if (length(spec$args) == 0L) {
    abort_marginplyr(c(
      "{.fun grouping_sets} requires at least one set.",
      i = "Use {.fun grouping_set} for the empty grouping set."
    ))
  }
  invisible(NULL)
}

validate_empty_grouping_units <- function(spec) {
  if (length(spec$args) == 0L) {
    abort_empty_grouping_units(spec$type)
  }
  invisible(NULL)
}

reject_nested_in_set <- function(parent, nested) {
  abort_marginplyr(
    "A {.fun grouping_set} can contain columns, not another grouping family."
  )
}

allow_nested_grouping <- function(parent, nested) {
  invisible(NULL)
}

validate_nested_grouping_units <- function(parent, nested) {
  # Classified rather than compared as read (#324, ADR 0008): what this reader
  # holds is a nested specification the caller wrote and not a plan. The
  # diagnostic below keeps the parent's field as read, which the same amendment
  # decides.
  if (!identical(grouping_kind_name(nested$type), "set")) {
    abort_marginplyr(paste0(
      "{.fun {parent$type}} only accepts columns or {.fun grouping_set} ",
      "composite dimensions."
    ))
  }
  if (length(nested$args) == 0L) {
    abort_empty_composite()
  }

  invisible(NULL)
}

# The operators tidyselect's walk combines a selection from. `/` is one:
# tidyselect reads it as a set difference in `eval_slash()`, which its
# `language.Rd` does not record. `(` is walked too but combines nothing, so it
# is not here.
selection_walk_operators <- function() {
  c("c", ":", "!", "-", "|", "&", "/")
}

# The spellings tidyselect refuses in place of one of those. It refuses them on
# sight, before anything under them is walked.
selection_refused_operators <- function() {
  c("&&", "||", "*", "^")
}

# Whether tidyselect settles this expression from the column names alone --
# its selection where it has one, its failure where it has none. `TRUE` is what
# lets a failure the names decide be raised before typed metadata is acquired
# (ADR 0005); a shape this cannot answer for is `FALSE`, which resolves it
# against the typed snapshot and is correct however that shape reads. Callers
# hold `env` as the expression's own environment and `data_vars` as the input's
# column names, which is the pair the symbol branch decides from.
is_name_only_expr <- function(expr, env, data_vars) {
  # `is_name_part()` rather than `rlang::is_symbol()`, for the reason its
  # header gives: a part read by subscript may be R's empty argument.
  if (is_name_part(expr)) {
    name <- as.character(expr)
    return(
      name %in% data_vars ||
        !rlang::env_has(
          env,
          name,
          inherit = TRUE
        )
    )
  }
  # The empty argument the branch above declined. tidyselect settles one from
  # the names too, which is what `TRUE` claims here;
  # `investigation/an-empty-argument-under-a-selection-walk.md` measures what
  # it settles it as, per operator. A selection written at a Grouping
  # specification argument reaches this reader as well, and `CONTEXT.md`'s
  # *Nested specification position* holds why that is not the empty argument
  # `preflight_grouping_spec()` refuses (#351).
  if (rlang::is_missing(expr)) {
    return(TRUE)
  }
  if (!is.language(expr)) {
    return(is.atomic(expr))
  }

  # tidyselect refuses a formula in a selection on sight, in `stop_formula()`,
  # whatever it wraps, so the refusal depends on no column type. It is read
  # from the shape rather than joined to `selection_refused_operators()`
  # because `is_nameable_call()` declines a call to `~`, so the name that set
  # is consulted by is never read (ADR 0019, #163).
  #
  # A quosure is a call to `~` too, and is excluded: tidyselect evaluates one
  # rather than refusing it, so what settles a quosure is the expression it
  # carries, which this reader does not open.
  if (rlang::is_call(expr, "~") && !rlang::is_quosure(expr)) {
    return(TRUE)
  }

  # A language object that is no call -- an expression vector, a pairlist --
  # answers `NULL` here, which the next line already turns into the `FALSE` a
  # guard would have returned.
  call_name <- static_call_name(expr)
  if (is.null(call_name)) {
    return(FALSE)
  }
  # The helpers that read names and nothing else. `where()` is the one that
  # reads column data, so it is absent and answers below.
  leaf_helpers <- c(
    "all_of", "any_of", "starts_with", "ends_with", "contains",
    "matches", "num_range", "everything", "last_col", "one_of"
  )
  if (call_name %in% leaf_helpers) {
    return(TRUE)
  }
  # A refused spelling fails on itself, so its operands are never reached and a
  # predicate under one settles nothing.
  if (call_name %in% selection_refused_operators()) {
    return(TRUE)
  }
  # The `.data` pronoun. Asked after the name, so a pair of parentheses around
  # the whole pronoun is already off it.
  if (call_name %in% c("$", "[[") && is_data_pronoun(expr)) {
    return(TRUE)
  }
  if (!call_name %in% c(selection_walk_operators(), "(")) {
    return(FALSE)
  }

  args <- static_call_args(expr)
  all(vapply(
    args,
    is_name_only_expr,
    logical(1),
    env = env,
    data_vars = data_vars
  ))
}

# Whether this call reads the `.data` pronoun, which the caller holds is named
# `$` or `[[`. Both halves are settled by the column names: `$` looks up the
# symbol beside it and `[[` a constant subscript, and tidyselect refuses every
# other spelling under the pronoun on sight -- a subscript that is not
# constant, one that is no string, a `$` whose right side is no symbol. So the
# subject is the pronoun rather than the two shapes of subscript, and #346's
# premise that `.data[[var]]` costs a read is measured in
# `investigation/what-the-data-pronoun-settles-in-a-selection.md`.
#
# The operand is compared with the `.data` symbol as written rather than
# through a redundant pair: tidyselect refuses `(.data)$region`, so reading
# through the pair here would answer for a spelling it rejects.
is_data_pronoun <- function(expr) {
  parts <- static_call_args(expr)
  length(parts) == 2L && identical(parts[[1L]], quote(.data))
}

# The lifecycle verbosity the discarded name-only pass runs under. The caller's
# own value is kept where it makes a deprecation an error, since that error is
# one the names decide.
discarded_pass_verbosity <- function() {
  verbosity <- getOption("lifecycle_verbosity")
  if (identical(verbosity, "error")) {
    return(verbosity)
  }
  "quiet"
}

is_name_only_selection <- function(arg, data_vars) {
  is_name_only_expr(
    rlang::quo_get_expr(arg),
    env = rlang::quo_get_env(arg),
    data_vars = data_vars
  )
}

grouping_name_proxy <- function(data_vars) {
  stats::setNames(as.list(seq_along(data_vars)), data_vars)
}

# The only site that reads a nested argument. It answers with the
# specification, whether names alone settle the plan, and one record per
# argument: `quo` is the caller's quosure and `nested` is this function's
# answer for it, a preflight of the same shape for a nested specification and
# `NULL` for a column selection. Expansion below reads that answer instead of
# taking one, which is what holds a recognized argument to one evaluation
# (ADR 0008).
#
# `spec` is `NULL` only for the absent `.grouping`, which
# `compile_grouping_spec_impl()` fills in. An empty `args` beside any other
# `spec` is a specification a caller wrote no arguments for.
preflight_grouping_spec <- function(grouping_spec, data_vars) {
  stopifnot(is.character(data_vars), !anyNA(data_vars))
  validate_grouping_spec_early(grouping_spec)
  if (is.null(grouping_spec)) {
    return(list(spec = NULL, args = list(), name_only = TRUE))
  }

  rule <- find_grouping_kind_rule(grouping_spec$type)
  stopifnot(!is.null(rule))
  name_only <- TRUE
  args <- vector("list", length(grouping_spec$args))
  for (i in seq_along(grouping_spec$args)) {
    arg <- grouping_spec$args[[i]]
    # The refusal is here rather than in the reader below because this is the
    # frame holding what the diagnostic names: the constructor and the
    # argument's position (#261). Which spellings count as empty, and why the
    # question is put to the quosure, are `is_empty_argument()`'s.
    if (is_empty_argument(arg)) {
      abort_empty_grouping_arg(rule$constructor, i)
    }
    nested <- grouping_arg_spec(arg, data_vars)
    if (is.null(nested)) {
      check_ambiguous_nested_name(arg, grouping_spec, rule, data_vars)
      name_only <- name_only && is_name_only_selection(arg, data_vars)
      args[[i]] <- list(quo = arg, nested = NULL)
      next
    }

    nested_preflight <- preflight_grouping_spec(nested, data_vars)
    rule$validate_nested(grouping_spec, nested_preflight$spec)
    name_only <- name_only && nested_preflight$name_only
    args[[i]] <- list(quo = arg, nested = nested_preflight)
  }
  list(spec = grouping_spec, args = args, name_only = name_only)
}

# The spelling a nested position reads an argument by: the caller's expression
# with its redundant parentheses removed (#178, #259). Every caller here runs
# downstream of `preflight_grouping_spec()`'s refusal of an empty argument
# (#261) and is handed none.
nested_arg_expr <- function(arg) {
  unparenthesized_value(rlang::quo_get_expr(arg))
}

# Refuses the one argument both readings claim: a bare name that is a column of
# the input and is bound to a specification of a kind this position admits.
# ADR 0026 holds that decision -- why such a name is refused rather than
# resolved either way, what makes the second reading available, and what
# reading the binding costs a caller. This is the site it names: the preflight,
# on the branch where the gate above answered "selection".
#
# The two conditions on the name below are the two that made the gate answer
# "selection" for a symbol, asked again here because the gate reports which
# reading it took and not why. A name nothing binds has no second reading, and
# a name the data does not hold is resolved by the gate itself.
#
# The name is read through the gate's own parenthesis reading, so a refusal
# `s` gets is a refusal `(s)` gets. Reading the caller's expression here
# instead would withhold it from the parenthesized spelling, which is the
# reading ADR 0026 refuses reached by a pair the gate has already dropped
# (#259).
#
# The kinds this position admits are asked before the binding is read: where a
# position admits none, the answer is already known, and reading a caller's
# binding to establish it would force a promise for nothing.
#
# The shape test comes before anything is bound to a local, and it is
# `is_name_part()` rather than a bare symbol test. Both halves are the rule
# `R/utils.R` states for every reader a walk asks first: binding R's empty
# argument raises `missingArgError` on the next read of it, and the empty
# argument is itself a symbol whose name is `""` (#168, #174). The caller
# refuses an empty argument before either reader is reached (#261), so the rule
# is followed here for what a caller could stop holding rather than for what one
# arrives with.
#
# A specification reading this site cannot complete is not a reading the
# position has, so the selection reading stands -- for a binding that raises
# when it is read, and for an object whose kind the shared readers answer
# `NULL` for (ADR 0008).
#
# `rule` is the parent's own, which the caller already holds because it
# validates nested arguments with it; deriving it again here would be the same
# lookup twice. It is derived from the parent's kind, so the pair handed to the
# memo below carries nothing that memo's key does not.
check_ambiguous_nested_name <- function(arg, parent, rule, data_vars) {
  if (!is_name_part(nested_arg_expr(arg))) {
    return(invisible(NULL))
  }
  name <- rlang::as_string(nested_arg_expr(arg))
  if (
    !name %in% data_vars ||
      !rlang::env_has(rlang::quo_get_env(arg), name, inherit = TRUE)
  ) {
    return(invisible(NULL))
  }
  admitted <- admitted_nested_kinds(parent, rule)
  if (length(admitted) == 0L) {
    return(invisible(NULL))
  }

  value <- tryCatch(rlang::eval_tidy(arg), error = function(cnd) NULL)
  if (!inherits(value, "margin_grouping_spec")) {
    return(invisible(NULL))
  }
  kind <- grouping_kind_name(grouping_spec_kind(value))
  if (is.null(kind) || !kind %in% admitted) {
    return(invisible(NULL))
  }
  abort_ambiguous_nested_name(name)
}

# Which kinds a nested position under `parent` admits. `rule` is `parent`'s own
# kind rule, which the caller holds. What this answer decides and how it is
# derived is ADR 0026's; the registry the kinds are enumerated from is
# ADR 0008's.
#
# Two things that derivation needs are the code below's to carry. `rule` is put
# to a stand-in of each kind, and the stand-in is built with one argument --
# `list(rlang::quo(NULL))`, not an empty list -- which is the arity the ADR
# requires. And the memo's key is `parent$type`, the field a kind is read off.
#
# The invariant that is this memo's own rather than the ADR's: the first
# computation for a kind is made with the real parent, so no parent asked about
# here is a specification that was not written.
#
# Memoized as `grouping_kind_rules()` is, and for the same reason: the table it
# derives from is fixed for the session.
admitted_nested_kinds <- local({
  admitted <- list()

  function(parent, rule) {
    parent_kind <- parent$type
    known <- admitted[[parent_kind]]
    if (!is.null(known)) {
      return(known)
    }
    kinds <- names(grouping_kind_rules())
    accepted <- vapply(
      kinds,
      function(kind) {
        tryCatch(
          {
            rule$validate_nested(
              parent,
              new_grouping_spec(kind, list(rlang::quo(NULL)))
            )
            TRUE
          },
          error = function(cnd) FALSE
        )
      },
      logical(1)
    )
    admitted[[parent_kind]] <<- unname(kinds[accepted])
    admitted[[parent_kind]]
  }
})

# The one way to turn a Grouping specification into a plan: preflight the
# specification, then compile what the preflight resolved. Every call site is
# here, `prepare_grouping_plan()`'s two compilation passes included, so a test
# that compiles through this function runs the sequence production runs. It was
# not always so — production reached `compile_grouping_spec_impl()` directly and
# performed the preflight and the `.duplicates` matching for itself, leaving
# this wrapper a second source of truth that only tests exercised (#119).
# `test-grouping-plan.R` holds the namespace to a single caller of the
# implementation so that cannot come back.
#
# `duplicates_choices` is the calling verb's own vocabulary, never the Margin
# one: a verb that narrows it hands its list down rather than being re-checked
# against the wider one (#110), and the hard-coded `margin_duplicates_choices`
# this replaces is what put the nesting verbs' narrowing beyond a test's reach.
# It has no default for the reason it has none in `normalize_margin_options()`
# — a default here is what let the nesting verbs be validated against a list
# their own formals exclude, and `compile_grouping_spec_impl()`'s membership
# assert would not catch it, since the wider list admits the narrower one's
# every value. `.duplicates` defaults to whichever list the caller stated,
# which keeps `match_margin_choice()`'s "an untouched formal stands for its
# first entry" idiom working for a narrowed vocabulary.
#
# `preflight` carries the reading each nested argument got, so a caller running
# more than one pass over one specification hands the same preflight to each
# and no pass reads a nested quosure of its own. A recognized nested argument
# is therefore evaluated once per call, whichever passes run (ADR-0008).
compile_grouping_spec <- function(
  .grouping,
  data_vars,
  data_proxy = NULL,
  .by = character(),
  .duplicates = duplicates_choices,
  duplicates_choices,
  preflight = preflight_grouping_spec(.grouping, data_vars)
) {
  .duplicates <- match_margin_choice(
    .duplicates,
    choices = duplicates_choices,
    arg_name = ".duplicates"
  )
  stopifnot(identical(preflight$spec, .grouping))
  compile_grouping_spec_impl(
    preflight,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = .by,
    .duplicates = .duplicates,
    duplicates_choices = duplicates_choices
  )
}

# Takes the preflight rather than the specification inside it, because
# expansion below needs the reading each nested argument got (ADR 0008).
compile_grouping_spec_impl <- function(preflight,
                                       data_vars,
                                       data_proxy,
                                       .by,
                                       .duplicates,
                                       duplicates_choices) {
  stopifnot(is.character(.by), !anyNA(.by))
  stopifnot(.duplicates %in% duplicates_choices)
  if (is.null(data_proxy)) {
    data_proxy <- grouping_name_proxy(data_vars)
  }

  # A backstop behind the resolutions that produce `.by`, not a Package
  # condition: each of them binds every value to `data_vars` already, so no
  # documented call reaches this (ADR 0015, #159). They are
  # `dplyr::group_vars()` of a grouped input, whose values are columns of that
  # input; a name-based selection resolved against
  # `grouping_name_proxy(data_vars)`, this same set; and a selection column
  # names alone cannot settle, resolved against the typed snapshot, whose
  # columns are that set on every backend. A `.by` source that could invent a
  # name is the thing that has to justify itself here. The `.by`/`.grouping`
  # overlap check below is the opposite case and stays a Package condition: an
  # ordinary call reaches it.
  unknown_by <- setdiff(.by, data_vars)
  if (length(unknown_by) > 0L) {
    stop(
      "Unknown `.by` column",
      if (length(unknown_by) == 1L) " " else "s ",
      paste0("`", unknown_by, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # The absent `.grouping` stands for the empty grouping set. Preflighted
  # rather than written out here, so that one function answers what a record
  # holds.
  if (is.null(preflight$spec)) {
    preflight <- preflight_grouping_spec(
      new_grouping_spec("set", list()),
      data_vars
    )
  }

  expanded <- unname(expand_grouping_family(preflight, data_proxy))
  dimensions <- unique(unlist(expanded, use.names = FALSE))

  overlap <- intersect(.by, dimensions)
  if (length(overlap) > 0L) {
    # The columns arrive alone in an `i` bullet, per ADR 0023's condition 2:
    # how many of them there are is the caller's decision.
    abort_marginplyr(c(
      "Columns cannot appear in both {.arg .by} and {.arg .grouping}:",
      i = "{.var {overlap}}."
    ))
  }

  normalized <- lapply(
    expanded,
    function(set) c(.by, dimensions[dimensions %in% set])
  )
  keys <- vapply(
    normalized,
    function(set) paste(as.integer(dimensions %in% set), collapse = ""),
    character(1)
  )
  duplicate_keys <- duplicated(keys) | duplicated(keys, fromLast = TRUE)

  if (any(duplicate_keys) && identical(.duplicates, "error")) {
    groups <- split(which(duplicate_keys), keys[duplicate_keys])
    # The policies this caller could have asked for instead, which is every
    # value of its own vocabulary but the one that raised this. Reading them
    # from the vocabulary is what keeps the nesting verbs, whose `.duplicates`
    # excludes `"keep"`, from being offered it here (#110).
    alternatives <- setdiff(duplicates_choices, .duplicates)
    # Joined here rather than interpolated as a vector, because a group is
    # itself a comma-joined list of positions, so cli's defaults would
    # serialise the groups with an `and` inside one -- the reason `R/share.R`
    # joins a class vector on its own slash. Read only from the cli template
    # below, which codetools cannot see.
    # nolint start: object_usage_linter.
    positions <- paste(
      vapply(groups, paste, collapse = ", ", character(1)),
      collapse = "; "
    )
    # nolint end
    offered <- paste0("\"", alternatives, "\"")
    offered[[1L]] <- paste0(".duplicates = ", offered[[1L]])
    abort_marginplyr(c(
      paste0(
        "Duplicate grouping sets were produced at ",
        "{cli::qty(length(groups))}{?positions/position groups}:"
      ),
      i = "{positions}.",
      i = "Use {.or {.code {offered}}}."
    ))
  }

  if (identical(.duplicates, "drop")) {
    keep <- !duplicated(keys)
    normalized <- normalized[keep]
    keys <- keys[keep]
  }

  masks <- matrix(
    0L,
    nrow = length(normalized),
    ncol = length(dimensions),
    dimnames = list(NULL, dimensions)
  )
  if (length(dimensions) > 0L) {
    for (i in seq_along(normalized)) {
      masks[i, ] <- as.integer(!dimensions %in% normalized[[i]])
    }
  }

  structure(
    list(
      # The name the field spells, not the field as read (#317, ADR 0008). The
      # preflight above ran the guard, so the field is one name and this is
      # never `NULL`.
      kind = grouping_kind_name(preflight$spec$type),
      by = unique(.by),
      dimensions = dimensions,
      sets = normalized,
      set_ids = seq_along(normalized),
      grouping_masks = masks,
      duplicates = .duplicates
    ),
    class = "margin_grouping_plan"
  )
}

# The families a specification expands to. Every function below it takes the
# preflight the caller holds, and reads each argument's reading off the record
# beside that argument's quosure rather than taking one (ADR 0008). A column
# selection is resolved here and not there, so it is resolved once per pass,
# against the proxy that pass was given.
expand_grouping_family <- function(preflight, data_proxy) {
  rule <- find_grouping_kind_rule(preflight$spec$type)
  if (is.null(rule)) {
    stop("Unknown grouping specification kind.", call. = FALSE)
  }
  rule$expand(preflight, data_proxy)
}

expand_single_grouping_set <- function(preflight, data_proxy) {
  list(resolve_grouping_set(preflight, data_proxy))
}

resolve_grouping_set <- function(preflight, data_proxy) {
  if (length(preflight$args) == 0L) {
    return(character())
  }

  cols <- unlist(
    lapply(
      preflight$args,
      function(arg) {
        # A `set` admits no nested kind, so the preflight refused one before
        # this ran (ADR 0015, #159).
        stopifnot(is.null(arg$nested))
        resolve_grouping_selection(arg$quo, data_proxy)
      }
    ),
    use.names = FALSE
  )
  unique(cols)
}

expand_grouping_sets <- function(preflight, data_proxy) {
  unlist(
    lapply(
      preflight$args,
      function(arg) {
        if (is.null(arg$nested)) {
          return(list(resolve_grouping_selection(arg$quo, data_proxy)))
        }
        expand_grouping_family(arg$nested, data_proxy)
      }
    ),
    recursive = FALSE
  )
}

resolve_grouping_units <- function(preflight, data_proxy) {
  units <- unlist(
    lapply(
      preflight$args,
      function(arg) {
        if (is.null(arg$nested)) {
          cols <- resolve_grouping_selection(arg$quo, data_proxy)
          return(lapply(cols, function(col) col))
        }
        # Classified as the guard that admitted this argument classifies
        # (#324): a comparison on the field as read fires here, untyped, for
        # exactly the population that guard now admits.
        stopifnot(identical(grouping_kind_name(arg$nested$spec$type), "set"))
        cols <- resolve_grouping_set(arg$nested, data_proxy)
        if (length(cols) == 0L) {
          abort_empty_composite()
        }
        list(cols)
      }
    ),
    recursive = FALSE
  )

  if (length(units) == 0L) {
    abort_empty_grouping_units(preflight$spec$type)
  }
  units
}

expand_rollup <- function(preflight, data_proxy) {
  units <- resolve_grouping_units(preflight, data_proxy)
  lapply(
    rev(seq.int(0L, length(units))),
    function(n) {
      if (n == 0L) character() else unique(unlist(units[seq_len(n)]))
    }
  )
}

expand_cube <- function(preflight, data_proxy) {
  units <- resolve_grouping_units(preflight, data_proxy)
  n <- length(units)
  indices <- unlist(
    lapply(
      rev(seq.int(0L, n)),
      function(size) utils::combn(seq_len(n), size, simplify = FALSE)
    ),
    recursive = FALSE
  )

  lapply(
    indices,
    function(index) {
      if (length(index) == 0L) character() else unique(unlist(units[index]))
    }
  )
}

expand_grouping_product <- function(preflight, data_proxy) {
  product <- list(character())
  if (length(preflight$args) == 0L) {
    return(product)
  }

  for (arg in preflight$args) {
    family <- if (is.null(arg$nested)) {
      list(resolve_grouping_selection(arg$quo, data_proxy))
    } else {
      expand_grouping_family(arg$nested, data_proxy)
    }

    product <- unlist(
      lapply(
        product,
        function(left) lapply(family, function(right) unique(c(left, right)))
      ),
      recursive = FALSE
    )
  }
  product
}

grouping_kind_rules <- local({
  rules <- NULL

  function() {
    if (is.null(rules)) {
      rules <<- list(
        set = list(
          constructor = "grouping_set",
          validate_empty = allow_empty_grouping,
          validate_nested = reject_nested_in_set,
          expand = expand_single_grouping_set
        ),
        sets = list(
          constructor = "grouping_sets",
          validate_empty = validate_empty_grouping_sets,
          validate_nested = allow_nested_grouping,
          expand = expand_grouping_sets
        ),
        rollup = list(
          constructor = "rollup",
          validate_empty = validate_empty_grouping_units,
          validate_nested = validate_nested_grouping_units,
          expand = expand_rollup
        ),
        cube = list(
          constructor = "cube",
          validate_empty = validate_empty_grouping_units,
          validate_nested = validate_nested_grouping_units,
          expand = expand_cube
        ),
        product = list(
          constructor = "grouping_spec",
          validate_empty = allow_empty_grouping,
          validate_nested = allow_nested_grouping,
          expand = expand_grouping_product
        )
      )
    }
    rules
  }
})

# The name a kind read off an object is, or `NULL` where it is none: one
# character element carrying no attribute, and not the missing one. Every caller
# holds a value nothing has validated, and every reader of one shares this, as
# they share the read in `grouping_spec_kind()`. What it decides, why it answers
# with the name rather than with whether there is one, and why the answer
# carries no attribute are ADR 0008's, in its amendments for a kind classified
# with its class off and for a kind read as a name and compared as a value.
grouping_kind_name <- function(kind) {
  # `is.character()` and `unclass()` are primitives that dispatch on neither S3
  # nor S4, so neither reaches a method the value carries: the type test is R's
  # own, and stripping is what puts the two questions below to a character
  # vector with no class rather than to whatever the class defines for them
  # (#289). Every remaining attribute comes off with it, because `identical()`
  # reads them all and that is what a consumer compares this answer with (#317).
  # `attributes<-` is a primitive too, and by then there is no class left for
  # anything to dispatch on.
  if (!is.character(kind)) {
    return(NULL)
  }
  name <- unclass(kind)
  attributes(name) <- NULL
  if (length(name) != 1L || is.na(name)) {
    return(NULL)
  }
  name
}

# `NULL` both for a kind that is no name and for a name the registry does not
# know, so a caller that has to tell those apart asks `grouping_kind_name()`
# instead of reading this answer.
find_grouping_kind_rule <- function(kind) {
  name <- grouping_kind_name(kind)
  if (is.null(name)) {
    return(NULL)
  }
  grouping_kind_rules()[[name]]
}

grouping_constructor_names <- function() {
  unname(vapply(
    grouping_kind_rules(),
    function(rule) rule$constructor,
    character(1)
  ))
}

# The specification a nested argument is, or `NULL` where it is a column
# selection. Every caller holds that `arg` carries an expression rather than R's
# empty argument, wrapped in parentheses or not: `preflight_grouping_spec()`
# refuses one (#261), and the restart below cannot make one, because
# `is_parenthesized()` answers `FALSE` for the pair that would hold it. That is
# what lets the local hold what the quosure carries without the missing marker
# reaching it.
grouping_arg_spec <- function(arg, data_vars) {
  expr <- rlang::quo_get_expr(arg)
  # A redundantly parenthesized argument is read as the argument it wraps, as
  # it is wherever a reading is taken from `R/utils.R`'s readers (#178,
  # ADR 0019). The tests below take none: they ask `is.symbol()` and
  # `is.language()` about the caller's own expression, so this site restarts on
  # what the pair wraps rather than inheriting the reading (#259).
  #
  # By restart rather than by rebinding `expr`, which is the rule `R/utils.R`
  # states and `static_spelling_reference_name()` follows: what a pair wraps
  # may be R's empty argument, and binding the missing marker raises
  # `missingArgError` on the next read of it (#168, #174). `is_parenthesized()`
  # answers `FALSE` for a constructed pair holding one, so the restart cannot
  # repeat.
  if (is_parenthesized(expr)) {
    return(grouping_arg_spec(
      rlang::new_quosure(nested_arg_expr(arg), rlang::quo_get_env(arg)),
      data_vars
    ))
  }
  if (
    is.symbol(expr) &&
      is_name_only_expr(
        expr,
        env = rlang::quo_get_env(arg),
        data_vars = data_vars
      )
  ) {
    return(NULL)
  }

  # The spelling gates evaluation and nothing else: a nested argument is
  # ambiguous between a tidyselect selection and a nested specification, and
  # evaluating every nested call would run `starts_with("re")` outside a
  # selection context. What runs once the gate opens is whatever the name is
  # bound to, so a constructor is not a Contextual helper even though it is
  # read statically (ADR 0019).
  #
  # It is not the only thing that evaluates a nested argument any more.
  # `check_ambiguous_nested_name()` reads only names this gate declined, and
  # only those it declined because the data holds them, in a position admitting
  # a nested kind (ADR 0026). So no name is read twice, and a reader auditing
  # when a caller's nested quosure is forced has that read to count as well.
  is_constructor_call <-
    !is.null(static_spelling_name(expr, "grouping_constructor"))

  should_evaluate <-
    is_constructor_call ||
    is.symbol(expr) ||
    !is.language(expr)
  if (!should_evaluate) {
    return(NULL)
  }

  value <- rlang::eval_tidy(arg)
  if (inherits(value, "margin_grouping_spec")) {
    return(value)
  }
  NULL
}

# The gate above reads spellings, so a specification a caller's own function
# returns arrives here, where a column selection is expected. tidyselect then
# reports a `margin_grouping_spec` as the wrong kind of object for a position
# where a specification is exactly what belongs, which is a diagnostic that
# contradicts itself (#190). The contract the position actually has -- a
# constructor spelling or a name -- is what the caller needs told, together
# with the binding that makes their call work.
#
# The refused value is read from the condition rather than by evaluating the
# quosure again to identify it: tidyselect has already evaluated it once, and
# ADR 0008 fixes how often a caller's quosure runs. This is marginplyr's own
# report about a position of its own, so it stays parentless, where every
# other failure is an External condition and propagates unchanged (ADR 0015).
#
# A specification stored as a function is refused from the frames instead,
# because tidyselect refuses no function: it calls one, as the predicate form
# of a selection, so the caller was left with base R's untyped complaint about
# the call tidyselect made and no condition named the object (#265). The two
# readings answer one question and reach one refusal. Which of them is
# available is decided by how the object is stored and not by what it is, which
# is why neither is written as the other's fallback.
#
# The frames are read as the error unwinds and the condition after it, because
# a predicate is applied inside `resolve_column_selection()` and the frames
# that applied it are gone once it returns. What the handler keeps is the
# condition it read them on, not that it read them: a calling handler runs for
# every error signalled under the selection, one tidyselect goes on to recover
# from included, and a reading kept as a flag would answer for whatever failed
# after it instead.
resolve_grouping_selection <- function(arg, data_proxy) {
  selection_frame <- sys.nframe()
  predicate <- NULL
  tryCatch(
    withCallingHandlers(
      resolve_column_selection(
        arg,
        data_proxy,
        on_rename = abort_grouping_rename
      ),
      error = function(cnd) {
        if (is_grouping_spec_predicate(selection_frame, nested_arg_expr(arg))) {
          predicate <<- cnd
        }
      }
    ),
    error = function(cnd) {
      label <- rlang::as_label(nested_arg_expr(arg))
      raised <- !is.null(predicate) &&
        any(vapply(condition_chain(cnd), identical, logical(1), predicate))
      if (!raised && !is_grouping_spec_subscript(cnd, label)) {
        stop(cnd)
      }
      abort_nested_grouping_spec(label)
    }
  )
}

# Whether the failure now unwinding is a specification tidyselect took for a
# predicate, in a position that can speak for it.
#
# The label comparison `is_grouping_spec_subscript()` makes has no counterpart
# here, since a condition tidyselect raised about a predicate names no
# subscript. The same distinction is drawn twice instead, because a part of an
# argument reaches this in two ways and neither reading answers the other. The
# caller's expression is the first: tidyselect walks the operators below in
# parts and evaluates everything else whole, so a part is what it applied under
# one of them. Which frame holds the object is the second, and
# `selection_frames()` is where that half is. The position does not speak for a
# part of an argument, for the reason recorded above
# `is_grouping_spec_subscript()`.
#
# The operators are both sets above: a specification reaches this under a
# refused spelling as readily as under one that is walked. `(` is not among
# them because `nested_arg_expr()` has already dropped a redundant pair, and a
# quosure is not, because `is_nameable_call()` declines the call to `~` that
# one is.
is_grouping_spec_predicate <- function(selection_frame, expr) {
  operators <- c(selection_walk_operators(), selection_refused_operators())
  if (!is_nameable_call(expr)) {
    return(FALSE)
  }
  name <- static_call_name(expr)
  if (!is.null(name) && name %in% operators) {
    return(FALSE)
  }
  any(vapply(
    selection_frames(selection_frame),
    holds_grouping_spec_function,
    logical(1)
  ))
}

# The frames tidyselect's own walk opened under this selection, outermost
# first.
#
# A frame is tidyselect's by the namespace its function closes over, never by
# name: reading a private name is what `:::` is, and a namespace that stopped
# opening these frames answers with none rather than raising, which leaves a
# caller the untyped diagnostic they had before this refusal existed.
#
# The walk is what the first of them opens, `resolve_column_selection()` having
# called `tidyselect::eval_select()`, and the first exported function reached
# after it is where the walk stops being what is running: a selection helper
# the caller wrote, holding what the caller handed it. `starts_with(spec)`,
# `all_of(spec)`, and `last_col(spec)` each fail a type check under one of
# those, with the specification bound to a formal of the helper or of an
# internal function it delegates to, having applied nothing. The scan therefore
# stops there rather than skipping the helper's own frame, which would still
# reach what it delegates to. Refusing under one of them would answer for a
# part of an argument, which is the distinction recorded above
# `is_grouping_spec_subscript()`.
selection_frames <- function(selection_frame) {
  namespace <- asNamespace("tidyselect")
  exported <- mget(
    getNamespaceExports("tidyselect"),
    envir = namespace,
    mode = "function",
    ifnotfound = list(NULL)
  )
  frames <- list()
  entered <- FALSE
  for (index in seq_len(sys.nframe())) {
    if (index <= selection_frame) {
      next
    }
    fn <- sys.function(index)
    if (!identical(environment(fn), namespace)) {
      next
    }
    if (!entered) {
      entered <- TRUE
      next
    }
    if (any(vapply(exported, identical, logical(1), fn))) {
      break
    }
    frames <- c(frames, list(sys.frame(index)))
  }
  frames
}

# Whether a frame holds a specification stored as a function.
#
# This is where the object is read, because tidyselect holds the value it is
# applying in a binding of the frame that applies it, and that binding outlives
# the call: a predicate whose output tidyselect rejects has already returned.
# What was measured of that frame, and of the shapes a specification stored as
# a function reaches it in, is in
# `investigation/reading-a-specification-tidyselect-called.md`.
#
# A specification stored as anything else is not looked for here. That one
# tidyselect refuses rather than calls, so it arrives in a condition that names
# both the object and the subscript refused, and reading it from a frame
# instead would answer for a sub-selection the position does not speak for.
#
# Nothing is forced to answer. A binding tidyselect has not forced is skipped,
# because forcing one can evaluate the caller's argument a second time
# (ADR 0008); an active binding is skipped because reading one runs a function;
# and `...` is skipped because it is not a binding a value can be read out of.
# What is left can still raise -- a formal a caller of tidyselect left missing
# is bound and unreadable -- and a value that cannot be read is not one
# carrying the class.
holds_grouping_spec_function <- function(frame) {
  names <- setdiff(ls(frame, all.names = TRUE), "...")
  if (length(names) == 0L) {
    return(FALSE)
  }
  readable <- !rlang::env_binding_are_lazy(frame, names) &
    !rlang::env_binding_are_active(frame, names)
  for (name in names[readable]) {
    value <- tryCatch(
      get(name, envir = frame, inherits = FALSE),
      error = function(cnd) NULL
    )
    if (is.function(value) && inherits(value, "margin_grouping_spec")) {
      return(TRUE)
    }
  }
  FALSE
}

# The refused subscript travels in the condition's `i` field, at whatever depth
# of the chain `condition_chain()` describes.
#
# What the position can speak for is its own argument, so the refusal has to
# name that argument and not a part of it: tidyselect reports the sub-selection
# it refused, and in `c(spec, region)` a specification is genuinely the wrong
# kind of object where it sits. Saying otherwise there would claim the whole
# argument is a specification, which is false, and send a caller to a binding
# they have already made. Comparing the labels is what separates the two, and
# both are written by `rlang::as_label()` from the same expression when the
# argument as a whole is what was refused.
#
# Both sides of that comparison are unparenthesized, which is what makes them
# comparable. tidyselect descends into a `(` call before it refuses anything,
# so `(f(region))` arrives here reported as `f(region)`, and a label written
# from the caller's expression matches neither that nor the argument it wraps
# (#259). `nested_arg_expr()` is what the label is written from for that
# reason.
#
# The refusal then names that same label, so `(f(region))` is refused in the
# words `f(region)` is. ADR 0019's third amendment holds that against
# ADR 0024's spelling rule.
#
# The sub-selection case is unaffected: `c((s), region)` labels the whole
# argument, which is not the `s` tidyselect refused inside it.
#
# Where a column shares the name, tidyselect refuses nothing and none of this
# runs: `c(s, region)` selects the column `s`, which is the one reading a
# selection has for a name the data holds. The ambiguity refusal does not reach
# inside a selection either, and deliberately -- the caller wrote a selection,
# so the specification reading was never available to that argument.
is_grouping_spec_subscript <- function(cnd, label) {
  any(vapply(
    condition_chain(cnd),
    function(condition) {
      inherits(condition$i, "margin_grouping_spec") &&
        identical(condition$subscript_arg, label)
    },
    logical(1)
  ))
}

# The name as a caller writes it in code, which is the name itself wherever R
# parses it as one and a backtick-quoted spelling everywhere else.
# `expr_deparse()` answers which of the two it is -- a syntactic name should
# not pay backticks it does not need -- and `encodeString()` writes the quoted
# form, because `expr_deparse()`'s own is `` `a`b` `` for a name holding a
# backtick, which does not parse.
quoted_name_spelling <- function(name) {
  deparsed <- rlang::expr_deparse(rlang::sym(name), width = Inf)
  if (identical(deparsed, name)) {
    return(name)
  }
  encodeString(name, quote = "`")
}

# Both bullets are spellings the caller can run rather than descriptions of
# one, so each is built as a value and interpolated as one: the quoting a
# non-syntactic name needs is different inside `all_of()` and after `!!`, and
# neither the name nor the template carries either.
abort_ambiguous_nested_name <- function(name) {
  # Both are read only from the cli template below, which codetools cannot
  # see.
  # nolint start: object_usage_linter.
  column <- paste0("all_of(", encodeString(name, quote = "\""), ")")
  specification <- paste0("!!", quoted_name_spelling(name))
  # nolint end
  abort_marginplyr(c(
    paste0(
      "{.var {name}} is both a column of the input and a name bound to a ",
      "grouping specification, so a nested position cannot tell which one ",
      "you mean."
    ),
    i = "For the column, write {.code {column}}.",
    i = "For the specification, write {.code {specification}}."
  ))
}

abort_nested_grouping_spec <- function(label) {
  abort_marginplyr(c(
    paste0(
      "{.code {label}} is a grouping specification, but a nested position ",
      "recognizes one only when it is a call to ",
      "{.or {.fun {grouping_constructor_names()}}}, or a name bound to a ",
      "specification."
    ),
    i = "Anything else is read as a column selection.",
    i = "Assign the specification to a name first, then use that name here."
  ))
}

# tidyselect reports a selection under the names the caller gave it, so
# `all_of(c(area = "region"))` selects `region` and calls it `area`. A column
# a Margin verb selects — a grouping dimension or a fixed `.by` key alike — is
# a column of the input, so a renamed selection would put a name the data does
# not have into the plan. Renaming is refused here, the one place that sees
# both names, rather than with `eval_select(allow_rename = FALSE)`, whose
# diagnostic says only that renaming is disallowed and never names the pair the
# caller has to fix. A name that repeats its own column renames nothing and is
# left alone.
resolve_column_selection <- function(arg, data_proxy, on_rename) {
  selected <- tidyselect::eval_select(
    arg,
    data = data_proxy,
    strict = TRUE
  )
  selected_names <- names(selected)
  # `tidyselect_data_proxy()` is how tidyselect itself sees the columns the
  # positions in `selected` index, so it reads a lazy table's columns rather
  # than the fields of the object holding them, which `names()` would return.
  source_names <- names(tidyselect::tidyselect_data_proxy(data_proxy))[selected]
  renamed <- selected_names != source_names
  if (any(renamed)) {
    on_rename(selected_names[renamed], source_names[renamed])
  }
  selected_names
}

# One caller mistake, one diagnostic: `.grouping` and `.by` refuse a renaming
# selection for the same reason, and only the noun and the rule it states
# differ. Two refusals rather than one, with the resolution above handed
# whichever speaks for its selection.
#
# Written out rather than kept as one template with the noun interpolated,
# because the noun is what pluralizes and `{?s}` reads the quantity beside it,
# so a shared template would have to branch on it. ADR 0023's `{?}` rule is
# what refuses that branch, and its third amendment names this pair beside
# `R/share.R`'s helper-position refusal as what an R branch may choose instead.
#
# The pairs arrive alone in an `i` bullet, per ADR 0023's condition 2: how many
# of them there are is the caller's decision.
abort_grouping_rename <- function(selected_names, source_names) {
  abort_marginplyr(c(
    "Can't rename {cli::qty(length(selected_names))}grouping dimension{?s}:",
    i = "{.code {selection_rename_pairs(selected_names, source_names)}}.",
    i = "Grouping dimensions must name existing columns."
  ))
}

abort_by_rename <- function(selected_names, source_names) {
  abort_marginplyr(c(
    "Can't rename {cli::qty(length(selected_names))}{.arg .by} column{?s}:",
    i = "{.code {selection_rename_pairs(selected_names, source_names)}}.",
    i = "Fixed {.arg .by} keys must name existing columns."
  ))
}

# The renaming a selection asked for, in the spelling the caller would write to
# fix it. Both halves are caller text, so this is an interpolated value and
# carries no markup of its own; the templates above give it `{.code}`.
selection_rename_pairs <- function(selected_names, source_names) {
  paste0(selected_names, " = ", source_names)
}
