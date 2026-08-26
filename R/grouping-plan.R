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

  kind <- grouping_spec$type
  args <- grouping_spec$args
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      !is.list(args)
  ) {
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
  if (rlang::quo_is_null(by_quo)) {
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
      grouping_spec <- rlang::eval_tidy(grouping_quo)
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
      # Preflighted once and handed to both compilation passes. Preflight is
      # not a free re-run: `grouping_arg_spec()` evaluates a symbol or a nested
      # constructor argument, so preflighting per pass would evaluate a
      # caller's quosure twice, and ADR-0008 holds the number and timing of
      # evaluations fixed.
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
        compile_grouping_spec(
          grouping_spec,
          data_vars = data_vars,
          data_proxy = grouping_name_proxy(data_vars),
          .by = if (is.null(by)) character() else by,
          .duplicates = .duplicates,
          duplicates_choices = duplicates_choices,
          preflight = preflight
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
  if (!identical(nested$type, "set")) {
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

is_name_only_expr <- function(expr, env, data_vars) {
  if (is.symbol(expr)) {
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
  if (!is.language(expr)) {
    return(is.atomic(expr))
  }

  # A language object that is no call -- an expression vector, a pairlist --
  # answers `NULL` here, which the next line already turns into the `FALSE` a
  # guard would have returned.
  call_name <- static_call_name(expr)
  if (is.null(call_name)) {
    return(FALSE)
  }
  leaf_helpers <- c(
    "all_of", "any_of", "starts_with", "ends_with", "contains",
    "matches", "num_range", "everything", "last_col"
  )
  if (call_name %in% leaf_helpers) {
    return(TRUE)
  }
  if (!call_name %in% c("c", ":", "!", "-", "|", "&", "(")) {
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

preflight_grouping_spec <- function(grouping_spec, data_vars) {
  stopifnot(is.character(data_vars), !anyNA(data_vars))
  validate_grouping_spec_early(grouping_spec)
  if (is.null(grouping_spec)) {
    return(list(spec = NULL, name_only = TRUE))
  }

  rule <- find_grouping_kind_rule(grouping_spec$type)
  stopifnot(!is.null(rule))
  name_only <- TRUE
  for (arg in grouping_spec$args) {
    nested <- grouping_arg_spec(arg, data_vars)
    if (is.null(nested)) {
      check_ambiguous_nested_name(arg, grouping_spec, rule, data_vars)
      name_only <- name_only && is_name_only_selection(arg, data_vars)
      next
    }

    nested_preflight <- preflight_grouping_spec(nested, data_vars)
    rule$validate_nested(grouping_spec, nested_preflight$spec)
    name_only <- name_only && nested_preflight$name_only
  }
  list(spec = grouping_spec, name_only = name_only)
}

# The one argument that has two readings available: a bare name that is a
# column of the input and is bound to a specification of a kind this position
# admits. The gate above answers it as a selection, because a selection is what
# tidyselect's own precedence makes of a name the data holds -- so the caller
# who meant the binding they wrote gets a well-formed plan of the other reading,
# and nothing says so (#255). Refusing is what keeps a nested position's reading
# decided by the spelling rather than by the input, without deciding it the
# other way and leaving the same silence behind (ADR 0026).
#
# Available, not disagreeing: whether the two readings would produce different
# grouping sets cannot be known without resolving the specification against this
# input, and deciding by the input is the defect. So a name whose two readings
# happen to agree is refused with the rest.
#
# Reading the binding is what the answer costs, and there is no cheaper
# question: which kind a name is bound to cannot be known without reading it.
# So a colliding name in a position that admits any kind is read once, where it
# was read not at all -- once for each argument it is written as, and whether or
# not its reading then changes, since what the read decides is whether it
# changes. No argument outside a collision is read any more often than it was,
# and a position that admits no kind reads nothing.
#
# It sits in the preflight rather than in the gate because the preflight runs
# once for a whole operation and is handed to the compilation passes, where
# the gate runs again on each.
#
# More than the count moves. Where the refusal fires, the arguments written
# after it are not read at all, and this refusal is reported in place of
# whatever the call would have been rejected for further along -- which is
# every diagnostic reachable past this point and not one of them: a missing
# column, a duplicate grouping set, a `.by` overlap, each nesting-grammar
# rejection, and #190's own refusal among them. Where that displaced rejection
# was an External condition, a caller who was receiving tidyselect's class and
# tidyselect's blamed call now receives a `marginplyr_error` blamed on the
# Margin verb.
#
# ADR 0008's compatibility list holds all of that, and not on the same terms.
# The number and timing of evaluations are held "without a separately accepted
# decision", which is the decision ADR 0026 makes. Condition classes, complete
# messages, public call contexts, and detection order are one bullet carrying
# no such clause, and this moves every item in it, so ADR 0026 amends that
# bullet whole rather than naming a part of it.
#
# What that costs a caller is the forcing itself, not only the count, and it
# reaches every colliding binding rather than the ones that turn out to be
# specifications -- the read is what establishes which those are. So a
# wrapper's own lazy argument is forced here, whatever it holds, for a call
# whose answer may not depend on it: a warning or a message it raises reaches
# the caller, and R's own `restarting interrupted promise evaluation` does
# where such a binding raises and the name is written more than once. ADR 0026
# accepts that rather than hiding it, since the alternative to reading the
# binding is deciding by the input, which is the defect.
#
# The two conditions on the name below are the two that made the gate answer
# "selection" for a symbol, asked again here because the gate reports which
# reading it took and not why. A name nothing binds has no second reading, and
# a name the data does not hold is resolved by the gate itself.
#
# The kinds this position admits are asked before the binding is read, so a
# position that admits none -- `grouping_set()`, which holds columns -- reads
# nothing. Nothing there could make the name ambiguous, and reading a caller's
# binding to establish that would force a promise for an answer that was
# already known.
#
# The shape test comes before anything is bound to a local, and it is
# `is_name_part()` rather than a bare symbol test. Both halves are the rule
# `R/utils.R` states for every reader a walk asks first: binding R's empty
# argument raises `missingArgError` on the next read of it, and the empty
# argument is itself a symbol whose name is `""` (#168, #174). No empty
# argument reaches this function today, because `grouping_arg_spec()` binds
# one a line earlier and raises there, which is #261; following the rule here
# is what keeps this from becoming a second site of it once that one is
# fixed.
#
# A binding that raises when it is read is not a specification, so the
# selection reading stands where it raises, and so does an object carrying the
# class with no kind to read -- an atomic vector given the class answers `$`
# with a condition of its own, and a list without the field answers `NULL`,
# neither of which is a kind this position could admit. Both catches are narrow
# in what they decide and not in what they swallow: everything they hide is a
# failure to produce a specification of an admitted kind, which is a
# specification reading that is not available. That is where the ADR-0015 line
# falls too, because nothing here is deciding what such an object is -- a list
# carrying the class and no kind, bound to a name nothing shadows, still
# reaches `validate_grouping_spec_early()` and is refused in its own words.
# Where that guard reads a field too early to reach its own refusal is #262,
# which reproduces at top level and so is not this position's to answer.
#
# `rule` is the parent's own, which the caller already holds because it
# validates nested arguments with it; deriving it again here would be the same
# lookup twice. That it is the parent's own is also what makes the memo below
# sound, since the key is the parent's kind and nothing reads the pair apart.
check_ambiguous_nested_name <- function(arg, parent, rule, data_vars) {
  if (!is_name_part(rlang::quo_get_expr(arg))) {
    return(invisible(NULL))
  }
  name <- rlang::as_string(rlang::quo_get_expr(arg))
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
  kind <- tryCatch(value$type, error = function(cnd) NULL)
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      !kind %in% admitted
  ) {
    return(invisible(NULL))
  }
  abort_ambiguous_nested_name(name)
}

# Which nested kinds this position admits: `grouping_set()` none, `rollup()`
# and `cube()` a `grouping_set()`, `grouping_sets()` and `grouping_spec()` all
# of them. It is the other half of "both readings are available", and the half
# the input may not answer.
#
# The kind is where the line sits, and nothing further. A specification of an
# admitted kind that is invalid on its own terms -- one with no arguments, one
# holding a family its own constructor forbids -- makes the name ambiguous just
# the same, because what is wrong with it is a property of what the caller
# wrote and `!!` is what reports it. What the refusal's advice promises is the
# reading, not that the reading succeeds: a caller who meant that specification
# receives the diagnostic about it, which is what the silent column selection
# withheld. Drawing the line further in would put arity and validity on the
# same footing as the input, and one of those is not like the others.
#
# Derived by asking each kind's own parent rule rather than by a second list of
# which kinds nest inside which (ADR 0008), so a sixth kind is admitted or
# refused here by the rule that decides it everywhere else. The rule answers
# about an instance, so it is asked about one that differs from a caller's only
# where it must not decide here -- each kind is offered carrying one argument,
# which is what keeps an empty `grouping_set()` binding from being read as a
# kind `rollup()` does not admit.
#
# Asking costs a raised Package condition for every kind the parent refuses,
# which is four of five under `rollup()` and `cube()` and every one of them
# under `grouping_set()` -- a cli expansion and a backtrace apiece, for
# conditions no caller sees. So the answer is kept, and the key is the parent's
# kind. What that key rests on is that no rule reads anything of the parent but
# its kind: of the three the five kinds share, two ignore the parent entirely
# and the third reads its type, to name it in a message. It does not rest on
# the nested side, where arity is read and where the stand-in above is what
# fixes it -- a stand-in carrying no argument would answer `{}` for `rollup()`
# and `cube()`, turning the refusal off in two of the five positions. A rule
# reading more of a parent than its kind would need this key widened; the first
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
# `preflight` lets a caller running more than one pass over one specification
# hand the same preflight to each, rather than paying a second evaluation of
# the caller's quosures for a result that cannot differ (ADR-0008).
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
    preflight$spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = .by,
    .duplicates = .duplicates,
    duplicates_choices = duplicates_choices
  )
}

compile_grouping_spec_impl <- function(.grouping,
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
  # name is the thing that has to justify itself here — before #134 one did,
  # and reported a column the caller never wrote. The `.by`/`.grouping` overlap
  # check below is the opposite case and stays a Package condition: an ordinary
  # call reaches it.
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

  if (is.null(.grouping)) {
    .grouping <- new_grouping_spec("set", list())
  }

  expanded <- unname(
    expand_grouping_family(.grouping, data_vars, data_proxy)
  )
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
      kind = .grouping$type,
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

expand_grouping_family <- function(spec, data_vars, data_proxy) {
  rule <- find_grouping_kind_rule(spec$type)
  if (is.null(rule)) {
    stop("Unknown grouping specification kind.", call. = FALSE)
  }
  rule$expand(spec, data_vars, data_proxy)
}

expand_single_grouping_set <- function(spec, data_vars, data_proxy) {
  list(resolve_grouping_set(spec, data_vars, data_proxy))
}

resolve_grouping_set <- function(spec, data_vars, data_proxy) {
  if (length(spec$args) == 0L) {
    return(character())
  }

  cols <- unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        stopifnot(is.null(nested))
        resolve_grouping_selection(arg, data_proxy)
      }
    ),
    use.names = FALSE
  )
  unique(cols)
}

expand_grouping_sets <- function(spec, data_vars, data_proxy) {
  unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        if (is.null(nested)) {
          return(list(resolve_grouping_selection(arg, data_proxy)))
        }
        expand_grouping_family(nested, data_vars, data_proxy)
      }
    ),
    recursive = FALSE
  )
}

resolve_grouping_units <- function(spec, data_vars, data_proxy) {
  units <- unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        if (is.null(nested)) {
          cols <- resolve_grouping_selection(arg, data_proxy)
          return(lapply(cols, function(col) col))
        }
        stopifnot(identical(nested$type, "set"))
        cols <- resolve_grouping_set(nested, data_vars, data_proxy)
        if (length(cols) == 0L) {
          abort_empty_composite()
        }
        list(cols)
      }
    ),
    recursive = FALSE
  )

  if (length(units) == 0L) {
    abort_empty_grouping_units(spec$type)
  }
  units
}

expand_rollup <- function(spec, data_vars, data_proxy) {
  units <- resolve_grouping_units(spec, data_vars, data_proxy)
  lapply(
    rev(seq.int(0L, length(units))),
    function(n) {
      if (n == 0L) character() else unique(unlist(units[seq_len(n)]))
    }
  )
}

expand_cube <- function(spec, data_vars, data_proxy) {
  units <- resolve_grouping_units(spec, data_vars, data_proxy)
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

expand_grouping_product <- function(spec, data_vars, data_proxy) {
  product <- list(character())
  if (length(spec$args) == 0L) {
    return(product)
  }

  for (arg in spec$args) {
    nested <- grouping_arg_spec(arg, data_vars)
    family <- if (is.null(nested)) {
      list(resolve_grouping_selection(arg, data_proxy))
    } else {
      expand_grouping_family(nested, data_vars, data_proxy)
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

find_grouping_kind_rule <- function(kind) {
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      is.na(kind)
  ) {
    return(NULL)
  }
  grouping_kind_rules()[[kind]]
}

grouping_constructor_names <- function() {
  unname(vapply(
    grouping_kind_rules(),
    function(rule) rule$constructor,
    character(1)
  ))
}

grouping_arg_spec <- function(arg, data_vars) {
  expr <- rlang::quo_get_expr(arg)
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
# report about a position of its own, so it stays parentless, as
# `abort_share_source_name()` does for the same reason. Every other failure is
# an External condition and is re-raised as it arrived, with its own class,
# diagnostic, and cause.
resolve_grouping_selection <- function(arg, data_proxy) {
  tryCatch(
    resolve_column_selection(
      arg,
      data_proxy,
      on_rename = abort_grouping_rename
    ),
    error = function(cnd) {
      label <- rlang::as_label(rlang::quo_get_expr(arg))
      if (!is_grouping_spec_subscript(cnd, label)) {
        stop(cnd)
      }
      abort_nested_grouping_spec(label)
    }
  )
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
# differ. That difference used to be a labels list the refusal read a singular
# or a plural noun out of; it is now two refusals, and the resolution above is
# handed whichever one speaks for its selection.
#
# Written out rather than kept as one template with the noun interpolated,
# because the noun is what pluralizes. `{?s}` reads the quantity beside it, so
# a shared template would have to choose between two noun pairs, which is the
# branch ADR 0023's `{?}` rule dissolves rather than relocates. Writing them
# out also keeps both inside the structural gate, which reads
# `abort_marginplyr()`'s own argument and refuses a template bound elsewhere --
# the shape `abort_share_helper_position()` records in `R/share.R`, where what
# two calls share is written out in each for that reason. ADR 0023's third
# amendment names that refusal and this pair together.
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
