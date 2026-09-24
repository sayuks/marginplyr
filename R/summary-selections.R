# Options the verb once had. A caller following older material writes them as
# if they were still arguments, and `...` would otherwise accept the value as
# an ordinary summary and return a constant column. Each entry carries its own
# guidance, because what replaces a removed option is specific to that option
# and a shared sentence parameterized by the name could say none of them.
#
# `.sort` was here until ADR 0018 returned it as a live argument. A removed
# option that becomes live leaves this list rather than staying as a second
# answer, because a name the verb has matches its own formal and never reaches
# `...`.
removed_summary_options <- list(
  .groups = "Margin-summary results are always ungrouped."
)

# Every dot-prefixed name the summary verb answers to. `...` sits before them
# in the signature, so R matches them exactly and a name that reaches `...`
# was either misspelled or spliced in.
#
# `.data` and `...` are excluded. Neither can reach `...` as a mistaken option
# name — `.data` matches its own formal exactly, and `...` is not a name a
# caller can write — so keeping them would only widen the near-miss net over
# ordinary output names, which is how `.date` came to resemble `.data`.
summary_option_names <- function() {
  formal_names <- names(formals(summarize_with_margins))
  setdiff(formal_names[startsWith(formal_names, ".")], c(".data", "..."))
}

# Leading-dot names a caller writes on purpose, exempt from the net below even
# though each sits one character from an option. `.group` is one deletion from
# the removed `.groups`, and a group label column is the likelier reading of it.
#
# The exemption is by name because no rule over the distance separates these
# from the mistakes: `.duplicate` is the same one-character deletion from
# `.duplicates` and is worth catching. What differs is whether callers write
# the name deliberately, which only a list can record. The cost is that a
# caller who typed `.group` meaning `.groups` gets a column instead of the
# guidance — accepted, because the name is common enough that the net cost
# landed on people who meant it.
summary_output_name_exemptions <- c(".group")

# Only dot-prefixed names are examined, and only against an exact match or a
# one-character difference: that catches the pluralizations real callers
# write (`.margin_labels`, `.groupings`, `.duplicate`) while leaving ordinary
# leading-dot output names such as `.n` alone.
nearest_summary_option <- function(name, known_options) {
  if (name %in% known_options) {
    return(name)
  }
  # After the exact match, so a name here would still be answered if it ever
  # became an option — the exemption covers resemblance, not the option itself.
  if (name %in% summary_output_name_exemptions) {
    return(NULL)
  }
  distances <- utils::adist(name, known_options)[1L, ]
  nearest <- known_options[distances <= 1L]
  if (length(nearest) == 0L) {
    return(NULL)
  }
  nearest[[1L]]
}

check_option_named_summaries <- function(dots) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    return(invisible(NULL))
  }
  candidates <- unique(dot_names[startsWith(dot_names, ".")])
  if (length(candidates) == 0L) {
    return(invisible(NULL))
  }

  # A call can carry more than one option-shaped name, and this loop answers
  # the first one the caller wrote. `.groups` used to be checked ahead of every
  # name by its own function, so it won regardless of where it appeared; inside
  # the shared loop it has no such standing.
  #
  # Written order is the rule rather than an ordering over the kinds of match,
  # because a caller who wrote two of these has to fix both, and the only thing
  # an ordering would change is which one they are sent to first. Reading in
  # written order keeps that walk down the call instead of jumping around it.
  known_options <- c(summary_option_names(), names(removed_summary_options))
  for (name in candidates) {
    matched <- nearest_summary_option(name, known_options)
    if (is.null(matched)) {
      next
    }
    # Both messages name what the caller wrote, not only what it resembles: a
    # caller who wrote `.sorts` never wrote `.sort`, and an error naming only
    # the option they were reaching for leaves them looking for a word that is
    # not in their code.
    if (matched %in% names(removed_summary_options)) {
      # Two `abort_marginplyr()` calls rather than one: the branch chooses a
      # whole main line, which ADR 0023's third amendment admits, where one
      # call would need a template bound elsewhere, which its *Two rules are
      # gated* refuses. The cost is the `i` bullet the two arms share, written
      # out in each.
      #
      # That bullet is the option's own guidance, which is written in the
      # table above rather than beside either call, so it arrives interpolated
      # as a value -- a template may not be bound elsewhere, and a value may.
      # It names no subject, so it loses no markup by arriving that way, and
      # the `;` the flat form joined it with is now the break between the
      # refusal and its bullet.
      if (identical(name, matched)) {
        abort_marginplyr(c(
          "{.fun summarize_with_margins} has no {.arg {matched}} argument.",
          i = "{removed_summary_options[[matched]]}"
        ))
      }
      abort_marginplyr(c(
        paste0(
          "{.arg {name}} is not an argument of ",
          "{.fun summarize_with_margins}, and neither is the ",
          "{.arg {matched}} it resembles."
        ),
        i = "{removed_summary_options[[matched]]}"
      ))
    }
    # One string under two styles: `{.arg}` where the caller wrote it as an
    # argument name, `{.var}` for the column it became instead. The two render
    # alike, which is the point the style table separates them over -- what
    # the sentence is made of is what each one records.
    abort_marginplyr(c(
      paste0(
        "{.arg {name}} is not an argument of {.fun summarize_with_margins}, ",
        "so it was captured as a summary named {.var {name}}."
      ),
      i = "Did you mean {.arg {matched}}?",
      i = "Rename the summary if the column is intended."
    ))
  }
  invisible(NULL)
}

# The refusal every reader of a summary expression holds. R's empty argument is
# a marker, and a reader that binds it to a local raises `missingArgError` on
# the next read of that local -- several below and in `R/share.R` do, so the
# guard runs ahead of all of them rather than inside one (#340).
#
# Every empty argument `rlang::enquos(...)` captures is refused, named or not,
# including one spliced in. What it captures no argument for is a trailing
# comma, which keeps the reading `grouping_set(region, )` already has.
#
# An unnamed one is named `..n`, the numbering `name_unnamed_by_position()`
# below already spells.
check_empty_summaries <- function(dots) {
  empty <- vapply(dots, is_empty_argument, logical(1), USE.NAMES = FALSE)
  if (!any(empty)) {
    return(invisible(NULL))
  }

  # The first one the caller wrote, which is the order
  # `check_option_named_summaries()` above answers its own candidates in.
  #
  # `name` is read from the cli template below and nowhere else, which
  # `codetools` cannot follow into.
  labels <- name_unnamed_by_position(rlang::names2(dots), "..")
  name <- labels[[which(empty)[[1L]]]] # nolint: object_usage_linter.
  # `{.arg}` rather than `{.var}`: the caller wrote this as an argument name,
  # and the refusal is what stops it becoming a column.
  abort_marginplyr(c(
    "Summary {.arg {name}} is empty.",
    i = "Remove the summary, or write the expression it computes."
  ))
}

check_summary_context_helpers <- function(dots) {
  unsupported <- unique(unlist(
    lapply(
      dots,
      function(dot) {
        find_summary_context_helpers(rlang::quo_get_expr(dot))
      }
    ),
    use.names = FALSE
  ))
  if (length(unsupported) == 0L) {
    return(invisible(NULL))
  }

  # `does not support` opens the message deliberately, which is what ADR 0019
  # asks of it.
  #
  # The helpers do not follow that phrase in the same line. They arrive alone
  # in an `i` bullet, per ADR 0023's condition 2, because how many of them the
  # caller wrote is the caller's decision.
  #
  # What the last bullet says is ADR 0019's decision. It inflects a
  # demonstrative, a noun, and two verbs, and `cli::qty()` is what carries the
  # count to all four: the vector deciding it is no longer in the line they sit
  # in.
  abort_marginplyr(c(
    "{.fun summarize_with_margins} does not support:",
    i = "{.fun {unsupported}}.",
    i = paste0(
      "{cli::qty(length(unsupported))}{?This/These} spelling{?s} {?is/are} ",
      "reserved inside a Margin summary and {?is/are} not resolved from the ",
      "calling environment."
    ),
    i = paste0(
      "These helpers describe one branch-local dplyr grouping or data mask, ",
      "but a margin result combines multiple grouping sets."
    ),
    i = paste0(
      "Use {.fun grouping_bit} or {.fun grouping_id} when identifying margin ",
      "levels."
    )
  ))
}

check_summary_group_overwrite <- function(output_names, group_vars) {
  overwritten_groups <- intersect(output_names, unique(group_vars))
  if (length(overwritten_groups) == 0L) {
    return(invisible(NULL))
  }

  # The columns arrive alone in an `i` bullet, per ADR 0023's condition 2: how
  # many of them there are is the caller's decision. `cli::qty()` is what
  # carries the count across that split, the noun it inflects no longer sitting
  # beside the vector.
  abort_marginplyr(c(
    paste0(
      "{cli::qty(length(overwritten_groups))}Summary results cannot ",
      "overwrite grouping column{?s}:"
    ),
    i = "{.var {overwritten_groups}}."
  ))
}

# The three questions to ask of the names a summary really produced, which the
# pre-execution checks can only ask of the names the static predictor could
# guess. Both execution paths ask them, and the point of asking twice is that
# the two agree: a call one backend rejects must be rejected on every other.
# Composing them here is what keeps the checks, their wording, and their order
# from drifting apart. Only `internal_names` differs between the callers,
# because each path puts columns of its own beside the summary outputs.
check_summary_output_names <- function(output_names,
                                       group_vars,
                                       internal_names,
                                       set_id_name,
                                       set_id_is_internal = FALSE) {
  # A Grouping set identifier the package allocated for itself is one of the
  # internal columns, not the caller's `.id`, and reporting it as `.id` names
  # an argument the caller never wrote and a column they cannot see.
  if (set_id_is_internal) {
    internal_names <- c(internal_names, set_id_name)
    set_id_name <- NULL
  }

  check_internal_summary_names(output_names, internal_names)
  check_summary_group_overwrite(output_names, group_vars = group_vars)
  check_margin_id_collision(set_id_name, output_names, "a summary output")
}

check_internal_summary_names <- function(output_names, internal_names) {
  conflicting_names <- intersect(output_names, internal_names)
  if (length(conflicting_names) == 0L) {
    return(invisible(NULL))
  }

  # The columns arrive alone in an `i` bullet, per ADR 0023's condition 2: how
  # many of them there are is the caller's decision. The `:` the flat form
  # already introduced them with is the break they arrive across, so the noun
  # ahead of it needs no inflection -- it was written plural whatever arrived.
  abort_marginplyr(c(
    paste0(
      "Dynamically generated summary output names conflict with internal ",
      "grouping columns:"
    ),
    i = "{.var {conflicting_names}}.",
    i = "Use different summary output names."
  ))
}

# An Assigned summary name is needed for scalar values, while an unnamed frame
# must expand in dplyr's mask before the next summary runs (ADR 0028).
# Only local branches use this: lazy backends keep their own naming behavior.
# The caller supplies one assigned name per dot, in the same order.
wrap_assigned_local_summaries <- function(dots, assigned_names) {
  for (i in which(!is.na(assigned_names))) {
    dot <- dots[[i]]
    expr <- rlang::call2(
      marginplyr_private_call("local_assigned_summary_value"),
      rlang::quo_get_expr(dot),
      assigned_names[[i]]
    )
    dots[[i]] <- rlang::new_quosure(expr, env = rlang::quo_get_env(dot))
    names(dots)[[i]] <- ""
  }
  dots
}

# Return a frame so dplyr places the output in its mask immediately. An actual
# frame keeps its own column names; a scalar takes the caller-facing name;
# `NULL` keeps dplyr's omission behavior.
# The caller supplies the evaluated value and its Assigned summary name.
local_assigned_summary_value <- function(value, name) {
  if (is.data.frame(value) || is.null(value)) {
    return(value)
  }
  vctrs::new_data_frame(
    stats::setNames(list(value), name),
    n = vctrs::vec_size(value)
  )
}

# What execution carries for the caller's summary arguments: the dots to hand
# dplyr, beside the caller's own label for each and the Assigned summary name
# for each. Constructed at the one point all three are final -- after every
# rewrite -- so vectors that stop agreeing in length cannot be built at all,
# which is an invariant rather than a Package condition (ADR 0015): no call
# produces it, and a map built from a misaligned pair would quote one
# argument's expression under another.
#
# The labels default to the dots' own, which is the truth for a caller reaching
# an adapter directly: what it passed is what it wrote. Nothing is restated
# there, because `branch_argument_map()` drops a label a rewrite left alone --
# so "no spelling to restore" needs no representation of its own, and a length
# is checked once rather than only when a second value says to. The assigned
# names default to none for the same reason: such a caller wrote every name its
# dots carry, and ADR 0028 applies only to a name marginplyr wrote.
# `selection_state` carries the local branch's internal key names to deferred
# selections; the union adapter fills those names before it runs any branch.
new_summary_arguments <- function(dots,
                                  labels = summary_argument_labels(dots),
                                  assigned_names = rep(
                                    NA_character_,
                                    length(dots)
                                  ),
                                  selection_state = NULL) {
  stopifnot(
    is.list(dots),
    is.character(labels),
    length(labels) == length(dots),
    is.character(assigned_names),
    length(assigned_names) == length(dots)
  )
  list(
    dots = dots,
    labels = labels,
    assigned_names = assigned_names,
    selection_state = selection_state
  )
}

# The name `dplyr::summarize()` gives an unnamed argument, for an argument
# marginplyr names in dplyr's place. dplyr's `expr_as_label()` is
# `rlang::as_label()` with rlang's infix labelling suppressed through an
# undocumented option, and the two spellings differ once an expression is long
# enough to abbreviate. Setting that option is the whole of the reproduction:
# the other branch of `expr_as_label()` deparses a bare data pronoun, which no
# expression reaching the one caller here can be (ADR 0022, amended).
dplyr_auto_name <- function(expr) {
  rlang::local_options(`rlang:::use_as_label_infix` = FALSE)
  rlang::as_label(expr)
}

# The name dplyr would have given each unnamed summary marginplyr rewrites,
# read from what the caller wrote rather than from the rewrite. The named dots
# arrive beside the Assigned summary name each one took, `NA` where none was
# assigned: ADR 0028 decides from the local value while dplyr evaluates it.
#
# dplyr names an unnamed summary by deparsing the expression it receives, and
# what a rewritten one hands it is marginplyr's spelling: a branch-local `0L`
# or `1L` where the caller wrote `grouping_bit()` or `grouping_id()`, and a
# qualified `all_of()` literal where they wrote a selection helper. The first
# spelled a different column name in each grouping-set branch, which is what
# the union's column invariant refused; the second named the column after the
# rewrite. `...` is documented as `dplyr::summarize()`'s name-value pairs, so
# the name is settled from the caller's own expression here instead (#430).
#
# Only a rewritten summary is named, which is what keeps the fix away from a
# summary dplyr expands rather than names: a data-frame-valued summary's
# columns go into the result while it is unnamed and are packed into one column
# under any name, and returning a one-row data frame is an ordinary way to
# write several columns at once. One no rewrite reaches goes on expanding.
#
# One a rewrite does reach receives an Assigned summary name. The recognized
# data-frame-valued shapes are excluded here, because
# they are the ones a static reading reaches: `across()` and `pick()` are
# selection helpers and `tibble()` beside one carries the rewrite up, and each
# names its own outputs whatever a branch rewrote inside it. What is left is a
# data-frame-valued expression the static reading does not recognize, and no
# reading separates it from a scalar one: `nrow(pick(v, w))` needs the assigned
# name, while `range_frame(pick(v))` needs its inner columns to expand. ADR 0028
# answers that question from the value in the local branch.
#
# The label is `dplyr_auto_name()` and not `rlang::as_label()`, because what is
# written here is a column name rather than a condition label: ADR 0022's
# rejection of reproducing dplyr's spelling reaches the label alone, and its
# amendment for #439 says so.
name_rewritten_summary_dots <- function(original, resolved,
                                        defer_local = FALSE) {
  stopifnot(length(original) == length(resolved))
  arg_names <- rlang::names2(resolved)
  assigned_names <- rep(NA_character_, length(resolved))
  for (i in which(!nzchar(arg_names))) {
    expr <- rlang::quo_get_expr(original[[i]])
    rewritten <- !identical(expr, rlang::quo_get_expr(resolved[[i]])) ||
      contains_grouping_helper(expr) ||
      (defer_local && contains_summary_selection(expr))
    if (!rewritten || !is.null(data_frame_valued_summary_kind(expr))) {
      next
    }
    arg_names[[i]] <- dplyr_auto_name(expr)
    assigned_names[[i]] <- arg_names[[i]]
  }
  list(
    dots = stats::setNames(resolved, arg_names),
    assigned_names = assigned_names
  )
}

# Whether an evaluated part of a summary contains an ordinary selection helper.
# The caller uses this only to decide which original dots need local deferral.
contains_summary_selection <- function(expr) {
  if (rlang::is_quosure(expr)) {
    return(contains_summary_selection(rlang::quo_get_expr(expr)))
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (!is.null(static_spelling_name(expr, "selection"))) {
    return(TRUE)
  }
  any(vapply(
    searched_call_parts(expr, call_name = static_call_name(expr)),
    contains_summary_selection,
    logical(1)
  ))
}

plan_summary_expressions <- function(dots,
                                     data_proxy,
                                     data_vars,
                                     plan,
                                     backend_kind,
                                     set_id_name,
                                     call) {
  stopifnot(inherits(plan, "margin_grouping_plan"))
  group_vars <- c(plan$by, plan$dimensions)
  # Read before anything is rewritten, which is the whole of what makes these
  # the caller's own labels: every rewrite below runs after this line, and ADR
  # 0007 has already captured the dots at the public verb.
  caller_labels <- summary_argument_labels(dots)
  original_dots <- dots
  defer_local <- identical(backend_kind, "local")
  has_shares <- any(vapply(
    dots,
    function(dot) contains_share_helper(rlang::quo_get_expr(dot)),
    logical(1)
  ))
  deferred <- rep(FALSE, length(dots))
  if (defer_local) {
    prior_ordinary <- FALSE
    for (i in seq_along(dots)) {
      expr <- rlang::quo_get_expr(dots[[i]])
      if (contains_share_helper(expr)) {
        next
      }
      deferred[[i]] <- !has_shares ||
        (prior_ordinary && contains_summary_selection(expr))
      prior_ordinary <- TRUE
    }
  }
  selection_proxy <- summary_selection_proxy(
    data_proxy,
    data_vars = data_vars,
    group_vars = group_vars
  )
  predictable_names <- if (defer_local) {
    predictable_local_across_names(original_dots, names(selection_proxy))
  } else {
    character()
  }
  if (
    defer_local && length(dots) > 0L &&
      !contains_share_helper(rlang::quo_get_expr(dots[[1L]]))
  ) {
    preflight_local_selection(dots[[1L]], selection_proxy)
  }
  if (!defer_local || has_shares) {
    dots <- resolve_summary_selections(
      dots,
      group_vars = group_vars,
      caller_labels = caller_labels,
      normalize_across_names = FALSE,
      skip_shares = TRUE,
      defer_local = deferred,
      skip_deferred = TRUE,
      selection_proxy = selection_proxy
    )
  }
  if (defer_local && has_shares) {
    for (i in seq_along(dots)) {
      deferred[[i]] <- deferred[[i]] || (
        contains_summary_selection(rlang::quo_get_expr(original_dots[[i]])) &&
          identical(
            rlang::quo_get_expr(original_dots[[i]]),
            rlang::quo_get_expr(dots[[i]])
          )
      )
    }
  }
  # Against the dots this rewrite received, and before share planning moves
  # one: a share summary carries an output name already, and every rewrite
  # below this either answers a named dot or is one of those moves. The labels
  # above are read first because they are the caller's spelling for a Condition
  # context, which a name assigned here would spell `sum(v) = sum(v)`.
  named <- name_rewritten_summary_dots(
    original_dots, dots, defer_local = defer_local
  )
  dots <- named$dots
  summary_plan <- if (has_shares) {
    plan_share_expressions(
      dots,
      selection_proxy = selection_proxy,
      plan = plan,
      set_id_name = set_id_name,
      validate_cardinality = wraps_share_sources_in_summary(backend_kind),
      defer_local = defer_local
    )
  } else {
    list(
      dots = dots,
      requests = list(),
      cardinality = list(),
      origin_positions = seq_along(dots)
    )
  }
  # Share planning is the one step that moves a dot, so it reports where each
  # dot it produced came from and both per-dot vectors are subscripted by that.
  # Every other rewrite here answers one dot with one dot in place.
  caller_labels <- caller_labels[summary_plan$origin_positions]
  assigned_names <- named$assigned_names[summary_plan$origin_positions]
  deferred <- deferred[summary_plan$origin_positions]
  selection_state <- if (defer_local) new.env(parent = emptyenv()) else NULL
  if (defer_local) {
    selection_state$internal_names <- character()
    share_positions <- which(vapply(
      original_dots,
      function(dot) contains_share_helper(rlang::quo_get_expr(dot)),
      logical(1)
    ))
    share_positions <- share_positions[
      share_positions %in% summary_plan$origin_positions
    ]
    stopifnot(length(share_positions) == length(summary_plan$requests))
    selection_state$by_dot <- lapply(
      summary_plan$origin_positions,
      function(position) {
        state <- new.env(parent = selection_state)
        state$forbidden_names <- unlist(lapply(
          summary_plan$requests[share_positions < position],
          `[[`, "outputs"
        ), use.names = FALSE)
        state
      }
    )
  }
  summary_plan$dots <- resolve_summary_selections(
    summary_plan$dots,
    group_vars = group_vars,
    caller_labels = caller_labels,
    normalize_across_names = identical(backend_kind, "dtplyr"),
    defer_local = deferred,
    forbidden_names = if (defer_local) selection_state else character(),
    selection_proxy = selection_proxy
  )
  if (length(summary_plan$cardinality) > 0L) {
    summary_plan$dots <- wrap_share_sources(
      summary_plan$dots,
      cardinality = summary_plan$cardinality,
      call = call,
      backend_kind = backend_kind
    )
  }
  list(
    summaries = new_summary_arguments(
      summary_plan$dots,
      caller_labels,
      assigned_names,
      selection_state = selection_state
    ),
    requests = summary_plan$requests,
    predictable_names = predictable_names
  )
}

# Names an unnamed local `across()` only when both its columns and `.names`
# template are literal. The input is a name list, so no caller function or
# summary expression runs while reserving internal key names.
predictable_local_across_names <- function(dots, input_names) {
  available <- input_names
  predicted <- character()
  arg_names <- rlang::names2(dots)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    name <- arg_names[[i]]
    expr <- rlang::quo_get_expr(dot)
    if (is_across_call(expr) && !nzchar(name)) {
      parsed <- parse_across_arguments(expr)
      cols <- parsed$cols
      template <- parsed$names
      simple_cols <- if (rlang::is_symbol(cols)) {
        rlang::as_string(cols) %in% available
      } else if (rlang::is_call(cols, "c")) {
        parts <- as.list(cols)[-1L]
        all(vapply(parts, rlang::is_symbol, logical(1))) &&
          all(vapply(parts, rlang::as_string, character(1)) %in% available)
      } else {
        FALSE
      }
      literal_template <- is.null(template) || (
        is.character(template) && length(template) == 1L &&
          !is.na(template) &&
          !grepl("{", gsub(
            "{.fn}", "", gsub("{.col}", "", template, fixed = TRUE),
            fixed = TRUE
          ), fixed = TRUE)
      )
      if (simple_cols && literal_template) {
        proxy <- stats::setNames(as.list(seq_along(available)), available)
        output <- tryCatch(
          known_across_output_names(
            expr, rlang::quo_get_env(dot), proxy
          ),
          vctrs_error_subscript_oob = function(cnd) character()
        )
        predicted <- c(predicted, output)
        available <- c(available, output)
      }
    }
    if (nzchar(name)) {
      available <- c(available, name)
    }
  }
  predicted
}

# A bare name in the first local selection can only name an input column.
# The empty pair is also statically invalid. Checking these before operation
# validation preserves tidyselect's condition and refusal order without
# evaluating a caller predicate.
preflight_local_selection <- function(dot, selection_proxy) {
  expr <- rlang::quo_get_expr(dot)
  selection <- if (is_static_spelling_call(expr, "selection", "across")) {
    parse_across_arguments(expr)$cols
  } else if (is_static_spelling_call(expr, "selection", "pick")) {
    parse_pick_selection(expr)
  } else {
    NULL
  }
  empty_pair <- rlang::is_call(selection, "(") &&
    length(selection) == 2L &&
    rlang::is_missing(selection[[2L]])
  if (rlang::is_symbol(selection) || empty_pair) {
    resolve_summary_selection(
      selection,
      env = rlang::quo_get_env(dot),
      data_proxy = selection_proxy
    )
  }
  invisible(NULL)
}

find_summary_context_helpers <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(character())
  }

  call_name <- static_call_name(expr)
  refused <- static_spelling_name(expr, "refused")
  found <- if (is.null(refused)) character() else refused

  # The arguments the mask evaluates, and the language the call evaluates. A
  # helper name the caller quoted describes no grouping this call has to
  # combine -- nothing calls it -- and refusing the call over one refused a
  # summary that only names the helper. Handing one to `eval()` is the opposite
  # case and needs the opposite answer: the helper runs, and it answers the
  # branch-local identifier this guard exists to refuse (#179).
  c(
    found,
    unlist(
      lapply(
        searched_call_parts(expr, call_name = call_name),
        find_summary_context_helpers
      ),
      use.names = FALSE
    )
  )
}

# Builds the proxy every summary-selection reader sees: source columns outside
# fixed keys and grouping dimensions. Keeping the exclusion here makes the
# planning, rewriting, and output-name readers agree on one selection context.
summary_selection_proxy <- function(data_proxy, data_vars, group_vars) {
  dplyr::select(
    data_proxy,
    dplyr::all_of(setdiff(data_vars, unique(group_vars)))
  )
}

# `caller_labels` is what a refusal quotes the failing dot by, and is passed in
# rather than read off `dots`: the second resolution runs over dots this one
# rewrote, whose labels are marginplyr's spelling and not the caller's
# (ADR 0022).
resolve_summary_selections <- function(dots,
                                       group_vars,
                                       caller_labels,
                                       selection_proxy,
                                       normalize_across_names = FALSE,
                                       skip_shares = FALSE,
                                       defer_local = FALSE,
                                       forbidden_names = character(),
                                       skip_deferred = FALSE) {
  stopifnot(length(dots) == length(caller_labels))
  if (length(defer_local) == 1L) {
    defer_local <- rep(defer_local, length(dots))
  }
  stopifnot(length(defer_local) == length(dots))

  lapply(
    seq_along(dots),
    function(i) {
      dot <- dots[[i]]
      expr <- rlang::quo_get_expr(dot)
      dot_forbidden_names <- if (
        is.environment(forbidden_names) &&
          !is.null(forbidden_names$by_dot)
      ) {
        forbidden_names$by_dot[[i]]
      } else {
        forbidden_names
      }
      if (skip_deferred && defer_local[[i]]) {
        return(dot)
      }
      if (
        skip_shares &&
          contains_share_helper(expr)
      ) {
        return(dot)
      }
      expr <- tryCatch(
        rewrite_summary_selections(
          expr,
          env = rlang::quo_get_env(dot),
          data_proxy = selection_proxy,
          normalize_across_names = normalize_across_names,
          defer_local = defer_local[[i]],
          group_vars = group_vars,
          forbidden_names = dot_forbidden_names
        ),
        error = function(cnd) {
          if (is_unsupported_predicate(cnd)) {
            abort_selection_predicate(caller_labels[[i]], cnd)
          }
          stop(cnd)
        }
      )
      rlang::new_quosure(expr, env = rlang::quo_get_env(dot))
    }
  ) |>
    stats::setNames(names(dots))
}

rewrite_summary_selections <- function(expr,
                                       env,
                                       data_proxy,
                                       normalize_across_names,
                                       defer_local = FALSE,
                                       group_vars = character(),
                                       forbidden_names = character()) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }

  # An injected quosure carries an environment of its own, and that is what
  # injecting one is for, so a selection inside it resolves there rather than
  # in the environment of the dot that contains it. Reading the outer one would
  # look up a name the caller never put in it.
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
  }

  # A selection the caller quoted is language data, so the walk descends past
  # it and gives the object back as it was written. Resolving it turned
  # `quote(dplyr::across(value, mean))` into
  # `quote(dplyr::across(dplyr::all_of("value"), mean))`, which is a different
  # expression to whatever the caller meant to carry (#179).
  expr <- rewrite_evaluated_call_parts(
    expr,
    function(part) {
      rewrite_summary_selections(
        part,
        env = env,
        data_proxy = data_proxy,
        normalize_across_names = normalize_across_names,
        defer_local = defer_local,
        group_vars = group_vars,
        forbidden_names = forbidden_names
      )
    }
  )

  call_name <- static_spelling_name(expr, "selection")
  if (is.null(call_name)) {
    return(expr)
  }

  # The head is qualified before either branch reads the call, so every rebuild
  # below inherits it through `rebuild_static_call()` and no branch has to
  # remember. ADR 0019's *Analysis and execution must agree* is authoritative
  # for why a recognized head is qualified at all.
  expr <- qualify_static_spelling(expr, "selection", call_name)

  if (call_name %in% c("across", "if_any", "if_all")) {
    if (defer_local) {
      return(rewrite_local_across_selection(
        expr, env, group_vars, forbidden_names
      ))
    }
    return(rewrite_across_selection(
      expr,
      env,
      data_proxy,
      normalize_across_names = normalize_across_names,
      call_name = call_name
    ))
  }
  if (identical(call_name, "pick")) {
    if (defer_local) {
      return(rewrite_local_pick_selection(
        expr, env, group_vars, forbidden_names
      ))
    }
    return(rewrite_pick_selection(expr, env, data_proxy))
  }

  # An invariant, not a Package condition (ADR 0015): the branches above answer
  # every name the `selection` family holds, so reaching this means a spelling
  # was registered without a rewrite. Falling through to one of them instead
  # would rewrite the new spelling as whichever helper happened to be last,
  # which is a silently wrong selection rather than a missing one.
  stop(
    "No rewrite is registered for the selection helper `",
    call_name,
    "()`.",
    call. = FALSE
  )
}

# Builds the selection passed to dplyr, preserving the caller's environment.
# An injected missing selection stays empty rather than taking the default.
local_summary_selection_expr <- function(selection, env, group_vars,
                                         forbidden_names) {
  if (rlang::is_quosure(selection)) {
    env <- rlang::quo_get_env(selection)
    selection <- rlang::quo_get_expr(selection)
  }
  if (rlang::is_missing(selection)) {
    selection <- rlang::expr(c())
  }
  rlang::call2(
    "all_of",
    rlang::call2(
      marginplyr_private_call("local_summary_selection"),
      rlang::call2("quote", selection),
      env,
      group_vars,
      forbidden_names
    ),
    .ns = "tidyselect"
  )
}

# Leaves `.cols` for local dplyr to resolve against its current summary mask.
rewrite_local_across_selection <- function(expr, env, group_vars,
                                           forbidden_names) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  selection <- local_summary_selection_expr(
    parsed$cols, env, group_vars, forbidden_names
  )
  if (parsed$cols_index == 0L) {
    call_args <- append(list(.cols = selection), call_args)
  } else {
    call_args[[parsed$cols_index]] <- selection
  }
  rebuild_static_call(expr, call_args)
}

# Applies the same deferred local selection to `pick()`.
rewrite_local_pick_selection <- function(expr, env, group_vars,
                                         forbidden_names) {
  selection <- local_summary_selection_expr(
    parse_pick_selection(expr), env, group_vars, forbidden_names
  )
  rebuild_static_call(expr, list(selection))
}

# Returns selected current column names for one local dplyr summary branch.
# The caller is inside a tidyselect selection in dplyr's summary mask, whose
# current columns include preceding outputs. ADR 0002 owns the input schema.
local_summary_selection <- function(selection, env, group_vars,
                                    forbidden_names) {
  data <- tidyselect::peek_data()
  if (is.environment(forbidden_names)) {
    forbidden_names <- c(
      forbidden_names$forbidden_names,
      get("internal_names", envir = forbidden_names, inherits = TRUE)
    )
  }
  data <- data[setdiff(names(data), c(group_vars, forbidden_names))]
  selected <- resolve_summary_selection(
    selection, env = env, data_proxy = data
  )
  source_names <- names(data)[unname(selected)]
  if (!identical(names(selected), source_names)) {
    names(source_names) <- names(selected)
  }
  source_names
}

# `call_name` is the caller's answer rather than one asked again here. Asking
# again would answer the same, since the shared read answers a formula as no
# name at all (#163); it would just be a second question about an expression
# the only caller enters this function having already named, and the two
# answers would then have to be kept in step by hand.
rewrite_across_selection <- function(expr,
                                     env,
                                     data_proxy,
                                     normalize_across_names,
                                     call_name) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  selection_index <- parsed$cols_index

  # One resolution, whichever way the selection was written: `parsed$cols` is
  # already the `dplyr::everything()` that an omitted `.cols` selects, and an
  # argument the caller wrote empty is omitted in exactly that sense. A quosure
  # carrying the empty argument is not, and reaches the resolution below as the
  # selection it is -- the empty one (#350).
  # What the branch decides is only where the resolved selection goes --
  # prepended when no argument occupies `.cols`, and written back over the
  # argument that does, so an empty one keeps its position instead of being
  # dropped from the middle of the call (#174).
  selected <- resolve_summary_selection(
    parsed$cols,
    env = env,
    data_proxy = data_proxy
  )
  if (selection_index == 0L) {
    call_args <- append(
      list(.cols = summary_all_of_expr(selected, data_proxy)),
      call_args
    )
    selection_index <- 1L
  } else {
    call_args[[selection_index]] <- summary_all_of_expr(selected, data_proxy)
  }

  if (identical(call_name, "across") && normalize_across_names) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    unpack_is_false <- is.null(parsed$unpack) || isFALSE(tryCatch(
      rlang::eval_tidy(parsed$unpack, env = env),
      error = function(cnd) NULL
    ))
    function_names <- known_across_function_names(parsed, env)

    if (
      !is.null(parsed$names) &&
        unpack_is_false &&
        length(function_names) == 1L
    ) {
      output_names <- known_across_output_names(expr, env, data_proxy)
      if (length(output_names) == length(selected)) {
        names(selected) <- output_names
        call_args[[selection_index]] <- summary_all_of_expr(
          selected,
          data_proxy
        )

        if (
          rlang::is_call(parsed$fns, "list") &&
            length(parsed$fns) == 2L
        ) {
          call_args[[parsed$fns_index]] <- parsed$fns[[2L]]
        }
        call_args <- call_args[-parsed$names_index]
      }
    }
  }

  if (identical(call_name, "across")) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    if (!is.null(parsed$names)) {
      # A template that does not evaluate is left as the caller wrote it, so
      # dplyr raises the condition their own argument produces rather than this
      # frame raising it first (ADR 0015). The two reads of `.names` and
      # `.unpack` above answer a failure the same way, with the value they use
      # when the argument is absent; here the value is the argument itself.
      template <- tryCatch(
        list(rlang::eval_tidy(parsed$names, env = env)),
        error = function(cnd) NULL
      )
      if (!is.null(template)) {
        call_args[[parsed$names_index]] <- template[[1L]]
      }
    }
  }

  rebuild_static_call(expr, call_args)
}

rewrite_pick_selection <- function(expr, env, data_proxy) {
  selection <- parse_pick_selection(expr)
  selected <- resolve_summary_selection(
    selection,
    env = env,
    data_proxy = data_proxy
  )

  rebuild_static_call(expr, list(summary_all_of_expr(selected, data_proxy)))
}

# Reads a `pick()` call's selection. An empty call selects everything, and both
# callers need that rule to agree on the columns `pick()` can produce.
parse_pick_selection <- function(expr) {
  call_args <- static_call_args(expr)
  if (length(call_args) == 0L) {
    return(rlang::expr(dplyr::everything()))
  }
  rlang::call2("c", !!!call_args)
}

resolve_summary_selection <- function(expr, env, data_proxy) {
  # A quosure is passed on rather than wrapped, because it carries the
  # environment `env` would supply and a selection needs the one it was written
  # in. Wrapping produces a quosure whose expression is a quosure, which
  # `eval_select()` reads as the lambda shorthand and refuses with `where()`
  # advice. `{{ }}` at `.cols` inlines a quosure, so this is the position the
  # ordinary idiom for forwarding a selection arrives at (#350). The
  # environment is what separates this from ADR 0019's amendment, which reads an
  # injected *name* for the name alone.
  selection <- if (rlang::is_quosure(expr)) {
    expr
  } else {
    rlang::new_quosure(expr, env = env)
  }

  tidyselect::eval_select(
    selection,
    data = data_proxy,
    strict = TRUE,
    allow_rename = TRUE
  )
}

summary_all_of_expr <- function(selected, data_proxy) {
  source_names <- get_col_names(
    data_proxy,
    dplyr::everything()
  )[unname(selected)]
  output_names <- names(selected)
  if (!identical(output_names, source_names)) {
    source_names <- stats::setNames(source_names, output_names)
  }
  rlang::expr(dplyr::all_of(!!source_names))
}

known_summary_output_names <- function(dots, data_proxy,
                                       defer_local = FALSE) {
  # A dot carrying its own name is not read for what it packs: dplyr packs a
  # data-frame result under that name rather than unpacking it, so the argument
  # names inside such a dot are not top-level outputs, and `names(dots)` at the
  # caller already holds the one that is (#431). A backend that unpacks anyway
  # is what the adapters' `check_summary_output_names()` reads the branch
  # result for -- Arrow's `across()` is that case.
  named <- nzchar(rlang::names2(dots))

  unlist(
    lapply(
      dots[!named],
      function(dot) {
        expr <- rlang::quo_get_expr(dot)
        env <- rlang::quo_get_env(dot)
        if (defer_local && (
          identical(data_frame_valued_summary_kind(expr), "pick") ||
            identical(data_frame_valued_summary_kind(expr), "across")
        )) {
          return(character())
        }
        known_data_frame_output_names(expr, env, data_proxy)
      }
    ),
    use.names = FALSE
  )
}

# Which data-frame-valued shape a summary is written as, or `NULL` for one that
# is not recognized as any. Two readers need the recognition and each needs a
# different half of it: `known_data_frame_output_names()` reads which outputs
# the shape produces, and `name_rewritten_summary_dots()` reads that dplyr
# expands them rather than naming one column for the summary. Answering both
# from here is what keeps a shape added for one from being invisible to the
# other.
#
# Two frame families rather than one, because the owner differs and the owner
# is what recognition tests: tibble owns `tibble()` and `data_frame()`, base
# owns `data.frame()`. Neither is a Contextual helper -- nothing rewrites them,
# and a caller who binds `tibble` gets their own function -- so what is read
# here is only which output names the summary is going to produce (ADR 0019).
data_frame_valued_summary_kind <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(NULL)
  }
  if (is_any_static_spelling_call(expr, c("tibble_frame", "base_frame"))) {
    return("frame")
  }
  if (is_static_spelling_call(expr, "selection", "pick")) {
    return("pick")
  }
  if (is_static_spelling_call(expr, "selection", "across")) {
    return("across")
  }
  NULL
}

known_data_frame_output_names <- function(expr, env, data_proxy) {
  kind <- data_frame_valued_summary_kind(expr)
  if (is.null(kind)) {
    return(character())
  }

  if (identical(kind, "frame")) {
    call_args <- static_call_args(expr)
    arg_names <- names(call_args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_args))
    }
    injected_names <- vapply(
      call_args[arg_names == ""],
      known_injected_argument_name,
      character(1)
    )
    return(setdiff(
      c(arg_names[nzchar(arg_names)], injected_names[nzchar(injected_names)]),
      ".name_repair"
    ))
  }

  if (identical(kind, "pick")) {
    selection <- parse_pick_selection(expr)
    return(names(resolve_summary_selection(selection, env, data_proxy)))
  }

  if (identical(kind, "across")) {
    return(known_across_output_names(expr, env, data_proxy))
  }

  # A kind the classifier answers and this does not. Both are in this file and
  # nothing outside it names a kind, so no call reaches this and it stays a
  # bare `stop()` (ADR 0015).
  stop("Unhandled data-frame-valued summary kind: ", kind, call. = FALSE)
}

known_injected_argument_name <- function(expr) {
  if (!rlang::is_call(expr, ":=") || length(expr) != 3L) {
    return("")
  }

  # By subscript, because a name-position argument the caller left empty is R's
  # missing marker: `lhs <- expr[[2L]]` binds it and raises `missingArgError`
  # on the first read of that name (#174). It names no output, which is what
  # the fall-through below already says.
  if (
    is.character(expr[[2L]]) &&
      length(expr[[2L]]) == 1L &&
      !is.na(expr[[2L]])
  ) {
    return(expr[[2L]])
  }
  if (is_name_part(expr[[2L]])) {
    return(rlang::as_name(expr[[2L]]))
  }
  ""
}

known_across_output_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  cols_expr <- parsed$cols
  column_names <- names(resolve_summary_selection(cols_expr, env, data_proxy))

  if (is.null(parsed$names)) {
    if (rlang::is_call(parsed$fns, "list")) {
      function_names <- known_across_function_names(parsed, env)
      return(unlist(
        lapply(
          column_names,
          paste,
          function_names,
          sep = "_"
        ),
        use.names = FALSE
      ))
    }
    # A bound `.fns` might be a list, whose default names include the function
    # names. Only a statically single function uses the column names alone.
    if (length(known_across_function_names(parsed, env)) == 1L) {
      return(column_names)
    }
    return(character())
  }
  names_template <- tryCatch(
    rlang::eval_tidy(parsed$names, env = env),
    error = function(cnd) NULL
  )
  if (
    !is.character(names_template) ||
      length(names_template) != 1L ||
      is.na(names_template)
  ) {
    return(character())
  }

  function_names <- known_across_function_names(parsed, env)
  if (length(function_names) == 0L) {
    return(character())
  }

  unlist(
    lapply(
      column_names,
      function(column) {
        vapply(
          function_names,
          function(fn) {
            expanded <- expand_across_name(names_template, column, fn, env)
            check_across_name_count(expanded, names_template, column)
          },
          character(1)
        )
      }
    ),
    use.names = FALSE
  )
}

expand_across_name <- function(template, column, function_name, env) {
  as.character(glue::glue_data(
    list(.col = column, .fn = function_name),
    template,
    .envir = env
  ))
}

# The expansion names one output per selected column, so a template that
# expands to any other number is one `across()` will reject too. That is what
# separates this from the `character()` the caller above returns: there the
# template could not be evaluated at all and the analysis simply does not
# know the names, whereas here it knows them and knows they are wrong. Saying
# so here reaches the caller before the summary is staged, rather than as a
# size error out of the query built from it (ADR-0005).
check_across_name_count <- function(expanded, template, column) {
  if (length(expanded) == 1L) {
    return(expanded)
  }

  # The template and the column both stay in the main line, under ADR 0023's
  # element-count reading of its condition 2: each is one caller subject rather
  # than a part the caller decides the count of, however long a template
  # renders. `{.code}` is what a spelling the caller typed takes, and the
  # braces inside one are inert because it arrives interpolated as a value.
  abort_marginplyr(c(
    paste0(
      "The {.fun across} {.arg .names} template {.code {template}} must ",
      "produce one name per column, but it produced {length(expanded)} for ",
      "column {.var {column}}."
    ),
    i = "Use a template that expands to a single name."
  ))
}

known_across_source_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  selected <- resolve_summary_selection(parsed$cols, env, data_proxy)
  get_col_names(data_proxy, dplyr::everything())[unname(selected)]
}

# `"1"` is what `{.fn}` expands to for a statically single function or an
# omitted `.fns`. Resolved function bindings, including namespace exports, are
# also single. `substitute()` reads a caller binding without forcing a promise;
# active bindings are left unknown so prediction cannot run caller code.
known_across_function_names <- function(parsed, env) {
  if (is.null(parsed$fns) || is.function(parsed$fns) ||
        rlang::is_call(parsed$fns, "function") ||
        rlang::is_call(parsed$fns, "\\") ||
        rlang::is_call(parsed$fns, "~")) {
    return("1")
  }

  if (rlang::is_call(parsed$fns, "::") ||
        rlang::is_call(parsed$fns, ":::")) {
    package <- parsed$fns[[2L]]
    name <- parsed$fns[[3L]]
    if (rlang::is_symbol(package) && rlang::is_symbol(name)) {
      package <- rlang::as_string(package)
      name <- rlang::as_string(name)
      # A namespace loaded solely for prediction could run its `.onLoad`.
      namespace <- if (isNamespaceLoaded(package)) {
        getNamespace(package)
      } else {
        NULL
      }
      if (!is.null(namespace) &&
            exists(name, envir = namespace, inherits = FALSE) &&
            !bindingIsActive(name, namespace)) {
        binding <- if (rlang::is_call(parsed$fns, "::")) {
          tryCatch(getExportedValue(package, name), error = function(cnd) NULL)
        } else {
          get(name, envir = namespace, inherits = FALSE)
        }
        if (is.function(binding)) {
          return("1")
        }
      }
    }
    return(character())
  }

  if (!rlang::is_call(parsed$fns, "list")) {
    if (rlang::is_symbol(parsed$fns)) {
      name <- rlang::as_string(parsed$fns)
      current <- env
      while (!identical(current, emptyenv())) {
        if (exists(name, envir = current, inherits = FALSE)) {
          if (bindingIsActive(name, current)) {
            return(character())
          }
          base_binding <- identical(current, baseenv()) ||
            identical(current, asNamespace("base"))
          binding <- if (base_binding ||
                           !rlang::env_binding_are_lazy(current, name)) {
            get(name, envir = current, inherits = FALSE)
          } else {
            eval(
              call("substitute", as.name(name), current),
              envir = baseenv()
            )
          }
          if (is.function(binding) ||
                rlang::is_call(binding, "function") ||
                rlang::is_call(binding, "\\") ||
                rlang::is_call(binding, "~")) {
            return("1")
          }
          return(character())
        }
        current <- parent.env(current)
      }
    }
    return(character())
  }

  fns <- static_call_args(parsed$fns)
  fns_names <- names(fns)
  if (is.null(fns_names)) {
    fns_names <- rep("", length(fns))
  }
  name_unnamed_by_position(fns_names, "")
}

# Every caller names the unnamed entries of an argument list by position, which
# is how dplyr refers to them: an argument forwarded through `across()`'s `...`
# is `..n`, and an unnamed `.fns` list entry takes its index. An empty summary
# `check_empty_summaries()` refuses is named the same way, so what the caller
# reads is one numbering rather than one per site. The replacement has to be
# indexed by the same positions that select it. Building it over the
# whole list instead makes the two sides differ in length whenever any entry is
# named, so base R recycles -- warning from a call that otherwise succeeds --
# and numbers the survivors by their position among the unnamed entries rather
# than among all of them (#104).
name_unnamed_by_position <- function(arg_names, prefix) {
  unnamed <- which(arg_names == "")
  arg_names[unnamed] <- paste0(prefix, unnamed)
  arg_names
}

# The one place that knows an `across()` argument can be empty, so that no
# caller has to. R's empty argument is what a caller leaves in a position they
# omitted, and R answers it as an omission: `across(v, )` takes `.fns`'s
# default exactly as `across(v)` does, which is why `dplyr::across()` treats
# the two alike down to the `{.fn}` expansion and the `.cols` deprecation. The
# value fields below therefore answer for an empty argument what they answer
# for an absent one, and the index fields keep naming the position it occupies:
# a rewrite puts its replacement back where the caller wrote it rather than
# dropping an argument, which would slide every positional argument after it
# into a formal that is not its own (#174).
#
# Reading a value here rather than out of `call_args` is also what keeps the
# empty argument from being bound to a name downstream. `parts[[index]]` is
# safe, and passing what it returns straight to a function is safe, but binding
# it -- `for (part in parts)` as in #168, or `part <- parts[[index]]` as here --
# raises base R's untyped `missingArgError` on the first read of that name.
parse_across_arguments <- function(expr) {
  # Through the shared reader, so that the arguments parsed here are the
  # arguments of the call recognized as `across()`. Both readings see through a
  # redundant pair of parentheses, and a parse that did not would read
  # `(across(v, sum))` as one argument -- the `across()` call itself -- and hand
  # it to `eval_select()` as a selection (#178).
  call_args <- static_call_args(expr)
  # Match unique placeholders against dplyr's formals: R handles partial names
  # before `...`, while the positions still identify empty or repeated values.
  helper <- get(static_call_name(expr), envir = asNamespace("dplyr"))
  markers <- lapply(seq_along(call_args), function(i) {
    as.name(paste0(".marginplyr_arg_", i))
  })
  names(markers) <- names(call_args)
  matched <- match.call(
    definition = helper,
    call = as.call(c(list(as.name("helper")), markers)),
    expand.dots = FALSE
  )
  matched_args <- as.list(matched)[-1L]
  marker_names <- vapply(markers, as.character, character(1))
  formal_index <- function(name) {
    argument <- matched_args[[name]]
    if (is.null(argument)) {
      return(0L)
    }
    match(as.character(argument), marker_names, nomatch = 0L)
  }
  cols_index <- formal_index(".cols")
  fns_index <- formal_index(".fns")
  names_index <- formal_index(".names")
  unpack_index <- formal_index(".unpack")
  arg_names <- rlang::names2(call_args)
  used <- c(cols_index, fns_index, names_index, unpack_index)
  additional <- setdiff(seq_along(call_args), used[used > 0L])
  additional_names <- name_unnamed_by_position(arg_names[additional], "..")
  supplied <- function(index) {
    index > 0L && !rlang::is_missing(call_args[[index]])
  }

  list(
    call_args = call_args,
    cols_index = cols_index,
    fns_index = fns_index,
    names_index = names_index,
    unpack_index = unpack_index,
    cols = if (supplied(cols_index)) {
      call_args[[cols_index]]
    } else {
      rlang::expr(dplyr::everything())
    },
    fns = if (supplied(fns_index)) call_args[[fns_index]] else NULL,
    names = if (supplied(names_index)) call_args[[names_index]] else NULL,
    unpack = if (supplied(unpack_index)) call_args[[unpack_index]] else NULL,
    additional = additional_names
  )
}
