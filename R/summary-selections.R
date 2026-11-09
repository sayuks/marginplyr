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
# The caller supplies one assigned name and one branch-local type state per dot.
wrap_assigned_local_summaries <- function(dots, assigned_names, group_vars,
                                          internal_names, set_id_name,
                                          set_id_is_internal,
                                          share_sources,
                                          source_definitions,
                                          type_states,
                                          alias_state = NULL) {
  for (i in which(!is.na(assigned_names))) {
    dot <- dots[[i]]
    expr <- rlang::call2(
      marginplyr_private_call("local_assigned_summary_value"),
      rlang::quo_get_expr(dot),
      assigned_names[[i]], group_vars, internal_names, set_id_name,
      set_id_is_internal,
      setdiff(share_sources, source_definitions[[i]]), type_states[[i]],
      alias_state, i
    )
    dots[[i]] <- rlang::new_quosure(expr, env = rlang::quo_get_env(dot))
    names(dots)[[i]] <- ""
  }
  dots
}

# Return a frame so dplyr places the output in its mask immediately. An actual
# frame keeps its own checked column names; a scalar takes the caller-facing
# name; `NULL` keeps dplyr's omission behavior.
# The caller supplies the evaluated value, its Assigned summary name, and a
# type state shared by this dot's groups within one branch.
local_assigned_summary_value <- function(value, name, group_vars,
                                         internal_names, set_id_name,
                                         set_id_is_internal,
                                         protected_sources,
                                         type_state,
                                         alias_state = NULL, position = NULL) {
  if (is.null(value)) {
    return(value)
  }
  type_state$ptype <- vctrs::vec_ptype2(
    type_state$ptype, value, x_arg = name, y_arg = name, call = NULL
  )
  frame <- is.data.frame(value)
  # The carrier expands the stored columns of a frame subclass.
  output_names <- if (frame) attr(value, "names", exact = TRUE) else name
  record_local_summary_aliases(output_names, alias_state, position)
  if (frame) {
    check_local_frame_output_names(
      output_names, group_vars, internal_names, set_id_name,
      set_id_is_internal, protected_sources
    )
  }
  new_local_summary_value(value, name)
}

# Check the names a local frame actually produced before dplyr can replace a
# grouping key or share source with a same-named column in its summary mask.
check_local_frame_output_names <- function(output_names, group_vars,
                                           internal_names, set_id_name,
                                           set_id_is_internal,
                                           protected_sources) {
  check_summary_output_names(
    output_names, group_vars, internal_names, set_id_name,
    set_id_is_internal
  )
  duplicate <- intersect(output_names, protected_sources)
  if (length(duplicate) > 0L) {
    # cli reads this source name from the condition template below.
    source <- duplicate[[1L]] # nolint: object_usage_linter.
    abort_marginplyr(c(
      paste0(
        "Contextual share requires source summary {.var {source}} ",
        "to be defined exactly once."
      ),
      i = "Use one uniquely named ordinary summary."
    ))
  }
  invisible(NULL)
}

# An unnamed result may be a frame regardless of its written expression.
# Wrap its value so frames expand and scalars keep their dplyr-assigned name.
# The caller supplies one branch-local type state per dot.
wrap_local_frame_summaries <- function(dots, group_vars, internal_names,
                                       set_id_name, set_id_is_internal,
                                       share_sources,
                                       source_definitions,
                                       source_reads,
                                       auto_names, type_states,
                                       alias_state = NULL) {
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (nzchar(rlang::names2(dots)[[i]])) {
      next
    }
    expr <- rlang::quo_get_expr(dot)
    if (identical(data_frame_valued_summary_kind(expr), "across")) {
      parsed <- parse_across_arguments(expr)
      unpack <- parsed$unpack
      # Keep across at the top level so dplyr owns its expansion context.
      # Share checks use the names that expansion actually produces.
      empty_unpack <- (
        parsed$unpack_index > 0L &&
          rlang::is_missing(parsed$call_args[[parsed$unpack_index]])
      ) || (
        rlang::is_quosure(unpack) &&
          rlang::is_missing(rlang::quo_get_expr(unpack))
      )
      unpack_is_false <- across_unpack_is_false(unpack, rlang::quo_get_env(dot))
      protected_sources <- setdiff(share_sources, source_definitions[[i]])
      if ((empty_unpack && is.null(alias_state)) ||
            (unpack_is_false && length(share_sources) == 0L)) {
        next
      }
      if (unpack_is_false || (!isTRUE(unpack) && !is.character(unpack))) {
        # Keep across at the top level so dplyr expands a FALSE unpack in its
        # ordinary phase. The two arguments check names at whichever phase
        # dplyr chooses from the value it actually reads.
        call_args <- parsed$call_args
        state <- new.env(parent = emptyenv())
        if (unpack_is_false || empty_unpack) {
          state$unpack <- FALSE
        }
        name_rules <- list(
          group_vars = group_vars,
          internal_names = internal_names,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal,
          protected_sources = protected_sources,
          source_reads = source_reads[[i]],
          alias_state = alias_state,
          position = i,
          type_state = type_states[[i]]
        )
        dot_env <- rlang::quo_get_env(dot)
        binding <- new_margin_internal_names(
          1L, c(all.names(expr), ls(dot_env, all.names = TRUE)),
          "..marginplyr_glue_name_"
        )
        while (exists(binding, envir = dot_env, inherits = TRUE)) {
          binding <- paste0(binding, "_")
        }
        dot_env <- rlang::env(dot_env)
        # Keep the rules in the checker so a column in the glue mask cannot
        # shadow them. The caller's template retains its own lookup.
        rlang::env_poke(dot_env, binding, local_checked_glue_name(name_rules))
        state$glue_name_expr <- binding
        index <- parsed$unpack_index
        if (index > 0L && !empty_unpack) {
          call_args[[index]] <- rlang::call2(
            marginplyr_private_call("local_checked_unpack_arg"),
            call_args[[index]], state
          )
        }
        index <- parsed$fns_index
        if (index > 0L && !rlang::is_missing(call_args[[index]])) {
          if (is_numbered_dot_reference(parsed$fns)) {
            # Runtime across must capture the original forwarded promise.
            # Its cached value is available when FALSE unpacking names outputs.
            state$forwarded_fns <- parsed$fns
            state$fns_env <- rlang::quo_get_env(dot)
          } else {
            call_args[[index]] <- rlang::call2(
              marginplyr_private_call("local_checked_across_fns_arg"),
              call_args[[index]], state
            )
          }
        }
        index <- parsed$names_index
        if (index > 0L) {
          value <- if (rlang::is_missing(call_args[[index]])) {
            NULL
          } else {
            call_args[[index]]
          }
          call_args[[index]] <- rlang::call2(
            marginplyr_private_call("local_checked_across_names_arg"),
            value, state
          )
        } else {
          call_args <- append(call_args, list(.names = rlang::call2(
            marginplyr_private_call("local_checked_across_names_arg"),
            NULL, state
          )))
        }
        expr <- rebuild_static_call(expr, call_args)
        dots[[i]] <- rlang::new_quosure(expr, env = dot_env)
        next
      }
    }
    expr <- rlang::call2(
      marginplyr_private_call("local_assigned_summary_value"),
      expr, auto_names[[i]], group_vars, internal_names,
      set_id_name,
      set_id_is_internal,
      setdiff(share_sources, source_definitions[[i]]), type_states[[i]],
      alias_state, i
    )
    dots[[i]] <- rlang::new_quosure(expr, env = wrapped_summary_env(dot))
  }
  dots
}

# Returns dplyr's unpack argument, adding a name check only for an unpacked
# frame. Its caller places it in the original top-level across call.
local_checked_unpack_arg <- function(value, state) {
  state$unpack <- value
  if (!isTRUE(value) &&
        !(is.character(value) && length(value) == 1L && !is.na(value))) {
    return(value)
  }
  spec <- if (isTRUE(value)) "{outer}_{inner}" else value
  paste0(
    "{", state$glue_name_expr, "(",
    deparse1(spec), ", unpacked = TRUE)}"
  )
}

# Returns the ordinary function argument and records whether dplyr will name
# one function or a list. dplyr evaluates this before its naming template.
local_checked_across_fns_arg <- function(value, state) {
  state$fns_kind <- if (is.function(value) || rlang::is_formula(value)) {
    "single"
  } else if (is.list(value)) {
    "list"
  } else {
    NULL
  }
  value
}

# Returns dplyr's naming template, checking expanded outer names when it has
# read FALSE unpacking. A missing template uses dplyr's function-count default.
local_checked_across_names_arg <- function(value, state) {
  if (!isFALSE(state$unpack)) {
    return(value)
  }
  if (is.null(value)) {
    if (!is.null(state$forwarded_fns)) {
      local_checked_across_fns_arg(
        rlang::eval_bare(state$forwarded_fns, state$fns_env), state
      )
    }
    value <- if (identical(state$fns_kind, "list")) {
      "{.col}_{.fn}"
    } else {
      "{.col}"
    }
  }
  if (!(is.character(value) && length(value) == 1L && !is.na(value))) {
    return(value)
  }
  paste0("{", state$glue_name_expr, "(", deparse1(value), ")}")
}

# Bind the rules for one dot to a checker called from dplyr's glue mask.
# Tag errors from the caller's template to remove the outer interpolation.
# Wrap generated refusals separately so their ownership ends at restatement.
# Unpacking supplies the expanded frame fields for branch schema validation.
local_checked_glue_name <- function(name_rules) {
  force(name_rules)
  function(spec, unpacked = FALSE) {
    names <- tryCatch(
      as.character(glue::glue(spec, .envir = parent.frame())),
      error = function(cnd) {
        rlang::abort(
          "Failed to evaluate the caller's name template.",
          class = "marginplyr_inner_glue_error",
          parent = cnd
        )
      }
    )
    with_generated_summary_check({
      check_share_alias_reads(names, name_rules$source_reads)
      record_local_summary_aliases(
        names, name_rules$alias_state, name_rules$position
      )
      check_local_frame_output_names(
        names, name_rules$group_vars, name_rules$internal_names,
        name_rules$set_id_name, name_rules$set_id_is_internal,
        name_rules$protected_sources
      )
    })
    if (unpacked) {
      type_state <- name_rules$type_state
      type_state$unpacked_fields <- union(type_state$unpacked_fields, names)
    }
    names
  }
}

# Retain the actual outputs of one local dot for later source selections.
# Its state is reset per branch; a dot may report names once per group.
record_local_summary_aliases <- function(names, state, position) {
  if (!is.null(state)) {
    state$summary_aliases[[position]] <- unique(c(
      state$summary_aliases[[position]], names
    ))
  }
  invisible(NULL)
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
# `selection_state` carries local internal keys and share placeholders to
# deferred selections. `frame_state` carries output names to the union adapter;
# direct adapter callers receive empty protections and their dots' auto names.
# `sql_share_sources` names the projections guarded by the two SQL adapters.
new_summary_arguments <- function(dots,
                                  labels = summary_argument_labels(dots),
                                  assigned_names = rep(
                                    NA_character_,
                                    length(dots)
                                  ),
                                  selection_state = NULL,
                                  sql_share_sources = character()) {
  stopifnot(
    is.list(dots),
    is.character(labels),
    length(labels) == length(dots),
    is.character(assigned_names),
    length(assigned_names) == length(dots)
  )
  frame_state <- if (is.environment(selection_state)) {
    selection_state
  } else {
    list(
      share_sources = character(),
      source_definitions = rep(list(character()), length(dots)),
      source_reads = rep(list(list()), length(dots)),
      auto_names = vapply(dots, function(dot) {
        dplyr_auto_name(rlang::quo_get_expr(dot))
      }, character(1))
    )
  }
  list(
    dots = dots,
    labels = labels,
    assigned_names = assigned_names,
    selection_state = selection_state,
    frame_state = frame_state,
    sql_share_sources = sql_share_sources
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
  has_nested_summary_selection(expr)
}

# Whether a call's evaluated parts contain an ordinary selection helper.
has_nested_summary_selection <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(FALSE)
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
                                     call,
                                     local_data = NULL) {
  stopifnot(inherits(plan, "margin_grouping_plan"))
  group_vars <- c(plan$by, plan$dimensions)
  if (!is.null(local_data) && first_across_needs_planning(dots)) {
    return(plan_first_local_across(
      dots, local_data, data_vars, group_vars, plan, call,
      planner = function(resolved) {
        plan_summary_expressions(
          resolved, data_proxy, data_vars, plan, backend_kind,
          set_id_name, call
        )
      }
    ))
  }
  # Read before anything is rewritten, which is the whole of what makes these
  # the caller's own labels: every rewrite below runs after this line, and ADR
  # 0007 has already captured the dots at the public verb.
  caller_labels <- summary_argument_labels(dots)
  original_dots <- dots
  defer_local <- identical(backend_kind, "local")
  # Arrow owns expression translation and does not run dplyr's pick expansion.
  shield_independent_masks <- backend_kind != "arrow"
  has_shares <- any(vapply(
    dots,
    function(dot) contains_share_helper(rlang::quo_get_expr(dot)),
    logical(1)
  ))
  deferred <- rep(FALSE, length(dots))
  nested_deferred <- rep(FALSE, length(dots))
  if (defer_local) {
    independent_shares <- shares_use_named_summaries(dots)
    prior_ordinary <- FALSE
    for (i in seq_along(dots)) {
      expr <- rlang::quo_get_expr(dots[[i]])
      if (contains_share_helper(expr)) {
        next
      }
      selection_kind <- data_frame_valued_summary_kind(expr)
      # A nested selection can execute in a scope created by its enclosing
      # expression; that scope is not available during share planning.
      deferred[[i]] <- !has_shares || (
        contains_summary_selection(expr) &&
          (prior_ordinary || nzchar(rlang::names2(dots)[[i]]) ||
             independent_shares ||
             !(identical(selection_kind, "across") ||
                 identical(selection_kind, "pick")))
      )
      # Share planning still needs this outer selector's output names; its
      # nested selections wait until the per-dot execution state exists.
      nested_deferred[[i]] <- !deferred[[i]] &&
        has_nested_summary_selection(expr)
      prior_ordinary <- TRUE
    }
  }
  selection_proxy <- summary_selection_proxy(
    data_proxy,
    data_vars = data_vars,
    group_vars = group_vars
  )
  if (
    defer_local && length(dots) > 0L &&
      !contains_share_helper(rlang::quo_get_expr(dots[[1L]]))
  ) {
    preflight_static_selection(dots[[1L]], selection_proxy)
  }
  if (!defer_local || has_shares) {
    dots <- resolve_summary_selections(
      dots,
      group_vars = group_vars,
      caller_labels = caller_labels,
      shield_independent_masks = shield_independent_masks,
      skip_shares = TRUE,
      defer_local = deferred,
      defer_nested_local = nested_deferred,
      skip_nested_deferred = TRUE,
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
    dots <- cache_local_context_names(dots, selection_proxy, group_vars)
  }
  predictable_names <- if (!defer_local) {
    character()
  } else if (has_shares) {
    cached_local_across_names(dots)
  } else {
    predictable_local_across_names(dots, names(selection_proxy))
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
      validate_ordinary_cardinality = identical(backend_kind, "dtplyr"),
      defer_local = defer_local
    )
  } else {
    list(
      dots = dots,
      requests = list(),
      cardinality = list(),
      ordinary_cardinality = list(),
      origin_positions = seq_along(dots)
    )
  }
  # Share planning is the one step that moves a dot, so it reports where each
  # dot it produced came from and both per-dot vectors are subscripted by that.
  # Every other rewrite here answers one dot with one dot in place.
  caller_labels <- caller_labels[summary_plan$origin_positions]
  assigned_names <- named$assigned_names[summary_plan$origin_positions]
  deferred <- deferred[summary_plan$origin_positions]
  nested_deferred <- nested_deferred[summary_plan$origin_positions]
  selection_state <- if (defer_local) new.env(parent = emptyenv()) else NULL
  if (defer_local) {
    selection_state$internal_names <- character()
    selection_state$share_aliases <- character()
    selection_state$auto_names <- vapply(original_dots, function(dot) {
      dplyr_auto_name(rlang::quo_get_expr(dot))
    }, character(1))[summary_plan$origin_positions]
    selection_state$share_sources <- unique(share_source_names(
      summary_plan$requests
    ))
    selection_state$source_definitions <- rep(
      list(character()), length(summary_plan$dots)
    )
    for (record in summary_plan$cardinality) {
      position <- record$position
      selection_state$source_definitions[[position]] <- unique(c(
        selection_state$source_definitions[[position]],
        record$source_summary
      ))
    }
    selection_state$initial_aliases <- lapply(
      seq_along(summary_plan$dots), function(i) {
        if (is.na(assigned_names[[i]])) {
          rlang::names2(summary_plan$dots)[[i]]
        } else {
          character()
        }
      }
    )
    selection_state$source_reads <- lapply(
      seq_along(summary_plan$dots),
      function(i) {
        later <- Filter(function(record) record$position > i,
                        summary_plan$cardinality)
        lapply(later, function(record) {
          source <- summary_plan$dots[[record$position]]
          expr <- record$dependency_expr
          record$symbols <- expression_data_symbols(expr, lookup = list(
            env = rlang::quo_get_env(source),
            mask = c(
              names(selection_proxy),
              rlang::names2(summary_plan$dots)[seq_len(record$position - 1L)]
            ),
            rebound = lookup_rebound_names(expr), deferred_selection = TRUE
          ))
          record
        })
      }
    )
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
      seq_along(summary_plan$dots),
      function(i) {
        position <- summary_plan$origin_positions[[i]]
        state <- new.env(parent = selection_state)
        state$position <- i
        state$source_checks <- Filter(function(record) {
          record$position == i
        }, summary_plan$cardinality)
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
    across_backend = backend_kind,
    shield_independent_masks = shield_independent_masks,
    defer_local = deferred,
    defer_nested_local = nested_deferred,
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
  if (length(summary_plan$ordinary_cardinality) > 0L) {
    summary_plan$dots <- wrap_dtplyr_share_ordinary(
      summary_plan$dots,
      checks = summary_plan$ordinary_cardinality,
      call = call
    )
  }
  list(
    summaries = new_summary_arguments(
      summary_plan$dots,
      caller_labels,
      assigned_names,
      selection_state = selection_state,
      sql_share_sources = if (backend_kind %in% c("duckdb", "postgres")) {
        share_source_names(summary_plan$requests)
      } else {
        character()
      }
    ),
    requests = summary_plan$requests,
    predictable_names = predictable_names
  )
}

# Whether share planning needs the first direct local across call's names.
# Only the shape dplyr expands before groups uses this preparation (ADR 0019).
first_across_needs_planning <- function(dots) {
  if (length(dots) == 0L || nzchar(rlang::names2(dots)[[1L]]) ||
        !rlang::is_call(rlang::quo_get_expr(dots[[1L]]), "across",
                        ns = c("", "dplyr")) ||
        contains_share_helper(rlang::quo_get_expr(dots[[1L]])) ||
        shares_use_named_summaries(dots)) {
    return(FALSE)
  }
  parsed <- parse_across_arguments(rlang::quo_get_expr(dots[[1L]]))
  if (length(known_across_function_names(
    parsed, rlang::quo_get_env(dots[[1L]])
  )) == 0L) {
    # An unresolved first source stays with source planning before its
    # function argument can run.
    static_dots <- Filter(function(dot) {
      expr <- rlang::quo_get_expr(dot)
      if (!contains_share_helper(expr) || is_share_helper_call(expr)) {
        return(TRUE)
      }
      selection <- unwrap_injected_quosure(parse_across_arguments(expr)$cols)
      is_name_part(selection) || is.character(selection)
    }, dots)
    if (!shares_use_named_summaries(static_dots)) return(FALSE)
  }
  length(parsed$additional) == 0L && any(vapply(dots, function(dot) {
    contains_share_helper(rlang::quo_get_expr(dot))
  }, logical(1))) && across_unpack_is_false(
    parsed$unpack,
    rlang::quo_get_env(dots[[1L]])
  )
}

# Resolve the first selector once in dplyr's expansion context, then plan with
# that selection. The zero-row carrier evaluates no input rows; tidyselect
# receives local columns. Independent summaries resume their original selector
# in later branches, while share sources retain their planned schema.
plan_first_local_across <- function(dots, data, data_vars, group_vars,
                                    plan, call, planner) {
  original <- dots[[1L]]
  expr <- rlang::quo_get_expr(original)
  env <- rlang::quo_get_env(original)
  parsed <- parse_across_arguments(expr)
  unknown_fns <- length(known_across_function_names(parsed, env)) == 0L
  columns <- setdiff(data_vars, group_vars)
  # Like dplyr's mask, use stored columns without subclass conversion.
  input <- vctrs::new_data_frame(data, n = nrow(data))
  selection_data <- input[columns]
  carrier <- vctrs::vec_slice(input, integer())
  state <- new.env(parent = emptyenv())
  state$planning <- TRUE
  state$warnings <- list()
  state$fns_warnings <- list()
  state$names_warnings <- list()
  state$planning_warnings <- list()
  nested_state <- rlang::env(
    emptyenv(), forbidden_names = character(), internal_names = character(),
    share_aliases = character(), source_checks = list()
  )
  capture_fns <- function(value) {
    state$fns_env <- parent.frame()
    state$fns <- list(withCallingHandlers(value, warning = function(cnd) {
      if (!state$planning) return()
      state$fns_warnings <- c(state$fns_warnings, list(cnd))
      invokeRestart("muffleWarning")
    }))
    value
  }
  capture_names <- function(value) {
    withCallingHandlers(value, warning = function(cnd) {
      state$names_warnings <- c(state$names_warnings, list(cnd))
      invokeRestart("muffleWarning")
    })
  }
  resolve <- function() {
    state$result <- tryCatch(withCallingHandlers({
      state$selecting <- TRUE
      # dplyr exposes a plain column list while resolving across selectors.
      selected <- resolve_summary_selection(
        rewrite_grouping_expr(
          parsed$cols, plan, grouping_set = plan$sets[[1L]],
          sql = FALSE, con = NULL
        ), env, as.list(selection_data)
      )
      state$selecting <- FALSE
      args <- parsed$call_args
      if (parsed$names_index > 0L) {
        args[parsed$names_index] <- list(rewrite_grouping_expr(
          parsed$names, plan, grouping_set = plan$sets[[1L]],
          sql = FALSE, con = NULL
        ))
        if (contains_grouping_helper(parsed$names)) {
          # Branch-rewritten templates report their first warning in the same
          # expansion context as later grains, so ADR 0021 can identify repeats.
          args[[parsed$names_index]] <- rlang::call2(
            capture_names, args[[parsed$names_index]]
          )
        }
      }
      selection <- summary_all_of_expr(selected, selection_data)
      if (parsed$cols_index == 0L) {
        args <- append(list(.cols = selection), args)
      } else {
        args[[parsed$cols_index]] <- selection
      }
      if (unknown_fns && parsed$fns_index > 0L) {
        # Forcing a promise does not establish static source provenance.
        args[[parsed$fns_index]] <- rlang::call2(
          "identity", parsed$fns, .ns = "base"
        )
      }
      dots[[1L]] <- rlang::new_quosure(rebuild_static_call(expr, args), env)
      planner(dots)
    }, warning = function(cnd) {
      if (state$selecting) {
        state$warnings <- c(state$warnings, list(cnd))
      } else {
        state$planning_warnings <- c(state$planning_warnings, list(cnd))
      }
      invokeRestart("muffleWarning")
    }), error = function(cnd) {
      state$error <- cnd
      NULL
    })
    if (!is.null(state$error) && state$selecting) stop(state$error)
    character()
  }
  probe <- rlang::quo(dplyr::across(dplyr::all_of(resolve()), identity))
  if (parsed$fns_index > 0L &&
        !rlang::is_missing(parsed$call_args[[parsed$fns_index]])) {
    probe_expr <- rlang::quo_get_expr(probe)
    fns <- rewrite_summary_selections(
      parsed$fns, env, selection_data,
      defer_local = TRUE, group_vars = group_vars,
      forbidden_names = nested_state
    )
    first_fns <- rewrite_grouping_expr(
      fns, plan, grouping_set = plan$sets[[1L]], sql = FALSE, con = NULL
    )
    probe_expr[[3L]] <- rlang::new_quosure(
      rlang::call2(capture_fns, first_fns), env
    )
    probe <- rlang::quo_set_expr(probe, probe_expr)
  }
  conditions <- new_branch_conditions(character(), call)
  on.exit(report_branch_warnings(conditions), add = TRUE)
  with_branch_conditions(
    dplyr::summarise(carrier, !!probe, .by = dplyr::all_of(plan$sets[[1L]])),
    conditions, restatements = branch_argument_map(
      list(probe), summary_argument_labels(list(original))
    )
  )
  if (!is.null(state$error)) {
    conditions$error <- state$error
    warnings <- c(state$fns_warnings, state$warnings, state$names_warnings,
                  state$planning_warnings)
    for (cnd in warnings) {
      buffer_branch_warning(cnd, conditions, character())
    }
    stop(state$error)
  }
  for (cnd in state$planning_warnings) warning(cnd)
  result <- state$result
  state$result <- NULL
  state$planning <- FALSE
  # Captured closures now use the first dot's live selection state.
  parent.env(nested_state) <- result$summaries$selection_state$by_dot[[1L]]
  rlang::env_unbind(nested_state, ls(nested_state, all.names = TRUE))
  result$summaries$labels[[1L]] <- summary_argument_labels(list(original))[[1L]]
  if (!is.null(state$fns)) {
    first <- result$summaries$dots[[1L]]
    result$summaries$dots[[1L]] <- rlang::new_quosure(
      cache_first_across_fns(rlang::quo_get_expr(first), state),
      rlang::quo_get_env(first)
    )
  }
  if (length(result$summaries$selection_state$source_definitions[[1L]]) == 0L) {
    first <- result$summaries$dots[[1L]]
    first_expr <- rlang::quo_get_expr(first)
    first_args <- parse_across_arguments(first_expr)
    args <- first_args$call_args
    args[[first_args$cols_index]] <- rlang::call2(
      "c", rlang::call2(
        replay_local_selector_warnings, state
      ), first_args$cols
    )
    result$summaries$dots[[1L]] <- rlang::new_quosure(
      rebuild_static_call(first_expr, args), rlang::quo_get_env(first)
    )
    result$summaries$subsequent_first <- resolve_summary_selections(
      list(original), group_vars, result$summaries$labels[[1L]],
      selection_data, defer_local = TRUE,
      forbidden_names = result$summaries$selection_state
    )[[1L]]
  } else {
    for (cnd in state$warnings) warning(cnd)
    # The source check wraps across(), so dplyr cannot expand it at the top
    # level. Prepare its functions once per grain in that expansion context.
    if (!is.null(state$fns)) {
      result$summaries$prepare_first_across <- function(grouping_set, set_id,
                                                        conditions) {
        capture <- if (set_id == 1L) {
          if (length(state$fns_warnings) == 0L) return(invisible(NULL))
          rlang::new_quosure(rlang::call2(first_local_across_fns, state), env)
        } else {
          branch_fns <- rewrite_grouping_expr(
            fns, plan, grouping_set = grouping_set, sql = FALSE, con = NULL
          )
          rlang::new_quosure(rlang::call2(capture_fns, branch_fns), env)
        }
        probe <- rlang::quo(dplyr::across(character(), !!capture))
        with_branch_conditions(
          dplyr::summarize(carrier, !!probe, .by = dplyr::all_of(grouping_set)),
          conditions, restatements = branch_argument_map(
            list(probe), summary_argument_labels(list(original))
          )
        )
        invisible(NULL)
      }
    }
  }
  if (length(state$names_warnings) > 0L) {
    prepare <- result$summaries$prepare_first_across
    result$summaries$prepare_first_across <- function(grouping_set, set_id,
                                                      conditions) {
      if (set_id == 1L) {
        replay <- rlang::call2(replay_local_name_warnings, state)
        probe <- rlang::quo(dplyr::across(character(), !!replay))
        with_branch_conditions(
          dplyr::summarize(carrier, !!probe, .by = dplyr::all_of(grouping_set)),
          conditions, restatements = branch_argument_map(
            list(probe), summary_argument_labels(list(original))
          )
        )
      }
      if (!is.null(prepare)) prepare(grouping_set, set_id, conditions)
      invisible(NULL)
    }
  }
  result
}

# Replay first-grain template warnings during dplyr's function preparation.
# The identity function leaves the empty probe selection without output.
replay_local_name_warnings <- function(state) {
  warnings <- state$names_warnings
  state$names_warnings <- list()
  for (cnd in warnings) warning(cnd)
  identity
}

# Replace the first planned across function argument after source analysis.
# A source check may wrap the call; its original syntax remains the planner's
# evidence for source eligibility rather than the cached function value.
cache_first_across_fns <- function(expr, state) {
  if (!is_across_call(expr)) {
    expr[[2L]] <- cache_first_across_fns(expr[[2L]], state)
    return(expr)
  }
  parsed <- parse_across_arguments(expr)
  args <- parsed$call_args
  args[[parsed$fns_index]] <- rlang::call2(
    first_local_across_fns, state
  )
  rebuild_static_call(expr, args)
}

# Reuse functions evaluated in this grain's expansion context. A function or
# formula created in the carrier's mask enters the current mask so dplyr can
# apply its ordinary lambda handling; factory closures keep their environments.
first_local_across_fns <- function(state) {
  warnings <- state$fns_warnings
  state$fns_warnings <- list()
  for (cnd in warnings) warning(cnd)
  mask <- parent.frame()
  restore <- function(value) {
    if (is.list(value)) return(lapply(value, restore))
    has_env <- (is.function(value) && !is.primitive(value)) ||
      rlang::is_formula(value)
    if (has_env && identical(rlang::get_env(value), state$fns_env)) {
      return(rlang::set_env(value, mask))
    }
    value
  }
  restore(state$fns[[1L]])
}

# Replay a captured selector's warnings inside its first dplyr branch so the
# ordinary branch handler can recognize repetitions from later grouping sets.
# Returning no columns leaves the cached selection beside this call unchanged.
replay_local_selector_warnings <- function(state) {
  warnings <- state$warnings
  state$warnings <- list()
  for (cnd in warnings) warning(cnd)
  character()
}

# Whether local name prediction can read a selection as known columns.
# Other selections stay with their ordinary resolver.
is_literal_summary_selection <- function(cols, available, allow_names = TRUE) {
  cols <- unwrap_injected_quosure(cols)
  if (rlang::is_missing(cols)) {
    return(FALSE)
  }
  if (is.character(cols)) {
    return(all(cols %in% available))
  }
  if (rlang::is_symbol(cols)) {
    return(allow_names && rlang::as_string(cols) %in% available)
  }
  if (rlang::is_call(cols, c("c", "("))) {
    parts <- as.list(cols)[-1L]
    return(all(vapply(
      parts, is_literal_summary_selection, logical(1),
      available = available, allow_names = allow_names
    )))
  }
  if (allow_names && identical(static_call_name(cols), "all_of") &&
        length(cols) == 2L) {
    return(is_literal_summary_selection(
      cols[[2L]], available, allow_names = FALSE
    ))
  }
  FALSE
}

# Resolves local names using a known summary schema and context-only templates.
# Selector helpers must take constant arguments without caller callbacks.
cache_local_context_names <- function(dots, selection_proxy, group_vars) {
  available <- names(selection_proxy)
  complete <- TRUE
  rebound <- character()
  for (i in seq_along(dots)) {
    complete <- complete && !unresolved_lookup_name() %in% rebound
    dot <- dots[[i]]
    expr <- rlang::quo_get_expr(dot)
    name <- rlang::names2(dots)[[i]]
    lookup <- list(
      env = rlang::quo_get_env(dot), mask = c(available, group_vars),
      columns = available, rebound = rebound
    )
    if (contains_share_helper(expr)) next
    if (nzchar(name)) {
      rebound <- context_name_rebindings(expr, lookup, selection_proxy)
      available <- unique(c(available, name))
      next
    }
    output <- NULL
    if (is_across_call(expr)) {
      parsed <- parse_across_arguments(expr)
      env <- rlang::quo_get_env(dot)
      output <- attr(parsed$names, "marginplyr_across_names", exact = TRUE)
      template <- known_context_constant(parsed$names, lookup)
      if (is.null(output) && complete && !is.null(template) &&
            (is.null(parsed$unpack) || isFALSE(parsed$unpack)) &&
            is_static_name_selection(parsed$cols, lookup) &&
            (is.null(template$value) ||
               is_context_name_template(template$value, lookup)) &&
            known_context_fns(parsed, lookup)) {
        proxy <- vctrs::new_data_frame(
          stats::setNames(rep(list(NA), length(available)), available), n = 1L
        )
        args <- parsed$call_args
        selected <- resolve_summary_selection(parsed$cols, env, proxy)
        selection <- summary_all_of_expr(selected, proxy)
        if (parsed$cols_index == 0L) {
          args <- append(list(.cols = selection), args)
        } else {
          args[[parsed$cols_index]] <- selection
        }
        prepared <- rebuild_static_call(expr, args)
        prepared_args <- parse_across_arguments(prepared)
        args <- prepared_args$call_args
        if (prepared_args$names_index > 0L) {
          args[prepared_args$names_index] <- list(template$value)
        }
        output <- known_across_output_names(
          rebuild_static_call(prepared, args), env, proxy
        )
        frozen <- resolved_across_name_template(output)
        if (prepared_args$names_index == 0L) {
          args <- append(args, list(.names = frozen))
        } else {
          args[[prepared_args$names_index]] <- frozen
        }
        dot <- rlang::quo_set_expr(dot, rebuild_static_call(expr, args))
        dots[[i]] <- dot
      }
      if (is.null(output)) complete <- FALSE
      available <- unique(c(available, output))
    } else if (!is_static_spelling_call(expr, "selection", "pick")) {
      complete <- FALSE
    }
    rebound <- context_name_rebindings(
      rlang::quo_get_expr(dot), lookup, selection_proxy
    )
  }
  dots
}

# Caller code may change bindings without a visible assignment at this site.
# Known calculations over plain columns preserve the proof for later names.
context_name_rebindings <- function(expr, lookup, proxy) {
  rebound <- c(lookup$rebound, lookup_rebound_names(
    expr, lookup$env, lookup$mask
  ))
  if (!context_summary_bindings_safe(expr, lookup, proxy)) {
    rebound <- c(rebound, unresolved_lookup_name())
  }
  unique(rebound)
}

# A conservative prefix proof; unknown callees and class dispatch stay opaque.
# Frozen across names still require safe functions on plain input columns.
context_summary_bindings_safe <- function(expr, lookup, proxy) {
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
    return(context_summary_bindings_safe(
      rlang::quo_get_expr(expr), lookup, proxy
    ))
  }
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_string(expr)
    if (name %in% lookup$mask) {
      return(name %in% names(proxy) && is.atomic(proxy[[name]]) &&
               !is.object(proxy[[name]]))
    }
    return(!is.null(known_context_constant(expr, lookup)))
  }
  if (!rlang::is_call(expr)) {
    return(!is.object(expr) && (is.atomic(expr) || is.null(expr)))
  }
  if (is_parenthesized(static_call_head(expr))) return(FALSE)
  if (is_across_call(expr)) {
    parsed <- parse_across_arguments(expr)
    if (is.null(attr(parsed$names, "marginplyr_across_names", exact = TRUE))) {
      return(FALSE)
    }
    if (!is.null(parsed$unpack) && !isFALSE(parsed$unpack)) return(FALSE)
    if (!all(vapply(parsed$additional_args, context_summary_bindings_safe,
                    logical(1), lookup = lookup, proxy = proxy))) return(FALSE)
    cols <- static_call_args(parsed$cols)
    if (length(cols) != 1L) return(FALSE)
    selected <- static_character_value(cols[[1L]])
    if (is.null(selected)) return(FALSE)
    fns <- if (is_known_function_list_call(parsed$fns, lookup$env)) {
      static_call_args(parsed$fns)
    } else {
      list(parsed$fns)
    }
    return(all(vapply(fns, function(fn) {
      if (is.null(fn)) return(FALSE)
      context_summary_bindings_safe(
        rlang::call2(fn, !!!rlang::syms(unname(selected))), lookup, proxy
      )
    }, logical(1))))
  }
  name <- static_call_name(expr)
  namespace <- static_call_ns(expr)
  if (identical(name, "n") && length(static_call_args(expr)) == 0L) {
    if (!is.null(namespace)) return(identical(namespace, "dplyr"))
    return(is_unrebound_name(name, lookup) && identical(
      ordinary_function_binding(name, lookup$env, "dplyr"), dplyr::n
    ))
  }
  if (!is.null(name) && name %in% c("$", "[[") && is.null(namespace) &&
        length(expr) == 3L && rlang::is_symbol(expr[[2L]], ".data")) {
    if (!is_unrebound_name(name, lookup)) return(FALSE)
    binding <- ordinary_function_binding(name, lookup$env)
    if (!identical(binding, getExportedValue("base", name))) return(FALSE)
    column <- if (rlang::is_symbol(expr[[3L]]) && name == "$") {
      rlang::as_string(expr[[3L]])
    } else {
      static_character_value(expr[[3L]])
    }
    return(length(column) == 1L && context_summary_bindings_safe(
      rlang::sym(column), lookup, proxy
    ))
  }
  operations <- c("sum", "prod", "min", "max", "mean", "length", "abs",
                  "c", "+", "-", "*", "/", "^", "%%", "%/%", ":", "(", "{")
  if (is.null(name) || !name %in% operations ||
        (!is.null(namespace) && namespace != "base")) return(FALSE)
  if (is.null(namespace)) {
    if (!is_unrebound_name(name, lookup)) return(FALSE)
    binding <- ordinary_function_binding(name, lookup$env)
    if (!identical(binding, getExportedValue("base", name))) return(FALSE)
  }
  all(vapply(static_call_args(expr), context_summary_bindings_safe,
             logical(1), lookup = lookup, proxy = proxy))
}

# Reads only already resolved output names from unnamed local across dots.
cached_local_across_names <- function(dots) {
  unlist(lapply(seq_along(dots), function(i) {
    expr <- rlang::quo_get_expr(dots[[i]])
    if (nzchar(rlang::names2(dots)[[i]]) || !is_across_call(expr)) {
      return(character())
    }
    attr(parse_across_arguments(expr)$names,
         "marginplyr_across_names", exact = TRUE)
  }), use.names = FALSE)
}

# Reads a literal or an existing plain caller constant without forcing a
# promise or active binding. Mask names remain owned by local execution.
known_context_constant <- function(expr, lookup) {
  if (rlang::is_missing(expr)) return(NULL)
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
    return(known_context_constant(
      rlang::quo_get_expr(expr), lookup
    ))
  }
  if (is_parenthesized(expr)) {
    return(known_context_constant(unparenthesized_value(expr), lookup))
  }
  if (is.null(expr) || (is.atomic(expr) && !is.object(expr))) {
    return(list(value = expr))
  }
  if (!rlang::is_symbol(expr) || rlang::as_string(expr) %in% lookup$mask) {
    return(NULL)
  }
  name <- rlang::as_string(expr)
  if (!is_unrebound_name(name, lookup)) return(NULL)
  current <- lookup$env
  while (!identical(current, emptyenv())) {
    if (exists(name, envir = current, inherits = FALSE)) {
      if (bindingIsActive(name, current) ||
            rlang::env_binding_are_lazy(current, name)) return(NULL)
      value <- get(name, envir = current, inherits = FALSE)
      if (is.null(value) || (is.atomic(value) && !is.object(value))) {
        return(list(value = value))
      }
      return(NULL)
    }
    current <- parent.env(current)
  }
  NULL
}

# Recognizes name-only tidyselect syntax on a known schema. Tidyselect owns
# bare helper bindings; helper arguments must be constant base expressions.
is_static_name_selection <- function(expr, lookup) {
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
    return(is_static_name_selection(
      rlang::quo_get_expr(expr), lookup
    ))
  }
  if (rlang::is_missing(expr)) return(FALSE)
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr) %in% lookup$columns)
  }
  if (!rlang::is_call(expr)) {
    return(!is.object(expr) && (is.character(expr) || is.numeric(expr)))
  }
  if (is_parenthesized(static_call_head(expr))) return(FALSE)
  name <- static_call_name(expr)
  namespace <- static_call_ns(expr)
  args <- static_call_args(expr)
  if (is.null(name)) return(FALSE)
  if (is_selection_walk_call(expr)) {
    return(all(vapply(args, is_static_name_selection, logical(1),
                      lookup = lookup)))
  }
  if (identical(name, "one_of") &&
        (is.null(namespace) || namespace %in% c("dplyr", "tidyselect"))) {
    # An absent column warns and .vars can change the positions returned.
    lookup$mask <- c(names(tidyselect::vars_select_helpers), ".data", ".env")
    return(!any(nzchar(rlang::names2(args))) && all(vapply(args, function(arg) {
      if (!is_context_only_name_expr(arg, lookup, allow_context = FALSE)) {
        return(FALSE)
      }
      constant <- known_context_constant(arg, lookup)
      value <- if (is.null(constant)) {
        static_character_value(arg)
      } else {
        constant$value
      }
      is.character(value) && is.null(dim(value)) &&
        all(value %in% lookup$columns)
    }, logical(1))))
  }
  helpers <- intersect(
    names(tidyselect::vars_select_helpers),
    c("all_of", "any_of", "contains", "ends_with", "everything", "last_col",
      "matches", "num_range", "starts_with")
  )
  if (is.null(name) || !name %in% helpers ||
        (!is.null(namespace) && !namespace %in% c("dplyr", "tidyselect"))) {
    return(is_context_only_name_expr(expr, lookup, allow_context = FALSE))
  }
  all(vapply(args, is_context_only_name_expr, logical(1),
             lookup = lookup, allow_context = FALSE))
}

# Whether prior visible mutations leave a lexical binding proof available.
is_unrebound_name <- function(name, lookup) {
  !any(c(name, unresolved_lookup_name()) %in% lookup$rebound)
}

# Function labels must remain known after earlier ordinary summaries run.
known_context_fns <- function(parsed, lookup) {
  fns <- unparenthesized_value(parsed$fns)
  if (rlang::is_symbol(fns) &&
        !is_unrebound_name(rlang::as_string(fns), lookup)) return(FALSE)
  operator <- if (rlang::is_call(fns, c("::", ":::"), ns = "")) {
    rlang::as_string(static_call_head(fns))
  } else {
    selection_namespace_operator(fns)
  }
  if (!is.null(operator) && !is_unrebound_name(operator, lookup)) return(FALSE)
  if (is_known_function_list_call(fns, lookup$env) &&
        is.null(static_call_ns(fns)) &&
        !is_unrebound_name("list", lookup)) return(FALSE)
  length(known_across_function_names(parsed, lookup$env)) > 0L
}

# Reads glue interpolations without evaluating them. The caller supplies a
# literal template and the lexical environment used to resolve its functions.
is_context_name_template <- function(template, lookup) {
  if (!is.character(template) || length(template) != 1L || is.na(template)) {
    return(FALSE)
  }
  expressions <- list()
  tryCatch({
    glue::glue(
      template, .envir = emptyenv(), .transformer = function(text, envir) {
        expressions[[length(expressions) + 1L]] <<- rlang::parse_expr(text)
        ""
      }
    )
    all(vapply(expressions, is_context_only_name_expr,
               logical(1), lookup = lookup))
  }, error = function(cnd) FALSE)
}

# Accepts only naming vectors, constants and these base operations on them.
# Bare function bindings must already be known without forcing caller code.
is_context_only_name_expr <- function(expr, lookup, allow_context = TRUE) {
  if (rlang::is_missing(expr)) return(FALSE)
  if (rlang::is_symbol(expr)) {
    if (allow_context) {
      return(rlang::as_string(expr) %in% c(".col", ".fn", "col", "fn"))
    }
    lookup$mask <- c(names(tidyselect::vars_select_helpers), ".data", ".env")
    return(!is.null(known_context_constant(expr, lookup)))
  }
  if (!rlang::is_call(expr)) {
    return(!is.object(expr) && (is.atomic(expr) || is.null(expr)))
  }
  if (is_parenthesized(static_call_head(expr))) return(FALSE)
  name <- static_call_name(expr)
  operations <- c(
    "c", "seq_along", "length", "rev", "match", "toupper", "("
  )
  if (!allow_context) operations <- c(operations, ":", "+", "-", "*", "/", "^")
  if (is.null(name) || !name %in% operations) return(FALSE)
  namespace <- static_call_ns(expr)
  if (!is.null(namespace)) {
    if (!identical(namespace, "base")) return(FALSE)
  } else if (!is_unrebound_name(name, lookup) || !identical(
    ordinary_function_binding(name, lookup$env), getExportedValue("base", name)
  )) {
    return(FALSE)
  }
  all(vapply(static_call_args(expr), is_context_only_name_expr,
             logical(1), lookup = lookup, allow_context = allow_context))
}

# Names an unnamed local `across()` only when both its columns and `.names`
# template are literal or already resolved. No caller function or summary
# expression runs while reserving internal key names from the input name list.
# Earlier visible mutations invalidate the lexical bindings they replace.
predictable_local_across_names <- function(dots, input_names) {
  available <- input_names
  predicted <- character()
  rebound <- character()
  arg_names <- rlang::names2(dots)
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    name <- arg_names[[i]]
    expr <- rlang::quo_get_expr(dot)
    lookup <- list(env = rlang::quo_get_env(dot), rebound = rebound)
    if (is_across_call(expr) && !nzchar(name) && !contains_share_helper(expr)) {
      parsed <- parse_across_arguments(expr)
      cols <- parsed$cols
      template <- parsed$names
      stable_unpack <- !rlang::is_symbol(parsed$unpack) ||
        is_unrebound_name(rlang::as_string(parsed$unpack), lookup)
      simple_cols <- is_literal_summary_selection(cols, available)
      literal_template <- !is.null(attr(
        template, "marginplyr_across_names", exact = TRUE
      )) || is.null(template) || (
        is.character(template) && length(template) == 1L &&
          !is.na(template) &&
          !grepl("{", gsub(
            "{.fn}", "", gsub("{.col}", "", template, fixed = TRUE),
            fixed = TRUE
          ), fixed = TRUE)
      )
      if (simple_cols && literal_template && stable_unpack &&
            known_context_fns(parsed, lookup)) {
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
    rebound <- union(rebound, lookup_rebound_names(
      expr, lookup$env, available, visible_removals = TRUE
    ))
  }
  predicted
}

# Checks the first summary selection's bare names and empty pair before label
# validation. External bindings stay with their normal selection resolver.
preflight_static_selection <- function(dot, selection_proxy) {
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
  env <- rlang::quo_get_env(dot)
  static_name <- rlang::is_symbol(selection) &&
    !is_numbered_dot_reference(selection) && (
    rlang::as_string(selection) %in% names(selection_proxy) ||
      !rlang::env_has(env, rlang::as_string(selection), inherit = TRUE)
  )
  if (static_name || empty_pair) {
    resolve_summary_selection(
      selection,
      env = env,
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
# The final resolution supplies `across_backend` for backend-specific
# rewrites after the planning pass has read the selection aliases.
resolve_summary_selections <- function(dots,
                                       group_vars,
                                       caller_labels,
                                       selection_proxy,
                                       across_backend = NULL,
                                       shield_independent_masks = TRUE,
                                       skip_shares = FALSE,
                                       defer_local = FALSE,
                                       defer_nested_local = FALSE,
                                       forbidden_names = character(),
                                       skip_nested_deferred = FALSE,
                                       skip_deferred = FALSE) {
  stopifnot(length(dots) == length(caller_labels))
  if (length(defer_local) == 1L) {
    defer_local <- rep(defer_local, length(dots))
  }
  stopifnot(length(defer_local) == length(dots))
  if (length(defer_nested_local) == 1L) {
    defer_nested_local <- rep(defer_nested_local, length(dots))
  }
  stopifnot(length(defer_nested_local) == length(dots))

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
          across_backend = across_backend,
          shield_independent_masks = shield_independent_masks,
          defer_local = defer_local[[i]],
          defer_nested_local = defer_nested_local[[i]],
          skip_nested_deferred = skip_nested_deferred,
          group_vars = group_vars,
          forbidden_names = dot_forbidden_names,
          top_level_unnamed = !nzchar(rlang::names2(dots)[[i]])
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
                                       across_backend = NULL,
                                       shield_independent_masks = TRUE,
                                       defer_local = FALSE,
                                       defer_nested_local = FALSE,
                                       skip_nested_deferred = FALSE,
                                       group_vars = character(),
                                       forbidden_names = character(),
                                       top_level_unnamed = FALSE,
                                       expand_local_pick = TRUE) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }

  # Eager selection resolution inside a quosure uses its captured environment.
  # Deferred local selections instead resolve in their execution scope.
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
  }

  if (top_level_unnamed &&
        is_static_spelling_call(expr, "selection", "across")) {
    call_args <- static_call_args(expr)
    forwarded <- vapply(call_args, rlang::is_symbol, logical(1), "...")
    if (any(forwarded)) {
      # Expand promises as dplyr does, before rewriting the selection.
      expanded <- match.call(dplyr::across, nameable_call(expr), envir = env)
      expr <- rebuild_static_call(expr, as.list(expanded)[-1L])
    }
  }

  # A selection the caller quoted is language data, so the walk descends past
  # it and gives the object back as it was written. Resolving it turned
  # `quote(dplyr::across(value, mean))` into
  # `quote(dplyr::across(dplyr::all_of("value"), mean))`, which is a different
  # expression to whatever the caller meant to carry (#179).
  # dplyr's pick expansion skips function bodies, defaults and computed heads.
  expand_nested_pick <- expand_local_pick &&
    !any(static_call_name(expr) %in% c("function", "~"))
  rewrite_part <- function(part, expand_pick = expand_nested_pick) {
    if (defer_nested_local && skip_nested_deferred) {
      return(part)
    }
    rewrite_summary_selections(
      part,
      env = env,
      data_proxy = data_proxy,
      across_backend = across_backend,
      shield_independent_masks = shield_independent_masks,
      defer_local = defer_local || defer_nested_local,
      group_vars = group_vars,
      forbidden_names = forbidden_names,
      expand_local_pick = expand_pick
    )
  }
  expr <- rewrite_evaluated_call_parts(
    expr, rewrite_part,
    rewrite_head = function(part) rewrite_part(part, expand_pick = FALSE)
  )

  if (shield_independent_masks && any(independent_mask_call_parts(expr))) {
    # dplyr expands pick() in call arguments, so hide the nested call in the
    # head. Construct the closure in the package namespace.
    return(rlang::call2(rlang::call2(
      nested_mask_thunk, expr
    )))
  }

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
      # An unnamed top-level across uses dplyr's expansion path (ADR 0019).
      rewritten <- rewrite_local_across_selection(
        expr, group_vars, forbidden_names,
        capture_forwarded = !(top_level_unnamed &&
                                identical(call_name, "across"))
      )
      if (top_level_unnamed && identical(call_name, "across") &&
            !is_parenthesized(rewritten)) {
        return(rewritten)
      }
      return(isolate_local_selection_call(rewritten, call_name))
    }
    return(rewrite_across_selection(
      expr,
      env,
      data_proxy,
      call_name = call_name,
      across_backend = across_backend
    ))
  }
  if (identical(call_name, "pick")) {
    if (defer_local) {
      rewritten <- rewrite_local_pick_selection(
        expr, group_vars, forbidden_names
      )
      if (expand_local_pick) return(rewritten)
      return(isolate_local_selection_call(rewritten, call_name))
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

# Bind the namespace operator for a rewritten local selection that dplyr
# evaluates instead of expanding syntactically.
isolate_local_selection_call <- function(expr, name) {
  if (!identical(static_spelling_name(expr, "selection"), name)) return(expr)
  rebuild_static_call(
    expr, static_call_args(expr),
    head = rlang::call2(base::`::`, rlang::sym("dplyr"), rlang::sym(name))
  )
}

# Returns a zero-argument closure for the supplied nested call.
# Calling the closure forces that promise in the original caller's scope.
nested_mask_thunk <- function(value) {
  function() value
}

# Builds a selection in dplyr's active selection scope, retaining an explicitly
# injected quosure's environment instead when the selection carries one.
# An injected missing selection stays empty rather than taking the default.
local_summary_selection_expr <- function(selection, group_vars,
                                         forbidden_names) {
  env <- rlang::sym(".env")
  if (rlang::is_quosure(selection)) {
    env <- rlang::quo_get_env(selection)
    selection <- rlang::quo_get_expr(selection)
  }
  if (rlang::is_missing(selection)) {
    selection <- rlang::expr(c())
  }
  rlang::call2(
    tidyselect::all_of,
    rlang::call2(
      local_summary_selection,
      # The generated capture must not resolve names in the caller's scope.
      rlang::call2(base::quote, selection),
      env,
      group_vars,
      forbidden_names
    )
  )
}

# Leaves `.cols` for local dplyr to resolve against its current summary mask.
rewrite_local_across_selection <- function(expr, group_vars,
                                           forbidden_names,
                                           capture_forwarded = TRUE) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  if (
    capture_forwarded &&
      (rlang::is_symbol(parsed$cols, "...") ||
         is_numbered_dot_reference(parsed$cols))
  ) {
    return(rlang::call2(
      rlang::call2(
        local_forwarded_selection,
        static_call_name(expr), group_vars, forbidden_names
      ),
      !!!call_args
    ))
  }
  selection <- local_summary_selection_expr(
    parsed$cols, group_vars, forbidden_names
  )
  if (parsed$cols_index == 0L) {
    call_args <- append(list(.cols = selection), call_args)
  } else {
    call_args[[parsed$cols_index]] <- selection
  }
  rebuild_static_call(expr, call_args)
}

# Captures expanded helper arguments at execution, before matching `.cols`.
# The caller supplies a recognized local helper and its selection exclusions.
local_forwarded_selection <- function(helper, group_vars, forbidden_names) {
  function(...) {
    frame <- environment()
    args <- rlang::enquos0(...)
    expr <- rlang::call2(helper, !!!args, .ns = "dplyr")
    cols_index <- parse_across_arguments(expr)$cols_index
    arg_names <- names(args)
    # missing(..i) follows forwarded promises without forcing them and treats
    # an omitted formal's default as supplied to this nested call.
    args <- lapply(seq_along(args), function(i) {
      # Runtime across gives a captured empty selector its default selection.
      missing_cols <- i == cols_index && rlang::quo_is_missing(args[[i]])
      if (missing_cols || (i != cols_index && eval(
        call("missing", as.name(paste0("..", i))), envir = frame
      ))) {
        rlang::missing_arg()
      } else {
        args[[i]]
      }
    })
    names(args) <- arg_names
    expr <- rlang::call2(helper, !!!args, .ns = "dplyr")
    expr <- rewrite_local_across_selection(expr, group_vars, forbidden_names)
    rlang::eval_tidy(expr)
  }
}

# Applies the same deferred local selection to `pick()`.
rewrite_local_pick_selection <- function(expr, group_vars,
                                         forbidden_names) {
  call_args <- static_call_args(expr)
  forwarded <- vapply(call_args, is_numbered_dot_reference, logical(1))
  if (any(forwarded)) {
    return(rlang::call2(
      rlang::call2(
        local_forwarded_pick_selection,
        group_vars, forbidden_names
      ),
      !!!call_args
    ))
  }
  selection <- parse_pick_selection(expr)
  if (is.null(selection)) {
    return(expr)
  }
  selection <- local_summary_selection_expr(
    selection, group_vars, forbidden_names
  )
  rebuild_static_call(expr, list(selection))
}

# Captures pick's forwarded promises in their calling scope before selection.
# The caller supplies the exclusions for the active local summary branch.
local_forwarded_pick_selection <- function(group_vars, forbidden_names) {
  function(...) {
    args <- rlang::enquos0(...)
    expr <- rlang::call2("pick", !!!args, .ns = "dplyr")
    expr <- rewrite_local_pick_selection(expr, group_vars, forbidden_names)
    rlang::eval_tidy(expr)
  }
}

# Returns selected current column names for one local dplyr summary branch.
# The caller is inside a tidyselect selection in dplyr's summary mask, whose
# current columns include preceding outputs. Share sources refuse selections
# of preceding summaries. ADR 0002 owns the input schema.
local_summary_selection <- function(selection, env, group_vars,
                                    forbidden_names) {
  data <- tidyselect::peek_data()
  state <- if (is.environment(forbidden_names)) forbidden_names else NULL
  if (is.environment(forbidden_names)) {
    forbidden_names <- c(
      forbidden_names$forbidden_names,
      get("internal_names", envir = forbidden_names, inherits = TRUE),
      get("share_aliases", envir = forbidden_names, inherits = TRUE)
    )
  }
  data <- data[setdiff(names(data), c(group_vars, forbidden_names))]
  preceding_aliases <- character()
  if (!is.null(state) && length(state$source_checks) > 0L) {
    preceding_aliases <- unlist(
      get("summary_aliases", envir = state, inherits = TRUE)[
        seq_len(state$position - 1L)
      ], use.names = FALSE
    )
  }
  # Tidyselect applies a predicate before a later exclusion can remove
  # the alias whose value it saw.
  visible_aliases <- intersect(preceding_aliases, names(data))
  selected <- if (length(visible_aliases) == 0L) {
    resolve_summary_selection(selection, env = env, data_proxy = data)
  } else {
    resolve_share_source_selection(
      selection, env, data, visible_aliases, state$source_checks[[1L]]
    )
  }
  source_names <- names(data)[unname(selected)]
  if (!is.null(state)) {
    for (check in state$source_checks) {
      check$symbols <- if (is.na(check$across_input)) {
        source_names
      } else {
        check$across_input
      }
      check$symbols <- unique(c(
        check$symbols,
        expression_data_symbols(check$dependency_expr, lookup = list(
          env = env, mask = names(data),
          rebound = lookup_rebound_names(check$dependency_expr),
          deferred_selection = TRUE
        ))
      ))
      with_generated_summary_check(
        check_share_alias_reads(preceding_aliases, list(check))
      )
    }
  }
  if (!identical(names(selected), source_names)) {
    names(source_names) <- names(selected)
  }
  source_names
}

# Resolve a local selector whose proxy includes earlier summary aliases.
resolve_share_source_selection <- function(selection, env, data, aliases,
                                           check) {
  scope <- list(list(
    env = env, bound = names(tidyselect::vars_select_helpers),
    rebound = lookup_rebound_names(selection)
  ))
  reads <- selection_proxy_value_symbols(
    selection, names(data), scope, in_selection = TRUE
  )
  dependencies <- intersect(reads, aliases)
  if (length(dependencies) > 0L) {
    with_generated_summary_check(abort_share_alias_dependency(
      check$share_kind, check$share_output, check$source_summary,
      dependencies[[1L]]
    ))
  }
  tryCatch(
    resolve_summary_selection(
      selection, env = env, data_proxy = data, allow_predicates = FALSE
    ),
    error = function(cnd) {
      if (is_unsupported_predicate(cnd)) {
        with_generated_summary_check(abort_share_alias_dependency(
          check$share_kind, check$share_output, check$source_summary,
          aliases[[1L]]
        ))
      }
      stop(cnd)
    }
  )
}

# Read values from the active tidyselect data proxy. A selector operator
# walks selector operands, while a contextual leaf evaluates ordinary R.
# Scope holds the current lexical frame first, followed by the frames in
# which its supplied promises evaluate. No caller expressions run here.
# `result_columns` limits reads from a proxy returned by this expression.
selection_proxy_value_symbols <- function(expr, columns, scope,
                                          seen = list(), in_selection = FALSE,
                                          result_columns = columns) {
  if (rlang::is_missing(expr)) return(character())
  if (rlang::is_quosure(expr)) {
    scope[[1L]]$env <- rlang::quo_get_env(expr)
    return(selection_proxy_value_symbols(
      rlang::quo_get_expr(expr), columns, scope, seen, in_selection,
      result_columns
    ))
  }
  if (in_selection) {
    if (is_selection_data_pronoun(expr) ||
          rlang::is_call(expr, c("~", selection_refused_operators()),
                         ns = "")) {
      return(character())
    }
    if (is_selection_walk_call(expr)) {
      return(unlist(lapply(
        static_call_args(expr), selection_proxy_value_symbols,
        columns = columns, scope = scope, seen = seen, in_selection = TRUE
      ), use.names = FALSE))
    }
  }
  expr <- unparenthesized_value(expr)
  if (identical(expr, quote(...)) && length(scope[[1L]]$dots) > 0L) {
    return(unlist(lapply(
      scope[[1L]]$dots, selection_proxy_value_symbols, columns = columns,
      scope = scope[-1L], seen = seen
    ), use.names = FALSE))
  }
  promise <- selection_proxy_promise(expr, scope)
  if (!is.null(promise)) {
    return(selection_proxy_value_symbols(
      promise$expression, columns, promise$scope[promise$evaluation], seen,
      result_columns = result_columns
    ))
  }
  if (!rlang::is_call(expr)) return(character())
  name <- static_call_name(expr)
  frame <- scope[[1L]]
  visible <- selection_proxy_visible_frames(scope)
  lookup <- list(env = frame$env, rebound = frame$rebound)
  arm <- literal_if_parts(expr, name, scope_bound_names(scope[visible]), lookup)
  if (!is.null(arm)) {
    if (length(arm) == 0L) return(character())
    return(selection_proxy_value_symbols(
      arm[[1L]], columns, scope, seen, result_columns = result_columns
    ))
  }
  if (is_binding_statement(name, expr)) {
    # The effects walk sees a lexical frame and a slot for argument
    # evaluation. Only the read walk opens the original caller scopes.
    read_part <- function(part, lexical, lookup, subset = columns) {
      selection_proxy_value_symbols(
        part, columns, c(lexical[1L], scope[-1L]), seen, result_columns = subset
      )
    }
    return(statement_reads_and_scope(
      expr, list(frame, list()), lookup = lookup,
      read_symbols = read_part,
      read_result_symbols = function(part, lexical, lookup) {
        read_part(part, lexical, lookup, result_columns)
      }
    )$reads)
  }
  callable <- selection_proxy_callable(static_call_head(expr), scope)
  fn <- callable$fn
  if (identical(fn, tidyselect::peek_data) ||
        selection_proxy_dispatch_peek(expr, fn, scope)) {
    return(result_columns)
  }
  args <- static_call_args(expr)
  subset <- selection_proxy_read_columns(fn, args, columns)
  if (!is.null(subset)) {
    return(selection_proxy_value_symbols(
      args[[1L]], columns, scope, seen, result_columns = subset
    ))
  }
  if (rlang::is_closure(fn) && !isNamespace(environment(fn)) &&
        !identical(environment(fn), baseenv()) &&
        !any(vapply(seen, identical, logical(1), fn))) {
    frame <- function_scope_frame(formals(fn), expr)
    frame$env <- environment(fn)
    frame$rebound <- lookup_rebound_names(body(fn))
    frame$lexical_parent <- callable$lexical_parent
    reads <- selection_proxy_value_symbols(
      body(fn), columns, c(list(frame), scope), c(seen, list(fn)),
      result_columns = result_columns
    )
    return(c(reads, unlist(lapply(
      frame$unmatched, selection_proxy_value_symbols, columns = columns,
      scope = scope, seen = seen
    ), use.names = FALSE)))
  }
  parts <- c(list(static_call_head(expr)),
             scope_call_args(expr, name, c(frame$bound, frame$rebound),
                             list(env = frame$env)),
             evaluated_language_parts(expr, name))
  unlist(lapply(
    parts, selection_proxy_value_symbols, columns = columns, scope = scope,
    seen = seen
  ), use.names = FALSE)
}

# Columns consumed by a known name inspection or literal extraction.
# NULL leaves argument traversal to the ordinary expression walk.
selection_proxy_read_columns <- function(fn, args, columns) {
  if (identical(fn, base::names) && length(args) == 1L) {
    return(character())
  }
  if (length(args) != 2L) return(NULL)
  if (identical(fn, base::`$`) && is_name_part(args[[2L]])) {
    return(columns[pmatch(rlang::as_string(args[[2L]]), columns, nomatch = 0L)])
  }
  if (identical(fn, base::`[[`) && rlang::is_string(args[[2L]])) {
    return(intersect(columns, args[[2L]]))
  }
  NULL
}

# Whether a known dispatcher calls peek_data with no arguments or only its
# literal diagnostic label. Neither the dispatch nor its arguments run here.
selection_proxy_dispatch_peek <- function(expr, fn, scope) {
  if (identical(fn, base::do.call)) {
    args <- static_matched_arguments(expr, fn)
    if (!"args" %in% names(args) ||
          ("quote" %in% names(args) && !rlang::is_bool(args[["quote"]]))) {
      return(FALSE)
    }
    forwarded <- selection_proxy_dispatch_args(args[["args"]], scope)
    target <- "what"
    env_arg <- "envir"
  } else if (identical(fn, rlang::exec)) {
    args <- static_matched_arguments(expr, fn, expand_dots = FALSE)
    forwarded <- as.list(args[["..."]])
    target <- ".fn"
    env_arg <- ".env"
  } else {
    return(FALSE)
  }
  if (!target %in% names(args) || is.null(forwarded)) return(FALSE)
  if (length(forwarded) > 0L &&
        (!identical(names(forwarded), "fn") ||
           !rlang::is_string(unparenthesized_value(forwarded[["fn"]])))) {
    return(FALSE)
  }
  env <- if (env_arg %in% names(args)) {
    selection_proxy_dispatch_env(args[[env_arg]], scope)
  }
  if (env_arg %in% names(args) && is.null(env)) return(FALSE)
  if (rlang::is_string(unparenthesized_value(args[[target]]))) {
    if (!is.null(env)) scope <- list(list(env = env))
    fn <- selection_proxy_function(
      rlang::sym(unparenthesized_value(args[[target]])), scope
    )
  } else {
    fn <- selection_proxy_function(
      args[[target]], scope, function_position = FALSE
    )
  }
  identical(fn, tidyselect::peek_data)
}

# The supplied argument list from a literal list value, base list() syntax,
# or a promise forwarding either of them. NULL means no statically known list.
selection_proxy_dispatch_args <- function(expr, scope) {
  expr <- unparenthesized_value(expr)
  if (is.list(expr) && !is.object(expr)) return(expr)
  promise <- selection_proxy_promise(expr, scope)
  if (!is.null(promise)) {
    return(selection_proxy_dispatch_args(
      promise$expression, promise$scope[promise$evaluation]
    ))
  }
  if (rlang::is_call(expr) &&
        identical(selection_proxy_function(static_call_head(expr), scope),
                  base::list)) {
    static_call_args(expr)
  }
}

# Read a literal, existing environment supplied to a dispatcher.
selection_proxy_dispatch_env <- function(expr, scope) {
  expr <- unparenthesized_value(expr)
  if (is.environment(expr)) return(expr)
  if (!rlang::is_call(expr) ||
        !identical(selection_proxy_function(static_call_head(expr), scope),
                   base::asNamespace)) {
    return(NULL)
  }
  args <- static_matched_arguments(expr, base::asNamespace)
  if (rlang::is_string(args[["ns"]]) && args[["ns"]] %in% loadedNamespaces()) {
    return(asNamespace(args[["ns"]]))
  }
  NULL
}

# The current frame and its lexical parents. A forwarded closure can skip
# its caller when finding captured bindings, while its supplied promises
# still evaluate in that caller.
selection_proxy_visible_frames <- function(scope) {
  visible <- integer()
  index <- 1L
  repeat {
    visible <- c(visible, index)
    parent <- scope[[index]]$lexical_parent
    if (is.null(parent)) return(visible)
    index <- index + parent
  }
}

# Follow a promise in the lexical scope. The full stack retains the calling
# frames needed to read its argument expression.
selection_proxy_promise <- function(expr, scope, function_position = FALSE) {
  if (!is_name_part(expr)) return(NULL)
  scope_promise(
    rlang::as_name(expr), scope, function_lookup = function_position,
    visible = selection_proxy_visible_frames(scope)
  )
}

# Resolve a function without running caller code or forcing promises.
selection_proxy_function <- function(head, scope, function_position = TRUE) {
  selection_proxy_callable(head, scope, function_position)$fn
}

# Resolve callable syntax and the relative offset to a literal closure's
# defining frame. The offset accounts for slices taken when following promises.
# A bare call head uses function lookup. A value position or parenthesized
# head reads its exact binding even if it is a nonfunction.
selection_proxy_callable <- function(head, scope, function_position = TRUE,
                                     offset = 0L) {
  function_position <- function_position && !is_parenthesized(head)
  head <- unparenthesized_value(head)
  promise <- selection_proxy_promise(head, scope, function_position)
  if (!is.null(promise)) {
    return(selection_proxy_callable(
      promise$expression, promise$scope[promise$evaluation],
      function_position = FALSE, offset = offset + promise$evaluation[[1L]] - 1L
    ))
  }
  if (is.function(head)) return(list(fn = head))
  frame <- scope[[1L]]
  if (rlang::is_call(head, "function", n = 3L) &&
        is.pairlist(head[[2L]]) &&
        identical(selection_proxy_function(static_call_head(head), scope),
                  base::`function`)) {
    return(list(
      fn = rlang::new_function(head[[2L]], head[[3L]], frame$env),
      lexical_parent = offset + 1L
    ))
  }
  if (rlang::is_call(head) &&
        !rlang::is_call(head, c("::", ":::")) &&
        identical(selection_proxy_function(static_call_head(head), scope),
                  base::getExportedValue)) {
    args <- static_matched_arguments(head, base::getExportedValue)
    if (identical(args[["ns"]], "tidyselect") &&
          identical(args[["name"]], "peek_data")) {
      return(list(fn = tidyselect::peek_data))
    }
  }
  if (!is_name_part(head) && !rlang::is_call(head, c("::", ":::"))) return(NULL)
  if (is_name_part(head) && !function_position) head <- call("(", head)
  visible <- scope[selection_proxy_visible_frames(scope)]
  if (function_position) {
    visible <- lapply(visible, function(frame) {
      values <- names(frame$kinds)[frame$kinds == "value"]
      frame$bound <- setdiff(frame$bound, values)
      frame
    })
  }
  fn <- selection_argument_function(
    rlang::call2(head), frame$env, c(scope_bound_names(visible), frame$rebound)
  )
  list(fn = fn)
}

# Answers whether `.unpack` is known to be FALSE without evaluating an
# expression, forcing a promise, or reading an active binding. Unknown values
# are left to dplyr when it evaluates the summary. The caller supplies the
# captured argument and its environment.
across_unpack_is_false <- function(unpack, env) {
  if (is.null(unpack) || isFALSE(unpack)) {
    return(TRUE)
  }
  if (!rlang::is_symbol(unpack)) {
    return(FALSE)
  }
  name <- rlang::as_string(unpack)
  current <- env
  while (!identical(current, emptyenv())) {
    if (exists(name, envir = current, inherits = FALSE)) {
      if (bindingIsActive(name, current) ||
            rlang::env_binding_are_lazy(current, name)) {
        return(FALSE)
      }
      return(isFALSE(get(name, envir = current, inherits = FALSE)))
    }
    current <- parent.env(current)
  }
  FALSE
}

# `call_name` is the caller's answer rather than one asked again here. Asking
# again would answer the same, since the shared read answers a formula as no
# name at all (#163); it would just be a second question about an expression
# the only caller enters this function having already named, and the two
# answers would then have to be kept in step by hand.
rewrite_across_selection <- function(expr,
                                     env,
                                     data_proxy,
                                     call_name,
                                     across_backend = NULL) {
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

  if (identical(call_name, "across")) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    if (!is.null(parsed$names)) {
      template <- rlang::eval_tidy(parsed$names, env = env)
      function_names <- known_across_function_names(parsed, env)
      if (is.character(template) && length(template) == 1L &&
            !is.na(template) && length(function_names) > 0L &&
            across_unpack_is_false(parsed$unpack, env)) {
        output_names <- attr(template, "marginplyr_across_names", exact = TRUE)
        if (is.null(output_names)) {
          output_names <- expand_across_name(
            template, names(selected), function_names, env
          )
        }
        # Planning and every backend consume these same resolved names.
        template <- resolved_across_name_template(output_names)
      }
      call_args[parsed$names_index] <- list(template)
    }
  }

  if (identical(call_name, "across") && identical(across_backend, "arrow")) {
    source_names <- names(data_proxy)[unname(selected)]
    if (!identical(names(selected), source_names)) {
      # Arrow uses selection names to look up the input fields.
      # Freeze output names before removing the selection aliases.
      if (anyDuplicated(source_names)) {
        abort_marginplyr(c(
          "Renamed {.fun across} selects an input column more than once.",
          i = "On Arrow, write separate summaries for repeated inputs."
        ))
      }
      resolved <- rebuild_static_call(expr, call_args)
      output_names <- known_across_output_names(resolved, env, data_proxy)
      if (length(output_names) == 0L) {
        abort_marginplyr(c(
          "Arrow cannot determine the output names for renamed {.fun across}.",
          i = "Use a single function or a literal list of functions."
        ))
      }
      call_args[[selection_index]] <- rlang::expr(
        dplyr::all_of(!!source_names)
      )
      call_args$.names <- resolved_across_name_template(output_names)
    }
  }

  if (identical(call_name, "across") && identical(across_backend, "dtplyr")) {
    parsed <- parse_across_arguments(rebuild_static_call(expr, call_args))
    unpack_is_false <- across_unpack_is_false(parsed$unpack, env)
    function_names <- known_across_function_names(parsed, env)

    if (
      !is.null(parsed$names) &&
        unpack_is_false &&
        length(function_names) == 1L
    ) {
      output_names <- known_across_output_names(
        rebuild_static_call(expr, call_args), env, data_proxy
      )
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

  rebuild_static_call(expr, call_args)
}

rewrite_pick_selection <- function(expr, env, data_proxy) {
  selection <- parse_pick_selection(expr)
  if (is.null(selection)) {
    return(expr)
  }
  selected <- resolve_summary_selection(
    selection,
    env = env,
    data_proxy = data_proxy
  )

  rebuild_static_call(expr, list(summary_all_of_expr(selected, data_proxy)))
}

# Reads a `pick()` call's selection, or `NULL` when no input was supplied.
# An omitted input stays with dplyr or the backend to answer (ADR 0015).
parse_pick_selection <- function(expr) {
  call_args <- static_call_args(expr)
  if (length(call_args) == 0L) {
    return(NULL)
  }
  rlang::call2("c", !!!call_args)
}

resolve_summary_selection <- function(expr, env, data_proxy,
                                      allow_predicates = TRUE) {
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
    allow_rename = TRUE,
    allow_predicates = allow_predicates
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
# is not recognized as any. Name assignment and local frame checking both
# use this spelling classification; actual frame names are read from the value
# when the summary runs.
#
# Two frame families rather than one, because the owner differs and the owner
# is what recognition tests: tibble owns `tibble()` and `data_frame()`, base
# owns `data.frame()`. Neither is a Contextual helper -- nothing rewrites them,
# and a caller who binds `tibble` gets their own function (ADR 0019).
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
    return(character())
  }

  if (identical(kind, "pick")) {
    selection <- parse_pick_selection(expr)
    if (is.null(selection)) {
      return(character())
    }
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

# Constructor argument names are only candidates for share diagnostics and
# internal-name reservation. Omission, expansion, and repair can change the
# actual schema, which local_assigned_summary_value() checks after evaluation.
frame_argument_candidates <- function(expr, env) {
  if (!identical(data_frame_valued_summary_kind(expr), "frame") ||
        !is_known_frame_constructor(expr, env)) {
    return(character())
  }
  call_args <- static_call_args(expr)
  arg_names <- unname(rlang::names2(call_args))
  controls <- frame_constructor_controls(expr)
  injected_names <- vapply(
    call_args[arg_names == ""], known_injected_argument_name, character(1)
  )
  unname(c(
    arg_names[nzchar(arg_names) & !arg_names %in% controls],
    injected_names[nzchar(injected_names) & !injected_names %in% controls]
  ))
}

# Gather unnamed constructor candidates for reserving internal adapter names.
# Named summaries remain packed under their caller-written outer name.
summary_frame_candidates <- function(dots) {
  unnamed <- dots[!nzchar(rlang::names2(dots))]
  unlist(lapply(unnamed, function(dot) {
    frame_argument_candidates(
      rlang::quo_get_expr(dot), rlang::quo_get_env(dot)
    )
  }), use.names = FALSE)
}

# Qualified constructors name their owner; an unqualified spelling may reach
# a caller's own function through ordinary lookup (ADR 0019).
is_known_frame_constructor <- function(expr, env) {
  name <- static_call_name(expr)
  namespace <- static_call_ns(expr)
  if (!is.null(namespace)) {
    return(TRUE)
  }
  constructor <- if (identical(name, "data.frame")) {
    base::data.frame
  } else if (identical(name, "tibble")) {
    tibble::tibble
  } else {
    tibble::data_frame
  }
  identical(ordinary_function_binding(name, env), constructor)
}

# A recognized constructor consumes these full formal names as controls.
frame_constructor_controls <- function(expr) {
  constructor <- if (is_static_spelling_call(
    expr, "base_frame", "data.frame"
  )) {
    base::data.frame
  } else if (is_static_spelling_call(expr, "tibble_frame", "tibble")) {
    tibble::tibble
  } else {
    tibble::data_frame
  }
  setdiff(names(formals(constructor)), "...")
}

# Read only literal injected names, without evaluating a naming expression.
known_injected_argument_name <- function(expr) {
  if (!rlang::is_call(expr, ":=") || length(expr) != 3L) {
    return("")
  }
  # An empty LHS must be inspected by subscript: assigning it errors (#174).
  if (
    is.character(expr[[2L]]) && length(expr[[2L]]) == 1L &&
      !is.na(expr[[2L]])
  ) {
    return(expr[[2L]])
  }
  if (is_name_part(expr[[2L]])) {
    return(rlang::as_name(expr[[2L]]))
  }
  ""
}

# Returns statically known across outputs for the supplied selection proxy.
# Resolved templates carry their names without evaluating glue again.
known_across_output_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  if (!across_unpack_is_false(parsed$unpack, env)) {
    # The outer names can be replaced by inner names whose schema is only
    # known after the summary runs. The branch checks those actual names.
    return(character())
  }
  cols_expr <- parsed$cols
  column_names <- names(resolve_summary_selection(cols_expr, env, data_proxy))

  if (is.null(parsed$names)) {
    if (is_known_function_list_call(parsed$fns, env)) {
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
  names_template <- rlang::eval_tidy(parsed$names, env = env)
  resolved <- attr(names_template, "marginplyr_across_names", exact = TRUE)
  if (!is.null(resolved)) {
    return(resolved)
  }
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

  expand_across_name(names_template, column_names, function_names, env)
}

# Carries expanded names as a backend glue template and a planning result.
resolved_across_name_template <- function(output_names) {
  structure(
    paste0("{", deparse1(rlang::expr(base::c(!!!as.list(output_names)))), "}"),
    marginplyr_across_names = output_names
  )
}

# Expands one template over dplyr's column-major column/function product.
# Share callers recycle a scalar name so their duplicate-name check owns it.
expand_across_name <- function(template, column, function_name, env,
                               recycle_scalar = FALSE) {
  columns <- rep(column, each = length(function_name))
  functions <- rep(function_name, times = length(column))
  mask <- rlang::env(env, .col = columns, .fn = functions)
  rlang::env_bind_active(
    mask, col = function() mask$.col, fn = function() mask$.fn
  )
  expanded <- as.character(glue::glue(template, .envir = mask))
  if (length(columns) == 0L) {
    return(character())
  }
  if (recycle_scalar && length(expanded) == 1L) {
    expanded <- rep(expanded, length(columns))
  }
  check_across_name_count(expanded, template, columns)
}

# Require one expanded name per column/function pair before staging a summary.
check_across_name_count <- function(expanded, template, column) {
  if (length(expanded) == length(column)) {
    return(expanded)
  }

  abort_marginplyr(c(
    paste0(
      "The {.fun across} {.arg .names} template {.code {template}} must ",
      "produce one name per column/function pair, but it produced ",
      "{length(expanded)} for {length(column)} outputs."
    ),
    i = "Use a template that expands to one name per output."
  ))
}

known_across_source_names <- function(expr, env, data_proxy) {
  parsed <- parse_across_arguments(expr)
  selected <- resolve_summary_selection(parsed$cols, env, data_proxy)
  get_col_names(data_proxy, dplyr::everything())[unname(selected)]
}

# `"1"` is what `{.fn}` expands to for a statically single function or an
# omitted `.fns`. Resolved function bindings, including namespace exports, are
# also single when the authored namespace operator is known. `substitute()`
# reads caller bindings without forcing promises or invoking active bindings.
known_across_function_names <- function(parsed, env) {
  parsed$fns <- unparenthesized_value(parsed$fns)
  if (is.null(parsed$fns) || is.function(parsed$fns) ||
        rlang::is_call(parsed$fns, "function") ||
        rlang::is_call(parsed$fns, "\\") ||
        rlang::is_call(parsed$fns, "~")) {
    return("1")
  }

  if (rlang::is_call(parsed$fns, c("::", ":::"), ns = "")) {
    operator <- rlang::as_string(parsed$fns[[1L]])
    if (!identical(ordinary_function_binding(operator, env),
                   get(operator, envir = baseenv(), inherits = FALSE))) {
      return(character())
    }
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

  if (!is_known_function_list_call(parsed$fns, env)) {
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

# The arguments of a literal `list()` name across functions only when that
# call reaches base's constructor. An ordinary caller binding may return a
# different list, whose entry names remain unknown until across evaluates it.
is_known_function_list_call <- function(expr, env) {
  if (!identical(static_call_name(expr), "list")) {
    return(FALSE)
  }
  namespace <- static_call_ns(expr)
  if (!is.null(namespace)) {
    return(identical(namespace, "base") && is_name_only_namespace(expr, env))
  }
  identical(ordinary_function_binding("list", env), base::list)
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
    additional = additional_names,
    additional_args = call_args[additional]
  )
}
