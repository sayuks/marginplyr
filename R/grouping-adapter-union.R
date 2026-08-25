# The portable adapter: the `UNION ALL` composition every branch list ends in,
# the one place the package combines branches, and -- because it is also the one
# place the caller's own summary expressions are handed to a backend once per
# grouping set -- where a backend that would absorb one is refused (ADR 0025).
#
# `Reduce(dplyr::union_all, branches)` is
# quadratic in the number of branches (#111): on an eager backend each step
# re-copies the accumulated frame, and on a lazy one dbplyr re-aligns every
# branch already folded in against the branch being added. The count is 2^n for
# a cube over n dimensions, so a ten-dimension cube folds 1024 branches, and the
# local backend has no `native_grouping_sets` capability, so it always folds.
#
# There is no single linear substitution, because a branch list is eager or
# lazy depending on the input the verb was handed, and only the eager one can
# be combined in one call. The two paths below are therefore chosen from the
# branches rather than from the operation's backend: what is being combined is
# what decides, and `build_lazy_parent_mapping()` in `R/share.R` reaches this
# from every backend despite its name.
combine_margin_branches <- function(branches) {
  # An invariant, not a Package condition (ADR-0015): a plan holds at least one
  # grouping set, and the share module gates its own call on a non-empty
  # mapping list. `Reduce()` answered `NULL` here, which is not a result any
  # caller could use.
  stopifnot(is.list(branches), length(branches) > 0L)

  if (length(branches) == 1L) {
    return(branches[[1L]])
  }
  if (is.data.frame(branches[[1L]])) {
    return(bind_margin_branches(branches))
  }
  union_margin_branches(branches)
}

# `dplyr::bind_rows()` combines the whole list in one pass, and it is the whole
# of the eager path: `union_all.data.frame()` ends in a `dplyr_reconstruct()`
# onto its first argument, but that call restores nothing `vec_rbind()` did not
# already take from the same frame, so reproducing it would add the
# reconstruction step ADR-0016 forbids an adapter and change no result.
#
# What does differ is strictness. `bind_rows()` fills a column a branch does not
# have with `NA` where `union_all()` rejects the pair, so the check below
# restores it explicitly: a branch of an unexpected shape is a defect in the
# builders further down this file, and widening the result would hide it.
bind_margin_branches <- function(branches) {
  check_branch_columns(branches)
  dplyr::bind_rows(branches)
}

# `names()` rather than `get_col_names()`: this runs on data frames only, where
# it is exact and costs nothing per branch. Column *order* is not checked,
# because `union_all()` accepts a reordered branch and matches by name.
check_branch_columns <- function(branches) {
  expected <- names(branches[[1L]])
  for (i in seq_along(branches)[-1L]) {
    actual <- names(branches[[i]])
    if (length(actual) == length(expected) && setequal(actual, expected)) {
      next
    }
    # Branch, not grouping-set branch: the share module's denominator mappings
    # are combined here too, and they are one per set that has a coarser one
    # rather than one per set.
    stop(
      "Union branch ", i, " does not have the columns of branch 1.\n",
      "Only in branch ", i, ": ",
      toString(setdiff(actual, expected)), "\n",
      "Only in branch 1: ",
      toString(setdiff(expected, actual)),
      call. = FALSE
    )
  }
  invisible(NULL)
}

# A lazy branch list has to be combined two at a time, since no backend here
# exposes an n-ary `union_all()`. Pairing adjacent branches and halving makes
# the work O(n log n) rather than quadratic and, just as importantly, bounds the
# nesting depth at log2(n): dtplyr builds one step object per pair, and
# collecting a left fold of 512 branches -- a cube over nine dimensions --
# exhausts the C stack.
#
# Re-associating is safe because `UNION ALL` concatenates, so the row sequence
# is the same however the pairs are bracketed. dbplyr flattens a union of
# unions into one query, so the rendered SQL is a single `UNION ALL` over every
# branch either way, with no subquery per pair.
union_margin_branches <- function(branches) {
  while (length(branches) > 1L) {
    n <- length(branches)
    pairs <- lapply(
      seq_len(n %/% 2L),
      function(i) dplyr::union_all(branches[[2L * i - 1L]], branches[[2L * i]])
    )
    branches <- if (n %% 2L == 1L) c(pairs, branches[n]) else pairs
  }
  branches[[1L]]
}

# The text an Absorbing backend marks an absorption with, which is the whole of
# what a backend can be recognised as absorbing by. Arrow's warning carries no
# class of its own, no `$parent`, and no `$call` -- it is an `rlang_warning`
# holding one rendered string -- so there is nothing else to key on.
#
# It is matched without regard to case, and that is a version range rather than
# caution. `DESCRIPTION` admits `arrow (>= 13.0.0)`, and Arrow rewrote this
# warning between 16.0.0 and 17.0.0: through 16.0.0 it was
# `warning(msg, "; pulling data into R")`, and from 17.0.0 it is an rlang
# warning whose body ends `"Pulling data into R"`. What survived the rewrite is
# the phrase and not its capitalisation, so matching the capitalised spelling
# alone left the refusal switched off for four of the versions this package
# says it supports, where Arrow absorbs exactly as it does on the newest
# (`investigation/what-arrow-does-with-an-untranslatable-summary.md`).
#
# This is undocumented behaviour of another package, and it is depended on the
# way `AGENTS.md` depends on roxygen's treatment of a markdown table row: by
# gating it. `test-grouping-backends.R` asserts that Arrow still absorbs the
# expressions the refusal is asserted over and still marks the absorption with
# this text, so a re-wording fails there rather than silently switching the
# refusal off. It is a function rather than a constant so that taking it away
# is how the backstop below is reached in a test.
#
# The gate calls `is_absorbing_backend_warning()` rather than matching this
# marker a second time of its own, and that is the difference between gating
# the reading and gating a copy of it: a second match answers for its own
# spelling, so it would go on reporting Arrow as marking the absorption after
# the handler had stopped recognising it. Which leaves one thing the gate
# cannot see, since it holds one Arrow and the case difference above spans two
# -- so the reading is asserted on both phrasings beside it, over synthesised
# warnings.
absorbing_warning_marker <- function() {
  "pulling data into R"
}

# `.data` decides as well as the text, because the text alone answers for a
# warning marginplyr did not cause: a caller's own summary expression may spell
# anything, and a refusal naming Arrow raised over a `dtplyr` input would be
# wrong twice over. The class list is `R/grouping-backend.R`'s, so the two
# readings cannot disagree about what an Arrow input is.
#
# `conditionMessage()` is checked for shape before it is matched: a condition
# class of another package's may carry a message method returning anything, and
# `grepl()` over a vector would answer for whichever element matched.
is_absorbing_backend_warning <- function(cnd, .data) {
  if (!inherits(.data, arrow_input_classes())) {
    return(FALSE)
  }
  text <- conditionMessage(cnd)
  is.character(text) &&
    length(text) == 1L &&
    grepl(absorbing_warning_marker(), text, ignore.case = TRUE)
}

# The label Arrow writes an absorbed expression by, reproduced so that the
# expression marginplyr handed it can be recognised in the warning it rendered.
# Arrow deparses the expression, keeps the first line, and marks a longer one
# with a trailing ellipsis -- which is neither `rlang::as_label()` nor a whole
# `deparse()`.
#
# The convention is reproduced rather than the internal called, which is ADR
# 0022's rule and its reason: an internal that changed would disagree silently,
# while a convention that changed stops matching, and a label that matches
# nothing is what sends the refusal to name every summary argument instead of
# one. That degradation is the same one ADR 0022 accepts for a span it cannot
# place.
absorbed_expression_label <- function(expr) {
  lines <- deparse(expr)
  if (length(lines) > 1L) paste0(lines[[1L]], "...") else lines[[1L]]
}

# The label Arrow blamed, read off the line its warning opens with. `NA` where
# the warning does not open with one, which is not a hypothetical: the phrasing
# through Arrow 16.0.0 named the offending expression inside a sentence rather
# than on a line of its own, so every version below 17.0.0 takes the degradation
# below rather than placing the blame on one argument.
absorbing_warning_label <- function(cnd) {
  lines <- strsplit(conditionMessage(cnd), "\n", fixed = TRUE)[[1L]]
  if (length(lines) == 0L) {
    return(NA_character_)
  }
  matched <- regmatches(lines[[1L]], regexec("^In (.*): *$", lines[[1L]]))[[1L]]
  if (length(matched) == 2L) matched[[2L]] else NA_character_
}

# Which summaries the refusal names, spelled as the caller spelled them (ADR
# 0024). Arrow blames one expression at a time, so a warning it can be placed
# from names one argument; a warning it cannot names them all, which is also
# what the backstop below has to do, having no warning to read.
absorbed_summary_labels <- function(cnd, dots, caller_labels) {
  # The same invariant `branch_argument_map()` opens with, for the same reason
  # (ADR-0015): the pair is parallel, and a misaligned one would quote one
  # argument's expression under another argument's name -- which is the ADR
  # 0024 failure this plumbing exists to prevent, arriving silently.
  stopifnot(length(dots) == length(caller_labels))

  blamed <- absorbing_warning_label(cnd)
  if (is.na(blamed) || length(dots) == 0L) {
    return(caller_labels)
  }
  # Not `summary_argument_labels()`, which spells `name = expr` with
  # `rlang::as_label()`. What has to be recognised here is Arrow's spelling of
  # the expression alone, and the two conventions disagree in exactly the
  # places ADR 0022 records.
  labels <- vapply(
    dots,
    function(dot) {
      expr <- if (rlang::is_quosure(dot)) rlang::quo_get_expr(dot) else dot
      absorbed_expression_label(expr)
    },
    character(1),
    USE.NAMES = FALSE
  )
  matched <- which(labels == blamed)
  if (length(matched) == 1L) caller_labels[matched] else caller_labels
}

# The refusal an Absorbing backend earns, and the reason ADR 0025 chose it over
# letting the backend absorb. Both rewrites are given because the second is the
# whole of what refusing buys the caller: an absorbed summary reads every
# column of the input, including the ones it does not use, and a caller who is
# told can read fewer. A caller who is not told cannot.
#
# The summaries arrive alone in an `i` bullet, per ADR 0023's condition 2: how
# many of them there are is the caller's decision, since a warning Arrow's
# older phrasing cannot be placed from names every summary argument rather than
# one. `cli::qty()` is what carries the count across the split, the refusal
# left behind inflecting both its noun and its pronoun with the vector gone
# from the line they sit in -- the same shape as `grouping_helper_vars()`.
abort_absorbed_summary <- function(labels) {
  abort_marginplyr(c(
    paste0(
      "{cli::qty(length(labels))}Arrow cannot evaluate {?this summary/these ",
      "summaries}, so Arrow would read the whole input to compute {?it/them}:"
    ),
    i = "{.code {labels}}.",
    i = paste0(
      "Collect the Arrow input first, then call ",
      "{.fun summarize_with_margins}."
    ),
    i = paste0(
      "Select the columns you need before collecting. Arrow reads every ",
      "column of the input, including the ones a summary does not use."
    )
  ))
}

# The caller's expressions are spliced into the `summarize()` call rather than
# forwarded through `...`, which is the shape `grouping-adapter-native.R`
# already uses. Arrow recovers the originating call from its own frame with
# `match.call()`, and a `...`-forwarding wrapper is the one shape it cannot be
# recovered from: the lookup falls through to `base::call`, and subsetting an
# object of type `"special"` raises an untyped error where the backend's own
# answer belonged (#254).
#
# `caller_labels` is `new_summary_arguments()`'s, parallel to `dots`, and is
# what makes the refusal name what the caller wrote rather than what the branch
# rewrote. It is required rather than defaulted: the branch's own dots are the
# rewritten ones, so a default computed from them would answer with a spelling
# ADR 0024 forbids, silently, at whichever call site forgot to pass the labels.
#
# Two things can raise the refusal and they fail in opposite directions, which
# is why both are here. The handler reads Arrow's warning, which is raised
# before the collect, so nothing is read; if the marker ever stops matching it
# does not fire at all. The guard reads the result's class, which cannot stop
# matching, but only after the branch has run. Between them the contract holds
# whichever way Arrow moves: the handler is what keeps the read from happening,
# and the guard is what keeps an absorbed result from ever being returned --
# bounded at one branch, because a Margin operation evaluates the caller's
# expression once per grouping set and this stops at the first.
#
# The guard raises the refusal rather than an internal invariant (ADR 0015),
# although what it reports is a defect. A caller reaching it can act on it, and
# the action is the one the refusal already names; making them read a bug report
# instead would move the cost of marginplyr's drift onto them. The maintainer's
# signal is `test-query-policy.R`, which fails when a read happens at all.
summarize_margin_branch <- function(.data,
                                    ...,
                                    .by,
                                    caller_labels) {
  dots <- rlang::enquos(...)

  result <- withCallingHandlers(
    rlang::inject(dplyr::summarize(
      .data = .data,
      !!!dots,
      .by = dplyr::all_of(.by)
    )),
    warning = function(cnd) {
      if (is_absorbing_backend_warning(cnd, .data)) {
        abort_absorbed_summary(
          absorbed_summary_labels(cnd, dots, caller_labels)
        )
      }
    }
  )

  # A lazy input whose branch came back local was read, and both arms of this
  # are that one fact. Which arm depends on who did the reading, because ADR
  # 0015 sorts a condition by what the caller can do about it rather than by
  # what happened: an Arrow input absorbing is something they can rewrite, and
  # the refusal names the rewrites; any other backend answering a lazy input
  # with a local frame is a defect here or there, which no rewrite of their
  # call avoids. Attributing the second to Arrow would be worse than not
  # reporting it -- a diagnostic that misdirects, over a defect.
  #
  # The second arm is also what catches a branch list that is part local and
  # part lazy, which `combine_margin_branches()` does not: a lazy-first mix is
  # accepted by `union_all()` and collects to the combined rows.
  if (!is.data.frame(.data) && is.data.frame(result)) {
    if (inherits(.data, arrow_input_classes())) {
      abort_absorbed_summary(caller_labels)
    }
    stop(
      "A lazy input produced a local summary branch, which no backend does.\n",
      "Branch class: ",
      toString(class(result)),
      call. = FALSE
    )
  }
  result
}

# A literal recycles to whatever the branch holds, which is the whole of the
# attachment wherever a column-less branch has a row count to recycle to. The
# `invents_row_on_column_add` capability in `R/grouping-backend.R` is where a
# backend says it has none, and every branch of such an input arrived carrying
# a row no source row produced (#184). Counting is what the literal cannot do
# there -- dtplyr translates `n()` to `.N`, which is zero for that table -- so
# the identifier lands on as many rows as the branch has and no more.
#
# `count_branch_rows` is what each call site knows and this cannot: whether a
# row this attachment materialises would be a row the branch does not stand
# for. Both sites spell it out, because the two answers come from different
# facts rather than from one being the default, and each is a property of the
# input rather than of a branch, so each is settled once per call.
add_grouping_set_id <- function(result,
                                set_id_name,
                                set_id,
                                count_branch_rows) {
  if (is.null(set_id_name)) {
    return(result)
  }
  set_id <- as.integer(set_id)
  value <- if (count_branch_rows) {
    rlang::expr(rep(!!set_id, dplyr::n()))
  } else {
    set_id
  }
  # Injected for the same reason as the factor methods: a bare `set_id` here
  # resolves against the data mask first, so a grouping column of that name
  # would number every row from its own values instead of the branch's
  # grouping-set id. The `{set_id_name}` glue is evaluated in this environment
  # rather than the mask, so only the value needs injecting.
  dplyr::mutate(
    result,
    "{set_id_name}" := !!value
  )
}

# This is the one function in the package that evaluates the caller's own
# summary expressions more than once, so it is the one that can report an
# External condition once per grouping set and in names the caller never wrote:
# the branches group by the `..marginplyr_key_N` columns allocated below, and
# dplyr builds every context it attaches out of those. `call` is the Margin
# verb a Condition context is owed instead of the internal `summarize()` the
# branch issues; a caller that reaches this directly has none to name and
# leaves the blamed call alone -- and `new_summary_arguments()` without labels
# is the same statement about the argument spelling.
summarize_margin_union <- function(.data,
                                   summaries,
                                   plan,
                                   margin_labels,
                                   column_info,
                                   reserved_names,
                                   set_id_name = NULL,
                                   set_id_is_internal = FALSE,
                                   call = NULL) {
  dots <- summaries$dots
  group_vars <- unique(c(plan$by, plan$dimensions))
  key_names <- new_margin_internal_names(
    length(group_vars),
    used_names = reserved_names,
    prefix = "..marginplyr_key_"
  )
  names(key_names) <- group_vars

  if (length(group_vars) > 0L) {
    key_exprs <- lapply(group_vars, margin_column_pronoun)
    names(key_exprs) <- unname(key_names)
    .data <- dplyr::mutate(.data, !!!key_exprs)
  }

  conditions <- new_branch_conditions(
    keys = rlang::set_names(names(key_names), unname(key_names)),
    call = call
  )
  # On exit rather than after the loop, so that the branches that ran before an
  # error still report what they raised. Withholding a warning and then leaving
  # by any path but the one that replays it would lose it outright, which no
  # reading of the contract allows.
  on.exit(report_branch_warnings(conditions), add = TRUE)

  branches <- Map(
    function(grouping_set, set_id) {
      branch_dots <- rewrite_grouping_dots(
        dots,
        plan = plan,
        grouping_set = grouping_set,
        sql = FALSE
      )

      # Only the caller's expressions are wrapped. The checks and the branch
      # builders below raise Package conditions, which carry their own context
      # and are never deduplicated.
      #
      # The map is built per branch rather than once, because `branch_dots`
      # is where `grouping_bit()` has become this branch's own constant, and
      # that is the expression dplyr will quote.
      result <- with_branch_conditions(
        summarize_margin_branch(
          .data = .data,
          !!!branch_dots,
          .by = unname(key_names[grouping_set]),
          caller_labels = summaries$labels
        ),
        conditions = conditions,
        restatements = branch_argument_map(branch_dots, summaries$labels)
      )

      check_summary_output_names(
        get_col_names(result, dplyr::everything()),
        group_vars = group_vars,
        internal_names = unname(key_names[setdiff(group_vars, grouping_set)]),
        set_id_name = set_id_name,
        set_id_is_internal = set_id_is_internal
      )

      if (length(grouping_set) > 0L) {
        rename_pairs <- rlang::set_names(
          rlang::syms(unname(key_names[grouping_set])),
          grouping_set
        )
        result <- dplyr::rename(result, !!!rename_pairs)
      }

      result <- label_margin_branch(
        result,
        plan = plan,
        grouping_set = grouping_set,
        margin_labels = margin_labels,
        prototypes = column_info$prototypes,
        factor_info = column_info$factors
      )

      # A summary branch holds one row per group rather than one per source
      # row, and a column-less summary branch is reachable only with no keys
      # and no summaries: one group, the Grand total. The row `mutate()`
      # materialises there while the identifier column lasts is that group's,
      # so nothing is counted. What a `data.table` still cannot represent is
      # the result once the column goes away again -- a one-row, zero-column
      # table -- which is dtplyr's own answer to such a summary rather than
      # something this attachment decides, and is documented as a limit on
      # `summarize_with_margins()` where a caller meets it.
      add_grouping_set_id(
        result,
        set_id_name,
        set_id,
        count_branch_rows = FALSE
      )
    },
    plan$sets,
    plan$set_ids
  )

  combine_margin_branches(branches)
}

# `backend` is the operation's own, and it sits among the required arguments
# rather than after the optional one so that no positional call can reach a
# different arrangement than the two callers write.
expand_margin_union <- function(.data,
                                plan,
                                margin_labels,
                                column_info,
                                backend,
                                set_id_name = NULL) {
  # An expansion branch is the input's own rows, so a column-less one standing
  # for no rows has to stay empty, and only a backend that invents a row when
  # given a column needs to be told so by counting. Both halves are read from
  # the input once rather than from each branch: labelling adds a column only
  # for a dimension, and a column-less input has none, so every branch of one
  # is column-less too -- and a cube folds 2^n of them.
  count_branch_rows <- backend$invents_row_on_column_add &&
    length(get_col_names(.data, dplyr::everything())) == 0L

  branches <- Map(
    function(grouping_set, set_id) {
      result <- label_margin_branch(
        .data,
        plan = plan,
        grouping_set = grouping_set,
        margin_labels = margin_labels,
        prototypes = column_info$prototypes,
        factor_info = column_info$factors
      )

      add_grouping_set_id(
        result,
        set_id_name,
        set_id,
        count_branch_rows = count_branch_rows
      )
    },
    plan$sets,
    plan$set_ids
  )

  combine_margin_branches(branches)
}
