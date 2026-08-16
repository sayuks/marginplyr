# The `UNION ALL` composition every branch list ends in, and the one place the
# package combines branches. `Reduce(dplyr::union_all, branches)` is
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

summarize_margin_branch <- function(.data,
                                    ...,
                                    .by) {
  dplyr::summarize(
    .data = .data,
    ...,
    .by = dplyr::all_of(.by)
  )
}

add_grouping_set_id <- function(result,
                                set_id_name,
                                set_id,
                                count_branch_rows) {
  if (is.null(set_id_name)) {
    return(result)
  }
  # Injected for the same reason as the factor methods: a bare `set_id` here
  # resolves against the data mask first, so a grouping column of that name
  # would number every row from its own values instead of the branch's
  # grouping-set id. The `{set_id_name}` glue is evaluated in this environment
  # rather than the mask, so only the value needs injecting.
  value <- set_id_expr(
    as.integer(set_id),
    result,
    count_branch_rows = count_branch_rows
  )
  dplyr::mutate(
    result,
    "{set_id_name}" := !!value
  )
}

# A literal recycles to whatever the branch holds, which is the whole of the
# attachment on a backend that can say how many rows a column-less branch has.
# `data.table` cannot: it reads a table's row count from its first column, so
# giving a zero-column table a column materialises one row, and every
# expansion branch of a zero-column `dtplyr` input used to arrive carrying a
# row no source row produced (#184). Counting is what the literal cannot do
# there -- dtplyr translates `n()` to `.N`, which is zero for that table -- so
# the identifier lands on as many rows as the branch has and no more.
#
# `count_branch_rows` is what each call site knows and this cannot: whether a
# row this attachment materialises would be a row the branch does not stand
# for. Both sites spell it out, because the two answers come from different
# facts rather than from one being the default.
#
# A branch that has columns keeps the literal whatever the site says, since
# that is every ordinary branch, on dtplyr as much as anywhere: the count is
# for the case the literal cannot express, not a second way of writing it.
set_id_expr <- function(set_id, result, count_branch_rows) {
  needs_count <- count_branch_rows &&
    length(get_col_names(result, dplyr::everything())) == 0L

  if (!needs_count) {
    return(set_id)
  }
  rlang::expr(rep(!!set_id, dplyr::n()))
}

summarize_margin_union <- function(.data,
                                   dots,
                                   plan,
                                   margin_labels,
                                   column_info,
                                   reserved_names,
                                   set_id_name = NULL,
                                   set_id_is_internal = FALSE) {
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

  branches <- Map(
    function(grouping_set, set_id) {
      branch_dots <- rewrite_grouping_dots(
        dots,
        plan = plan,
        grouping_set = grouping_set,
        sql = FALSE
      )

      result <- summarize_margin_branch(
        .data = .data,
        !!!branch_dots,
        .by = unname(key_names[grouping_set])
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
      # table -- which is dtplyr's own answer to such a summary and not
      # something this attachment decides.
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

expand_margin_union <- function(.data,
                                plan,
                                margin_labels,
                                column_info,
                                set_id_name = NULL,
                                backend = grouping_backend(.data)) {
  # An expansion branch is the input's own rows, so a column-less one standing
  # for no rows has to stay empty, and only a backend that invents a row when
  # given a column needs to be told so by counting. Read once rather than per
  # branch, since the answer is the input's and a cube folds 2^n of them.
  count_branch_rows <- backend$invents_row_on_column_add

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
