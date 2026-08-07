summarize_margin_branch <- function(.data,
                                    ...,
                                    .by) {
  dplyr::summarize(
    .data = .data,
    ...,
    .by = dplyr::all_of(.by)
  )
}

add_grouping_set_id <- function(result, set_id_name, set_id) {
  if (is.null(set_id_name)) {
    return(result)
  }
  # Injected for the same reason as the factor methods: a bare `set_id` here
  # resolves against the data mask first, so a grouping column of that name
  # would number every row from its own values instead of the branch's
  # grouping-set id. The `{set_id_name}` glue is evaluated in this environment
  # rather than the mask, so only the value needs injecting.
  dplyr::mutate(
    result,
    "{set_id_name}" := !!as.integer(set_id)
  )
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

      add_grouping_set_id(result, set_id_name, set_id)
    },
    plan$sets,
    plan$set_ids
  )

  Reduce(dplyr::union_all, branches)
}

expand_margin_union <- function(.data,
                                plan,
                                margin_labels,
                                column_info,
                                set_id_name = NULL) {
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

      add_grouping_set_id(result, set_id_name, set_id)
    },
    plan$sets,
    plan$set_ids
  )

  Reduce(dplyr::union_all, branches)
}
