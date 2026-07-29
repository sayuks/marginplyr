summarize_margin_branch <- function(.data,
                                    ...,
                                    .by) {
  dplyr::summarize(
    .data = .data,
    ...,
    .by = dplyr::all_of(.by)
  )
}

summarize_margin_union <- function(.data,
                                   dots,
                                   plan,
                                   .margin_label,
                                   column_info,
                                   reserved_names) {
  group_vars <- unique(c(plan$by, plan$dimensions))
  key_names <- new_margin_internal_names(
    length(group_vars),
    used_names = reserved_names,
    prefix = "..marginplyr_key_"
  )
  names(key_names) <- group_vars

  if (length(group_vars) > 0L) {
    key_exprs <- lapply(
      group_vars,
      function(var) rlang::expr(.data[[!!var]])
    )
    names(key_exprs) <- unname(key_names)
    .data <- dplyr::mutate(.data, !!!key_exprs)
  }

  branches <- lapply(
    plan$sets,
    function(grouping_set) {
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

      result_names <- get_col_names(result, dplyr::everything())
      unexpected_internal_names <- intersect(
        result_names,
        unname(key_names[setdiff(group_vars, grouping_set)])
      )
      if (length(unexpected_internal_names) > 0L) {
        stop(
          "Dynamically generated summary output names conflict with internal ",
          "grouping columns: ",
          paste0("`", unexpected_internal_names, "`", collapse = ", "),
          ". Use different summary output names.",
          call. = FALSE
        )
      }

      check_summary_group_overwrite(
        result_names,
        group_vars = group_vars
      )
      if (length(grouping_set) > 0L) {
        rename_pairs <- rlang::set_names(
          rlang::syms(unname(key_names[grouping_set])),
          grouping_set
        )
        result <- dplyr::rename(result, !!!rename_pairs)
      }

      label_margin_branch(
        result,
        plan = plan,
        grouping_set = grouping_set,
        .margin_label = .margin_label,
        prototypes = column_info$prototypes
      )
    }
  )

  Reduce(dplyr::union_all, branches)
}

expand_margin_union <- function(.data,
                                plan,
                                .margin_label,
                                column_info,
                                set_id_name = NULL) {
  branches <- Map(
    function(grouping_set, set_id) {
      result <- label_margin_branch(
        .data,
        plan = plan,
        grouping_set = grouping_set,
        .margin_label = .margin_label,
        prototypes = column_info$prototypes
      )

      if (!is.null(set_id_name)) {
        result <- dplyr::mutate(
          result,
          "{set_id_name}" := as.integer(set_id)
        )
      }
      result
    },
    plan$sets,
    seq_along(plan$sets)
  )

  Reduce(dplyr::union_all, branches)
}
