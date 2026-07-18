summarize_margin_union <- function(.data,
                                   dots,
                                   plan,
                                   .margin_label,
                                   column_info) {
  branches <- lapply(
    plan$sets,
    function(grouping_set) {
      branch_dots <- rewrite_grouping_dots(
        dots,
        plan = plan,
        grouping_set = grouping_set,
        sql = FALSE
      )

      result <- summarize_impl(
        .data = .data,
        !!!branch_dots,
        .margin_pairs = list(),
        .by = grouping_set
      )

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
                                include_set_id = FALSE) {
  branches <- Map(
    function(grouping_set, set_id) {
      result <- label_margin_branch(
        .data,
        plan = plan,
        grouping_set = grouping_set,
        .margin_label = .margin_label,
        prototypes = column_info$prototypes
      )

      if (include_set_id) {
        result <- dplyr::mutate(
          result,
          .marginplyr_set_id = as.integer(set_id)
        )
      }
      result
    },
    plan$sets,
    seq_along(plan$sets)
  )

  Reduce(dplyr::union_all, branches)
}
