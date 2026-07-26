new_margin_internal_names <- function(count, used_names, prefix) {
  stopifnot(
    length(count) == 1L,
    is.numeric(count),
    !is.na(count),
    count >= 0L,
    is.character(used_names),
    length(prefix) == 1L,
    is.character(prefix),
    !is.na(prefix)
  )

  result <- character(count)
  for (i in seq_len(count)) {
    candidate <- paste0(prefix, i)
    while (candidate %in% c(used_names, result)) {
      candidate <- paste0(candidate, "_")
    }
    result[[i]] <- candidate
  }
  result
}

check_summary_group_overwrite <- function(output_names, group_vars) {
  overwritten_groups <- intersect(output_names, unique(group_vars))
  if (length(overwritten_groups) == 0L) {
    return(invisible(NULL))
  }

  stop(
    "Summary results cannot overwrite grouping column",
    if (length(overwritten_groups) == 1L) " " else "s ",
    paste0("`", overwritten_groups, "`", collapse = ", "),
    ".",
    call. = FALSE
  )
}
