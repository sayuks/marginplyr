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
