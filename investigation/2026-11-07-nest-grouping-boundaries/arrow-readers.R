suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})
err <- function(expr) tryCatch(expr, error = identity)
fixtures <- list(
  empty = tibble(id = integer(), `fixed key` = character(), value = as.Date(character())),
  populated = tibble(id = 1:3, `fixed key` = c("a", "b", "a"), value = as.Date("2020-01-01") + 1:3)
)
derive <- list(
  direct = function(x) x,
  names = function(x) select(x, dplyr::everything()),
  projection = function(x) select(x, id, `fixed key`),
  rename = function(x) rename(x, date = value),
  derived = function(x) mutate(x, updated = id + 1L),
  absent = function(x) filter(x, id < 0L),
  group = function(x) group_by(x, id),
  union = function(x) union_all(x, filter(x, id < 0L))
)
selects <- list(
  rlang::expr(everything()),
  rlang::expr(all_of("fixed key")),
  rlang::expr(any_of(c("missing", "id"))),
  rlang::expr(where(is.character)),
  rlang::expr(where(is.integer)),
  rlang::expr(where(function(x) inherits(x, "Date")))
)
count <- c(match=0L, mismatch=0L, unsupported=0L, reader=0L)
for (kind in names(fixtures)) {
  frame <- fixtures[[kind]]
  nb <- nrow(frame)
  for (shape in names(derive)) {
    old <- arrow::arrow_table(frame)
    native <- err(derive[[shape]](old) |> collect() |> ungroup())
    if (inherits(native, "error")) {
      count[["unsupported"]] <- count[["unsupported"]] + 1L
      next
    }
    for (query in selects) {
      for (slot in c("fixed", "dimension")) {
        if (slot == "fixed" && shape == "group") next
        batch1 <- arrow::record_batch(head(frame, 1))
        batch2 <- arrow::record_batch(frame[seq_len(nb) > 1L,])
        reader <- arrow::RecordBatchReader$create(batch1,batch2)
        subject <- derive[[shape]](reader)
        groups <- if (inherits(subject,"RecordBatchReader")) character() else group_vars(subject)
        expected <- names(tidyselect::eval_select(query, native[0,]))
        res <- if (slot == "fixed") {
          inspect_grouping(subject, .by = !!query, .format = "list")$fixed[[1L]]
        } else {
          expected <- setdiff(expected, groups)
          inspect_grouping(subject, .grouping = grouping_set(!!query & all_of(setdiff(names(native), groups))), .format = "list")$included[[1L]]
        }
        if (!identical(res, expected)) {
          cat(kind, shape, slot, deparse(query), "wanted", toString(expected), "got", toString(res), "\n")
          count[["mismatch"]] <- count[["mismatch"]] + 1L
        } else {
          count[["match"]] <- count[["match"]] + 1L
        }
        readback <- as.data.frame(reader)
        stopifnot(identical(readback$id, frame$id), identical(readback[["fixed key"]], frame[["fixed key"]]))
        count[["reader"]] <- count[["reader"]] + 1L
      }
    }
  }
}
print(count)
stopifnot(count[["mismatch"]] == 0L)
