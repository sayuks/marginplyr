pkgload::load_all(".", quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

root <- Table$create(tibble(g = "a", source_id = 19L))
spec <- rollup(g)
expand <- function(src, sort = "none") {
  result <- expand_with_margins(src, .grouping = spec, .id = "sid", .sort = sort)
  stopifnot(inherits(result, "arrow_dplyr_query"))
  result
}
check_payload <- function(result) {
  stopifnot(
    identical(result$source_id, c(19L, 19L)),
    identical(result$g, c("a", "Total")),
    identical(result$sid, c(1L, 2L))
  )
  identical(result$payload[[1L]], result$payload[[2L]])
}

payload_source <- mutate(root, payload = arrow_random())
stopifnot(inherits(payload_source, "arrow_dplyr_query"))
frozen <- collect(payload_source, as_data_frame = FALSE)
stopifnot(frozen$num_rows == 1L)
stopifnot(check_payload(collect(expand(frozen, "last"))))

payload_query <- expand(payload_source, "last")
payload_matches <- vapply(seq_len(16L), function(i) {
  check_payload(collect(payload_query))
}, logical(1))
cat("Equal random payloads for the two copies:", sum(payload_matches),
    "of", length(payload_matches), "\n")

# A single filter answer contains one row or no rows, so expansion must
# contain both occurrence ids or neither without knowing the random draw.
filter_source <- filter(root, arrow_random() < 0.5)
stopifnot(inherits(filter_source, "arrow_dplyr_query"))
frozen_filter <- collect(filter_source, as_data_frame = FALSE)
stopifnot(collect(expand(frozen_filter)) |> nrow() == 2L * frozen_filter$num_rows)
filter_query <- expand(filter_source)
row_counts <- vapply(seq_len(32L), function(i) {
  rows <- collect(filter_query)
  stopifnot(all(rows$source_id == 19L), all(rows$sid %in% c(1L, 2L)))
  nrow(rows)
}, integer(1))
print(table(row_counts))
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(all(payload_matches), all(row_counts %in% c(0L, 2L)))
}
