# Compare emitted copies to branch-by-branch source slicing.
pkgload::load_all(quiet = TRUE)

check_copies <- function(input, fixed, sets, order_mode) {
  specification <- grouping_sets(!!!lapply(sets, function(columns) {
    grouping_set(tidyselect::all_of(columns))
  }))
  observed <- expand_with_margins(
    input, .by = tidyselect::all_of(fixed), .grouping = specification,
    .duplicates = "keep", .margin_label = NULL, .id = "set",
    .sort = order_mode
  )

  dimensions <- unique(unlist(sets, use.names = FALSE))
  branches <- lapply(seq_along(sets), function(id) {
    result <- input
    for (column in setdiff(dimensions, sets[[id]])) {
      result[[column]] <- vctrs::vec_init(input[[column]], nrow(input))
    }
    result$set <- rep.int(id, nrow(input))
    result
  })
  expected <- do.call(vctrs::vec_rbind, branches)
  names_expected <- c(fixed, dimensions, "set",
                      setdiff(names(input), c(fixed, dimensions)))
  stopifnot(identical(names(observed), names_expected))
  row_order <- order(observed$set, observed$row_id)
  expected_order <- order(expected$set, expected$row_id)
  for (column in names_expected) {
    stopifnot(identical(
      observed[[column]][row_order], expected[[column]][expected_order]
    ))
  }
  if (order_mode != "none") {
    keys <- list()
    for (column in c(fixed, dimensions)) {
      if (column %in% dimensions) {
        included <- vapply(sets, function(set) column %in% set, logical(1))
        bit <- as.integer(!included[observed$set])
        keys <- c(keys, list(if (order_mode == "first") 1L - bit else bit))
      }
      values <- observed[[column]]
      keys <- c(keys, list(is.na(values), values))
    }
    keys <- c(keys, list(observed$set))
    ordered <- do.call(order, keys)
    stopifnot(all(vapply(keys, function(key) {
      identical(key, key[ordered])
    }, logical(1))))
  }
}

fixtures <- list(
  c(2L, NA_integer_, 1L, 1L),
  c(1.25, NA_real_, 0.25, 0.25),
  c("z", NA_character_, "a", "a"),
  c(TRUE, NA, FALSE, FALSE),
  factor(c("z", NA, "a", "a"), levels = c("z", "a", "unused")),
  ordered(c("z", NA, "a", "a"), levels = c("z", "a", "unused")),
  as.Date(c("2026-01-02", NA, "2026-01-01", "2026-01-01")),
  as.POSIXct(c("2026-01-02", NA, "2026-01-01", "2026-01-01"), tz = "UTC")
)
families <- list(
  list(character()), list("a"), list(c("a", "b"), "a", character()),
  list(character(), "b", c("a", "b")),
  list("b", "a", "b", character())
)
comparisons <- 0L
for (values in fixtures) {
  input <- tibble::tibble(
    a = values, b = c(1L, 2L, NA_integer_, 2L),
    fixed = c(2L, NA_integer_, 1L, 1L), row_id = seq_along(values),
    payload = list(NULL, integer(), NA_integer_, 1:3)
  )
  for (rows in list(integer(), 1L, c(4L, 2L, 1L, 3L))) {
    for (fixed in list(character(), "fixed")) {
      for (sets in families) {
        for (order_mode in c("none", "first", "last")) {
          check_copies(input[rows, ], fixed, sets, order_mode)
          comparisons <- comparisons + 1L
        }
      }
    }
  }
}
stopifnot(comparisons == 720L)
cat(comparisons, "local row-copy, schema and structural-order controls matched\n")
