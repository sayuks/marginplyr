# Compare cell membership with native nesting, independently of outer order.
pkgload::load_all(quiet = TRUE)

keys <- list(
  empty_frame = tibble::tibble(.rows = 3L),
  empty_array = array(integer(), c(3L, 0L, 1L)),
  empty_matrix = matrix(integer(), 3L, 0L),
  asis = I(c(1, 1, 2)),
  named = c(a = 1, b = 2, c = 3),
  nested_empty = tibble::tibble(a = tibble::tibble(.rows = 3L)),
  nested_factor = tibble::tibble(a = structure(
    c(1L, 2L, NA_integer_), levels = c("a", NA_character_), class = "factor"
  )),
  POSIXlt = as.POSIXlt(as.Date("2020-01-01") + 0:2),
  expression = expression(x, x, y),
  nested = list(data.frame(x = 1), data.frame(x = 1), data.frame(x = 2))
)
count <- 0L
for (key in keys) {
  input <- tibble::tibble(g = key, row_id = 1:3)
  before <- serialize(input, NULL)
  reference <- dplyr::nest_by(input, g, .keep = TRUE)
  groups <- lapply(reference$data, `[[`, "row_id")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      for (sort in c("none", "first", "last")) {
        result <- verb(input, .grouping = rollup(g), .margin_label = NULL,
                       .keep = keep, .id = "set", .sort = sort)
        detail <- lapply(result$data[result$set == 1L], `[[`, "row_id")
        total <- result$data[result$set == 2L]
        projection <- c("g"[keep], "row_id")
        stopifnot(
          length(detail) == length(groups), setequal(detail, groups),
          length(total) == 1L, identical(total[[1L]]$row_id, 1:3),
          all(vapply(result$data, function(cell) {
            identical(names(cell), projection)
          }, logical(1))),
          identical(serialize(input, NULL), before)
        )
        count <- count + 1L
      }
    }
  }
}
cat(count, "native source-membership comparisons matched.\n")
