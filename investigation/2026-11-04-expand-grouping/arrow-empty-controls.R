pkgload::load_all(quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
fixtures <- list(
  factor(c("b", "a"), levels = c("b", "z", "a")),
  ordered(c("b", "a"), levels = c("b", "z", "a")),
  bit64::as.integer64(c("9007199254740995", "9007199254740993"))
)
for (col in fixtures) {
  full <- Table$create(data.frame(g = col, source_id = 1:2))
  empty <- filter(full, source_id < 0L)
  for (x in list(full, empty)) {
    for (q in list(
      plain = x, sorted = arrange(x, source_id),
      collapsed = collapse(x), union = union_all(x, x),
      branch = expand_with_margins(
        x, .grouping = grouping_set(g), .margin_label = NULL, .id = "sid"
      ),
      rollup = expand_with_margins(
        x, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
      ),
      rollup_ordered = expand_with_margins(
        x, .grouping = rollup(g), .margin_label = NULL, .id = "sid",
        .sort = "last"
      )
    )) {
      output <- collect(q)$g
      cat(schema(q)$GetFieldByName("g")$type$ToString(),
          length(output), paste(class(output), collapse = "/"),
          paste(levels(output), collapse = "/"), "\n")
    }
  }
}
