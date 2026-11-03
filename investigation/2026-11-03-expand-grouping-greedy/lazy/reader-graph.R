Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
root <- "investigation/2026-11-03-expand-grouping-greedy/lazy"
source <- tibble(g = c("b", "a", NA_character_), x = 1:3, z = c(TRUE, FALSE, NA))
fresh <- function() arrow::RecordBatchReader$create(arrow::record_batch(source[1, ]), arrow::record_batch(source[2:3, ]))
other <- arrow::Table$create(tibble(g = c("a", "b"), y = c(4L, 5L)))
builders <- list(
 direct = identity,
 select = function(x) select(x, everything()),
 filter = function(x) filter(x, x > 1L),
 mutate = function(x) mutate(x, x = x + 1L),
 group = function(x) group_by(x, z),
 summary = function(x) summarise(group_by(x, g), x = sum(x)),
 arrange = function(x) arrange(x, desc(x)),
 head = function(x) head(x, 1L),
 collapse = function(x) collapse(select(x, everything())),
 distinct = function(x) distinct(x),
 join_left = function(x) left_join(x, other, by = "g"),
 join_right = function(x) left_join(other, x, by = "g"),
 semi_left = function(x) semi_join(x, other, by = "g"),
 semi_right = function(x) semi_join(other, x, by = "g"),
 anti_left = function(x) anti_join(x, other, by = "g"),
 anti_right = function(x) anti_join(other, x, by = "g"),
 union_left = function(x) union_all(x, arrow::Table$create(source)),
 union_right = function(x) union_all(arrow::Table$create(source), x),
 union_nested = function(x) union_all(filter(arrow::Table$create(source), x > 1), filter(union_all(arrow::Table$create(source), x), x < 3)),
 join_nested = function(x) left_join(left_join(other, x, by = "g"), other, by = "g", suffix = c("", "_copy")),
 join_union = function(x) left_join(other, union_all(arrow::Table$create(source), x), by = "g"),
 union_join = function(x) union_all(left_join(other, x, by = "g"), left_join(other, arrow::Table$create(source), by = "g"))
)
rows <- list()
for (name in names(builders)) for (selection in c("name", "typed")) for (action in c("inspect", "expand")) {
 reader <- fresh()
 input <- tryCatch(builders[[name]](reader), error = identity)
 if (inherits(input, "error")) {
  rows[[length(rows) + 1L]] <- data.frame(shape = name, selection, action, outcome = "source-error", remaining = NA_integer_, detail = conditionMessage(input))
  next
 }
 value <- tryCatch(if (action == "inspect") {
  if (selection == "name") inspect_grouping(input, .grouping = rollup(g), .format = "list") else inspect_grouping(input, .grouping = rollup(where(is.character)), .format = "list")
 } else {
  if (selection == "name") expand_with_margins(input, .grouping = rollup(g)) else expand_with_margins(input, .grouping = rollup(where(is.character)))
 }, error = identity)
 remaining <- nrow(arrow::as_arrow_table(reader))
 outcome <- if (inherits(value, "error")) {
  if (action == "expand" && inherits(value, "marginplyr_error") && grepl("reader", conditionMessage(value), ignore.case = TRUE)) "expected-reader-refusal" else "error"
 } else if (action == "inspect") "inspection-success" else "unexpected-admission"
 if (remaining != 3L) outcome <- "reader-consumed"
 rows[[length(rows) + 1L]] <- data.frame(shape = name, selection, action, outcome, remaining, detail = if (inherits(value, "error")) conditionMessage(value) else paste(value$included, collapse = ";"))
}
results <- bind_rows(rows)
write.csv(results, file.path(root, "reader-graph.csv"), row.names = FALSE)
print(count(results, action, outcome))
print(filter(results, !outcome %in% c("expected-reader-refusal", "inspection-success")))
stopifnot(all(results$outcome %in% c("expected-reader-refusal", "inspection-success")))
