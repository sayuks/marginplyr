pkgload::load_all(".", quiet = TRUE)
input <- tibble::tibble(a = c("x", "y"), b = 1:2)
reads <- 0L
predicate <- function(x) { reads <<- reads + 1L; is.integer(x) }
suffix <- grouping_sets(grouping_set(where(predicate)), grouping_set())
spec <- grouping_spec(grouping_sets(a, a), suffix)
for (verb in c("inspect_grouping", "nest_with_margins", "nest_by_with_margins")) {
  reads <- 0L
  result <- tryCatch(get(verb)(input, .grouping = spec), error = identity)
  cat(verb, "predicate calls:", reads, "\n")
  print(result)
}
reads <- 0L
control <- tryCatch(inspect_grouping(input, .grouping = grouping_spec(grouping_sets(a, a), grouping_set(where(predicate)))), error = identity)
cat("singleton control predicate calls:", reads, "\n")
print(control)
reads <- 0L
print(inspect_grouping(input, .grouping = spec, .duplicates = "keep", .format = "list"))
cat("keep predicate calls:", reads, "\n")
