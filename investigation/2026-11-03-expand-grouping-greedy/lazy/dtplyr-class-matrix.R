Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
root <- "investigation/2026-11-03-expand-grouping-greedy/lazy"
ll <- logLik(lm(c(1, 3, 2, 5) ~ c(1, 2, 3, 4)))
families <- list(logLik = ll, dist = dist(matrix(1:2)))
shapes <- list(root = identity, filter = function(x) filter(x, TRUE), cache = function(x) compute(x, name = "cached"))
plans <- list(included = list("g"), rollup = list("g", character()), omitted_first = list(character(), "g"), duplicates = list("g", character(), character()))
rows <- list()
for (family in names(families)) for (n in c(0L, 1L, 3L)) for (role in c("dimension", "fixed", "payload")) for (shape in names(shapes)) for (plan in names(plans)) for (label in c("NULL", "NA_character_")) for (id in c(FALSE, TRUE)) for (sort in c("none", "last")) {
 classed <- vctrs::vec_rep(families[[family]], n)
 src <- tibble(g = if (role == "dimension") classed else rep(c("b", "a", NA_character_), length.out = n), rid = seq_len(n))
 if (role == "fixed") src$k <- classed
 if (role == "payload") src$p <- classed
 dt <- data.table::as.data.table(src)
 before <- data.table::copy(dt)
 input <- shapes[[shape]](dtplyr::lazy_dt(dt, immutable = TRUE))
 sets <- plans[[plan]]
 branches <- lapply(seq_along(sets), function(i) {
  out <- input
  if (length(sets[[i]]) == 0L) {
   missing <- vctrs::vec_init(src$g, 1L)
   out <- mutate(out, g = vctrs::vec_rep(!!missing, length(g)))
  }
  if (id) out <- mutate(out, set = as.integer(i))
  out
 })
 control <- tryCatch(Reduce(union_all, branches) |> collect(), error = identity)
 spec <- switch(plan, included = grouping_set(g), rollup = rollup(g), omitted_first = grouping_sets(grouping_set(), grouping_set(g)), duplicates = grouping_sets(grouping_set(g), grouping_set(), grouping_set()))
 actual <- tryCatch(if (role == "fixed") {
  expand_with_margins(input, .by = k, .grouping = spec, .margin_label = if (label == "NULL") NULL else NA_character_, .id = if (id) "set" else NULL, .sort = sort, .duplicates = "keep") |> collect()
 } else {
  expand_with_margins(input, .grouping = spec, .margin_label = if (label == "NULL") NULL else NA_character_, .id = if (id) "set" else NULL, .sort = sort, .duplicates = "keep") |> collect()
 }, error = identity)
 outcome <- if (inherits(control, "error")) "ordinary-error" else if (inherits(actual, "error")) "package-error" else {
  order_rows <- function(x) {
   ix <- order(x$rid, if (id) x$set else vctrs::vec_detect_missing(x$g))
   x <- x[ix, names(control), drop = FALSE]
   x
  }
  if (isTRUE(all.equal(order_rows(actual), order_rows(control)))) "match" else "mismatch"
 }
 unchanged <- identical(before, dt)
 rows[[length(rows) + 1L]] <- data.frame(family, n, role, shape, plan, label, id, sort, outcome, unchanged, detail = if (inherits(actual, "error")) conditionMessage(actual) else if (inherits(control, "error")) conditionMessage(control) else "")
}
results <- bind_rows(rows)
write.csv(results, file.path(root, "dtplyr-class-matrix.csv"), row.names = FALSE)
print(count(results, family, role, outcome))
print(count(filter(results, outcome == "package-error"), family, n, plan))
stopifnot(all(results$unchanged), !any(results$outcome %in% c("mismatch", "ordinary-error")))
