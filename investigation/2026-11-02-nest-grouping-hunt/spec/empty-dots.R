# Reproduction and finite controls for a valid helper argument carrying empty dots.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
assert <- "--assert" %in% args
d <- tibble::tibble(a = 1L, payload = "x")
empty_by <- function(...) inspect_grouping(d, .by = all_of(c(...)), .format = "list")
minimal <- tryCatch(empty_by(), error = identity)
cat("Minimal result:\n")
print(minimal)
cat("Direct tidyselect control:\n")
direct <- function(...) tidyselect::eval_select(rlang::quo(all_of(c(...))), d)
print(direct())
stopifnot(identical(unname(direct()), integer()))
if (assert) {
  stopifnot(!inherits(minimal, "error"), identical(minimal$fixed[[1L]], character()))
  quit(status = 0L)
}
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
consumers <- list(
  inspect = function(data, by, spec) inspect_grouping(data, .by = !!by, .grouping = !!spec, .format = "list"),
  nest = function(data, by, spec) nest_with_margins(data, .by = !!by, .grouping = !!spec),
  nest_by = function(data, by, spec) nest_by_with_margins(data, .by = !!by, .grouping = !!spec),
  expand = function(data, by, spec) expand_with_margins(data, .by = !!by, .grouping = !!spec),
  summarize = function(data, by, spec) summarize_with_margins(data, n = dplyr::n(), .by = !!by, .grouping = !!spec)
)
rows <- list()
for (count in c(0L, 1L)) {
  for (helper in c("all_of", "any_of")) {
    for (aggregation in c("c", "unlist_list", "identity_c")) {
      for (position in c("by", constructors)) {
        for (consumer in names(consumers)) {
          wrapper <- function(...) {
            selection <- switch(aggregation, c = quote(c(...)), list = quote(list(...)),
              unlist_list = quote(unlist(list(...))), identity_c = quote(identity(c(...))))
            expr <- rlang::call2(helper, selection)
            if (aggregation == "identity_c") expr <- rlang::call2("identity", rlang::call2(helper, quote(c(...))))
            q <- rlang::new_quosure(expr, environment())
            reference <- tidyselect::eval_select(q, d)
            empty <- length(reference) == 0L
            by <- if (position == "by") q else rlang::quo(NULL)
            spec <- if (position == "by") NULL else eval(rlang::call2(get(position, asNamespace("marginplyr")), q))
            actual <- tryCatch(consumers[[consumer]](d, by, spec), error = identity)
            expected_refusal <- empty && position %in% c("rollup", "cube")
            observed <- if (inherits(actual, "error")) paste(class(actual)[[1L]], conditionMessage(actual), sep = ": ") else "success"
            cnd_call <- if (inherits(actual, "error")) paste(deparse(conditionCall(actual)), collapse = " ") else ""
            rows[[length(rows) + 1L]] <<- data.frame(count, helper, aggregation, position, consumer,
              expected = if (expected_refusal) "empty-unit refusal" else "success", observed, cnd_call)
          }
          if (count == 0L) wrapper() else wrapper("a")
        }
      }
    }
  }
}
result <- do.call(rbind, rows)
write.csv(result, "investigation/2026-11-02-nest-grouping-hunt/spec/empty-dots.csv", row.names = FALSE)
print(with(result, table(count, aggregation, observed == "success")))
# Empty c/unlist-list dots are valid selections; identity() is a control
# that defers to the real selection pass before any helper-name classification.
bad <- subset(result, count == 0L & aggregation != "identity_c")
stopifnot(nrow(bad) == 120L, all(grepl('getvarError: argument "value" is missing', bad$observed, fixed = TRUE)))
controls <- subset(result, count == 1L | aggregation == "identity_c")
stopifnot(nrow(controls) == 240L,
  all(controls$observed == "success" | (controls$expected == "empty-unit refusal" & grepl("marginplyr_error", controls$observed))))
cat("Confirmed 120 empty-dots classifier failures and 240 successful/required-refusal controls.\n")
print(sessionInfo())
