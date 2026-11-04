# Replay the ambiguity check with a locally masked parenthesis operator.
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

data <- data.frame(s = c("p", "p"), key = c("x", "y"))
state <- rlang::env(hits = 0L)
scope <- rlang::env(
  s = grouping_set(key),
  "(" = function(x) {
    state$hits <- state$hits + 1L
    "s"
  }
)
construct <- function(constructor, argument) {
  eval(rlang::call2(constructor, argument), scope)
}
inspect <- function(spec, input = data) {
  inspect_grouping(input, .grouping = spec, .format = "list")
}
expand <- function(spec) {
  expand_with_margins(data, .grouping = spec, .margin_label = NULL, .id = "set")
}
ambiguous <- function(result) {
  inherits(result, "marginplyr_error") &&
    grepl("both a column", conditionMessage(result), fixed = TRUE)
}

mismatches <- 0L
for (constructor in c("grouping_sets", "grouping_spec", "rollup", "cube")) {
  bare <- construct(constructor, quote(s))
  for (consume in list(inspect, expand)) {
    stopifnot(ambiguous(tryCatch(consume(bare), error = identity)))
    for (depth in c(1L, 3L)) {
      expression <- quote(s)
      for (i in seq_len(depth)) {
        expression <- rlang::call2("(", expression)
      }
      wrapped <- construct(constructor, expression)
      result <- tryCatch(consume(wrapped), error = identity)
      if (!ambiguous(result)) {
        stopifnot(is.data.frame(result))
        mismatches <- mismatches + 1L
      }
    }
  }
}
cat("Parenthesized collisions accepted:", mismatches, "of 16\n")
cat("Calls to the masked operator:", state$hits, "\n")
print(tryCatch(inspect(construct("grouping_sets", quote((s))))$included, error = conditionMessage))

# The explicit column and specification remedies, plus a no-collision control.
explicit <- construct("grouping_sets", quote(tidyselect::all_of("s")))
stopifnot(identical(inspect(explicit)$included, list("s")))
stopifnot(identical(inspect(grouping_sets(!!scope$s))$included, list("key")))
no_collision <- inspect(construct("grouping_sets", quote((s))), data["key"])
stopifnot(identical(no_collision$included, list("key")))

if (assert) {
  stopifnot(mismatches == 0L, state$hits == 0L)
}
