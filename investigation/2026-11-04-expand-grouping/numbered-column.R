# Replay against the checkout passed in MARGINPLYR_HUNT_SOURCE, or the cwd.
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

read_spec <- function(data, spec) {
  inspect_grouping(data, .grouping = spec, .format = "list")
}

constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
spellings <- c("..01", "..0001", "..+1", ".. 1", ".. +02", "..\t1", "..0", "..-1")
refusals <- 0L
matches <- 0L
for (column in spellings) {
  data <- data.frame(key = c("a", "b"))
  data[[column]] <- c("u", "v")
  symbol <- as.name(column)
  selection <- rlang::new_quosure(symbol, globalenv())
  ordinary <- names(tidyselect::eval_select(selection, data))
  stopifnot(identical(ordinary, column))
  by <- inspect_grouping(data, .by = !!selection, .format = "list")
  stopifnot(identical(by$fixed, list(column)))

  for (constructor in constructors) {
    explicit <- eval(rlang::call2(
      constructor, rlang::expr(tidyselect::all_of(!!column))
    ))
    expected <- read_spec(data, explicit)
    stopifnot(identical(expected$included[[1L]], column))
    for (depth in c(0L, 1L, 3L)) {
      argument <- symbol
      for (i in seq_len(depth)) {
        argument <- rlang::call2("(", argument)
      }
      written <- if (depth == 0L) {
        get(constructor)(!!selection)
      } else {
        eval(rlang::call2(constructor, argument), globalenv())
      }
      result <- tryCatch(read_spec(data, written), error = identity)
      if (inherits(result, "error")) {
        stopifnot(grepl("used in an incorrect context", conditionMessage(result)))
        refusals <- refusals + 1L
      } else {
        stopifnot(identical(result, expected))
        matches <- matches + 1L
      }
    }
  }
}
cat("Injected/parenthesized constructor comparisons:", matches, "matches,",
    refusals, "false refusals\n")

data <- data.frame(key = c("a", "b"), check.names = FALSE)
data[["..01"]] <- c("u", "v")
ordinary_expansion <- expand_with_margins(
  data, .grouping = rollup(tidyselect::all_of("..01")),
  .margin_label = NULL, .id = "set"
)
written_expansion <- tryCatch(
  expand_with_margins(
    data, .grouping = rollup(!!rlang::new_quosure(as.name("..01"), globalenv())), .margin_label = NULL, .id = "set"
  ),
  error = identity
)
print(ordinary_expansion)
if (inherits(written_expansion, "error")) {
  cat(conditionMessage(written_expansion), "\n")
}

forward <- function(...) {
  # The constructed pair reaches the reader instead of rlang's initial capture.
  argument <- rlang::call2("(", as.name("..01"))
  read_spec(data, grouping_sets(!!argument))
}
stopifnot(identical(forward(grouping_set(key))$included, list("key")))

if (assert) {
  stopifnot(refusals == 0L, identical(written_expansion, ordinary_expansion))
}
