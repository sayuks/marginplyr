# Compare separately captured constructor positions with explicit name vectors.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
suppressPackageStartupMessages(library(tidyselect))

data <- data.frame(a01 = c("A", "B"), a02 = 1:2, b01 = c("C", "D"),
                   flag = c(TRUE, FALSE), value = c(4, 8))
helpers <- rlang::exprs(
  starts_with("a"), ends_with("01"), contains("02"), matches("01$"),
  num_range("a", 1:2, width = 2), one_of("value", "a02"), everything(),
  last_col(1), all_of(c("b01", "a01")), any_of(c("missing", "flag")),
  where(is.integer)
)
selected <- function(x) names(tidyselect::eval_select(x, data))
fixed <- function(x) rlang::call2(quote(tidyselect::all_of), selected(x))
inspect <- function(maker, x, y) {
  spec <- eval(rlang::call2(maker, x, y))
  tryCatch(inspect_grouping(data, .grouping = spec, .duplicates = "keep",
                            .format = "list"), error = identity)
}
same <- function(x, y) {
  if (inherits(x, "error") && inherits(y, "error")) {
    return(identical(class(x), class(y)) &&
             identical(conditionMessage(x), conditionMessage(y)))
  }
  identical(x, y)
}
failures <- 0L
calls <- 0L
for (maker in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
  for (first in helpers) {
    for (second in helpers) {
      for (op in c("-", "!")) {
        excluded <- rlang::call2(op, second)
        actual <- inspect(maker, first, excluded)
        reference <- inspect(maker, fixed(first), fixed(excluded))
        calls <- calls + 1L
        if (!same(actual, reference)) {
          failures <- failures + 1L
          print(list(maker = maker, first = first, second = excluded,
                     actual = actual, reference = reference))
        }
      }
    }
  }
}
cat(calls, "sibling comparisons,", failures, "mismatches\n")
stopifnot(failures == 0L)
