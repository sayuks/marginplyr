# The input the Contextual helper probes read, in a helper file because two
# suites probe those helpers along different axes and had a copy each.
# `test-contextual-helpers.R` varies how a spelling is *written* -- bare,
# qualified, parenthesized -- and `test-static-expression-analysis.R` varies
# what an argument *is*, which is #169's question. Neither varies the input, so
# a second copy of it recorded nothing except which suite was written first.
#
# Two numeric columns so that a selection can select more than one, and two
# dimensions so that a rollup and a cube of them differ -- which is what the
# constructor case needs to tell a caller's own function from the package's,
# and what gives a `rollup()` a parent level for a Parent share to divide by.
#
# A column added here reaches both suites, and it is not free in either: this
# input's column set is pinned. `test-contextual-helpers.R` selects
# `where(is.numeric)` and `dplyr::everything()` over it and asserts the names
# and the count that come back, which is what makes a caller binding of those
# spellings visible at all, and the probes here take a `rollup()` of both
# dimensions. Adding one means reading those expectations rather than only the
# suite that wanted it -- which is the cost of one input, paid against a second
# copy silently disagreeing with the first.
contextual_probe_data <- function() {
  data.frame(
    region = c("E", "E", "W", "W"),
    grade = c("a", "b", "a", "b"),
    units = c(1, 2, 3, 4),
    qty = c(4, 5, 6, 7),
    stringsAsFactors = FALSE
  )
}
