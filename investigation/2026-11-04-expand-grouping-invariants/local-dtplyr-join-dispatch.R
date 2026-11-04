pkgload::load_all(".", quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)

capture <- function(expr) tryCatch(expr, error = identity)
plain <- function() dtplyr::lazy_dt(tibble(id = 1L))
derived <- function() {
  mutate(dtplyr::lazy_dt(tibble(id = 1L, g = "a", v = 10L)), w = v + 1L)
}
inspect <- function(source) {
  inspect_grouping(source, .grouping = grouping_set(where(is.character)))
}
expand <- function(source) {
  expand_with_margins(source, .grouping = rollup(where(is.character)),
                      .id = "sid", .sort = "last")
}

# In this join orientation, ordinary execution uses the right step's
# environment to evaluate the left step's arithmetic.
ordinary <- left_join(derived(), plain(), by = "id")
expected_plan <- inspect(collect(ordinary))
stopifnot(identical(expected_plan$included, "(g)"))
expected <- tibble(g = c("a", "Total"), sid = 1:2, id = c(1L, 1L),
                   v = c(10L, 10L), w = c(11L, 11L))
stopifnot(identical(inspect(ordinary), expected_plan),
          identical(collect(expand(ordinary)), expected))

sizes <- integer()
rhs <- local({
  `+` <- function(x, y) {
    sizes <<- c(sizes, length(x))
    if (length(x)) base::`+`(x, y) else character()
  }
  dtplyr::lazy_dt(tibble(id = 1L))
})
joined <- left_join(derived(), rhs, by = "id")
explicit <- collect(joined)
stopifnot(identical(explicit$w, 11L), identical(sizes, 1L))

sizes <- integer()
plan <- capture(inspect(joined))
cat("Custom operator lengths during typed inspection:", sizes, "\n")
if (inherits(plan, "error")) {
  cat(conditionMessage(plan), "\n")
} else {
  print(plan)
}
# A user-defined arithmetic dispatch can depend on row values. Refusal is
# permitted before executing it, as is a faithful plan if provable.
plan_ok <- length(sizes) == 0L &&
  (inherits(plan, "marginplyr_error") || identical(plan, expected_plan))

sizes <- integer()
query <- capture(expand(joined))
cat("Custom operator lengths during expansion construction:", sizes, "\n")
before_collect <- sizes
if (inherits(query, "error")) {
  cat(conditionMessage(query), "\n")
  expansion_ok <- inherits(query, "marginplyr_error")
} else {
  rows <- collect(query)
  print(rows)
  expansion_ok <- identical(rows, expected)
}
expansion_ok <- length(before_collect) == 0L && expansion_ok

# Reverse the lexical hazard. This promise is in the arithmetic step's
# discarded environment. Ordinary execution never forces it.
forced <- 0L
make_left <- function(`+`) {
  mutate(dtplyr::lazy_dt(tibble(id = 1L, g = "a", v = 10L)), w = v + 1L)
}
with_unused_binding <- function() {
  lhs <- make_left({
    forced <<- forced + 1L
    stop("The left join does not use this binding")
  })
  left_join(lhs, plain(), by = "id")
}
stopifnot(identical(collect(with_unused_binding())$w, 11L), forced == 0L)
unused_plan <- capture(inspect(with_unused_binding()))
cat("Unused operator promises forced during inspection:", forced, "\n")
unused_plan_ok <- forced == 0L && identical(unused_plan, expected_plan)
forced <- 0L
unused_query <- capture(expand(with_unused_binding()))
cat("Unused operator promises forced during expansion:", forced, "\n")
unused_expansion_ok <- forced == 0L &&
  inherits(unused_query, "dtplyr_step") &&
  identical(collect(unused_query), expected) &&
  forced == 0L

if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(plan_ok, expansion_ok, unused_plan_ok, unused_expansion_ok)
}
