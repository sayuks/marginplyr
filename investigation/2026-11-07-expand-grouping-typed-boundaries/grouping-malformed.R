pkgload::load_all(".", quiet = TRUE)
data <- data.frame(a = "A", b = 1L)
constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)
entrances <- list(top = identity, nested = function(spec) grouping_sets(!!spec))
verbs <- list(inspect_grouping, expand_with_margins)
confirmed <- 0L
for (constructor in constructors) {
  control <- constructor(1L)
  expected <- constructor(a)
  empty <- eval(rlang::call2(constructor, rlang::missing_arg(), quote(a)))
  trailing <- eval(rlang::call2(constructor, quote(a), rlang::missing_arg()))
  non_list <- constructor(a)
  non_list$args <- 1L
  for (enter in entrances) {
    for (verb in verbs) {
      stopifnot(identical(verb(data, .grouping = enter(control)),
                          verb(data, .grouping = enter(expected))),
                identical(verb(data, .grouping = enter(trailing)),
                          verb(data, .grouping = enter(expected))))
      empty_result <- tryCatch(verb(data, .grouping = enter(empty)), error = identity)
      non_list_result <- tryCatch(
        verb(data, .grouping = enter(non_list)), error = identity
      )
      stopifnot(inherits(empty_result, "marginplyr_error"),
                grepl("empty", conditionMessage(empty_result), fixed = TRUE),
                inherits(non_list_result, "marginplyr_error"),
                identical(conditionMessage(non_list_result),
                          "Invalid Grouping specification."))
      for (item in list(1L, quote(a), quote(a + 1L), NULL)) {
        malformed <- constructor(a)
        malformed$args <- list(item)
        spec <- enter(malformed)
        result <- tryCatch(verb(data, .grouping = spec), error = identity)
        stopifnot(identical(class(result)[[1L]], "rlang_error"),
                  identical(conditionMessage(result), "`quo` must be a quosure"))
        confirmed <- confirmed + 1L
      }
    }
  }
}
cat(confirmed, "non-quosure internal-list entries leaked rlang diagnostics\n")
cat("20 ordinary captured integer selectors, 20 trailing-empty, 20 leading-empty, and 20 non-list controls matched\n")
