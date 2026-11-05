pkgload::load_all(quiet = TRUE)

local({
  data <- tibble::tibble(a = "a", b = 1L, value = 2, flag = TRUE)
  order <- character()
  by_predicate <- function(x) {
    order <<- c(order, paste0("by:", typeof(x)))
    is.logical(x)
  }
  dimension_predicate <- function(x) {
    order <<- c(order, paste0("dimension:", typeof(x)))
    is.double(x)
  }
  family <- grouping_sets(a, value, where(dimension_predicate))
  failing <- grouping_spec(!!family, b)
  passing <- grouping_spec(grouping_sets(a, where(dimension_predicate)), b)
  known_duplicate <- grouping_spec(grouping_sets(a, a), where(dimension_predicate))
  kinds <- vapply(data, typeof, character(1L))
  expected <- unname(c(paste0("by:", kinds), paste0("dimension:", kinds)))

  for (spec in list(failing, passing)) {
    for (policy in c("error", "drop", "keep")) {
      order <- character()
      outcome <- tryCatch(inspect_grouping(data, .grouping = spec,
        .by = where(by_predicate), .duplicates = policy, .format = "list"),
        error = identity)
      stopifnot(identical(order, expected))
      if (identical(spec, failing) && policy == "error") {
        stopifnot(inherits(outcome, "marginplyr_error"),
                  grepl("2, 3", conditionMessage(outcome), fixed = TRUE))
      } else {
        stopifnot(identical(outcome$fixed, rep(list("flag"), nrow(outcome))))
        want <- list(c("a", "b"), c("b", "value"))
        if (identical(spec, failing) && policy == "keep") {
          want <- c(want, want[2L])
        }
        stopifnot(identical(outcome$included, want))
      }
    }
  }

  order <- character()
  outcome <- tryCatch(inspect_grouping(data, .grouping = known_duplicate,
    .by = where(by_predicate), .format = "list"), error = identity)
  stopifnot(inherits(outcome, "marginplyr_error"),
            grepl("1, 2", conditionMessage(outcome), fixed = TRUE),
            identical(order, character()))
  cat("Uncertain duplicates and valid products preserve typed .by before",
      "dimension predicates in all three policies.\n")
})
