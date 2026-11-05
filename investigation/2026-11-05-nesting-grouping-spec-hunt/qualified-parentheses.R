# Run from the package root using the command in the accompanying README.
pkgload::load_all(quiet = TRUE)

data <- tibble::tibble(a = c("x", "y"), b = 1:2)
selector <- quote(all_of(base::`(`("a")))
expected <- list("a")
spec <- grouping_set(!!selector)
ordinary <- inspect_grouping(data, .grouping = spec, .format = "list")
stopifnot(identical(ordinary$included, expected))

local({
  namespace <- get("::", baseenv())
  calls <- 0L
  `::` <- function(package, name) {
    if (identical(substitute(package), quote(base)) &&
        identical(substitute(name), as.name("("))) {
      return(function(...) {
        calls <<- calls + 1L
        if (is.character(tidyselect::peek_data()$a)) "a" else "b"
      })
    }
    eval(as.call(list(namespace, substitute(package), substitute(name))),
         envir = parent.frame())
  }

  direct <- tidyselect::eval_select(selector, data, env = environment())
  stopifnot(identical(names(direct), "a"), calls == 1L)
  spec <- grouping_set(!!selector)
  selected <- inspect_grouping(data, .grouping = spec, .format = "list")
  stopifnot(identical(selected$included, expected), calls == 3L)

  fixed <- inspect_grouping(data, .by = !!selector, .format = "list")
  stopifnot(identical(fixed$fixed, list("b")))
  cat("Direct selection selects a. Local inspection fixes b.\n")

  distinct <- grouping_sets(grouping_set(!!selector), grouping_set(b))
  mixed <- grouping_sets(
    grouping_set(!!selector),
    grouping_set(b),
    grouping_set(where(is.logical))
  )
  for (candidate in list(distinct, mixed)) {
    for (verb in list(inspect_grouping, nest_with_margins, nest_by_with_margins)) {
      calls <- 0L
      failure <- tryCatch(verb(data, .grouping = candidate), error = identity)
      stopifnot(
        inherits(failure, "marginplyr_error"),
        grepl("Duplicate grouping sets", conditionMessage(failure), fixed = TRUE),
        calls == 1L
      )
    }
  }
  cat("Inspection and both Nest verbs report false duplicates.\n")

  lazy <- dtplyr::lazy_dt(data)
  inspected <- inspect_grouping(lazy, .grouping = spec, .format = "list")
  nested <- dplyr::collect(nest_with_margins(lazy, .grouping = spec))
  stopifnot(
    identical(inspected$included, list("b")),
    identical(names(nested), c("a", "data")),
    identical(nested$a, c("x", "y"))
  )
  cat("dtplyr inspection selects b. Its Nest groups on a.\n")
})

cat(R.version.string, R.version$platform, "\n")
for (package in c("dplyr", "dtplyr", "tidyselect", "rlang")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
