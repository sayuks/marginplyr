Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
outside <- new.env(parent = baseenv())
outside$prior <- 7
source <- tryCatch(summarize_with_margins(
  input, prior = sum(x),
  value = {
    base::list2env({list(outside = environment())}, envir = environment())
    sum(get("prior", envir = outside))
  },
  fraction = share_of_total(value), .grouping = rollup(g)
), error = identity)
print(source)
outside <- new.env(parent = baseenv())
outside$prior <- 7
later <- tryCatch(summarize_with_margins(
  input, value = sum(x), prior = share_of_total(value),
  derived = {
    base::list2env({list(outside = environment())}, envir = environment())
    sum(get("prior", envir = outside))
  },
  .grouping = rollup(g)
), error = identity)
print(later)
literal <- tryCatch(summarize_with_margins(
  input, prior = sum(x),
  value = {
    base::list2env(list(outside = environment()), envir = environment())
    sum(get("prior", envir = outside))
  },
  fraction = share_of_total(value), .grouping = rollup(g)
), error = identity)
stopifnot(inherits(literal, "marginplyr_error"))
cat("Literal-list control:", conditionMessage(literal), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(inherits(source, "marginplyr_error"), inherits(later, "marginplyr_error"))
} else {
  stopifnot(identical(source$value, c(1, 3, 4)),
            identical(later$derived, c(100, 300, 400)),
            identical(later$prior, c(.25, .75, 1)))
}
