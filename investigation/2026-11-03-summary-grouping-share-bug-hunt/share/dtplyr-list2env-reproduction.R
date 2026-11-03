Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
input <- data.frame(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
summarize <- function(source) {
  summarize_with_margins(
    source, value = sum(x),
    independent = local({
      base::list2env(list(.BY = c(5, 7)), envir = environment())
      sum(.BY)
    }),
    fraction = share_of_total(value), .grouping = rollup(g)
  )
}
actual <- tryCatch({
  query <- summarize(dtplyr::lazy_dt(input))
  stopifnot(inherits(query, "dtplyr_step"))
  cat("Lazy expression:\n")
  dplyr::show_query(query)
  dplyr::collect(query)
}, error = identity)
print(actual)
local <- summarize(input)
print(local)
safe <- input
names(safe)[names(safe) == ".BY"] <- "measure"
renamed <- dplyr::collect(summarize(dtplyr::lazy_dt(safe)))
print(renamed)
stopifnot(identical(local$independent, rep(12, 3)),
          identical(renamed$independent, rep(12, 3)),
          identical(input$.BY, c(1, 3)))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(inherits(actual, "marginplyr_error") ||
              identical(actual$independent, rep(12, 3)))
} else {
  stopifnot(identical(actual$independent, c(1, 3, 4)),
            identical(actual$fraction, c(.25, .75, 1)))
}
