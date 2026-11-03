Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
input <- data.table::data.table(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
before <- data.table::copy(input)
observed <- new.env(parent = emptyenv())
observed$rows <- 0L
track <- function(x) {
  observed$rows <- observed$rows + length(x)
  rep(TRUE, length(x))
}
source <- dplyr::filter(dtplyr::lazy_dt(input), track(x))
query <- summarize_with_margins(
  source, value = sum(x),
  independent = local({
    base::list2env(list(.BY = c(5, 7)), envir = environment())
    sum(.BY)
  }),
  fraction = share_of_total(value), .grouping = rollup(g)
)
stopifnot(inherits(query, "dtplyr_step"), observed$rows == 0L,
          identical(input, before))
cat("Construction visited", observed$rows, "input rows.\n")
out <- dplyr::collect(query)
stopifnot(observed$rows > 0L, identical(input, before),
          identical(out$independent, c(1, 3, 4)))
cat("Explicit collection visited", observed$rows, "input rows.\n")
ordinary <- tryCatch({
  dplyr::collect(dplyr::summarise(
    dtplyr::lazy_dt(input), independent = local({
      base::list2env(list(.BY = c(5, 7)), envir = environment())
      sum(.BY)
    }), .by = g
  ))
}, error = identity)
plain <- tryCatch({
  dplyr::collect(summarize_with_margins(
    dtplyr::lazy_dt(input), value = sum(x),
    independent = local({
      base::list2env(list(.BY = c(5, 7)), envir = environment())
      sum(.BY)
    }), .grouping = rollup(g)
  ))
}, error = identity)
stopifnot(inherits(ordinary, "error"), inherits(plain, "error"),
          identical(input, before))
cat("Ordinary refusal:", conditionMessage(ordinary), "\n")
cat("No-share refusal:", conditionMessage(plain), "\n")
