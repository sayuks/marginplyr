# Investigation-only public-interface replay, 2026-11-02.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

input <- tibble(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
replay <- function(direction, setter = "list2env", share = TRUE) {
  e <- new.env(parent = baseenv())
  mutation <- if (setter == "list2env") {
    quote(base::list2env(list(e = environment()), envir = environment()))
  } else {
    quote(base::assign("e", environment(), envir = environment()))
  }
  value <- rlang::expr({ !!mutation; sum(get("prior", envir = e)) })
  dots <- if (direction == "source") {
    list(prior = quote(sum(x)), value = value)
  } else {
    list(value = quote(sum(x)), prior = if (share) quote(share_of_total(value)) else quote(sum(x)),
         derived = value)
  }
  if (direction == "source" && share) {
    dots$fraction <- quote(share_of_total(value))
  }
  call <- rlang::call2("summarize_with_margins", quote(input), !!!dots,
                       .grouping = quote(rollup(g)))
  tryCatch(eval(call), error = identity)
}

missing <- 0L
for (direction in c("source", "later")) {
  observed <- replay(direction)
  canonical <- replay(direction, "assign")
  ordinary <- replay(direction, share = FALSE)
  stopifnot(inherits(canonical, "marginplyr_error"), !inherits(ordinary, "error"))
  cat(direction, "list2env result:\n")
  print(observed)
  cat("assign control:", conditionMessage(canonical), "\n")
  if (!inherits(observed, "marginplyr_error")) missing <- missing + 1L
  if (direction == "source") {
    stopifnot(identical(observed$value, c(1, 3, 4)))
  } else {
    stopifnot(identical(observed$prior, c(0.25, 0.75, 1)),
              identical(observed$derived, c(100, 300, 400)))
  }
}
cat("Missing dependency refusals:", missing, "\n")
if ("--assert" %in% commandArgs(TRUE)) stopifnot(missing == 0L)
