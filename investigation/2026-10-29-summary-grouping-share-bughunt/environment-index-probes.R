suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
assert_expected <- "--assert" %in% commandArgs(trailingOnly = TRUE)
foo <- 10
bar <- 30
data <- data.frame(g = c("a", "b"), x = c(1, 3), first = c("foo", "bar"))
indices <- list(
  quote(.env[[dplyr::first(first)]]),
  quote((.env)[[dplyr::first(first)]]),
  quote(base::`[[`(.env, dplyr::first(first)))
)
source_indices <- list(
  quote(.env[[if (previous > 0) "foo" else "bar"]]),
  quote((.env)[[if (previous > 0) "foo" else "bar"]]),
  quote(base::`[[`(.env, if (previous > 0) "foo" else "bar"))
)
misses <- 0L
for (helper in c("share_of_total", "share_of_parent")) {
  for (i in seq_along(indices)) {
    result <- tryCatch(rlang::inject(summarize_with_margins(
      data, total = sum(x),
      first = !!rlang::call2(helper, quote(total)),
      derived = !!indices[[i]], .grouping = rollup(g)
    )), error = identity)
    if (inherits(result, "error")) {
      stopifnot(inherits(result, "marginplyr_error"))
    } else {
      stopifnot(identical(result$derived, c(10, 30, 10)))
      misses <- misses + 1L
      cat("Missed preceding share:", helper, deparse1(indices[[i]]), "\n")
      print(result)
    }
    source <- tryCatch(rlang::inject(summarize_with_margins(
      data, previous = sum(x), source = !!source_indices[[i]],
      fraction = !!rlang::call2(helper, quote(source)), .grouping = rollup(g)
    )), error = identity)
    if (inherits(source, "error")) {
      stopifnot(inherits(source, "marginplyr_error"))
    } else {
      stopifnot(identical(source$source, rep(10, 3L)))
      misses <- misses + 1L
      cat("Missed source alias:", helper, deparse1(source_indices[[i]]), "\n")
      print(source)
    }
  }
  for (expr in list(quote(.env$foo), quote(.env[["foo"]]),
                    quote((.env)[["foo"]]))) {
    result <- rlang::inject(summarize_with_margins(
      data, previous = sum(x), source = !!expr,
      fraction = !!rlang::call2(helper, quote(source)), .grouping = rollup(g)
    ))
    stopifnot(identical(result$source, rep(10, 3L)))
  }
}
cat("Computed environment indices silently accepted:", misses,
    "; all six independent environment controls passed.\n")
if (assert_expected && misses) {
  stop("Pronoun indices must retain preceding-summary dependency checks.",
       call. = FALSE)
}
