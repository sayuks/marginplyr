# Replay from an archive of the investigated SHA. Default observes the dated
# failures; --assert instead enforces the public expansion contract.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
calls <- integer()
behavior <- "change"
as.list.hunt_summary_frame <- function(x, ...) {
  calls <<- c(calls, nrow(x))
  if (identical(behavior, "refuse") && nrow(x) > 0L) {
    stop("as.list is not a row-value conversion for this class")
  }
  list(out = rep(99L, nrow(x)))
}
frame <- function(x) {
  structure(data.frame(out = sum(unlist(x))),
            class = c("hunt_summary_frame", "data.frame"))
}
inputs <- list(data.frame = data.frame, tibble = tibble::as_tibble,
               data.table = data.table::as.data.table)
wrong_values <- false_refusals <- controls <- contract_cases <- 0L
for (input in names(inputs)) {
  data <- inputs[[input]](data.frame(g = c("a", "a", "b"), x = 1:3))
  for (role in c("grand", "fixed", "included", "rollup")) {
    for (selection in c(FALSE, TRUE)) {
      expr <- if (selection) quote(frame(dplyr::pick(x))) else quote(frame(x))
      ordinary_args <- list(quote(data), expr)
      if (role != "grand") ordinary_args$.by <- "g"
      calls <- integer()
      behavior <- "change"
      expected <- eval(as.call(c(list(quote(dplyr::summarize)), ordinary_args)))
      stopifnot(all(calls == 0L))
      if (role == "rollup") {
        total <- eval(as.call(list(quote(dplyr::summarize), quote(data), expr)))
        total$g <- "Total"
        expected <- dplyr::bind_rows(expected, total)
      }
      margin_args <- list(quote(data), expr)
      if (role == "fixed") margin_args$.by <- "g"
      if (role == "included") margin_args$.grouping <- grouping_set(g)
      if (role == "rollup") margin_args$.grouping <- rollup(g)
      calls <- integer()
      actual <- eval(as.call(c(list(quote(summarize_with_margins)), margin_args)))
      positive_calls <- sum(calls > 0L)
      if (contract) {
        stopifnot(identical(actual$out, expected$out))
      } else {
        stopifnot(identical(actual$out, rep(99L, nrow(actual))))
        stopifnot(positive_calls == nrow(actual))
        wrong_values <- wrong_values + 1L
        cat("wrong-value", input, role, selection,
            "expected", paste(expected$out, collapse = ","),
            "actual", paste(actual$out, collapse = ","),
            "positive-row as.list calls", positive_calls, "\n")
      }
      behavior <- "refuse"
      calls <- integer()
      reference <- eval(as.call(c(list(quote(dplyr::summarize)), ordinary_args)))
      stopifnot(all(calls == 0L))
      calls <- integer()
      refusal_result <- tryCatch(
        eval(as.call(c(list(quote(summarize_with_margins)), margin_args))),
        error = identity
      )
      if (contract) {
        stopifnot(!inherits(refusal_result, "error"))
        stopifnot(identical(refusal_result$out, expected$out))
        contract_cases <- contract_cases + 1L
      } else {
        stopifnot(inherits(refusal_result, "error"), any(calls > 0L))
        stopifnot(grepl("as.list is not a row-value conversion",
                       conditionMessage(refusal_result), fixed = TRUE))
        false_refusals <- false_refusals + 1L
      }
      names(margin_args) <- c("", "packed", tail(rlang::names2(margin_args), -2L))
      calls <- integer()
      packed <- eval(as.call(c(list(quote(summarize_with_margins)), margin_args)))
      stopifnot(all(calls == 0L), identical(packed$packed$out, expected$out))
      controls <- controls + 1L
    }
  }
}
if (contract) {
  cat("contract cases", contract_cases, "named-packed controls", controls, "\n")
} else {
  cat("wrong-values", wrong_values, "false-refusals", false_refusals,
      "named-packed controls", controls, "\n")
}
cat("R", as.character(getRversion()), "\n")
for (package in c("pkgload", "dplyr", "rlang", "vctrs", "tibble", "data.table")) {
  cat(package, as.character(packageVersion(package)), "\n")
}
