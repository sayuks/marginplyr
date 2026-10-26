# Dated public-API probes; run from the repository root. Each FAIL identifies
# a contract violation measured on the investigation's pinned main commit.
suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

capture_error <- function(code) {
  tryCatch(force(code), error = identity)
}

probe_grouping_operator <- function() {
  input <- data.frame(a = c("x", "x"), b = 1:2)
  expected <- summarise(
    input, n = n(),
    .by = base::`*`(
      if (is.numeric(tidyselect::peek_data()$a)) 1L else 2L, 1L
    )
  )
  actual <- summarize_with_margins(
    input, n = n(),
    .by = base::`*`(
      if (is.numeric(tidyselect::peek_data()$a)) 1L else 2L, 1L
    )
  )
  cat("dplyr fixed key b, two count-one rows:\n")
  print(expected)
  cat("Margin actual:\n")
  print(actual)
  identical(actual, expected)
}

probe_parenthesized_across <- function() {
  input <- tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  factory <- function(offset) {
    force(offset)
    function(x) sum(x) + offset
  }
  expected <- summarise(
    group_by(input, g), (across(x, factory(n()))), .groups = "drop"
  )
  actual <- summarize_with_margins(
    input, (across(x, factory(n()))), .grouping = grouping_set(g)
  )
  cat("dplyr x:", paste(expected$x, collapse = ", "), "\n")
  cat("Margin x:", paste(actual$x, collapse = ", "), "\n")
  identical(actual, expected)
}

probe_template_error <- function() {
  input <- tibble(x = 1:2)
  run <- function(with_share) {
    reads <- 0L
    template <- function() {
      reads <<- reads + 1L
      if (reads == 1L) {
        rlang::abort("template failed", class = "hunt_template_error")
      }
      "total"
    }
    result <- capture_error(if (with_share) {
      summarize_with_margins(
        input, across(x, sum, .names = template()),
        p = share_of_total(total)
      )
    } else {
      summarize_with_margins(input, across(x, sum, .names = template()))
    })
    cat("with_share:", with_share, "; reads:", reads,
        "; error:", inherits(result, "error"), "\n")
    if (!inherits(result, "error")) print(result)
    list(reads = reads, result = result)
  }
  control <- run(FALSE)
  actual <- run(TRUE)
  inherits(control$result, "error") && inherits(actual$result, "error") &&
    identical(actual$reads, 1L)
}

probe_selector_context <- function() {
  input <- tibble(x = 1:2)
  expected <- summarise(
    input,
    across(all_of(if (n() == 0L) "x" else character()), sum,
           .names = "unused"),
    total = sum(x)
  )
  actual <- capture_error(summarize_with_margins(
    input,
    across(all_of(if (n() == 0L) "x" else character()), sum,
           .names = "unused"),
    total = sum(x), p = share_of_total(total)
  ))
  cat("dplyr ordinary columns:\n")
  print(expected)
  cat("Margin actual:\n")
  if (inherits(actual, "error")) {
    cat(conditionMessage(actual), "\n")
  } else {
    print(actual)
  }
  !inherits(actual, "error") && identical(actual$unused, expected$unused) &&
    identical(actual$p, 1)
}

probe_reflective_dependency <- function() {
  input <- tibble(g = c("a", "b"), v = c(2, 6), share = c(20, 60))
  direct <- capture_error(summarize_with_margins(
    input, first = sum(v), total = first + 1,
    share = share_of_total(total), .grouping = rollup(g)
  ))
  actual <- capture_error(summarize_with_margins(
    input, first = sum(v), total = get("first", pos = -1L) + 1,
    share = share_of_total(total), .grouping = rollup(g)
  ))
  leaked <- capture_error(summarize_with_margins(
    input, total = sum(v), share = share_of_total(total),
    derived = sum(get("share", pos = -1L)), .grouping = rollup(g)
  ))
  cat("direct reference rejected:", inherits(direct, "marginplyr_error"),
      "\nReflective source:\n")
  if (inherits(actual, "error")) {
    cat(conditionMessage(actual), "\n")
  } else {
    print(actual)
  }
  cat("Later ordinary summary reading earlier share:\n")
  if (inherits(leaked, "error")) {
    cat(conditionMessage(leaked), "\n")
  } else {
    print(leaked)
  }
  inherits(direct, "marginplyr_error") &&
    inherits(actual, "marginplyr_error") &&
    inherits(leaked, "marginplyr_error")
}

probe_bound_selection <- function() {
  input <- data.frame(a = "A", b = "B")
  reads <- 0L
  selection_env <- new.env(parent = environment())
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (reads %% 2L == 1L) "a" else "b"
  }, selection_env)
  selected <- suppressWarnings(eval(
    quote(tidyselect::eval_select(rlang::quo(cols), input)), selection_env
  ))
  control_reads <- reads
  reads <- 0L
  actual <- suppressWarnings(eval(quote(inspect_grouping(
    input, .grouping = grouping_set(cols), .format = "list"
  )), selection_env))
  cat("tidyselect reads:", control_reads,
      "; selected:", names(selected), "\n")
  cat("Margin reads:", reads,
      "; selected:", actual$included[[1L]], "\n")
  identical(reads, 1L) && identical(actual$included[[1L]], names(selected))
}

probes <- list(
  F1 = probe_grouping_operator,
  F2 = probe_parenthesized_across,
  F3 = probe_template_error,
  F4 = probe_selector_context,
  F5 = probe_reflective_dependency,
  F6 = probe_bound_selection
)
requested <- commandArgs(trailingOnly = TRUE)
if (length(requested) > 0L) probes <- probes[requested]
stopifnot(length(probes) > 0L, !any(vapply(probes, is.null, logical(1))))
passed <- vapply(names(probes), function(name) {
  cat("\n--- ", name, " ---\n", sep = "")
  result <- capture_error(probes[[name]]())
  pass <- isTRUE(result)
  if (inherits(result, "error")) cat(conditionMessage(result), "\n")
  cat(if (pass) "PASS" else "FAIL", name, "\n")
  pass
}, logical(1))
cat("\n", sum(passed), "/", length(passed), " contract assertions passed.\n",
    sep = "")
quit(status = as.integer(!all(passed)))
