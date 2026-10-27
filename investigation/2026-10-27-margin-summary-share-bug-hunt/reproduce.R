# Dated public-contract probes; run from the repository root.
# Investigated: 2026-10-27. Baseline failures are intentional, with exit 1.
suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

packages <- c(
  "dplyr", "dbplyr", "dtplyr", "data.table", "arrow", "duckdb",
  "RSQLite", "vctrs", "bit64"
)
cat(R.version.string, "\n")
print(vapply(packages, function(package) {
  as.character(utils::packageVersion(package))
}, character(1)))

failures <- character()
check <- function(name, expression) {
  tryCatch({
    force(expression)
    cat("PASS", name, "\n")
  }, error = function(error) {
    failures <<- c(failures, name)
    cat("FAIL", name, ":", conditionMessage(error), "\n")
  })
}

assert <- function(value, message) {
  if (!isTRUE(value)) {
    stop(message, call. = FALSE)
  }
}

capture_error <- function(expression) {
  tryCatch({
    force(expression)
    NULL
  }, error = identity)
}

data <- data.frame(
  g = c("a", "b"), x = c(1, 3), first = c(100, 300)
)

check("control: ordinary data-pronoun dependency is refused", {
  error <- capture_error(summarize_with_margins(
    data, v = sum(x), first = share_of_total(v),
    out = sum(.data$first), .grouping = rollup(g)
  ))
  assert(inherits(error, "marginplyr_error"), "Expected Package condition")
})

check("F1 / #918: parentheses preserve the dependency refusal", {
  error <- capture_error(summarize_with_margins(
    data, v = sum(x), first = share_of_total(v),
    out = sum((.data)$first), .grouping = rollup(g)
  ))
  assert(
    inherits(error, "marginplyr_error"),
    "Forbidden share read was accepted; baseline out was 100, 300, 400"
  )
})

check("control: eager shares use their visible denominator", {
  counter <- 0L
  stamp <- function(x) {
    counter <<- counter + 1L
    as.double(counter)
  }
  result <- summarize_with_margins(
    data, v = stamp(x), s = share_of_total(v),
    .grouping = rollup(g), .margin_label = NULL, .id = "set"
  )
  expected <- result$v / result$v[result$set == 2L]
  expected[result$set == 2L] <- 1
  assert(isTRUE(all.equal(result$s, expected)), "Eager denominator mismatch")
})

check("F2 / #919: lazy shares use their visible denominator", {
  counter <- 0L
  stamp <- function(x) {
    counter <<- counter + 1L
    as.double(counter)
  }
  query <- summarize_with_margins(
    dtplyr::lazy_dt(data), v = stamp(x), s = share_of_total(v),
    .grouping = rollup(g), .margin_label = NULL, .id = "set"
  )
  assert(identical(counter, 0L), "Construction evaluated source rows")
  result <- collect(query)
  print(result)
  expected <- result$v / result$v[result$set == 2L]
  expected[result$set == 2L] <- 1
  assert(isTRUE(all.equal(result$s, expected)), "Visible denominator mismatch")
})

integer_keys <- tibble::tibble(
  g = bit64::as.integer64(c(1, 2)), x = c(1, 3)
)
check("control: integer64 Margin summary without shares", {
  result <- collect(summarize_with_margins(
    dtplyr::lazy_dt(integer_keys), v = sum(x),
    .grouping = rollup(g), .margin_label = NULL
  ))
  assert(identical(result$v, c(1, 3, 4)), "Wrong integer64 summary")
})

check("F3 / #920: integer64 keys support contextual shares", {
  result <- collect(summarize_with_margins(
    dtplyr::lazy_dt(integer_keys), v = sum(x), p = share_of_total(v),
    .grouping = rollup(g), .margin_label = NULL
  ))
  assert(identical(result$p, c(0.25, 0.75, 1)), "Wrong integer64 shares")
})

special_keys <- tibble::tibble(
  .N = c("u", "v"), g = c("a", "b"), v = c(1L, 3L)
)
special_source <- dtplyr::lazy_dt(special_keys)
check("control: ordinary dtplyr labelled union with fixed .N", {
  detail <- summarise(special_source, total = sum(v), .by = c(".N", "g"))
  subtotal <- summarise(special_source, total = sum(v), .by = ".N") |>
    mutate(g = "Total") |>
    select(all_of(c(".N", "g", "total")))
  result <- collect(union_all(detail, subtotal))
  assert(identical(result$total, c(1L, 3L, 1L, 3L)), "Wrong ordinary union")
})

check("F4 / #921: display-labelled margins preserve fixed .N", {
  result <- collect(summarize_with_margins(
    special_source, total = sum(v), .by = all_of(".N"),
    .grouping = rollup(g)
  ))
  assert(identical(result$total, c(1L, 3L, 1L, 3L)), "Wrong fixed-key totals")
})

arrow_source <- arrow::Table$create(data.frame(x = 1:2))
check("control: ordinary Arrow dot-dot summary output", {
  result <- collect(summarise(arrow_source, ..1 = sum(x)))
  assert(identical(names(result), "..1"), "Ordinary Arrow changed the name")
  assert(identical(result[[1L]], 3L), "Wrong ordinary Arrow value")
})

check("F5 / #922: Margin summary preserves Arrow dot-dot output", {
  result <- collect(summarize_with_margins(arrow_source, ..1 = sum(x)))
  assert(identical(names(result), "..1"), "Margin summary changed the name")
  assert(identical(result[[1L]], 3L), "Wrong Arrow Margin value")
})

cat("FAILED CHECKS:", length(failures), "\n")
quit(status = as.integer(length(failures) > 0L))
