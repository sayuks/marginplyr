# Reproductions accompanying bug-hunt-2026-09-24.md. Run from the repo root.
# With no arguments every case runs; pass case IDs to select a subset.
# Each assertion states the expected public behavior, so the investigated
# revision exits nonzero. This is an investigation artifact, not a test gate.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

cases <- list(
  C01 = function() {
    x <- tibble(g = c("a", "a", "b"), x = 1:3, total = 100)
    frame <- function(x) data.frame(total = sum(x[[1L]]))
    control <- summarise(x, frame(pick(x)), later = sum(total), .by = g)
    stopifnot(identical(control$later, c(3L, 3L)))
    actual <- summarize_with_margins(
      x, frame(pick(x)), later = sum(total), .grouping = rollup(g)
    )
    cat("  actual later:", actual$later, "; expected: 3 3 6\n")
    stopifnot(identical(as.numeric(actual$later), c(3, 3, 6)))
  },
  C02 = function() {
    x <- tibble(g = c(1, 1 + 1e-15), value = 1:2)
    control <- nest_with_margins(
      x, .grouping = grouping_set(g), .margin_label = NULL
    )
    stopifnot(nrow(control) == 2L)
    actual <- nest_with_margins(x, .grouping = grouping_set(g))
    cat("  nested groups:", nrow(actual), "; expected: 2\n")
    stopifnot(nrow(actual) == 2L)
  },
  C03 = function() {
    x <- tibble(g = c("a", "b"), x = 1:2, y = 3:4)
    control <- summarise(x, across(.c = x, .fns = sum), .by = g)
    stopifnot(identical(control$x, 1:2), identical(names(control), c("g", "x")))
    actual <- suppressWarnings(summarize_with_margins(
      x, across(.c = x, .fns = sum), .grouping = grouping_set(g)
    ))
    cat("  actual x:", actual$x, "; columns:", names(actual), "\n")
    stopifnot(identical(actual, control))
  },
  C04 = function() {
    x <- tibble(g = c("a", "a", "b"), x = 1:3)
    control <- summarize_with_margins(
      x, out = ncol(pick(everything())), .grouping = rollup(g)
    )
    stopifnot(identical(control$out, c(1L, 1L, 1L)))
    actual <- summarize_with_margins(
      x,
      out = (function(z) function(dummy) z)(ncol(pick(everything())))(0),
      .grouping = rollup(g)
    )
    cat("  selected column counts:", actual$out, "; expected: 1 1 1\n")
    stopifnot(identical(actual$out, control$out))
  },
  C05 = function() {
    x <- dtplyr::lazy_dt(tibble(g = c("a", "b"), v = c(2, 4)))
    control <- summarize_with_margins(
      x, z = sum(v), extra = 1:2, .grouping = rollup(g)
    ) |> collect()
    stopifnot(nrow(control) == 6L)
    actual <- tryCatch(
      summarize_with_margins(
        x, z = sum(v), p = share_of_parent(z), extra = 1:2,
        .grouping = rollup(g)
      ) |> collect(),
      marginplyr_error = function(cnd) cnd
    )
    # A targeted refusal of non-unique staged keys is also a valid remedy.
    if (inherits(actual, "marginplyr_error")) {
      stopifnot(inherits(actual, "marginplyr_share_cardinality_error"))
      return(invisible(NULL))
    }
    cat("  rows with Parent share:", nrow(actual), "; without:", nrow(control), "\n")
    stopifnot(nrow(actual) == nrow(control))
  },
  C06 = function() {
    x <- arrow::Table$create(
      g = arrow::Array$create(c(1L, 2L), type = arrow::int64())
    )
    control <- x |>
      summarise(n = n(), .by = g) |>
      union_all(
        x |> summarise(n = n()) |>
          mutate(g = arrow::Scalar$create(NA_integer_, type = arrow::int64())) |>
          select(g, n)
      ) |>
      collect()
    stopifnot(nrow(control) == 3L, sum(control$n) == 4L)
    actual <- summarize_with_margins(
      x, n = n(), .grouping = rollup(g), .margin_label = NULL
    ) |> collect()
    stopifnot(nrow(actual) == 3L, sum(actual$n) == 4L)
  },
  C07 = function() {
    seen <- integer()
    probe <- function(x) {
      seen <<- c(seen, length(x))
      x
    }
    x <- dtplyr::lazy_dt(tibble(g = c("a", "b", "c"), v = 1:3)) |>
      mutate(v = probe(v))
    control <- summarise(x, n = n())
    stopifnot(inherits(control, "dtplyr_step"), length(seen) == 0L)
    plan <- inspect_grouping(x, .grouping = rollup(where(is.character)))
    inspection_rows <- sum(seen)
    stopifnot(nrow(plan) == 2L)
    seen <- integer()
    actual <- summarize_with_margins(x, n = n(), .grouping = rollup(g))
    cat("  rows evaluated before collect():", sum(seen), "; expected: 0\n")
    cat("  rows evaluated by inspection:", inspection_rows, "; expected: 0\n")
    stopifnot(
      inherits(actual, "dtplyr_step"), sum(seen) == 0L, inspection_rows == 0L
    )
  },
  C08 = function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    x <- copy_to(con, tibble(g = NA_character_, v = 1), "probe")
    control <- summarize_with_margins(
      x, z = sum(v, na.rm = TRUE), .grouping = rollup(g), .margin_label = NULL
    ) |> collect()
    stopifnot(typeof(control$g) == "character")
    actual <- summarize_with_margins(
      x, z = sum(v, na.rm = TRUE), p = share_of_total(z),
      .grouping = rollup(g), .margin_label = NULL, .check_share_source = FALSE
    ) |> collect()
    cat("  all-missing key type:", typeof(actual$g), "; expected: character\n")
    stopifnot(identical(actual$g, control$g))
  },
  C09 = function() {
    x <- tibble(g = c("a", "b"), x = 1:2)
    control <- summarise(
      x, across(x, ~ tibble(n = sum(.x)), .names = "out", .unpack = TRUE),
      .by = g
    )
    stopifnot(identical(names(control), c("g", "out_n")))
    actual <- summarize_with_margins(
      x, across(x, ~ tibble(n = sum(.x)), .names = "out", .unpack = TRUE),
      .grouping = grouping_set(g), .id = "out"
    )
    stopifnot(identical(actual$out_n, control$out_n), all(actual$out == 1L))
  },
  C10 = function() {
    x <- tibble(g = c("a", "b"), x = 1:2)
    control <- summarise(x, data.frame(x = sum(x), check.names = FALSE), .by = g)
    stopifnot(identical(names(control), c("g", "x")))
    actual <- summarize_with_margins(
      x, data.frame(x = sum(x), check.names = FALSE),
      .grouping = grouping_set(g), .id = "check.names"
    )
    stopifnot(identical(actual$x, control$x), all(actual$check.names == 1L))
  },
  C11 = function() {
    con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    x <- copy_to(con, tibble(g = c("a", "b"), x = 1:2), "probe")
    calls <- 0L
    nm <- function() {
      calls <<- calls + 1L
      if (calls <= 2L) "out" else "g"
    }
    actual <- tryCatch(
      summarize_with_margins(
        x, across(x, sum, .names = "{nm()}"),
        .grouping = rollup(g), .margin_label = NULL
      ) |> collect(),
      marginplyr_error = function(cnd) cnd
    )
    if (inherits(actual, "marginplyr_error")) {
      stopifnot(grepl(
        "overwrite grouping column", conditionMessage(actual), fixed = TRUE
      ))
      return(invisible(NULL))
    }
    cat("  naming evaluations:", calls, "; result columns:", names(actual), "\n")
    stopifnot("out" %in% names(actual), identical(sort(actual$g), c("a", "b")))
  },
  C12 = function() {
    x <- tibble(g = matrix(1:4, ncol = 2), v = 1:2)
    control <- summarize_with_margins(
      x, n = n(), .grouping = rollup(g), .margin_label = NULL
    )
    stopifnot(nrow(control) == 3L)
    actual <- tryCatch(
      summarize_with_margins(x, n = n(), .grouping = rollup(g)),
      error = function(cnd) cnd
    )
    stopifnot(!inherits(actual, "error") || inherits(actual, "marginplyr_error"))
  }
)

selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) == 0L) selected <- names(cases)
stopifnot(all(selected %in% names(cases)))
failed <- character()
for (id in selected) {
  cat(id, "\n")
  result <- tryCatch({ cases[[id]](); NULL }, error = identity)
  if (inherits(result, "error")) {
    failed <- c(failed, id)
    cat("  FAIL:", conditionMessage(result), "\n")
  } else {
    cat("  PASS\n")
  }
}
cat(length(failed), "of", length(selected), "checks failed:", failed, "\n")
if (length(failed) > 0L) quit(status = 1L)
