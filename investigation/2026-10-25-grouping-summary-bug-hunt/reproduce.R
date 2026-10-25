# Replay the public-contract findings recorded on 2026-10-25.
# Run from the repository root with the documented investigation dependencies.
suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

# Report each contract independently so one defect does not hide another.
check_finding <- function(id, check) {
  cat("\n", id, "\n", sep = "")
  tryCatch({
    check()
    cat("PASS\n")
    TRUE
  }, error = function(condition) {
    cat("FAIL: ", conditionMessage(condition), "\n", sep = "")
    FALSE
  })
}

results <- logical()
results[["F1"]] <- check_finding("F1: whitespace fixed keys in dtplyr shares", function() {
  input <- tibble(` a ` = c("F", "G"), a = c("X", "X"),
                  g = c("x", "x"), value = c(2, 3))
  failed <- character()
  for (kind in c("parent", "total")) {
    share <- switch(kind,
      parent = quote(share_of_parent(total)),
      total = quote(share_of_total(total))
    )
    summarize <- function(data) {
      summarize_with_margins(
        data, total = sum(value), share = !!share,
        .by = all_of(c(" a ", "a")), .grouping = rollup(g)
      ) |>
        collect() |>
        arrange(` a `, g)
    }
    expected <- summarize(input)
    actual <- summarize(dtplyr::lazy_dt(input))
    cat(kind, ": expected rows=", nrow(expected),
        "; actual rows=", nrow(actual), "\n", sep = "")
    cat("expected shares: ", paste(expected$share, collapse = ", "), "\n", sep = "")
    cat("actual shares: ", paste(round(actual$share, 6), collapse = ", "), "\n", sep = "")
    if (!isTRUE(all.equal(actual, expected))) failed <- c(failed, kind)
  }
  if (length(failed)) stop("Cross-partition share joins: ", paste(failed, collapse = ", "))
})

results[["F2"]] <- check_finding("F2: typed selection in fixed keys", function() {
  input <- tibble(a = c("x", "y"), b = c(1, 1))
  selection <- rlang::expr(tidyselect::all_of(
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  ))
  expected <- dplyr::summarize(input, n = dplyr::n(), .by = !!selection)
  actual <- summarize_with_margins(input, n = dplyr::n(), .by = !!selection)
  cat("expected:\n")
  print(expected)
  cat("actual:\n")
  print(actual)
  stopifnot(identical(actual, expected))
})

results[["F3"]] <- check_finding("F3: SQLite Boolean-word output names", function() {
  connection <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(connection))
  input <- tibble(`TRUE` = "a", value = 1L)
  remote <- copy_to(connection, input, "bool_name", temporary = TRUE)
  control <- remote |>
    dplyr::summarize(total = sum(value, na.rm = TRUE), .by = `TRUE`) |>
    collect()
  stopifnot(nrow(control) == 1L, control$total == 1L)
  result <- summarize_with_margins(
    remote, total = sum(value, na.rm = TRUE),
    .grouping = rollup(`TRUE`), .margin_label = NULL
  ) |>
    collect()
  stopifnot(nrow(result) == 2L, identical(sort(result$total), c(1L, 1L)))
})

results[["F4"]] <- check_finding("F4: dot-dot summary output names", function() {
  input <- tibble(value = 1L)
  expected <- dplyr::summarize(input, `..1` = sum(value))
  actual <- summarize_with_margins(input, `..1` = sum(value))
  stopifnot(identical(actual, expected))
})

results[["F5"]] <- check_finding("F5: cyclic Grouping specification", function() {
  spec <- grouping_sets(spec)
  condition <- tryCatch(
    inspect_grouping(data.frame(a = 1), .grouping = spec),
    error = identity
  )
  cat("condition class: ", paste(class(condition), collapse = ", "), "\n", sep = "")
  if (inherits(condition, "error")) cat(conditionMessage(condition), "\n", sep = "")
  stopifnot(inherits(condition, "marginplyr_error"))
})

results[["F6"]] <- check_finding("F6: NA factor levels in dtplyr shares", function() {
  input <- tibble(
    key = structure(c(1L, NA_integer_), levels = NA_character_, class = "factor"),
    value = c(2, 3)
  )
  expected <- summarize_with_margins(
    input, total = sum(value), share = share_of_total(total),
    .grouping = rollup(key), .margin_label = NULL
  )
  stopifnot(nrow(expected) == 3L,
            identical(sort(expected$share), c(0.4, 0.6, 1)))
  cat("expected rows=3; shares=0.4, 0.6, 1\n")
  actual <- summarize_with_margins(
    dtplyr::lazy_dt(input), total = sum(value), share = share_of_total(total),
    .grouping = rollup(key), .margin_label = NULL
  ) |>
    collect()
  stopifnot(nrow(actual) == 3L,
            identical(sort(actual$share), sort(expected$share)))
})

cat("\nContract failures: ", sum(!results), "/", length(results), "\n", sep = "")
quit(status = as.integer(!all(results)))
