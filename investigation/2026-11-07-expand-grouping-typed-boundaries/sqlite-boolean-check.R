# Replay from the package root. Set --assert for desired-contract failure.
pkgload::load_all(".", quiet = TRUE)

check <- function(con, value) {
  root <- dplyr::copy_to(
    con, tibble::tibble(`TRUE` = value), "facts", overwrite = TRUE
  )
  source <- dplyr::collapse(root)
  native <- dplyr::collect(source)
  expected <- c(value, "Total")
  expand <- function(x, column, check_label) {
    expand_with_margins(
      x, .grouping = rollup(dplyr::all_of(column)),
      .check_margin_label = check_label
    )
  }
  caught <- function(expr) tryCatch(expr, error = identity)
  result <- caught(expand(source, "TRUE", TRUE))
  audit <- last_sent_queries()
  checked_root <- caught(expand(root, "TRUE", TRUE))
  renamed <- dplyr::collapse(dplyr::rename(root, g = "TRUE"))
  checked_renamed <- caught(expand(renamed, "g", TRUE))
  stopifnot(
    identical(native, tibble::tibble(`TRUE` = value)),
    identical(dplyr::collect(expand(source, "TRUE", FALSE))[[1L]], expected),
    inherits(result, "error"),
    !inherits(result, "marginplyr_error"),
    grepl("no such column: TRUE", conditionMessage(result), fixed = TRUE),
    identical(audit$purpose, "observed_label_collision")
  )
  if (value == "Total") {
    for (control in list(checked_root, checked_renamed)) {
      stopifnot(
        inherits(control, "marginplyr_error"),
        grepl("already present", conditionMessage(control), fixed = TRUE)
      )
    }
  } else {
    stopifnot(
      identical(dplyr::collect(checked_root)[[1L]], expected),
      identical(dplyr::collect(checked_renamed)[[1L]], expected)
    )
  }
  cat("Source value:", value, "\n")
  cat("Observed collision query:\n", audit$sql, "\n", sep = "")
  cat("Checked collapsed input:", conditionMessage(result), "\n")
  if (value == "Total") {
    return(inherits(result, "marginplyr_error"))
  }
  !inherits(result, "error")
}

run <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  prior <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(prior), add = TRUE)
  correct_clean <- check(con, "detail")
  correct_collision <- check(con, "Total")
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(correct_clean, correct_collision)
  }
}
run()
