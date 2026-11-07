# Run from the package root. --assert requires the desired contract.
pkgload::load_all(".", quiet = TRUE)

is_collision <- function(x) {
  inherits(x, "marginplyr_error") &&
    grepl("already present", conditionMessage(x), fixed = TRUE)
}

check <- function(con, column, value, require_contract) {
  frame <- function(x) stats::setNames(tibble::tibble(key = x), column)
  root <- dplyr::copy_to(con, frame(value), "facts", overwrite = TRUE)
  source <- dplyr::collapse(root)
  expected <- sort(c(value, "Total"))
  collect_ordered <- function(x) {
    result <- dplyr::collect(x)
    result[order(result[[1L]]), , drop = FALSE]
  }
  expand <- function(x, key, check_label) {
    expand_with_margins(
      x, .grouping = rollup(dplyr::all_of(key)),
      .check_margin_label = check_label
    )
  }
  caught <- function(expr) tryCatch(expr, error = identity)
  result <- caught(expand(source, column, TRUE))
  audit <- last_sent_queries()
  checked_root <- caught(expand(root, column, TRUE))
  renamed <- dplyr::collapse(dplyr::rename(root, g = dplyr::all_of(column)))
  checked_renamed <- caught(expand(renamed, "g", TRUE))
  stopifnot(
    identical(dplyr::collect(source), frame(value)),
    identical(collect_ordered(expand(source, column, FALSE)), frame(expected))
  )
  if (value == "Total") {
    stopifnot(is_collision(checked_root), is_collision(checked_renamed))
  } else {
    stopifnot(
      identical(collect_ordered(checked_root), frame(expected)),
      identical(collect_ordered(checked_renamed)$g, expected)
    )
  }
  if (require_contract) {
    if (value == "Total") {
      return(is_collision(result))
    }
    return(!inherits(result, "error") &&
      identical(collect_ordered(result), frame(expected)))
  }
  stopifnot(
    inherits(result, "error"),
    !inherits(result, "marginplyr_error"),
    grepl(paste("no such column:", column), conditionMessage(result), fixed = TRUE),
    identical(audit$purpose, "observed_label_collision")
  )
  cat("Column:", column, "| source value:", value, "\n")
  cat("Observed collision query:\n", audit$sql, "\n", sep = "")
  cat("Checked collapsed input:", conditionMessage(result), "\n")
}

run <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  prior <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(prior), add = TRUE)
  require_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
  for (column in c("TRUE", "FaLsE")) {
    correct_clean <- check(con, column, "detail", require_contract)
    correct_collision <- check(con, column, "Total", require_contract)
    if (require_contract) {
      stopifnot(correct_clean, correct_collision)
    }
  }
}
run()
