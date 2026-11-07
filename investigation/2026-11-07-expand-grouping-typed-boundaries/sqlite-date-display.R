# Run from the tested package root. --assert requires the desired contract.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

check <- function(extended_types, require_contract) {
  con <- DBI::dbConnect(
    RSQLite::SQLite(), ":memory:", extended_types = extended_types
  )
  on.exit(DBI::dbDisconnect(con))
  day <- as.Date("2020-01-01")
  source <- dplyr::copy_to(
    con, tibble::tibble(day = day, payload = 7L), "date_display"
  )
  typed_source <- dplyr::collect(source)
  detail <- dplyr::mutate(source, day = as.character(day), sid = 1L)
  total <- dplyr::mutate(source, day = "Total", sid = 2L)
  expected <- dplyr::union_all(detail, total) |>
    dplyr::collect() |>
    dplyr::select(day, sid, payload) |>
    dplyr::arrange(sid)
  stopifnot(identical(expected$payload, c(7L, 7L)),
            identical(expected$sid, 1:2), identical(expected$day[[2L]], "Total"))
  if (extended_types) {
    stopifnot(identical(typed_source$day, day),
              identical(expected$day, c("18262", "Total")))
  }

  observations <- logical(2L)
  for (enabled in c(FALSE, TRUE)) {
    warnings <- character()
    query <- expand_with_margins(
      source, .grouping = rollup(day), .id = "sid",
      .check_margin_label = enabled
    )
    actual <- withCallingHandlers(
      dplyr::collect(query),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    actual <- dplyr::arrange(actual, sid)
    sorted <- dplyr::collect(expand_with_margins(
      source, .grouping = rollup(day), .id = "sid", .sort = "last",
      .check_margin_label = enabled
    ))
    missing <- dplyr::collect(expand_with_margins(
      source, .grouping = rollup(day), .id = "sid", .margin_label = NULL
    ))
    missing <- dplyr::arrange(missing, sid)
    materialized <- dplyr::compute(query) |>
      dplyr::collect() |>
      dplyr::arrange(sid)
    stopifnot(
      identical(as.data.frame(sorted), as.data.frame(expected)),
      identical(materialized, expected),
      identical(actual[c("sid", "payload")], expected[c("sid", "payload")]),
      identical(names(actual), names(expected))
    )
    if (extended_types) {
      stopifnot(identical(missing$day, c(day, as.Date(NA_character_))))
    }
    if (extended_types && !require_contract) {
      stopifnot(
        identical(actual$day, rep(NA_character_, 2L)),
        any(grepl("Unknown string format", warnings, fixed = TRUE))
      )
    }
    cat("extended_types =", extended_types, "checked =", enabled,
        "| expected", paste(expected$day, collapse = "/"),
        "| collected", paste(actual$day, collapse = "/"),
        "| warnings", length(warnings), "\n")
    observations[[as.integer(enabled) + 1L]] <-
      identical(actual, expected) && !length(warnings)
  }
  all(observations)
}

require_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
ordinary <- check(FALSE, require_contract)
extended <- check(TRUE, require_contract)
stopifnot(ordinary)
if (require_contract) {
  stopifnot(extended)
}
