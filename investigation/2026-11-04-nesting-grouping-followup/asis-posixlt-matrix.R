pkgload::load_all(quiet = TRUE)

count <- c(branch_errors = 0L, literal_refusals = 0L,
  included_only_matches = 0L, normalized_matches = 0L)
date_vectors <- list("2020-01-01", c("2020-01-02", NA_character_, "2020-01-01"))
for (dates in date_vectors) {
  timestamp <- I(as.POSIXlt(dates, format = "%Y-%m-%d", tz = "UTC"))
  x <- tibble::tibble(g = timestamp, v = seq_along(dates))
  for (source in list(x, as.data.frame(x))) {
    normalized <- vctrs::vec_slice(source, seq_len(nrow(source)))
    stopifnot(inherits(source$g, "POSIXlt"), is.data.frame(normalized$g))
    for (label in list(NULL, NA_character_, "Total")) {
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        for (keep in c(FALSE, TRUE)) {
          for (plan in list(rollup(g), grouping_sets(grouping_set(), grouping_set(g)))) {
            native <- dplyr::nest_by(source, g, .keep = keep)
            native_members <- sort(vapply(native$data, function(cell) paste(cell$v, collapse = ","), ""))
            if (!is.null(label) && !is.na(label)) {
              result <- tryCatch(verb(source, .grouping = plan, .margin_label = label, .keep = keep),
                error = identity)
              stopifnot(inherits(result, "marginplyr_error"),
                grepl("cannot label packed grouping dimensions", conditionMessage(result), fixed = TRUE))
              count[["literal_refusals"]] <- count[["literal_refusals"]] + 1L
              next
            }
            included <- verb(source, .grouping = grouping_set(g),
              .margin_label = label, .keep = keep)
            stopifnot(identical(sort(vapply(included$data,
              function(cell) paste(cell$v, collapse = ","), "")), native_members))
            count[["included_only_matches"]] <- count[["included_only_matches"]] + 1L

            control <- verb(normalized, .grouping = plan, .margin_label = label,
              .keep = keep, .id = "set")
            stopifnot(nrow(control) == nrow(native) + 1L,
              sum(vctrs::vec_detect_missing(control$g)) == 1L)
            grand <- which(vctrs::vec_detect_missing(control$g))
            stopifnot(identical(control$data[[grand]]$v, seq_along(dates)),
              identical(sort(vapply(control$data[-grand],
                function(cell) paste(cell$v, collapse = ","), "")), native_members))
            count[["normalized_matches"]] <- count[["normalized_matches"]] + 1L

            result <- tryCatch(verb(source, .grouping = plan, .margin_label = label,
              .keep = keep, .id = "set"), error = identity)
            stopifnot(inherits(result, "vctrs_error_ptype2"),
              grepl("<POSIXlt<UTC>>", conditionMessage(result), fixed = TRUE),
              grepl("<data.frame>", conditionMessage(result), fixed = TRUE))
            count[["branch_errors"]] <- count[["branch_errors"]] + 1L
          }
        }
      }
    }
  }
}
print(count)
