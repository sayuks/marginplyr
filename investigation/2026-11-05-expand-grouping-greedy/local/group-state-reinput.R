# Dated group-admission and repeated-expansion observations.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")

make_input <- function(n) {
  tibble::tibble(
    k = rep(c("b", NA, "a"), length.out = n),
    g = factor(rep(c("c", NA, "a"), length.out = n),
               levels = c("c", "a", "unused")),
    h = rep(c(2L, 1L, NA_integer_), length.out = n),
    row = seq_len(n)
  )
}

counter <- 0L
for (n in c(0L, 1L, 3L)) for (drop in c(TRUE, FALSE)) {
  for (kind in c("plain", "grouped", "rowwise")) {
    for (sort in c("none", "first", "last")) {
      d <- make_input(n)
      expected <- expand_with_margins(
        d, .by = k, .grouping = rollup(g, h), .margin_label = NULL,
        .id = "sid", .sort = sort
      )
      input <- switch(
        kind,
        plain = d,
        grouped = dplyr::group_by(d, k, .drop = drop),
        rowwise = dplyr::rowwise(d, k)
      )
      if (kind == "rowwise" || (kind == "grouped" && !drop)) {
        result <- tryCatch(
          expand_with_margins(input, .grouping = rollup(g, h),
                              .margin_label = NULL),
          error = identity
        )
        refusal <- if (kind == "rowwise") "rowwise()" else ".drop = FALSE"
        stopifnot(inherits(result, "marginplyr_error"),
                  grepl(refusal, conditionMessage(result), fixed = TRUE))
        counter <- counter + 1L
        next
      }
      actual <- if (kind == "plain") {
        expand_with_margins(
          input, .by = k, .grouping = rollup(g, h), .margin_label = NULL,
          .id = "sid", .sort = sort
        )
      } else {
        expand_with_margins(
          input, .grouping = rollup(g, h), .margin_label = NULL,
          .id = "sid", .sort = sort
        )
      }
      stopifnot(identical(actual, expected), !dplyr::is_grouped_df(actual),
                !inherits(actual, "rowwise_df"))

      # The previous occurrence identifier is ordinary source payload here.
      second <- expand_with_margins(
        actual, .by = k, .grouping = rollup(g, h), .margin_label = NULL,
        .id = "sid2", .sort = sort
      )
      stopifnot(nrow(second) == nrow(actual) * 3L,
                all(table(second$sid2) == nrow(actual)))
      for (id in 1:3) {
        branch <- second[second$sid2 == id, , drop = FALSE]
        branch <- branch[order(branch$row, branch$sid), , drop = FALSE]
        source <- actual[order(actual$row, actual$sid), , drop = FALSE]
        stopifnot(identical(branch$row, source$row),
                  identical(branch$sid, source$sid),
                  identical(branch$k, source$k))
      }
      counter <- counter + 1L
    }
  }
}
cat("Group-state and re-input matches:", counter, "\n")
