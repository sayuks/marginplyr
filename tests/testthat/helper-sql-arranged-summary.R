# Shared public acceptance for DuckDB and the live PostgreSQL runner. A
# typed-missing Margin label keeps the input's window ordering in the native
# summary up to the Margin order, where a term over a column the result lacks
# would reach its ORDER BY (#1641).
expect_window_ordered_summary <- function(con) {
  remote <- dplyr::copy_to(
    con,
    data.frame(f = c("p", "p", "q", "q", "p"), a = c("x", "x", "y", "y", "x"),
               v = c(5, 3, 9, 1, 7)),
    "window_ordered_summary_source", temporary = TRUE, overwrite = TRUE
  )
  inputs <- list(
    window_order = dbplyr::window_order(remote, .data$v),
    arrange = dplyr::arrange(remote, .data$v),
    limited = utils::head(dplyr::arrange(remote, .data$v), 3L)
  )
  specs <- list(rollup = rollup(dplyr::all_of("a")),
                cube = cube(dplyr::all_of("a")))
  run <- function(x, spec, sort, label, by) {
    summarize_with_margins(
      x, s = sum(.data$v, na.rm = TRUE), .grouping = spec,
      .by = dplyr::all_of(by), .margin_label = label, .sort = sort
    )
  }
  for (input in names(inputs)) {
    for (spec in names(specs)) {
      for (sort in c("last", "first")) {
        for (label in list(NULL, NA_character_)) {
          for (by in list(character(), "f")) {
            info <- paste(input, spec, sort, deparse(label), deparse(by))
            query <- run(inputs[[input]], specs[[spec]], sort, label, by)
            # Clearing the window ordering by hand is the caller's
            # workaround; its ORDER BY holds the documented key alone, and
            # the input's window ordering renders nowhere in either query.
            unordered <- run(dbplyr::window_order(inputs[[input]]),
                             specs[[spec]], sort, label, by)
            expect_identical(dbplyr::sql_render(query),
                             dbplyr::sql_render(unordered), info = info)
            rows <- as.data.frame(dplyr::collect(query))
            expect_identical(rows, as.data.frame(dplyr::collect(unordered)),
                             info = info)
            expect_identical(
              as.data.frame(dplyr::collect(dplyr::compute(query))), rows,
              info = info
            )
          }
        }
      }
    }
  }
  # The documented key, applied by hand: bit(a), is.na(a), a.
  expect_equal(
    as.data.frame(dplyr::collect(
      run(inputs$window_order, specs$rollup, "last", NULL, NULL)
    )),
    data.frame(a = c("x", "y", NA), s = c(15, 10, 25))
  )
  # The three smallest `v` are 1 (y), 3 and 5 (x).
  expect_equal(
    as.data.frame(dplyr::collect(
      run(inputs$limited, specs$rollup, "last", NULL, NULL)
    )),
    data.frame(a = c("x", "y", NA), s = c(8, 1, 9))
  )
}
