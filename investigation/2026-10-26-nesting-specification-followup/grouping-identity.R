# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
run <- function() {
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  x <- tibble::tibble(
    f = c("x", "x", NA, "y", "y", "x"),
    a = c("u", "u", "v", "u", NA, "u"),
    b = c(1L, 2L, 1L, NA, 1L, 1L),
    c = c(TRUE, FALSE, TRUE, TRUE, NA, TRUE)
  )
  specs <- list(
    rollup(a, b, c),
    cube(grouping_set(a, b), c),
    grouping_sets(grouping_set(a, b, c), grouping_set(b),
                  grouping_set(), grouping_set(b)),
    grouping_spec(rollup(a, b), rollup(c)),
    grouping_spec(grouping_set(a), cube(b, c))
  )
  checks <- 0L
  observations <- 0L
  for (size in c(0L, nrow(x))) {
    input <- x[seq_len(size), ]
    sources <- list(
      local = input,
      dtplyr = dtplyr::lazy_dt(input),
      arrow = arrow::Table$create(input),
      sqlite = dplyr::copy_to(sqlite, input, paste0("s", size)),
      duckdb = dplyr::copy_to(duck, input, paste0("d", size))
    )
    for (spec in specs) for (policy in c("drop", "keep")) {
      for (fixed in c(FALSE, TRUE)) {
        by <- if (fixed) "f" else character()
        plan <- inspect_grouping(input, .by = dplyr::all_of(by),
                                 .grouping = spec, .duplicates = policy,
                                 .format = "list")
        for (backend in names(sources)) {
          result <- dplyr::collect(summarize_with_margins(
            sources[[backend]],
            n = dplyr::n(),
            bit_a = grouping_bit(a),
            bit_b = grouping_bit(b),
            bit_c = grouping_bit(c),
            mask = grouping_id(),
            reversed = grouping_id(c, b, a),
            fixed_bit = !!if (fixed) quote(grouping_bit(f)) else 0L,
            .by = dplyr::all_of(by), .grouping = spec,
            .duplicates = policy, .margin_label = NULL, .id = "sid"
          ))
          for (row in seq_len(nrow(result))) {
            bits <- plan$grouping_bits[[result$sid[row]]]
            stopifnot(
              result$bit_a[row] == bits[["a"]],
              result$bit_b[row] == bits[["b"]],
              result$bit_c[row] == bits[["c"]],
              result$fixed_bit[row] == 0,
              result$mask[row] == plan$grouping_id[result$sid[row]],
              result$reversed[row] == sum(bits[c("c", "b", "a")] * c(4, 2, 1))
            )
            observations <- observations + 1L
          }
          for (column in c("bit_a", "bit_b", "bit_c", "mask", "reversed")) {
            stopifnot(is.numeric(result[[column]]))
            if (backend %in% c("local", "dtplyr")) {
              stopifnot(is.integer(result[[column]]))
            }
          }
          checks <- checks + 1L
        }
      }
    }
  }
  cat("identity result checks:", checks, "rows checked:", observations, "PASS\n")
}
run()
