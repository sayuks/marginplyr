pkgload::load_all(quiet = TRUE)

run <- function() {
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  for (width in c(1L, 30L, 31L, 32L)) {
    columns <- paste0("g", seq_len(width))
    source <- tibble::as_tibble(stats::setNames(rep(list(1L), width), columns))
    family <- grouping_sets(grouping_set(dplyr::all_of(columns)), grouping_set())
    inputs <- list(
      local = source,
      dtplyr = dtplyr::lazy_dt(source),
      duckdb = dplyr::copy_to(duck, source, paste0("t", width)),
      sqlite = dplyr::copy_to(sqlite, source, paste0("t", width))
    )
    for (input in inputs) {
      plan <- inspect_grouping(input, .grouping = family, .format = "list")
      expected_ids <- if (width <= 31L) c(0L, as.integer(2^width - 1)) else
        rep(NA_integer_, 2L)
      stopifnot(
        identical(plan$set_id, c(1L, 2L)),
        identical(plan$included, list(columns, character())),
        identical(plan$omitted, list(character(), columns)),
        identical(plan$grouping_id, expected_ids),
        identical(unname(plan$grouping_bits[[1L]]), rep(0L, width)),
        identical(unname(plan$grouping_bits[[2L]]), rep(1L, width))
      )
      if (width <= 31L) {
        result <- dplyr::collect(summarize_with_margins(
          input, bits = grouping_id(), last_bit = grouping_bit(!!rlang::sym(tail(columns, 1L))),
          .grouping = family, .id = "occurrence", .margin_label = NULL,
          .sort = "none"
        ))
        result <- result[order(result$occurrence), ]
        stopifnot(
          identical(as.integer(result$occurrence), c(1L, 2L)),
          identical(as.integer(result$bits), expected_ids),
          identical(as.integer(result$last_bit), c(0L, 1L))
        )
      } else {
        err <- tryCatch(summarize_with_margins(
          input, bits = grouping_id(), .grouping = family, .sort = "none"
        ), error = identity)
        stopifnot(inherits(err, "marginplyr_error"),
                  grepl("at most 31 columns", conditionMessage(err), fixed = TRUE))
      }
    }
  }
}

run()
cat("16 inspection / summary identity pairs matched expected bits and identifiers.\n")
