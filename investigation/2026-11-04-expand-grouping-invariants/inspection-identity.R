Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)

run_ids <- function() {
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  sqlite <- DBI::dbConnect(RSQLite::SQLite())
  on.exit({
    DBI::dbDisconnect(duck, shutdown = TRUE)
    DBI::dbDisconnect(sqlite)
  })
  backends <- list(
    local = identity,
    dtplyr = dtplyr::lazy_dt,
    arrow = arrow::arrow_table,
    duckdb = function(x) copy_to(duck, x, "witness", overwrite = TRUE),
    sqlite = function(x) copy_to(sqlite, x, "witness", overwrite = TRUE)
  )
  count <- 0L
  for (backend in names(backends)) {
    for (width in c(0L, 1L, 2L, 30L, 31L, 32L)) {
      keys <- if (width == 0L) character() else paste0("k", seq_len(width))
      x <- tibble(fixed = c("a", "b"), rid = 1:2)
      for (key in keys) x[[key]] <- c(NA_integer_, 11L)
      input <- group_by(backends[[backend]](x), fixed)
      sets <- unique(list(character(), keys, keys[seq_along(keys) %% 2L == 1L]))
      for (duplicates in c("error", "keep")) {
        spec <- if (duplicates == "keep") {
          grouping_sets(
            grouping_set(), grouping_set(all_of(keys)),
            grouping_set(all_of(keys[seq_along(keys) %% 2L == 1L])), grouping_set()
          )
        } else {
          grouping_sets(!!!lapply(sets, function(set) grouping_set(!!!syms(set))))
        }
        plan <- inspect_grouping(input, .grouping = spec, .duplicates = duplicates)
        got <- tryCatch(
          summarize_with_margins(
            input, count = n(), implicit = grouping_id(),
            fixed_bit = grouping_bit(fixed), fixed_mask = grouping_id(fixed),
            .grouping = spec, .duplicates = duplicates, .id = "occurrence",
            .margin_label = NULL
          ),
          error = identity
        )
        if (width == 32L) {
          stopifnot(
            inherits(got, "marginplyr_error"),
            grepl("supports at most 31 columns", conditionMessage(got), fixed = TRUE),
            all(is.na(plan$grouping_id))
          )
        } else {
          if (inherits(got, "error")) stop(got)
          got <- if (is.data.frame(got)) got else collect(got)
          stopifnot(
            nrow(got) == 2L * nrow(plan),
            all(got$count == 1L), all(got$fixed_bit == 0L),
            all(got$fixed_mask == 0L),
            identical(as.integer(got$implicit), plan$grouping_id[got$occurrence])
          )
        }
        count <- count + 1L
      }
    }
    cat(backend, "completed", count, "cumulative helper comparisons\n")
  }
}

run_ids()
