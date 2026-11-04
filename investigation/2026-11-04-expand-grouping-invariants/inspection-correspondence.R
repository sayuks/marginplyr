# Run from the repository root. Compare public plans and row copies with
# explicit set membership and binary weights at the mask-width boundary.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)

run_matrix <- function() {
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

  tested <- 0L
  for (backend in names(backends)) {
    for (width in c(0L, 1L, 30L, 31L, 32L)) {
      keys <- if (width == 0L) character() else paste0("k", seq_len(width))
      x <- tibble(fixed = c("a", "b"), rid = 1:2)
      for (key in keys) x[[key]] <- 10:11
      source <- backends[[backend]](x)
      # Omitted-first, then all dimensions, then alternating members, then
      # a duplicate empty set. Small widths exercise extra repetitions.
      sets <- list(character(), keys, keys[seq_along(keys) %% 2L == 1L],
                   character())
      spec <- grouping_sets(
        grouping_set(),
        grouping_set(all_of(keys)),
        grouping_set(all_of(keys[seq_along(keys) %% 2L == 1L])),
        grouping_set()
      )
      for (grouped in c(FALSE, TRUE)) {
        input <- if (grouped) group_by(source, fixed) else source
        by <- if (grouped) NULL else sym("fixed")
        for (duplicates in c("drop", "keep")) {
          retained <- if (duplicates == "drop") unique(sets) else sets
          ids <- seq_along(retained)
          bits <- lapply(retained, function(set) {
            setNames(as.integer(!keys %in% set), keys)
          })
          masks <- vapply(bits, function(bit) {
            if (width > 31L) return(NA_integer_)
            as.integer(sum(bit * 2 ^ rev(seq_along(keys) - 1L)))
          }, integer(1))
          plan <- inspect_grouping(
            input, .by = !!by, .grouping = spec, .format = "list",
            .duplicates = duplicates
          )
          expected_bits <- if (width == 0L) rep(list(integer()), length(ids)) else bits
          stopifnot(
            identical(plan$set_id, ids),
            identical(plan$included, retained),
            identical(plan$fixed, rep(list("fixed"), length(ids))),
            identical(plan$omitted, lapply(retained, function(set) setdiff(keys, set))),
            identical(plan$grouping_bits, expected_bits),
            identical(plan$grouping_id, masks)
          )
          for (ordering in c("none", "last", "first")) {
            got <- expand_with_margins(
              input, .by = !!by, .grouping = spec, .margin_label = NULL,
              .id = "occurrence", .duplicates = duplicates, .sort = ordering
            )
            stopifnot(length(group_vars(got)) == 0L)
            got <- if (is.data.frame(got)) got else collect(got)
            got <- arrange(got, occurrence, rid)
            stopifnot(
              identical(got$occurrence, rep(ids, each = 2L)),
              identical(got$rid, rep(1:2, length(ids))),
              identical(got$fixed, rep(x$fixed, length(ids)))
            )
            for (key in keys) {
              expected <- unlist(lapply(retained, function(set) {
                if (key %in% set) x[[key]] else rep(NA_integer_, 2L)
              }))
              stopifnot(identical(got[[key]], expected))
            }
            tested <- tested + 1L
          }
        }
      }
    }
    cat(backend, "completed", tested, "cumulative comparisons\n")
  }
}
run_matrix()
