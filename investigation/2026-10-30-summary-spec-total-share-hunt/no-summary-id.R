# Dated investigation evidence. Run from the checkout root.
pkgload::load_all(".", quiet = TRUE)

ordinary_oracle <- function(data, fixed, sets, dimensions) {
  branches <- lapply(seq_along(sets), function(index) {
    included <- sets[[index]]
    keys <- c(fixed, included)
    branch <- dplyr::summarise(data, .by = dplyr::all_of(keys))
    for (name in setdiff(dimensions, included)) {
      branch[[name]] <- rep("Total", nrow(branch))
    }
    branch$sid <- rep(as.integer(index), nrow(branch))
    branch$omitted <- rep(as.integer(length(setdiff(dimensions, included)) > 0L),
                          nrow(branch))
    branch
  })
  expected <- dplyr::bind_rows(branches)
  expected <- dplyr::arrange(
    expected, dplyr::across(dplyr::all_of(c(fixed, "omitted", dimensions)))
  )
  expected <- dplyr::select(
    expected, dplyr::all_of(c(fixed, dimensions, "sid"))
  )
  as.data.frame(expected)
}

sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
duckdb <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
plans <- list(
  list(spec = NULL, sets = list(character()), dimensions = character()),
  list(spec = grouping_set(g), sets = list("g"), dimensions = "g"),
  list(spec = rollup(g), sets = list("g", character()), dimensions = "g"),
  list(spec = grouping_sets(grouping_set(), grouping_set(g)),
       sets = list(character(), "g"), dimensions = "g")
)
checks <- 0L
for (empty in c(FALSE, TRUE)) {
  data <- data.frame(f = c("a", "b"), g = c("x", "y"), x = 1:2)
  if (empty) data <- data[FALSE, ]
  inputs <- list(
    local = data,
    dtplyr = dtplyr::lazy_dt(data),
    arrow = arrow::arrow_table(data),
    sqlite = dplyr::copy_to(sqlite, data, paste0("s", empty)),
    duckdb = dplyr::copy_to(duckdb, data, paste0("s", empty))
  )
  for (backend in names(inputs)) {
    for (plan in plans) {
      for (fixed in c(FALSE, TRUE)) {
        input <- inputs[[backend]]
        specification <- plan$spec
        expected <- ordinary_oracle(
          data, if (fixed) "f" else character(), plan$sets, plan$dimensions
        )
        actual <- if (fixed) {
          summarize_with_margins(
            input, .by = f, .grouping = specification,
            .id = "sid", .sort = "last"
          )
        } else {
          summarize_with_margins(
            input, .grouping = specification, .id = "sid", .sort = "last"
          )
        }
        actual <- as.data.frame(dplyr::collect(actual))
        stopifnot(identical(actual, expected))
        checks <- checks + 1L
      }
    }
  }
}
DBI::dbDisconnect(sqlite)
DBI::dbDisconnect(duckdb, shutdown = TRUE)
cat("NO-SUMMARY ID ORACLE:", checks, "comparisons passed\n")
