pkgload::load_all(".", quiet = TRUE)
data <- data.frame(a1 = c("A", "B"), a2 = 1L, value = 3:4)
sel <- rlang::quo(c(a = c(a1, a2)))
opaque <- rlang::quo(c(a = identity(c("a1", "a2"))))
collect_result <- function(x) tibble::as_tibble(dplyr::collect(x))

compare <- function(label, carrier, proxy) {
  stopifnot(identical(tidyselect::eval_select(sel, proxy), c(a1 = 1L, a2 = 2L)),
            identical(as.character(dplyr::tbl_vars(dplyr::select(carrier, !!sel))),
                      c("a1", "a2")))
  for (verb in list(inspect_grouping, expand_with_margins)) {
    for (position in c("fixed", "dimension")) {
      observed <- if (position == "fixed") {
        tryCatch(verb(carrier, .by = !!sel), error = identity)
      } else {
        tryCatch(verb(carrier, .grouping = grouping_set(!!sel)), error = identity)
      }
      stopifnot(inherits(observed, "marginplyr_error"),
                grepl("a = a1", conditionMessage(observed), fixed = TRUE),
                grepl("a = a2", conditionMessage(observed), fixed = TRUE))
      if (position == "fixed") {
        expected <- verb(carrier, .by = c(a1, a2))
        typed <- verb(carrier, .by = !!opaque)
      } else {
        expected <- verb(carrier, .grouping = grouping_set(a1, a2))
        typed <- verb(carrier, .grouping = grouping_set(!!opaque))
      }
      if (identical(verb, expand_with_margins)) {
        expected <- collect_result(expected)
        typed <- collect_result(typed)
      }
      stopifnot(identical(expected, typed))
    }
  }
  cat(label, "native and opaque controls matched; both verbs falsely refused both direct positions\n")
}
main <- function() {
  step <- dtplyr::lazy_dt(data)
  compare("dtplyr", step, data.table::as.data.table(data[0L, ]))
  arrow <- arrow::Table$create(data)
  compare("Arrow", arrow, as.data.frame(arrow::schema(arrow)))
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  sqlite_tbl <- dplyr::copy_to(sqlite, data, "rows")
  compare("SQLite", sqlite_tbl, sqlite_tbl)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  duck_tbl <- dplyr::copy_to(duck, data, "rows")
  compare("DuckDB", duck_tbl, dplyr::collect(utils::head(duck_tbl, 0L)))
}
main()
