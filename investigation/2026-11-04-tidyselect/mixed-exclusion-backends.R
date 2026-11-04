# Run from the root of the archived package revision.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
suppressPackageStartupMessages(library(dplyr))

run <- function() {
  data <- tibble(x = 1:2, y = -3:-4, z = 100:101)
  sqlite <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
                         config = list(threads = 2L))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  inputs <- list(
    local = data, dtplyr = dtplyr::lazy_dt(data),
    SQLite = copy_to(sqlite, data), DuckDB = copy_to(duck, data),
    Arrow = arrow::Table$create(data)
  )
  forms <- rlang::exprs(
    bare = c(!y, -x),
    parens = c(!y, (-x)),
    helpers = c(!tidyselect::ends_with("y"), (-tidyselect::starts_with("x")))
  )
  differences <- 0L
  for (backend in names(inputs)) {
    for (form in names(forms)) {
      selector <- forms[[form]]
      want <- names(tidyselect::eval_select(selector, data))
      got <- summarize_with_margins(
        inputs[[backend]], across(!!selector, ~ sum(.x, na.rm = TRUE))
      )
      actual <- names(collect(got))
      if (form == "bare") stopifnot(identical(actual, want))
      if (!identical(actual, want)) {
        differences <- differences + 1L
        cat(backend, form, paste(want, collapse = ","),
            paste(actual, collapse = ","), "\n")
      }
    }
  }
  cat("Divergences:", differences, "\n")
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(differences == 0L)
  }
}
run()
