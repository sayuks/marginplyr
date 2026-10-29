pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
source <- tibble(g = c("a", "b"), v = c(1, 3))
connections <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
checks <- 0L
for (backend in names(connections)) {
  input <- copy_to(connections[[backend]], source, "ordinary_name_controls")
  for (role in c("summary", "identifier")) {
    answer <- tryCatch({
      query <- if (role == "summary") {
        summarise(input, `...` = sum(v, na.rm = TRUE), .by = g)
      } else {
        mutate(input, `...` = 1L)
      }
      collect(query)
    }, error = identity)
    stopifnot(inherits(answer, "error"))
    cat(backend, role, conditionMessage(answer), "\n")
    checks <- checks + 1L
  }
}
input <- dtplyr::lazy_dt(source)
answer <- tryCatch(collect(summarise(input, .SD = sum(v))), error = identity)
stopifnot(inherits(answer, "error"))
cat("dt ungrouped .SD", conditionMessage(answer), "\n")
checks <- checks + 1L
for (connection in connections) DBI::dbDisconnect(connection)
cat("UPSTREAM_CONTROLS", checks, "PASSED\n")
