Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
run <- function(label, code) {
  cat("\n---", label, "---\n")
  tryCatch(print(force(code)), error = function(e) {
    cat("ERROR:", conditionMessage(e), "\nCLASS:", class(e)[1L], "\n")
  })
}
for (backend in c("sqlite", "duckdb")) local({
  cat("\nBACKEND:", backend, "\n")
  con <- if (backend == "sqlite") DBI::dbConnect(RSQLite::SQLite()) else
    DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = backend == "duckdb"))
  src <- copy_to(con, tibble(g = "a", `TRUE` = 7L), "src", temporary = TRUE)
  cte <- dbplyr::sql_options(cte = TRUE)
  for (sort in c("none", "last")) {
    cat("\nSORT:", sort, "\n")
    x <- expand_with_margins(src, .grouping = rollup(g), .sort = sort)
    run("direct", collect(x))
    run("collapse", collect(collapse(x)))
    run("collapse SQL", dbplyr::sql_render(collapse(x)))
    run("direct cte", collect(x, sql_options = cte))
    run("direct SQL cte", dbplyr::sql_render(x, sql_options = cte))
    run("ordinary source cte", collect(src, sql_options = cte))
    run("ordinary source collapse", collect(collapse(src)))
    run("union self", collect(union_all(x, x)))
    run("ordinary ordered union", collect(union_all(arrange(src, g), arrange(src, g))))
  }
})
