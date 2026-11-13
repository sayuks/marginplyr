# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Direct collect() of a SQLite Margin result refuses dbplyr's deprecated `cte` argument, which native collect() and the package's compute() accept.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
options(lifecycle_verbosity = "warning")
try_it <- function(expr) {
  warned <- character()
  tryCatch(
    {
      out <- withCallingHandlers(expr, warning = function(w) {
        warned <<- union(warned, class(w)[1])
        invokeRestart("muffleWarning")
      })
      paste0(nrow(out), " rows: ", paste(out[[1]], collapse = ","), if (length(warned)) paste0(" [", warned, "]"))
    },
    error = function(e) paste0("<", class(e)[1], "> ", gsub(" at [^ ]+\\.R:[0-9:]+", "", gsub("\\s+", " ", conditionMessage(e))))
  )
}
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "t", data.frame(g = c("b", "a", "B"), v = 1:3))
s <- tbl(con, "t")
unsorted <- summarize_with_margins(s, n = sum(v, na.rm = TRUE), .grouping = rollup(g))
sorted <- summarize_with_margins(s, n = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last")
typed <- summarize_with_margins(s, n = sum(v, na.rm = TRUE), .grouping = rollup(g), .margin_label = NULL)
expansion <- expand_with_margins(s, .grouping = rollup(g), .sort = "last")
native <- union_all(
  summarise(group_by(s, g), n = sum(v, na.rm = TRUE)),
  mutate(summarise(s, n = sum(v, na.rm = TRUE)), g = "Total") |> select(g, n)
)
cat("class of the Margin result       :", class(unsorted)[1], "\n")
cat("Margin unsorted, cte = TRUE      :", try_it(collect(unsorted, cte = TRUE)), "\n")
cat("Margin sorted, cte = TRUE        :", try_it(collect(sorted, cte = TRUE)), "\n")
cat("Margin sorted, cte = FALSE       :", try_it(collect(sorted, cte = FALSE)), "\n")
cat("Margin typed-missing, cte = TRUE :", try_it(collect(typed, cte = TRUE)), "\n")
cat("Margin expansion, cte = TRUE     :", try_it(collect(expansion, cte = TRUE)), "\n")
cat("Margin sorted, sql_options       :", try_it(collect(sorted, sql_options = dbplyr::sql_options(cte = TRUE))), "\n")
cat("Margin sorted, compute(cte=TRUE) :", try_it(collect(compute(sorted, cte = TRUE))), "\n")
cat("Margin one set (.by), cte = TRUE :", try_it(collect(summarize_with_margins(s, n = sum(v, na.rm = TRUE), .by = g), cte = TRUE)), "\n")
cat("native union, cte = TRUE         :", try_it(collect(native, cte = TRUE)), "\n")
cat("native arranged union, cte = TRUE:", try_it(collect(arrange(native, g), cte = TRUE)), "\n")
cat("native, cte + sql_options = NULL :", try_it(collect(native, cte = TRUE, sql_options = NULL)), "\n")
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
DBI::dbWriteTable(duck, "t", data.frame(g = c("b", "a", "B"), v = 1:3))
cat("DuckDB Margin sorted, cte = TRUE :", try_it(collect(summarize_with_margins(tbl(duck, "t"), n = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last"), cte = TRUE)), "\n")
DBI::dbDisconnect(duck, shutdown = TRUE)
DBI::dbDisconnect(con)
