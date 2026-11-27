# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A share across() selection written all_of(c("ok", "missing")) is refused naming the eligible summary (#1804).
# Candidate 4: a share across() selection written with all_of() and one
# misspelled summary name is refused as though the *first* name -- a valid,
# eligible preceding summary -- were unavailable. The handler in
# resolve_share_selection() reads every character subscript in the condition
# chain's `i` field, and for all_of() tidyselect's `i` is the whole vector.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(LANGUAGE = "en")
sales <- data.frame(region = c("E", "E", "W"), revenue = c(1, 2, 5), units = c(3, 4, 6))
first_line <- function(cnd) strsplit(conditionMessage(cnd), "\n")[[1]][[1]]
show <- function(label, expr) {
  cnd <- tryCatch({ force(expr); NULL }, error = identity)
  out <- if (is.null(cnd)) "RETURNED" else paste0(
    "<", class(cnd)[[1]], "> ", first_line(cnd),
    if (inherits(cnd, "marginplyr_error")) {
      paste0(" [source_summary = ", deparse(cnd$source_summary), "; ",
             if (is.null(cnd$parent)) "no parent]" else "parent kept]")
    } else "")
  cat(sprintf("  %-40s %s\n", label, out))
}
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
con_duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
inputs <- list(
  data.frame = sales,
  dtplyr = dtplyr::lazy_dt(sales),
  sqlite = copy_to(con_sqlite, sales, "sales"),
  duckdb = copy_to(con_duck, sales, "sales")
)
share_with <- function(d, sel) {
  rlang::inject(summarize_with_margins(
    d, revenue = sum(revenue), units = sum(units),
    across(!!sel, share_of_parent, .names = "{.col}_share"),
    .grouping = rollup(region)
  ))
}
cat("Expected: the refusal names `nope`, the one name no summary answers,\n",
    "as c(revenue, nope) does and as tidyselect does natively.\n\n", sep = "")
for (nm in names(inputs)) {
  d <- inputs[[nm]]
  cat("==", nm, "\n")
  show("all_of(c(\"revenue\", \"nope\")):", share_with(d, quote(all_of(c("revenue", "nope")))))
  show("c(revenue, nope):", share_with(d, quote(c(revenue, nope))))
  show("all_of(c(\"revenue\", \"units\")) (valid):",
       collect(share_with(d, quote(all_of(c("revenue", "units"))))))
}
cat("\n== native control on a data frame (tidyselect's own report)\n")
show("dplyr across(all_of(c(\"revenue\", \"nope\"))):",
     summarise(sales, revenue = sum(revenue), units = sum(units),
               across(all_of(c("revenue", "nope")), ~ .x / sum(.x)), .by = region))
cnd <- tryCatch(summarise(sales, revenue = sum(revenue),
  across(all_of(c("revenue", "nope")), ~ .x / sum(.x)), .by = region), error = identity)
cat("  native message:", gsub("\n", " | ", conditionMessage(cnd)), "\n")
cat("\n== what the handler reads: tidyselect's condition for all_of()\n")
oob <- tryCatch(tidyselect::eval_select(rlang::quo(all_of(c("revenue", "nope"))),
                data = list(revenue = 1L, units = 2L), strict = TRUE), error = identity)
cat("  parent class:", class(oob$parent)[[1]], " i =", deparse(oob$parent$i),
    " names =", deparse(oob$parent$names), "\n")

cat("\n== the same handler, with a caller's helper failing on another frame\n")
config <- data.frame(revenue = 1)
share_cols <- function() names(select(config, all_of(c("revenue", "units"))))
show("all_of(share_cols()):", share_with(sales, quote(all_of(share_cols()))))
show("native across(all_of(share_cols())):",
     summarise(sales, revenue = sum(revenue), units = sum(units),
               across(all_of(share_cols()), ~ .x / sum(.x)), .by = region))
DBI::dbDisconnect(con_sqlite); DBI::dbDisconnect(con_duck, shutdown = TRUE)
