# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A caller function's own error in `.by` or a constructor argument is replaced by the computed-expression refusal (#1802).
# Candidate 1: the computed-selection refusal (#1703 b) replaces any error a
# caller's own function raises inside `.by` or a Grouping specification
# argument, with no parent, whenever the call names an input column.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(LANGUAGE = "en")

sales <- data.frame(region = c("E", "E", "W"), store = c("A", "B", "C"),
                    yr = c(1L, 1L, 2L), v = 1:3)

# A caller's selection helper that fails for its own reason; it never reads
# its argument, so no column lookup is involved.
key_cols <- function(x) {
  rlang::abort("The key registry is offline.", class = "registry_offline")
}

outcome <- function(expr) {
  rlang::try_fetch(
    { force(expr); "returned" },
    # rlang::try_fetch() also matches a condition's parents.
    registry_offline = function(cnd) "the caller's registry_offline condition reaches the handler",
    marginplyr_error = function(cnd) paste0(
      "marginplyr_error \"", strsplit(conditionMessage(cnd), "\n")[[1]][[1]], "\"",
      if (is.null(cnd$parent)) " (no parent)" else " (parent kept)"
    ),
    error = function(cnd) paste("other error", class(cnd)[[1]])
  )
}

cat("Expected: the caller's own condition reaches a handler for its class, as natively.\n\n")
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
con_duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
inputs <- list(
  data.frame = sales,
  data.table = data.table::as.data.table(sales),
  dtplyr = dtplyr::lazy_dt(sales),
  arrow = arrow::arrow_table(sales),
  sqlite = copy_to(con_sqlite, sales, "sales"),
  duckdb = copy_to(con_duck, sales, "sales")
)
for (nm in names(inputs)) {
  d <- inputs[[nm]]
  cat("==", nm, "\n")
  cat("  native  summarise(.by = key_cols(region)):        ",
      if (nm == "arrow") {
        outcome(collect(summarise(group_by(d, across(key_cols(region))), n = n())))
      } else {
        outcome(collect(summarise(d, n = n(), .by = key_cols(region))))
      }, "\n")
  cat("  marginplyr .by = key_cols(region):                ",
      outcome(summarize_with_margins(d, n = n(), .by = key_cols(region),
                                     .grouping = rollup(store))), "\n")
  cat("  marginplyr .grouping = rollup(key_cols(region)):  ",
      outcome(summarize_with_margins(d, n = n(),
                                     .grouping = rollup(key_cols(region)))), "\n")
}
cat("\n== data.frame, other entry points\n")
cat("  expand_with_margins(.by = key_cols(region)):      ",
    outcome(expand_with_margins(sales, .by = key_cols(region),
                                .grouping = rollup(store))), "\n")
cat("  inspect_grouping(rollup(key_cols(region))):       ",
    outcome(inspect_grouping(sales, .grouping = rollup(key_cols(region)))), "\n")
cat("  grouping_sets(c(yr, key_cols(region))):           ",
    outcome(inspect_grouping(sales, .grouping = grouping_sets(c(yr, key_cols(region))))), "\n")
cat("\n== control: the same function over a name that is no column\n")
cat("  marginplyr .by = key_cols(zone):                  ",
    outcome(summarize_with_margins(sales, n = n(), .by = key_cols(zone),
                                   .grouping = rollup(store))), "\n")
cat("\n== the refusal's own case (#1703 b): the column lookup fails\n")
cat("  marginplyr .by = toupper(region):                 ",
    outcome(summarize_with_margins(sales, n = n(), .by = toupper(region),
                                   .grouping = rollup(store))), "\n")

cat("\n== the full condition for .by = key_cols(region) on a data frame\n")
cnd <- tryCatch(summarize_with_margins(sales, n = n(), .by = key_cols(region),
                                       .grouping = rollup(store)),
                error = identity)
print(cnd)
cat("parent: "); print(cnd$parent)

cat("\n== a time limit expiring inside a slow caller helper (no trace)\n")
slow_cols <- function(x) { for (i in 1:40) Sys.sleep(0.01); "region" }
lim <- tryCatch({
  setTimeLimit(elapsed = 0.1, transient = TRUE)
  summarize_with_margins(sales, n = n(), .by = slow_cols(region), .grouping = rollup(store))
  setTimeLimit(elapsed = Inf); "returned"
}, error = function(e) { setTimeLimit(elapsed = Inf)
  paste0("<", class(e)[[1]], "> ", strsplit(conditionMessage(e), "\n")[[1]][[1]],
         if (is.null(e$parent)) " (no parent)" else "") })
cat("  marginplyr:", lim, "\n")
lim_native <- tryCatch({
  setTimeLimit(elapsed = 0.1, transient = TRUE)
  summarise(sales, n = n(), .by = slow_cols(region))
  setTimeLimit(elapsed = Inf); "returned"
}, error = function(e) { setTimeLimit(elapsed = Inf)
  paste0("<", class(e)[[1]], "> ", gsub("\n", " | ", conditionMessage(e))) })
cat("  native:    ", lim_native, "\n")
DBI::dbDisconnect(con_sqlite); DBI::dbDisconnect(con_duck, shutdown = TRUE)
