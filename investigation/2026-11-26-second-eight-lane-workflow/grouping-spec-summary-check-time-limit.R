# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The check for a Grouping specification written as a summary reads the binding of a bare-name summary inside a handler, which consumes a time limit and runs a failing argument twice.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
invisible(Sys.setlocale("LC_MESSAGES", "C")); Sys.setenv(LANGUAGE = "en")
show <- function(expr) {
  t0 <- Sys.time()
  r <- withCallingHandlers(tryCatch({force(expr); "RETURNED"},
    error = function(e) sprintf("ERROR <%s>: %s", class(e)[1], gsub("\n", " ", conditionMessage(e)))),
    warning = function(w) {cat("    warning reached caller:", gsub("\n", " ", conditionMessage(w)), "\n"); invokeRestart("muffleWarning")})
  setTimeLimit(elapsed = Inf)
  cat(sprintf("    %s after %.2f s\n", r, as.numeric(Sys.time() - t0, units = "secs")))
}
df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
runs <- 0
lookup_weight <- function() { runs <<- runs + 1; stop("weight service unavailable") }
slow_weight <- function() { runs <<- runs + 1; for (i in 1:20) Sys.sleep(0.025); 1 }
mp <- function(data, w) summarize_with_margins(data, s = sum(v), k = w, .grouping = rollup(g))
nat <- function(data, w) summarise(group_by(data, g), s = sum(v), k = w)

cat("A. failing promise: expected one run of the caller's code, as in dplyr\n")
runs <- 0; cat("  marginplyr:\n"); show(mp(df, lookup_weight())); cat("    caller code runs:", runs, "\n")
runs <- 0; cat("  native dplyr:\n"); show(nat(df, lookup_weight())); cat("    caller code runs:", runs, "\n")

cat("B. time limit 0.2 s, promise takes 0.5 s: expected an error\n")
runs <- 0; cat("  marginplyr:\n"); show({setTimeLimit(elapsed = 0.2, transient = TRUE); mp(df, slow_weight())}); cat("    caller code runs:", runs, "\n")
runs <- 0; cat("  native dplyr:\n"); show({setTimeLimit(elapsed = 0.2, transient = TRUE); nat(df, slow_weight())}); cat("    caller code runs:", runs, "\n")

cat("C. lazy SQLite input, failing promise\n")
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(con, "t", df); t <- tbl(con, "t")
runs <- 0; cat("  marginplyr:\n"); show(mp(t, lookup_weight())); cat("    caller code runs:", runs, "\n")
runs <- 0; cat("  native dbplyr:\n"); show(nat(t, lookup_weight())); cat("    caller code runs:", runs, "\n")

cat("D. evaluation order of caller code (no error)\n")
o <- function(data, w) summarize_with_margins(data, s = {cat("    [s]\n"); sum(v)}, k = w, .grouping = grouping_set(g))
no <- function(data, w) summarise(group_by(data, g), s = {cat("    [s]\n"); sum(v)}, k = w)
pr <- function() { cat("    [w]\n"); 1 }
cat("  marginplyr:\n"); invisible(o(df, pr())); cat("  native dplyr:\n"); invisible(no(df, pr()))
DBI::dbDisconnect(con)
