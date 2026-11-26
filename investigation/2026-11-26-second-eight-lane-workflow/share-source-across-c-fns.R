# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A share naming the output of an across() whose .fns is a literal c() of functions is refused as an unknown summary, where the list() spelling is accepted.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})

d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
show <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) e)
  cat("\n--", label, "\n")
  if (inherits(r, "error")) {
    cat("ERROR <", paste(class(r), collapse = "/"), ">:", conditionMessage(r), "\n")
  } else {
    print(as.data.frame(collect(r)))
  }
}

# Independent expectation: plain dplyr at each grain, divided by hand.
by_g <- summarise(d, across(c(x, y), c(s = sum, m = mean)), .by = g)
grand <- summarise(d, across(c(x, y), c(s = sum, m = mean)))
expected <- c(by_g$y_s, grand$y_s) / grand$y_s
cat("expected y_s (native, rows a, b, grand):", c(by_g$y_s, grand$y_s), "\n")
cat("expected sh = y_s / grand y_s:", expected, "\n")

show("observed: c() .fns, share_of_total(y_s)",
     summarize_with_margins(d, across(c(x, y), c(s = sum, m = mean)),
                            sh = share_of_total(y_s), .grouping = rollup(g)))
show("observed: c() .fns, share_of_parent(y_s)",
     summarize_with_margins(d, across(c(x, y), c(s = sum, m = mean)),
                            sh = share_of_parent(y_s), .grouping = rollup(g)))
show("observed: c() .fns, column-wise share",
     summarize_with_margins(d, across(c(x, y), c(s = sum, m = mean)),
                            across(c(x_s, y_s), share_of_total, .names = "{.col}_sh"),
                            .grouping = rollup(g)))
show("observed: c(s = sum) with explicit .names",
     summarize_with_margins(d, across(c(x, y), c(s = sum), .names = "{.col}_t"),
                            sh = share_of_total(y_t), .grouping = rollup(g)))
show("control: same call without a share (outputs are produced)",
     summarize_with_margins(d, across(c(x, y), c(s = sum, m = mean)), .grouping = rollup(g)))
show("control: list() instead of c() computes the share",
     summarize_with_margins(d, across(c(x, y), list(s = sum, m = mean)),
                            sh = share_of_total(y_s), .grouping = rollup(g)))

con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "d", d)
show("native control, DuckDB: c() .fns names y_s",
     summarise(tbl(con, "d"), across(c(x, y), c(s = sum, m = mean)), .by = g))
show("observed, DuckDB: c() .fns share",
     summarize_with_margins(tbl(con, "d"), across(c(x, y), c(s = sum, m = mean)),
                            sh = share_of_total(y_s), .grouping = rollup(g)))
DBI::dbDisconnect(con, shutdown = TRUE)
invisible(0)
