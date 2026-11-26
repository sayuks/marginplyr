# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On DuckDB a share summary with no Margin order loses the usable window ordering of its input.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
options(warn = 1)

d <- data.frame(a = c("x", "y", "x", "y", "z", "z", "x"), v = c(1, 2, 3, 4, 5, 6, 7))
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
DBI::dbWriteTable(duck, "df", d)
input <- dbplyr::window_order(tbl(duck, "df"), a)
os <- function(q) paste(vapply(dbplyr::op_sort(q), function(e) paste(deparse(e), collapse = ""), ""), collapse = ", ")
running <- function(q) {
  r <- suppressWarnings(collect(mutate(q, r = cumsum(n))))
  paste(r$a, r$r, sep = "=", collapse = " ")
}

cat("Expected (ADR 0018 amendment: .sort = \"none\" leaves every usable window-order term\n")
cat("available to a later window expression, on both adapters): op_sort = a, and a later\n")
cat("cumsum(n) runs in a order. Plain dplyr on the collected per-grain rows, ordered by a in C locale:\n")
exp <- bind_rows(summarise(d, n = sum(v), .by = a), tibble(a = "Total", n = sum(d$v)))
exp <- exp[order(exp$a, method = "radix"), ]
exp$r <- cumsum(exp$n)
cat("  ", paste(exp$a, exp$r, sep = "=", collapse = " "), "\n\n")

no_share <- summarize_with_margins(input, n = sum(v, na.rm = TRUE), .grouping = rollup(a))
total <- summarize_with_margins(input, n = sum(v, na.rm = TRUE), p = share_of_total(n), .grouping = rollup(a))
parent <- summarize_with_margins(input, n = sum(v, na.rm = TRUE), p = share_of_parent(n), .grouping = rollup(a))

cat("Observed on DuckDB:\n")
cat("  no share          op_sort:", os(no_share), "| cumsum:", running(no_share), "\n")
cat("  share_of_total()  op_sort:", os(total), "| cumsum:", running(total), "\n")
cat("  share_of_parent() op_sort:", os(parent), "| cumsum:", running(parent), "\n")
cat("\n  rendered window over the share result:\n")
cat(sub("\\n.*", "", suppressWarnings(dbplyr::sql_render(mutate(total, r = cumsum(n))))), "\n")
cat("  warning dbplyr raises for it:\n")
withCallingHandlers(invisible(dbplyr::sql_render(mutate(total, r = cumsum(n)))),
                    warning = function(w) { cat("   ", conditionMessage(w), "\n"); invokeRestart("muffleWarning") })

cat("\nNative control, same DuckDB connection: per-grain dbplyr summaries with a share-like\n")
cat("window mutate keep the input's window ordering:\n")
native <- mutate(summarise(input, n = sum(v, na.rm = TRUE), .by = a), p = n / sum(n, na.rm = TRUE))
cat("  native summarise() |> mutate(p = n / sum(n)) op_sort:", os(native), "\n")

cat("\nSame share call on SQLite (portable adapter):\n")
sq <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(sq), add = TRUE)
DBI::dbWriteTable(sq, "df", d)
sq_total <- summarize_with_margins(dbplyr::window_order(tbl(sq, "df"), a), n = sum(v, na.rm = TRUE), p = share_of_total(n),
                                   .grouping = rollup(a), .check_share_source = FALSE)
cat("  SQLite share_of_total() op_sort:", os(sq_total), "| cumsum:", running(sq_total), "\n")
