# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Boundary map for E1/E2: all-missing INTEGER fixed key `k` and INTEGER dimension `i` on live SQLite.
suppressPackageStartupMessages({library(dplyr); library(dbplyr)})
pkgload::load_all(".", quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
invisible(DBI::dbExecute(con, "CREATE TABLE tna (k INTEGER, i INTEGER, x REAL)"))
invisible(DBI::dbExecute(con, "INSERT INTO tna VALUES (NULL, NULL, 1)"))
src <- tbl(con, "tna"); n <- 0
cl <- function(v) class(v)[1]
cat(sprintf("%-6s %-5s %-5s %-5s %-8s | collect k,i        | compute->collect k,i\n", "label", "sort", ".id", "share", "plan"))
for (lab in list(NULL, "Total")) for (srt in c("none", "last")) for (id in list(NULL, "gid")) for (sh in c(FALSE, TRUE)) for (plan in c("rollup", "one set")) {
  extra <- if (sh) rlang::exprs(p = share_of_total(s)) else list()
  if (sh && plan == "one set") next
  q <- if (plan == "rollup") summarize_with_margins(src, s = sum(x, na.rm = TRUE), !!!extra, .by = k, .grouping = rollup(i), .margin_label = lab, .sort = srt, .id = id, .check_share_source = FALSE)
       else summarize_with_margins(src, s = sum(x, na.rm = TRUE), .by = k, .grouping = grouping_set(i), .margin_label = lab, .sort = srt, .id = id)
  a <- collect(q); n <- n + 1; b <- collect(compute(q, name = paste0("o", n)))
  want_i <- if (is.null(lab)) "integer" else "character"
  flag <- function(z) paste0(cl(z$k), if (cl(z$k) != "integer") "(!)", ",", cl(z$i), if (cl(z$i) != want_i) "(!)")
  cat(sprintf("%-6s %-5s %-5s %-5s %-8s | %-20s| %s\n", if (is.null(lab)) "NULL" else lab, srt, if (is.null(id)) "-" else "gid", sh, plan, flag(a), flag(b)))
}
cat("native control (group_by(k, i) |> summarise): collect", { z <- collect(summarise(group_by(src, k, i), s = sum(x, na.rm = TRUE), .groups = "drop")); paste(cl(z$k), cl(z$i)) },
    "| compute->collect", { z <- collect(compute(summarise(group_by(src, k, i), s = sum(x, na.rm = TRUE), .groups = "drop"))); paste(cl(z$k), cl(z$i)) }, "\n")
