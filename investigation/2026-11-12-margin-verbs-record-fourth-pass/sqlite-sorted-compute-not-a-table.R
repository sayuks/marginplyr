# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# compute() of a sorted SQLite Margin summary returns an arranged query, not a named table.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(dbplyr)})
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "a", "b"), v = 1:3))
x <- tbl(con, "t")
try_ <- function(tag, e) cat(sprintf("%-28s", tag), tryCatch(paste(format(e), collapse = " "), error = function(c) paste("ERROR:", gsub("\n", " ", conditionMessage(c)))), "\n")
m <- summarize_with_margins(x, s = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last") |>
  compute(name = "margins", temporary = FALSE)
n <- x |> group_by(g) |> summarise(s = sum(v, na.rm = TRUE)) |> arrange(g) |>
  compute(name = "native", temporary = FALSE)
u <- summarize_with_margins(x, s = sum(v, na.rm = TRUE), .grouping = rollup(g)) |>
  compute(name = "unsorted", temporary = FALSE)
for (nm in c("m", "n", "u")) { y <- get(nm); cat("##", nm, "\n")
  try_("remote_name", remote_name(y))
  try_("union_all rows", nrow(collect(union_all(y, y))))
  try_("union rows", nrow(collect(union(y, y))))
  try_("union_all as y", nrow(collect(union_all(n, y))))
  try_("rows_append rows", nrow(collect(rows_append(y, y))))
  try_("pivot_longer rows", nrow(collect(tidyr::pivot_longer(y, c(g, s), values_transform = as.character))))
  try_("rows_append in_place", { rows_append(y, n, in_place = TRUE); DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s", c(m = "margins", n = "native", u = "unsorted")[[nm]]))[[1]] })
}
