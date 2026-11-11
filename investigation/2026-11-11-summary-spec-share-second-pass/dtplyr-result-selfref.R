# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# An unsorted dtplyr Margin summary that keeps a variable label evaluates to a
# data.table with an invalid .internal.selfref: a later mutate() warns, and
# as.data.table() hands back a table `:=` cannot update by reference.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr)); options(warn = 1)
.datatable.aware <- TRUE; `:=` <- data.table::`:=`
d <- data.frame(year = c(1L, 1L, 2L, 2L), region = c("a", "b", "a", "b"), v = 1:4)
attr(d$year, "label") <- "Fiscal year"                      # e.g. a haven import
l <- dtplyr::lazy_dt(data.table::as.data.table(d))
m <- summarize_with_margins(l, tot = sum(v), .by = year, .grouping = rollup(region))
nat <- union_all(summarise(l, tot = sum(v), .by = c(year, region)),   # native reference
  summarise(l, tot = sum(v), .by = year) |> mutate(region = "Total") |> select(year, region, tot))
show <- function(tag, x) { dt <- data.table::as.data.table(x)
  cat(sprintf("%-28s selfrefok=%d truelength=%d label=%s\n", tag, utils::getFromNamespace("selfrefok", "data.table")(dt),
              data.table::truelength(dt), format(attr(dt$year, "label")))) }
show("margin .sort='none'", m)
show("margin .sort='last'", summarize_with_margins(l, tot = sum(v), .by = year, .grouping = rollup(region), .sort = "last"))
show("margin, unlabelled source", summarize_with_margins(dtplyr::lazy_dt(data.table::as.data.table(transform(d, year = as.integer(year)))), tot = sum(v), .by = year, .grouping = rollup(region)))
show("native union_all", nat)
cat("-- native |> mutate |> collect\n"); invisible(collect(mutate(nat, z = 1L)))
cat("-- margin |> mutate |> collect\n"); invisible(collect(mutate(m, z = 1L)))
add <- function(dt) { dt[, flag := TRUE]; invisible(NULL) }   # update by reference in a helper
n2 <- data.table::as.data.table(nat); add(n2); cat("native: caller sees flag:", "flag" %in% names(n2), "\n")
m2 <- data.table::as.data.table(m); suppressWarnings(add(m2)); cat("margin: caller sees flag:", "flag" %in% names(m2), "\n")
