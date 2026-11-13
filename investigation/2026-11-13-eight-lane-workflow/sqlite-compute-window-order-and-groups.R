# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# compute() of a SQLite Margin result: a sorted result over an expression-ordered input fails, and the computed table drops the window ordering and grouping native compute() keeps.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
d <- data.frame(g = c("a", "b", "a", "b", "a", "b"), h = c("x", "y", "y", "x", "x", "y"), v = c(6, 3, 5, 1, 2, 4))
DBI::dbWriteTable(con, "t", d)
t <- tbl(con, "t")
terms <- function(x) paste0("[", paste(vapply(dbplyr::op_sort(x), rlang::as_label, ""), collapse = ", "), "]")
try_it <- function(expr) tryCatch(
  paste(nrow(collect(expr)), "rows"),
  error = function(e) paste0("<", class(e)[1], "> ", gsub("\\s+", " ", conditionMessage(e)))
)

cat("-- a sorted result over an input ordered by an expression\n")
input <- head(arrange(t, -v), 5)
sorted <- expand_with_margins(input, .grouping = rollup(h), .sort = "last")
cat("window ordering the result records      :", terms(sorted), "\n")
cat("window ordering of its type anchor      :", terms(attr(sorted, "marginplyr_public_anchor")), "\n")
cat("collect(sorted)                         :", try_it(sorted), "\n")
cat("compute(sorted)                         :", try_it(compute(sorted)), "\n")
cat("compute(), input arrange(desc(v))       :", try_it(compute(expand_with_margins(head(arrange(t, desc(v)), 5), .grouping = rollup(h), .sort = "last"))), "\n")
cat("native compute(input), records the term :", try_it(compute(input)), "\n")
cat("native compute() of an ordered query recording none:", try_it(compute(dbplyr::window_order(arrange(mutate(input, k = 1L), k)))), "\n")

cat("-- an unsorted result over a window-ordered input\n")
unsorted <- expand_with_margins(dbplyr::window_order(t, v), .grouping = rollup(h))
cat("window ordering before compute()        :", terms(unsorted), "\n")
cat("window ordering after compute()         :", terms(compute(unsorted)), "\n")
cat("native compute(window_order(t, v)) keeps:", terms(compute(dbplyr::window_order(t, v))), "\n")
rank_by_hand <- function(rows) ave(rows$v, rows$h, FUN = rank)
ranked <- function(x) {
  rows <- collect(mutate(x, r = row_number(), .by = h))
  isTRUE(all.equal(as.numeric(rows$r), rank_by_hand(rows)))
}
cat("row_number(.by = h) follows v before compute():", ranked(unsorted), "\n")
cat("row_number(.by = h) follows v after compute() :", ranked(compute(unsorted)), "\n")

cat("-- a grouped query derived from a result that holds a Boolean-word column\n")
DBI::dbWriteTable(con, "tb", cbind(d, true = 1))
derived <- dbplyr::window_order(group_by(expand_with_margins(tbl(con, "tb"), .grouping = rollup(h)), g), v)
computed <- compute(derived)
native <- compute(dbplyr::window_order(group_by(tbl(con, "tb"), g), v))
show <- function(label, x) cat(sprintf("%-40s groups [%s] window order %s\n", label, paste(group_vars(x), collapse = ", "), terms(x)))
show("derived Margin query", derived)
show("compute(derived Margin query)", computed)
show("native compute(grouped, ordered table)", native)
counts <- function(x) { out <- collect(summarise(x, n = n())); paste(sprintf("%s=%s", if ("g" %in% names(out)) out$g else "all", out$n), collapse = " ") }
cat("summarise(n = n()) by hand: a=6 b=6; before compute():", counts(derived), "; after compute():", counts(computed), "\n")
plain <- compute(dbplyr::window_order(group_by(expand_with_margins(t, .grouping = rollup(h)), g), v))
show("control: no Boolean-word column", plain)
DBI::dbDisconnect(con)
