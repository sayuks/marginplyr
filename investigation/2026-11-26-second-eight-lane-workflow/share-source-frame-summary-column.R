# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A share source that reads a column of an earlier unnamed data-frame-valued summary is accepted, where the same read of a named summary is refused.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})

d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
frame_of <- function(v) tibble::tibble(prior = sum(v))
show <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) e)
  cat("\n--", label, "\n")
  if (inherits(r, "error")) {
    cat("ERROR <", paste(class(r), collapse = "/"), ">:", conditionMessage(r), "\n")
  } else {
    print(as.data.frame(collect(r)))
  }
}

cat("expected (contract): every call below that reads `prior` (or `y_prior`) from an",
    "earlier summary in the share source is refused with the earlier-alias dependency",
    "condition, as the named-summary control is.\n")

show("control: named earlier summary, refused",
     summarize_with_margins(d, prior = sum(y), value = sum(x) + prior,
                            total = share_of_total(value), .grouping = rollup(g)))
show("control: across() output read by the source, refused",
     summarize_with_margins(d, across(y, sum, .names = "prior"), value = sum(x) + prior,
                            total = share_of_total(value), .grouping = rollup(g)))
show("control: frame output read inside an across() source, refused",
     summarize_with_margins(d, tibble::tibble(prior = sum(y)),
                            across(x, ~ sum(.x) + prior, .names = "value"),
                            total = share_of_total(value), .grouping = rollup(g)))

show("observed: unnamed tibble() summary read by the source",
     summarize_with_margins(d, tibble::tibble(prior = sum(y)), value = sum(x) + prior,
                            total = share_of_total(value), .grouping = rollup(g)))
show("observed: unnamed data.frame() summary, share_of_parent",
     summarize_with_margins(d, data.frame(prior = sum(y)), value = sum(x) * prior,
                            total = share_of_parent(value), .grouping = rollup(g)))
show("observed: a function returning a frame",
     summarize_with_margins(d, frame_of(y), value = sum(x) + prior,
                            total = share_of_total(value), .grouping = rollup(g)))
show("observed: across(.unpack = TRUE)",
     summarize_with_margins(d, across(y, function(v) data.frame(prior = sum(v)), .unpack = TRUE),
                            value = sum(x) + y_prior, total = share_of_total(value),
                            .grouping = rollup(g)))
show("observed: .data$prior pronoun read",
     summarize_with_margins(d, tibble::tibble(prior = sum(y)), value = sum(x) + .data$prior,
                            total = share_of_total(value), .grouping = rollup(g)))
show("observed: data.table input",
     summarize_with_margins(data.table::as.data.table(d), tibble::tibble(prior = sum(y)),
                            value = sum(x) + prior, total = share_of_total(value),
                            .grouping = rollup(g)))

cat("\nnative control: plain dplyr reads the unpacked column as an alias (local only)\n")
print(summarise(d, tibble::tibble(prior = sum(y)), value = sum(x) + prior, .by = g))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "d", d)
show("native control, DuckDB: the alias cannot be reused (backend refuses)",
     summarise(tbl(con, "d"), tibble::tibble(prior = sum(y)), value = sum(x) + prior, .by = g))
DBI::dbDisconnect(con, shutdown = TRUE)
invisible(0)
