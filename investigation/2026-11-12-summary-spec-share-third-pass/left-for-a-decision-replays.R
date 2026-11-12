# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>
#
# Replays seven of the observations the note lists under "Left for a
# decision", each beside the control it was compared with.

suppressMessages({
  library(dplyr)
  pkgload::load_all(".", quiet = TRUE)
})

show <- function(label, expr) {
  cat(label, ": ", sep = "")
  out <- tryCatch(
    suppressWarnings(expr),
    error = function(cnd) {
      sprintf("<%s> %s", class(cnd)[1], conditionMessage(cnd))
    }
  )
  cat(paste(format(out), collapse = " | "), "\n")
}

d <- data.frame(
  a = c("x", "x", "y"), g = c("a", "B", "b"),
  v = c(1L, 2L, 3L), w = c(4, 5, 6)
)
sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(sqlite, "t0", d)
s <- tbl(sqlite, "t0")
duck <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(duck, "t0", d)
k <- tbl(duck, "t0")
dt <- dtplyr::lazy_dt(d)

cat("== dot-prefixed output names one edit from an option\n")
for (nm in c(".y", ".b", ".i", ".idx", ".ids", ".by2", ".x", ".n", ".key")) {
  call <- bquote(summarize_with_margins(
    d, .(as.name(nm)) := mean(v), .grouping = rollup(g)
  ))
  show(nm, names(eval(call)))
}
show("native .y", names(summarise(d, .y = mean(v), .by = g)))

cat("== class of a collected sorted SQLite summary\n")
sorted <- function(x) {
  summarize_with_margins(
    x, s = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last"
  )
}
show("SQLite sorted", class(collect(sorted(s))))
show("SQLite unsorted", class(collect(summarize_with_margins(
  s, s = sum(v, na.rm = TRUE), .grouping = rollup(g)
))))
show("SQLite native arranged", class(collect(arrange(
  summarise(group_by(s, g), s = sum(v, na.rm = TRUE)), g
))))
show("DuckDB sorted", class(collect(sorted(k))))

cat("== advice when the plan has no Grand total set\n")
no_total <- function(spec) {
  summarize_with_margins(
    d, s = sum(v), p = share_of_total(s), .grouping = spec
  )
}
show("grouping_spec", no_total(grouping_spec(grouping_set(a), rollup(g))))
show("grouping_set", no_total(grouping_set(a)))
show("grouping_sets", no_total(grouping_sets(grouping_set(a))))
show("advice followed", no_total(
  grouping_spec(grouping_set(a), rollup(g), grouping_set())
))

cat("== a share helper called with a name that is not its formal\n")
show("share_of_total(y = s)", summarize_with_margins(
  d, s = sum(v), p = share_of_total(y = s), .grouping = rollup(g)
)$p)
show("share_of_parent(y = s)", summarize_with_margins(
  d, s = sum(v), p = share_of_parent(y = s), .grouping = rollup(a, g)
)$p)
show("DuckDB share_of_total(y = s)", collect(summarize_with_margins(
  k, s = sum(v, na.rm = TRUE), p = share_of_total(y = s),
  .grouping = rollup(g)
))$p)

cat("== Margin order under dplyr.legacy_locale\n")
legacy <- function(x) {
  withr::with_options(
    list(dplyr.legacy_locale = TRUE),
    withr::with_collate("en_US.UTF-8", collect(sorted(x))$g)
  )
}
show("local, default", sorted(d)$g)
show("local, legacy", legacy(d))
show("SQLite, legacy", legacy(s))
show("dtplyr, legacy", legacy(dt))

cat("== bare NA beside an integer aggregate between grouping sets\n")
mixed <- function(x) {
  collect(summarize_with_margins(
    x, s = if (grouping_bit(g) == 1L) NA else sum(v, na.rm = TRUE),
    .grouping = rollup(g)
  ))
}
show("local", class(mixed(d)$s))
show("SQLite", class(mixed(s)$s))
show("DuckDB", class(mixed(k)$s))
show("dtplyr", class(mixed(dt)$s))
show("dtplyr, NA_integer_", class(collect(summarize_with_margins(
  dt, s = if (grouping_bit(g) == 1L) NA_integer_ else sum(v),
  .grouping = rollup(g)
))$s))

cat("== .names with a parenthesized function as across() .fns\n")
paren <- function(x) {
  names(collect(summarize_with_margins(
    x, across(c(v, w), (sum), .names = "s_{.col}"), .grouping = rollup(g)
  )))
}
show("local", paren(d))
show("SQLite", paren(s))
show("dtplyr", paren(dt))
show("DuckDB", paren(k))
show("DuckDB, formula", names(collect(summarize_with_margins(
  k, across(c(v, w), (~ sum(.x, na.rm = TRUE)), .names = "s_{.col}"),
  .grouping = rollup(g)
))))
show("DuckDB, no pair", names(collect(summarize_with_margins(
  k, across(c(v, w), sum, .names = "s_{.col}"), .grouping = rollup(g)
))))
native <- function(x) {
  names(collect(summarise(
    group_by(x, g), across(c(v, w), (sum), .names = "s_{.col}")
  )))
}
show("native DuckDB", native(k))
show("native SQLite", native(s))
show("native dtplyr", native(dt))
