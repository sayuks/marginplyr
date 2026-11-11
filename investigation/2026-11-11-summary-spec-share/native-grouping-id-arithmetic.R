# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# Native GROUPING SETS: a multi-column grouping_id() is spliced into the
# caller's arithmetic without parentheses.
d <- data.frame(a = c("x", "x", "y", "y"), b = c("p", "q", "p", "q"), v = 1:4)
query <- function(source, ...) {
  summarize_with_margins(
    source, level = grouping_id(), weight = grouping_id() * 10L,
    depth = 3L - grouping_id(), negated = -grouping_id(),
    .grouping = rollup(a, b), .id = "id", ...
  )
}
values <- function(result) {
  result |> collect() |> distinct(id, level, weight, depth, negated) |>
    arrange(id) |> as.data.frame()
}
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
lazy <- copy_to(con, d, "t", temporary = TRUE)
# Independent oracle: rollup(a, b) has masks 0, 1, 3 in plan order.
mask <- c(0, 1, 3)
expected <- data.frame(id = 1:3, level = mask, weight = mask * 10,
                       depth = 3 - mask, negated = -mask)
local_result <- values(query(d))
native <- values(query(lazy))
portable <- values(query(lazy, .duplicates = "keep"))
print(local_result); print(native); print(portable)
show_query(query(lazy))
same <- function(x) isTRUE(all.equal(x, expected, check.attributes = FALSE))
stopifnot(same(local_result), same(portable))
if (assert_contract) stopifnot(same(native))
