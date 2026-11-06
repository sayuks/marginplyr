Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
make_spec <- function(key) rollup(base::`(`({{ key }}))
make_control <- function(key) rollup(identity({{ key }}))
reference <- function(key, data) {
  names(tidyselect::eval_select(rlang::expr(base::`(`({{ key }})), data))
}
data <- data.frame(u = c("north", "south"), v = c(7L, 9L), rid = 1:2)
u <- "v"
stopifnot(identical(reference(u, data), "v"))
connections <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
)
expected <- rbind(data, transform(data, v = NA_integer_))
expected$sid <- rep(1:2, each = 2L)
expected <- expected[c("v", "sid", "u", "rid")]
failed <- character()
for (engine in names(connections)) {
  con <- connections[[engine]]
  DBI::dbWriteTable(con, "facts", data)
  input <- tbl(con, "facts")
  plan <- inspect_grouping(input, .grouping = make_spec(u), .format = "list")
  actual <- collect(expand_with_margins(input, .grouping = make_spec(u),
    .margin_label = NULL, .id = "sid"))
  control <- collect(expand_with_margins(input, .grouping = make_control(u),
    .margin_label = NULL, .id = "sid"))
  canonical <- function(value) {
    value <- as.data.frame(value)
    value <- value[order(value$sid, value$rid), , drop = FALSE]
    rownames(value) <- NULL
    value
  }
  stopifnot(identical(canonical(control), expected))
  cat(engine, "qualified included:", plan$included[[1L]], "\n")
  print(actual)
  cat(engine, "ordinary/identity oracle controls: pass\n")
  if (!identical(canonical(actual), expected)) failed <- c(failed, engine)
}
for (engine in names(connections)) {
  if (identical(engine, "duckdb")) {
    DBI::dbDisconnect(connections[[engine]], shutdown = TRUE)
  } else DBI::dbDisconnect(connections[[engine]])
}
cat("wrong expansion results:", paste(failed, collapse = ", "), "\n")
if ("--assert" %in% commandArgs(TRUE)) stopifnot(length(failed) == 0L)
