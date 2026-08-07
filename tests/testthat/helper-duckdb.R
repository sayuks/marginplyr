# The one place this suite opens a DuckDB connection.
#
# `shared_home = FALSE` is the whole reason the helper exists. DuckDB resolves a
# home directory for its extension cache and stored secrets each time a driver
# is created, and it picks the shared `~/.duckdb` whenever that directory is
# already present -- which it is on any machine that has ever run DuckDB. A
# connection opened without the argument therefore writes outside the R
# session's temporary directory during `R CMD check`, which CRAN Repository
# Policy forbids. The argument redirects both locations into `tempdir()`; it
# needs duckdb (>= 1.5.5), which is what DESCRIPTION states.
#
# Passing it at one site rather than at each of the suite's connections is what
# makes the rule checkable: `test-duckdb-storage.R` reads the resolved
# directories back out of a connection, and its source scan holds every other
# file to going through here.
#
# Callers keep their own `on.exit(DBI::dbDisconnect(con, shutdown = TRUE))`.
# Deferring the disconnect from inside would need withr, which this package does
# not depend on, and the tests that open a connection already tear it down.
duckdb_test_connection <- function() {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
}
