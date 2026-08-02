# dbplyr's SQLite simulator opens no connection, but rendering SQL from it
# reads the installed driver version through `RSQLite::rsqliteVersion()`.
# Simulated SQLite queries therefore need the optional RSQLite package even
# though nothing is executed.
sqlite_simulation_available <- function() {
  requireNamespace("RSQLite", quietly = TRUE)
}

skip_if_no_sqlite_simulation <- function() {
  if (!sqlite_simulation_available()) {
    skip("RSQLite is not installed")
  }
}
