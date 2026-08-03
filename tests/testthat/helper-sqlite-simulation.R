# dbplyr's SQLite simulator opens no connection, but rendering SQL from it
# reads the installed driver version through `RSQLite::rsqliteVersion()`.
# Simulated SQLite queries therefore need the optional RSQLite package even
# though nothing is executed.
sqlite_simulation_available <- function() {
  backend_available("RSQLite")
}

skip_if_no_sqlite_simulation <- function() {
  skip_if_backend_absent("RSQLite")
}

# Drops the dialects whose SQL cannot be rendered without an optional driver
# package, so the remaining ones stay under test.
available_simulators <- function(simulators) {
  if (!sqlite_simulation_available()) {
    simulators <- setdiff(simulators, "simulate_sqlite")
  }
  simulators
}
