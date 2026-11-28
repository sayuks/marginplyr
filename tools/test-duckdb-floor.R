# Exercise how the DuckDB floor job chooses what to install and what it accepts
# as installed, against fixed DESCRIPTION text and release histories rather
# than the network.
module <- new.env(parent = baseenv())
sys.source(".github/scripts/duckdb-floor.R", envir = module)
expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)
expect_error_matching <- function(code, pattern) {
  message <- tryCatch({
    force(code)
    NA_character_
  }, error = conditionMessage)
  detail <- paste0("Expected an error matching '", pattern, "', got: ", message)
  expect(!is.na(message) && grepl(pattern, message), detail)
}
show <- function(x) paste(utils::capture.output(print(x)), collapse = "\n")

description <- function(suggests) {
  path <- tempfile(fileext = ".dcf")
  suggests <- paste0("Suggests: ", suggests)
  writeLines(c("Package: fixture", "Version: 0.0.1", suggests), path)
  path
}

# The floor is the version DESCRIPTION's Suggests entry states.
floor <- module$duckdb_floor(
  description("DBI,\n    duckdb (>= 1.5.5),\n    testthat (>= 3.1.9)")
)
expect(
  identical(floor, package_version("1.5.5")),
  paste("Read floor", format(floor), "instead of 1.5.5.")
)
for (suggests in c("DBI, duckdbx (>= 1.0)", "duckdb")) {
  expect_error_matching(
    module$duckdb_floor(description(suggests)), "states no duckdb floor"
  )
}

# CRAN's duckdb history around the 1.5 series, shuffled: publication times are
# UTC, as CRAN records them.
releases <- data.frame(
  version = c(
    "2.0.1", "1.5.4", "1.5.4.2", "1.5.5", "1.5.4.1", "1.5.6", "2.0.0"
  ),
  published = as.POSIXct(c(
    "2026-11-23 08:10", "2026-06-19 15:30", "2026-06-29 18:40",
    "2026-07-25 13:40", "2026-06-29 08:00", "2026-09-29 17:50",
    "2026-10-24 00:10"
  ), tz = "UTC")
)
today <- as.Date("2026-11-28")
plan <- function(floor) {
  module$duckdb_floor_plan(releases, package_version(floor), today)
}

# Each candidate is taken from the last daily snapshot at which it was still
# CRAN's current release, lowest first, and only within the floor's series.
got <- plan("1.5.5")
expect(
  identical(got$version, c("1.5.5", "1.5.6")) &&
    identical(got$snapshot, as.Date(c("2026-09-28", "2026-10-23"))),
  show(got)
)

# A release superseded on the day it appeared was never current in a daily
# snapshot, so the next patch stands in for it.
got <- plan("1.5.4.1")
expect(
  identical(got$version, c("1.5.4.2", "1.5.5", "1.5.6")) &&
    identical(got$snapshot[1L], as.Date("2026-07-24")),
  show(got)
)

# CRAN's current release is taken from yesterday's snapshot.
got <- plan("2.0.1")
expect(
  identical(got$version, "2.0.1") &&
    identical(got$snapshot, as.Date("2026-11-27")),
  show(got)
)

expect_error_matching(
  plan("1.6.0"), "No CRAN release of duckdb at or above 1.6.0"
)

# What the job accepts as installed: the floor or later, and below 2.0.
accept <- function(installed, floor = "1.5.5") {
  if (!is.null(installed)) installed <- package_version(installed)
  module$duckdb_floor_accept(installed, package_version(floor))
}
accepted <- accept("1.5.6")
expect(
  grepl("DuckDB 1.5.6 installed; DESCRIPTION's floor is 1.5.5", accepted),
  accepted
)
expect_error_matching(accept(NULL), "DuckDB is not installed")
expect_error_matching(
  accept("1.5.4.3"), "1.5.4.3 is below DESCRIPTION's floor 1.5.5"
)
expect_error_matching(accept("2.0.0"), "2.0.0 is not below 2.0")
expect_error_matching(accept("2.0.1", "2.0.1"), "2.0.1 is not below 2.0")

# A downloaded package is a binary for the running R only when its `Built`
# field names that R series; P3M serves source silently when it has none.
binary <- function(built) module$duckdb_floor_binary(built, "4.6")
stamp <- "; x86_64-pc-linux-gnu; 2026-09-28 01:02:03 UTC; unix"
expect(
  binary(paste0("R 4.6.2", stamp)), "A Built field for R 4.6 was not accepted."
)
expect(!binary(NA_character_), "A source package was accepted as a binary.")
expect(
  !binary(paste0("R 4.5.3", stamp)), "A binary for R 4.5 was accepted."
)
expect(
  !binary(paste0("R 4.60.1", stamp)), "A Built field for R 4.60 was accepted."
)

message("DuckDB floor fixtures passed.")
