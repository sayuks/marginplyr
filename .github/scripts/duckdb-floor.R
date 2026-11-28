# What `R-CMD-check.yaml`'s `duckdb-floor` job installs and accepts, written as
# functions of their inputs so `tools/test-duckdb-floor.R` can hold them to
# fixed release histories. `provision-duckdb-floor.R` and
# `verify-duckdb-floor.R` supply the network and the installed library.

# The version DESCRIPTION's `duckdb (>= x)` Suggests entry states. Read here
# rather than written in the workflow, so raising the floor changes what the
# job installs with no other edit.
duckdb_floor <- function(description = "DESCRIPTION") {
  suggests <- gsub("\\s+", " ", read.dcf(description, fields = "Suggests"))
  entries <- trimws(strsplit(suggests, ",", fixed = TRUE)[[1L]])
  pattern <- "^duckdb \\(>= *([0-9.-]+)\\)$"
  floor <- sub(pattern, "\\1", entries[grepl(pattern, entries)])
  if (length(floor) != 1L) {
    stop(
      description, " states no duckdb floor as `duckdb (>= x)` in Suggests.",
      call. = FALSE
    )
  }
  package_version(floor)
}

# The releases to try, lowest first: every CRAN release at or above `floor` in
# its major.minor series, each with the last daily snapshot at which it was
# CRAN's current release. That is the day before its successor appeared, or
# the day before `today` for the current release; the latest such day is the
# one most likely to carry a binary for the running R. A release superseded
# within a day has no such snapshot and is left out, so the next patch stands
# in for it. `releases` holds a `version` string and a UTC `published` time.
duckdb_floor_plan <- function(releases, floor, today = Sys.Date()) {
  releases <- releases[order(package_version(releases$version)), ,
                       drop = FALSE]
  versions <- package_version(releases$version)
  published <- as.Date(releases$published, tz = "UTC")
  last_current <- c(published[-1L], today) - 1L
  series <- function(version) unlist(version)[1:2]
  same_series <- vapply(seq_along(versions), function(i) {
    identical(series(versions[i]), series(floor))
  }, logical(1))
  keep <- versions >= floor & same_series & last_current > published
  if (!any(keep)) {
    stop(
      "No CRAN release of duckdb at or above ", format(floor),
      " in its series was current in any daily snapshot.",
      call. = FALSE
    )
  }
  data.frame(version = releases$version[keep], snapshot = last_current[keep])
}

# Whether a package's `Built` field names the running R series. P3M answers a
# request it has no binary for with the source package, silently, and `Built`
# is the field that tells the two apart.
duckdb_floor_binary <- function(built, r_series) {
  !is.na(built) && startsWith(built, paste0("R ", r_series, "."))
}

# The line the job prints for an accepted installation, or the error that stops
# it before any test runs. `installed` is NULL when duckdb is absent. The upper
# bound is what the job exists for: the 1.5 side of the version-conditional
# repairs, which DuckDB 2.0 and later never reach.
duckdb_floor_accept <- function(installed, floor) {
  if (is.null(installed)) {
    stop("DuckDB is not installed.", call. = FALSE)
  }
  if (installed < floor) {
    stop(
      "DuckDB ", format(installed), " is below DESCRIPTION's floor ",
      format(floor), ".",
      call. = FALSE
    )
  }
  if (installed >= "2.0") {
    stop("DuckDB ", format(installed), " is not below 2.0.", call. = FALSE)
  }
  paste0(
    "DuckDB ", format(installed), " installed; DESCRIPTION's floor is ",
    format(floor), "."
  )
}
