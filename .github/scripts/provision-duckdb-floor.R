# Installs the lowest DuckDB at or above DESCRIPTION's floor that a dated Posit
# Package Manager snapshot carries as a binary for the running R, and prints
# which it installed. `duckdb-floor.R` states how the candidates and their
# snapshots are chosen.
#
# A candidate is downloaded and inspected before anything is installed: a
# source build of duckdb takes most of an hour, so a snapshot without a binary
# moves on to the next candidate rather than compiling.
source(".github/scripts/duckdb-floor.R")

# CRAN's duckdb releases with their UTC publication times: the archive's file
# times, and the current release's `Published` field.
cran_duckdb_releases <- function(cran = "https://cran.r-project.org") {
  index <- url(paste0(cran, "/src/contrib/Meta/archive.rds"))
  archive <- readRDS(index)$duckdb
  current <- tools::CRAN_package_db()
  current <- current[current$Package == "duckdb", , drop = FALSE]
  archived <- sub("^duckdb/duckdb_(.*)[.]tar[.]gz$", "\\1", rownames(archive))
  data.frame(
    version = c(archived, current$Version),
    published = c(
      as.POSIXct(archive$mtime, tz = "UTC"),
      as.POSIXct(current$Published, tz = "UTC")
    )
  )
}

# The DESCRIPTION inside a downloaded package, extracted alone.
package_description <- function(tarball, directory) {
  exdir <- tempfile("description-", tmpdir = directory)
  utils::untar(tarball, files = "duckdb/DESCRIPTION", exdir = exdir)
  file.path(exdir, "duckdb", "DESCRIPTION")
}

provision <- function() {
  # setup-r selects the P3M repository for the runner's distribution.
  latest <- Sys.getenv("RSPM")
  pattern <- paste0(
    "^(https://packagemanager[.]posit[.]co/cran/__linux__/[^/]+/)latest/?$"
  )
  if (!grepl(pattern, latest)) {
    stop(
      "setup-r did not configure a P3M `latest` Linux repository: ", latest,
      call. = FALSE
    )
  }
  base <- sub(pattern, "\\1", latest)
  minor <- sub("[.].*$", "", R.version$minor)
  r_series <- paste(R.version$major, minor, sep = ".")
  floor <- duckdb_floor()
  plan <- duckdb_floor_plan(cran_duckdb_releases(), floor, Sys.Date())

  downloads <- tempfile("duckdb-floor-")
  dir.create(downloads)
  for (i in seq_len(nrow(plan))) {
    version <- plan$version[i]
    repo <- paste0(base, format(plan$snapshot[i]))
    served <- utils::available.packages(repos = repo)
    if (!identical(unname(served["duckdb", "Version"]), version)) {
      message("Snapshot ", repo, " does not serve duckdb ", version, ".")
      next
    }
    # P3M chooses binary or source from the User-Agent, which R's libcurl
    # method does not send from `HTTPUserAgent` unless asked.
    fetched <- utils::download.packages(
      "duckdb", downloads,
      repos = repo, available = served, type = "source", quiet = TRUE,
      headers = c("User-Agent" = getOption("HTTPUserAgent"))
    )
    if (nrow(fetched) != 1L) {
      stop(
        "Could not download duckdb ", version, " from ", repo, ".",
        call. = FALSE
      )
    }
    tarball <- fetched[1L, 2L]
    built <- read.dcf(package_description(tarball, downloads), "Built")[1L, 1L]
    if (!duckdb_floor_binary(built, r_series)) {
      message(
        "Snapshot ", repo, " serves duckdb ", version,
        " without a binary for R ", r_series, "."
      )
      next
    }
    utils::install.packages(tarball, repos = NULL, type = "source")
    message(
      "Installed duckdb ", version, " (DESCRIPTION's floor is ",
      format(floor), ") from ", repo, ": Built ", built
    )
    return(invisible(version))
  }
  stop(
    "No snapshot carries a binary of duckdb ",
    paste(plan$version, collapse = ", "), " for R ", r_series, ".",
    call. = FALSE
  )
}

provision()
