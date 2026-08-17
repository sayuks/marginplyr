# CRAN Repository Policy allows a package to write in the R session's temporary
# directory and nowhere else. DuckDB resolves a "home" for its extension cache
# and its stored secrets when a driver is created, and its default is the shared
# `~/.duckdb` whenever that directory already exists -- so every connection this
# package opens has to say otherwise. These tests read the resolved directories
# back out of the connection rather than trusting the argument, because
# `duckdb::duckdb_storage_status()` reports the ambient default rather than what
# a given driver settled on.

# Paths arrive from two sources that agree on the directory but not on its
# spelling: `tempdir()` keeps whatever separators `TMPDIR` carried, while DuckDB
# echoes back the string it was handed. Neither is normalised, and normalising
# here would not help -- the directories need not exist yet, so
# `normalizePath()` would resolve one side's symlinks and leave the other's
# alone.
canonical_path <- function(path) {
  gsub("/+", "/", gsub("\\\\", "/", path))
}

# Strictly inside, which is why the separator is appended to the directory
# rather than left to the prefix: without it `/var/folders/ab` reads as a child
# of `/var/folders/a`.
is_inside <- function(path, directory) {
  root <- sub("/?$", "/", canonical_path(directory))
  startsWith(canonical_path(path), root)
}

duckdb_storage_directories <- function(con) {
  settings <- DBI::dbGetQuery(
    con,
    "SELECT name, value FROM duckdb_settings()
     WHERE name IN ('extension_directory', 'secret_directory')"
  )
  stats::setNames(settings$value, settings$name)
}

test_that("a test DuckDB connection stores its files under tempdir() alone", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  directories <- duckdb_storage_directories(con)
  # Both locations, because DuckDB resolves them separately and either one
  # landing in the shared home is the violation.
  expect_setequal(
    names(directories),
    c("extension_directory", "secret_directory")
  )

  home <- path.expand("~")
  for (directory in directories) {
    expect_true(is_inside(directory, tempdir()))
    expect_false(is_inside(directory, home))
  }
})

# The test above proves one connection. The scan below is what extends that
# proof to every driver call in the package, because a connection opened without
# the argument is only observable when the code opening it runs -- an example
# nobody executes and a vignette chunk behind an availability guard would each
# write to `~/.duckdb` in silence.
#
# The text the scan searches for is assembled rather than written out, and the
# prose here spells the constructor `duckdb-colon-colon-duckdb`, because this
# file is one of the files scanned. A literal occurrence anywhere in it would be
# read as a call site and reported against itself.
duckdb_driver_prefix <- function() {
  paste0("duckdb", "::", "duckdb", "(")
}

# Reads the call whole, from its opening parenthesis to the parenthesis that
# closes it, so a site written across several lines is judged on all of its
# arguments rather than on the line the constructor happens to sit on.
duckdb_call_text <- function(text, open) {
  remainder <- strsplit(substring(text, open, nchar(text)), "")[[1]]
  depth <- 0L
  for (index in seq_along(remainder)) {
    if (remainder[[index]] == "(") {
      depth <- depth + 1L
    } else if (remainder[[index]] == ")") {
      depth <- depth - 1L
      if (depth == 0L) {
        return(paste(remainder[seq_len(index)], collapse = ""))
      }
    }
  }
  # An unbalanced call is not something to pass over. The file is either
  # truncated or the call is assembled by pasting strings, and neither can be
  # read for the argument, so the remaining text is returned and fails the
  # expectation rather than disappearing from it.
  paste(remainder, collapse = "")
}

duckdb_call_sites <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  prefix <- duckdb_driver_prefix()
  matches <- gregexpr(prefix, text, fixed = TRUE)[[1]]
  if (matches[[1]] == -1L) {
    return(NULL)
  }
  lapply(matches, function(match) {
    before <- substr(text, 1L, match)
    list(
      path = path,
      line = length(regmatches(
        before,
        gregexpr("\n", before, fixed = TRUE)
      )[[1]]) + 1L,
      call = duckdb_call_text(text, match + nchar(prefix) - 1L)
    )
  })
}

site_locations <- function(sites) {
  vapply(
    sites,
    function(site) sprintf("%s:%d", site$path, site$line),
    character(1)
  )
}

# Sources reachable from wherever the suite is running. `tests/testthat` is the
# working directory and so is always readable, but `R CMD check` executes from
# `<pkg>.Rcheck/tests`, where the package's R sources, vignette sources, and
# `man/` were never unpacked. Those roots are therefore scanned when a source
# checkout supplies them -- which is the `structure` job and any local run --
# and passed over otherwise, the same accommodation `rd_topics()` makes in
# `test-documentation.R`.
#
# `man/` is generated from the roxygen comments in `R/`, so scanning it largely
# repeats what scanning `R/` covers. It is scanned anyway because the generated
# file is the one that ships, and a `man/` left stale by a skipped
# `roxygen2::roxygenise()` would otherwise keep a call site the sources no
# longer have.
source_files_in <- function(root, pattern) {
  if (!dir.exists(root)) {
    return(character())
  }
  list.files(root, pattern = pattern, full.names = TRUE, recursive = TRUE)
}

suite_files <- function() {
  source_files_in(testthat::test_path("."), "[.]R$")
}

duckdb_source_files <- function() {
  c(
    suite_files(),
    unlist(
      lapply(
        c(
          testthat::test_path("..", "..", "R"),
          testthat::test_path("..", "..", "vignettes"),
          testthat::test_path("..", "..", "man")
        ),
        source_files_in,
        pattern = "[.](R|r|Rd|qmd|Rmd)$"
      ),
      use.names = FALSE
    )
  )
}

sites_in <- function(files) {
  unlist(lapply(files, duckdb_call_sites), recursive = FALSE)
}

test_that("no DuckDB driver call omits shared_home = FALSE", {
  sites <- sites_in(duckdb_source_files())
  # The scan asserts itself before it concludes anything. A scan that found
  # nothing would report every call site as compliant, and `helper-duckdb.R`
  # is in the working directory in every context this runs from -- including
  # `<pkg>.Rcheck/tests/testthat` -- so finding no site means the scan broke,
  # not that the package stopped opening connections. Skipping here instead
  # would fail a `backend` job outright: `verify-backend.R` rejects any skip
  # whose reason does not name a backend the job withheld.
  expect_gt(length(sites), 0L)

  offenders <- Filter(
    function(site) !grepl("shared_home = FALSE", site$call, fixed = TRUE),
    sites
  )
  expect_equal(site_locations(offenders), character())
})

test_that("the suite opens DuckDB connections only through the helper", {
  sites <- sites_in(suite_files())
  expect_equal(
    unique(basename(vapply(sites, function(site) site$path, character(1)))),
    "helper-duckdb.R"
  )
})
