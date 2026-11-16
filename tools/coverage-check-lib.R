# Strict line-coverage check for CI and explicit local measurements.

source("tools/required-test-suggests.R")

# Refuses every repository-level route that could shrink covr's denominator.
coverage_verify_policy <- function(root = ".") {
  ignored <- file.path(root, c(".covrignore", "inst/.covrignore"))
  present <- ignored[file.exists(ignored)]
  if (length(present) > 0L) {
    stop("Coverage exclusions are forbidden: ", paste(present, collapse = ", "),
      call. = FALSE)
  }

  sources <- list.files(file.path(root, "R"), pattern = "[.][Rr]$",
    full.names = TRUE)
  if (length(sources) == 0L) {
    stop("Coverage found no R source files.", call. = FALSE)
  }
  excluded <- unlist(lapply(sources, function(path) {
    parsed <- utils::getParseData(parse(path, keep.source = TRUE))
    comments <- parsed[parsed$token == "COMMENT", , drop = FALSE]
    matches <- grepl("#[[:space:]]*nocov\\b", comments$text, perl = TRUE,
      ignore.case = TRUE)
    if (!any(matches)) {
      return(character())
    }
    paste0(path, ":", comments$line1[matches])
  }), use.names = FALSE)
  if (length(excluded) > 0L) {
    stop("Coverage exclusions are forbidden: ", paste(excluded, collapse = ", "),
      call. = FALSE)
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Coverage policy verification requires yaml.", call. = FALSE)
  }
  config <- yaml::read_yaml(file.path(root, "codecov.yml"))
  if ("ignore" %in% names(config)) {
    stop("Codecov ignore rules are forbidden.", call. = FALSE)
  }
  invisible(TRUE)
}

# Checks that a local R profile or environment cannot redirect covr exclusions.
coverage_verify_runtime <- function() {
  if (nzchar(Sys.getenv("COVR_COVRIGNORE", ""))) {
    stop("COVR_COVRIGNORE must be unset for coverage.", call. = FALSE)
  }
  expected <- list(
    covr.covrignore = ".covrignore",
    covr.exclude_pattern = "#[[:space:]]*nocov",
    covr.exclude_start = "#[[:space:]]*nocov[[:space:]]*start",
    covr.exclude_end = "#[[:space:]]*nocov[[:space:]]*end"
  )
  for (name in names(expected)) {
    if (!identical(unclass(getOption(name)), expected[[name]])) {
      stop("Nonstandard covr exclusion option: ", name, call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Runs tests against one instrumented package and returns a verdict without
# rounding. The Cobertura file, when requested, is written even on a miss so
# Codecov can show the uncovered lines for a failed CI run.
coverage_check <- function(cobertura = NULL, root = ".") {
  coverage_verdict(coverage_measure(root), cobertura, root)
}

# Measures the package at `path` under the test environment the verdict
# assumes; `root` is the checkout whose policy and Suggests are checked, and
# differs from `path` only for a shard's pruned copy of it.
coverage_measure <- function(root = ".", path = root) {
  coverage_verify_policy(root)
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Coverage requires covr.", call. = FALSE)
  }
  coverage_verify_runtime()
  packages <- required_test_suggests(root)
  previous <- Sys.getenv("MARGINPLYR_REQUIRED_SUGGESTS", unset = NA_character_)
  on.exit(if (is.na(previous)) {
    Sys.unsetenv("MARGINPLYR_REQUIRED_SUGGESTS")
  } else {
    Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = previous)
  }, add = TRUE)
  Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = paste(packages, collapse = ","))

  previous_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  on.exit(if (is.na(previous_not_cran)) {
    Sys.unsetenv("NOT_CRAN")
  } else {
    Sys.setenv(NOT_CRAN = previous_not_cran)
  }, add = TRUE)
  # test_check() skips snapshots in a noninteractive process unless this is set.
  Sys.setenv(NOT_CRAN = "true")

  temporary <- Sys.getenv("RUNNER_TEMP", unset = tempdir())
  install_path <- file.path(normalizePath(temporary, winslash = "/"), "package")
  covr::package_coverage(path = path, type = "tests", quiet = FALSE,
    clean = FALSE, install_path = install_path)
}

# Applies the complete-coverage verdict to a measured or merged result.
coverage_verdict <- function(cov, cobertura = NULL, root = ".") {
  lines <- covr::tally_coverage(cov, by = "line")
  if (nrow(lines) == 0L || anyNA(lines$value)) {
    stop("Coverage has no valid measured source lines.", call. = FALSE)
  }
  sources <- list.files(file.path(root, "R"), pattern = "[.][Rr]$",
    full.names = TRUE)
  executable <- sources[vapply(sources, function(path) {
    expressions <- as.list(parse(path, keep.source = TRUE))
    any(vapply(expressions, is.call, logical(1)))
  }, logical(1))]
  missing_files <- setdiff(file.path("R", basename(executable)),
    unique(lines$filename))
  if (length(missing_files) > 0L) {
    stop("Coverage omitted executable source files: ",
      paste(missing_files, collapse = ", "), call. = FALSE)
  }
  missed <- lines[lines$value == 0L, c("filename", "line"), drop = FALSE]
  cat("covr ", as.character(utils::packageVersion("covr")), ": ",
    nrow(lines) - nrow(missed), "/", nrow(lines), " lines (",
    covr::percent_coverage(cov), "%).\n", sep = "")
  if (!is.null(cobertura)) {
    covr::to_cobertura(cov, filename = cobertura)
  }
  if (nrow(missed) > 0L) {
    cat("Uncovered source lines:\n")
    write.table(missed, row.names = FALSE, col.names = FALSE, quote = FALSE,
      sep = ":")
    return(1L)
  }
  cat("Strict line coverage passed.\n")
  0L
}

# The test files shard `index` of `count` runs: every count-th file of the
# sorted inventory, so the shards partition it. Helpers and setup files are
# not test files and stay in every shard.
coverage_shard_files <- function(index, count, root = ".") {
  files <- sort(list.files(file.path(root, "tests/testthat"),
    pattern = "^test.*[.][Rr]$"), method = "radix")
  valid <- function(value) {
    length(value) == 1L && !is.na(value) && value >= 1L
  }
  if (!valid(count) || !valid(index) || index > count) {
    stop("Coverage shard must be <index>/<count> with 1 <= index <= count.",
      call. = FALSE)
  }
  if (count > length(files)) {
    stop("Coverage shard count ", count, " exceeds the ", length(files),
      " test files.", call. = FALSE)
  }
  files[(seq_along(files) - 1L) %% count + 1L == index]
}

# Measures one shard and saves the raw covr result for coverage_merge(). The
# shard runs from a copy of `root` whose tests/testthat.R passes test_check()
# a filter naming the shard's files. Every test file stays in the copy, since
# suite scans read test files they do not run.
coverage_check_shard <- function(index, count, output, root = ".") {
  files <- coverage_shard_files(index, count, root)
  copy <- file.path(tempfile("coverage-shard-"), basename(normalizePath(root)))
  dir.create(copy, recursive = TRUE)
  on.exit(unlink(dirname(copy), recursive = TRUE), add = TRUE)
  entries <- setdiff(list.files(root, all.files = TRUE, no.. = TRUE), ".git")
  if (!all(file.copy(file.path(root, entries), copy, recursive = TRUE))) {
    stop("Coverage shard could not copy ", root, call. = FALSE)
  }
  script <- file.path(copy, "tests", "testthat.R")
  expressions <- as.list(parse(script, keep.source = FALSE))
  check <- which(vapply(expressions, function(expression) {
    is.call(expression) && identical(expression[[1L]], quote(test_check))
  }, logical(1)))
  if (length(check) != 1L) {
    stop("Coverage shard needs one top-level test_check() call in ",
      "tests/testthat.R.", call. = FALSE)
  }
  # testthat matches the filter against each file name without its `test-`
  # prefix and extension.
  names <- sub("^test[-_]?", "", sub("[.][Rr]$", "", files))
  if (!all(grepl("^[A-Za-z0-9._-]+$", names))) {
    stop("Coverage shard cannot filter test file names outside [A-Za-z0-9._-].",
      call. = FALSE)
  }
  expressions[[check]]$filter <- paste0("^(",
    paste(gsub(".", "[.]", names, fixed = TRUE), collapse = "|"), ")$")
  writeLines(unlist(lapply(expressions, deparse)), script)
  cov <- coverage_measure(root, copy)
  attr(cov, "marginplyr_shard") <- list(index = index, count = count,
    files = files)
  saveRDS(cov, output)
  cat("Coverage shard ", index, "/", count, " measured ", length(files),
    " test files.\n", sep = "")
  0L
}

# Sums the per-expression hits of every shard result under `directory` into
# one covr result for coverage_verdict(). It refuses unless it holds exactly
# the shards that partition this tree's test files, each instrumenting the
# same expressions.
coverage_merge <- function(directory, root = ".") {
  paths <- list.files(directory, pattern = "[.]rds$", recursive = TRUE,
    full.names = TRUE)
  if (length(paths) == 0L) {
    stop("Coverage merge found no shard results in ", directory, call. = FALSE)
  }
  legs <- lapply(paths, readRDS)
  shards <- lapply(legs, attr, "marginplyr_shard")
  if (any(vapply(shards, is.null, logical(1)))) {
    stop("Coverage merge found a result that is not a shard.", call. = FALSE)
  }
  count <- shards[[1L]]$count
  indices <- vapply(shards, `[[`, numeric(1), "index")
  counts <- vapply(shards, `[[`, numeric(1), "count")
  if (any(counts != count) || !identical(sort(as.integer(indices)), seq_len(count))) {
    stop("Coverage merge requires exactly ", count, " shard results, one per ",
      "index; found indices ", paste(sort(indices), collapse = ", "), " of ",
      paste(unique(counts), collapse = ", "), ".", call. = FALSE)
  }
  legs <- legs[order(indices)]
  shards <- shards[order(indices)]
  for (index in seq_len(count)) {
    if (!identical(shards[[index]]$files,
                   coverage_shard_files(index, count, root))) {
      stop("Coverage shard ", index, "/", count, " ran different test files ",
        "than this tree assigns it.", call. = FALSE)
    }
  }
  # Each shard instruments its own copy, so expressions are matched by their
  # path below that copy and their source position.
  relative_keys <- function(cov) {
    prefix <- paste0(attr(cov, "root"), "/")
    vapply(cov, function(entry) {
      file <- utils::getSrcFilename(entry$srcref, full.names = TRUE)
      paste(c(sub(prefix, "", file, fixed = TRUE), entry$srcref), collapse = ":")
    }, character(1), USE.NAMES = FALSE)
  }
  merged <- legs[[1L]]
  keys <- relative_keys(merged)
  for (leg in legs[-1L]) {
    leg_keys <- relative_keys(leg)
    if (!setequal(keys, leg_keys) || anyDuplicated(leg_keys)) {
      stop("Coverage shards instrumented different source expressions.",
        call. = FALSE)
    }
    values <- vapply(leg, `[[`, numeric(1), "value")[match(keys, leg_keys)]
    for (position in seq_along(merged)) {
      merged[[position]]$value <- merged[[position]]$value + values[[position]]
    }
  }
  attr(merged, "marginplyr_shard") <- NULL
  # Cobertura names the checkout as its source, as an unsharded run does.
  package <- attr(merged, "package")
  package$path <- normalizePath(root)
  attr(merged, "package") <- package
  merged
}
