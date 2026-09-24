# Shared, strict line-coverage check for the review-ready archive and CI.

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

# Reads the test suite's one optional-Suggest table and checks every declared
# version using the same guard the tests use before asking covr to run them.
coverage_required_suggests <- function(root = ".") {
  suite <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests/testthat/helper-optional-backends.R"),
    envir = suite)
  guard <- new.env(parent = globalenv())
  sys.source(file.path(root, "inst/suggests/guard.R"), envir = guard)
  packages <- names(suite$optional_suggest_spec())
  if (length(packages) == 0L) {
    stop("No optional Suggests are registered for coverage.", call. = FALSE)
  }
  suggests <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Suggests")[1L, 1L]
  status <- lapply(packages, guard$marginplyr_suggest_status,
    suggests = suggests)
  missing <- vapply(status, function(item) !isTRUE(item$available), logical(1))
  if (any(missing)) {
    stop("Coverage requires usable optional Suggests: ",
      paste(vapply(status[missing], `[[`, character(1), "reason"),
        collapse = "; "), call. = FALSE)
  }
  packages
}

# Runs tests against one instrumented package and returns a verdict without
# rounding. The Cobertura file, when requested, is written even on a miss so
# Codecov can show the uncovered lines for a failed CI run.
coverage_check <- function(cobertura = NULL, root = ".") {
  coverage_verify_policy(root)
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Coverage requires covr.", call. = FALSE)
  }
  coverage_verify_runtime()
  packages <- coverage_required_suggests(root)
  previous <- Sys.getenv("MARGINPLYR_REQUIRED_SUGGESTS", unset = NA_character_)
  on.exit(if (is.na(previous)) {
    Sys.unsetenv("MARGINPLYR_REQUIRED_SUGGESTS")
  } else {
    Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = previous)
  }, add = TRUE)
  Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = paste(packages, collapse = ","))

  temporary <- Sys.getenv("RUNNER_TEMP", unset = tempdir())
  install_path <- file.path(normalizePath(temporary, winslash = "/"), "package")
  cov <- covr::package_coverage(path = root, type = "tests", quiet = FALSE,
    clean = FALSE, install_path = install_path)
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
