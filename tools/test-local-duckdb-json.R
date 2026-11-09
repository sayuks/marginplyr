# Exercise file-scoped preparation with native JSON but no extension download.
# The caller supplies a signed binary already prepared by the provisioning fixture.
run_local_duckdb_json_fixtures <- function(extension) {
  root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  extension <- normalizePath(extension, winslash = "/", mustWork = TRUE)
  workspace <- tempfile("local-json-fixture-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  suite <- file.path(workspace, "suite")
  dir.create(suite)
  helper <- file.path(root, "tests/testthat/helper-duckdb.R")
  stopifnot(file.copy(helper, file.path(suite, "helper-duckdb.R")))
  writeLines(c(
    'fixture_env_list <- function(name) {',
    '  value <- trimws(strsplit(Sys.getenv(name, ""), ",", fixed = TRUE)[[1L]])',
    '  value[nzchar(value)]',
    '}',
    'required_suggests <- function() fixture_env_list("MARGINPLYR_REQUIRED_SUGGESTS")',
    'hidden_suggests <- function() fixture_env_list("MARGINPLYR_HIDE_SUGGESTS")',
    'suggest_available <- function(package) {',
    '  !identical(package, Sys.getenv("FIXTURE_MISSING_PACKAGE"))',
    '}',
    'suggest_absence_reason <- function(package) paste0("{", package, "} is not installed")',
    'repository_file <- function(path) {',
    '  if (identical(path, "tools/duckdb-json.R")) Sys.getenv("FIXTURE_MODULE") else NA_character_',
    '}'
  ), file.path(suite, "helper-fixture.R"))
  module <- file.path(workspace, "duckdb-json.R")
  writeLines(c(
    'writeLines("sourced", Sys.getenv("FIXTURE_SOURCE_MARKER"))',
    'duckdb_json_default_cache <- function() Sys.getenv("FIXTURE_CACHE")',
    'duckdb_json_acquire <- function(install = TRUE, path = "", cache = duckdb_json_default_cache()) {',
    '  saveRDS(list(install = install, path = path), Sys.getenv("FIXTURE_ACQUIRE_MARKER"))',
    '  if (Sys.getenv("FIXTURE_ACQUIRE_ERROR") == "true") stop("fixture acquisition failed")',
    '  list(path = Sys.getenv("FIXTURE_RETURN_PATH"), id = "fixture lease")',
    '}',
    'duckdb_json_release <- function(lease) {',
    '  stopifnot(identical(lease$id, "fixture lease"))',
    '  cat("released\\n", file = Sys.getenv("FIXTURE_RELEASE_MARKER"), append = TRUE)',
    '}'
  ), module)
  driver <- c(
    'methods::setClass("LocalJSONFailureConnection", contains = "DBIConnection", where = globalenv())',
    'methods::setMethod("dbExecute",',
    '  signature(conn = "LocalJSONFailureConnection", statement = "character"),',
    '  function(conn, statement, ...) stop("fixture JSON load failed"),',
    '  where = globalenv())',
    'con <- methods::new("LocalJSONFailureConnection")',
    'condition <- tryCatch(duckdb_test_json_extension(con), skip = identity, error = identity)'
  )
  writeLines(c(
    'prepare_duckdb_json_tests()',
    'testthat::test_that("selected JSON file observes its preparation", {',
    '  writeLines("case", Sys.getenv("FIXTURE_CASE_MARKER"))',
    '  if (Sys.getenv("FIXTURE_OPTIONAL") == "true") {',
    paste0("    ", driver),
    '    testthat::expect_true(inherits(condition, "skip"))',
    '  } else {',
    '    testthat::expect_identical(Sys.getenv("MARGINPLYR_DUCKDB_JSON_EXTENSION"),',
    '      Sys.getenv("FIXTURE_EXTENSION"))',
    '    testthat::expect_identical(Sys.getenv("MARGINPLYR_REQUIRED_DUCKDB_JSON"), "true")',
    '    testthat::expect_false(file.exists(Sys.getenv("FIXTURE_RELEASE_MARKER")))',
    '  }',
    '})'
  ), file.path(suite, "test-json.R"))
  writeLines(c(
    'testthat::test_that("unrelated selection does not prepare JSON", {',
    '  writeLines("case", Sys.getenv("FIXTURE_CASE_MARKER"))',
    '  testthat::expect_false(file.exists(Sys.getenv("FIXTURE_SOURCE_MARKER")))',
    '})'
  ), file.path(suite, "test-plain.R"))
  child <- file.path(workspace, "consumer.R")
  writeLines(c(
    'library(testthat)',
    'library(DBI)',
    'action <- Sys.getenv("FIXTURE_ACTION")',
    'if (action %in% c("helper", "load-error")) {',
    '  source(file.path(Sys.getenv("FIXTURE_SUITE"), "helper-duckdb.R"))',
    '  source(file.path(Sys.getenv("FIXTURE_SUITE"), "helper-fixture.R"))',
    '  stopifnot(!file.exists(Sys.getenv("FIXTURE_SOURCE_MARKER")))',
    '  if (action == "load-error") {',
    paste0("    ", driver),
    '    stopifnot(inherits(condition, "error"), !inherits(condition, "skip"),',
    '      identical(conditionMessage(condition), "fixture JSON load failed"))',
    '  }',
    '} else {',
    '  variables <- c("MARGINPLYR_DUCKDB_JSON_EXTENSION", "MARGINPLYR_REQUIRED_DUCKDB_JSON")',
    '  before <- Sys.getenv(variables, unset = NA_character_)',
    '  testthat::test_dir(Sys.getenv("FIXTURE_SUITE"), filter = Sys.getenv("FIXTURE_FILTER"),',
    '    reporter = "summary", stop_on_failure = TRUE)',
    '  stopifnot(identical(Sys.getenv(variables, unset = NA_character_), before))',
    '}',
    'cat("Local JSON entry fixture passed.\\n")'
  ), child)
  run <- function(name, values = character(), status = 0L, acquired = FALSE,
                  released = FALSE, sourced = FALSE, cases = TRUE,
                  diagnostic = NULL) {
    directory <- file.path(workspace, name)
    dir.create(directory)
    env <- Sys.getenv()
    defaults <- c(
      TESTTHAT_PARALLEL = "false", TESTTHAT_IS_CHECKING = "false",
      MARGINPLYR_REQUIRED_DUCKDB_JSON = "", MARGINPLYR_REQUIRED_SUGGESTS = "",
      MARGINPLYR_HIDE_SUGGESTS = "", MARGINPLYR_DUCKDB_JSON_EXTENSION = "",
      `_R_CHECK_DEPENDS_ONLY_` = "", FIXTURE_ACTION = "tests", FIXTURE_FILTER = "^json$",
      FIXTURE_OPTIONAL = "false", FIXTURE_ACQUIRE_ERROR = "false",
      FIXTURE_MISSING_PACKAGE = "", FIXTURE_RETURN_PATH = extension,
      FIXTURE_EXTENSION = extension, FIXTURE_CACHE = dirname(extension),
      FIXTURE_MODULE = module, FIXTURE_SUITE = suite,
      FIXTURE_SOURCE_MARKER = file.path(directory, "sourced"),
      FIXTURE_ACQUIRE_MARKER = file.path(directory, "acquired.rds"),
      FIXTURE_RELEASE_MARKER = file.path(directory, "released"),
      FIXTURE_CASE_MARKER = file.path(directory, "case")
    )
    env[names(defaults)] <- defaults
    env[names(values)] <- values
    result <- processx::run(file.path(R.home("bin"), "Rscript"), child,
      env = env, error_on_status = FALSE, timeout = 60000, cleanup_tree = TRUE)
    output <- paste(result$stdout, result$stderr)
    if (!identical(result$status, status)) {
      stop(name, " returned ", result$status, ":\n", output, call. = FALSE)
    }
    stopifnot(
      identical(file.exists(defaults[["FIXTURE_SOURCE_MARKER"]]), sourced),
      identical(file.exists(defaults[["FIXTURE_ACQUIRE_MARKER"]]), acquired),
      identical(file.exists(defaults[["FIXTURE_RELEASE_MARKER"]]), released),
      identical(file.exists(defaults[["FIXTURE_CASE_MARKER"]]), cases)
    )
    if (released) {
      stopifnot(identical(readLines(defaults[["FIXTURE_RELEASE_MARKER"]]), "released"))
    }
    if (acquired) {
      stopifnot(isTRUE(readRDS(defaults[["FIXTURE_ACQUIRE_MARKER"]])$install))
    }
    if (!is.null(diagnostic)) stopifnot(grepl(diagnostic, output, fixed = TRUE))
    cat("Local JSON entry fixture: ", name, " passed.\n", sep = "")
  }
  run("helper-load", c(FIXTURE_ACTION = "helper"), cases = FALSE)
  run("non-json-filter", c(FIXTURE_FILTER = "^plain$"))
  run("local-json", acquired = TRUE, released = TRUE, sourced = TRUE)
  run("missing-package", c(FIXTURE_MISSING_PACKAGE = "duckdb"), status = 1L,
      cases = FALSE, diagnostic = "DuckDB JSON preparation failed")
  run("acquisition-failure", c(FIXTURE_ACQUIRE_ERROR = "true"), status = 1L,
      acquired = TRUE, sourced = TRUE, cases = FALSE,
      diagnostic = "fixture acquisition failed")
  missing <- file.path(workspace, "missing.duckdb_extension")
  run("invalid-returned-lease", c(FIXTURE_RETURN_PATH = missing), status = 1L,
      acquired = TRUE, released = TRUE, sourced = TRUE, cases = FALSE,
      diagnostic = "DuckDB JSON preparation failed")
  run("managed-explicit", c(MARGINPLYR_DUCKDB_JSON_EXTENSION = extension),
      acquired = TRUE, released = TRUE, sourced = TRUE)
  run("external-explicit", c(MARGINPLYR_DUCKDB_JSON_EXTENSION = extension,
      FIXTURE_CACHE = file.path(workspace, "other-cache")), sourced = TRUE)
  run("checking-explicit", c(TESTTHAT_IS_CHECKING = "true",
      MARGINPLYR_REQUIRED_DUCKDB_JSON = "true",
      MARGINPLYR_DUCKDB_JSON_EXTENSION = extension))
  run("checking-invalid-explicit", c(TESTTHAT_IS_CHECKING = "true",
      MARGINPLYR_REQUIRED_DUCKDB_JSON = "true",
      MARGINPLYR_DUCKDB_JSON_EXTENSION = missing), status = 1L, cases = FALSE,
      diagnostic = "DuckDB JSON preparation failed")
  run("cran-optional", c(TESTTHAT_IS_CHECKING = "true", FIXTURE_OPTIONAL = "true"))
  run("depends-only", c(`_R_CHECK_DEPENDS_ONLY_` = "true", FIXTURE_OPTIONAL = "true"))
  run("other-backend", c(MARGINPLYR_REQUIRED_SUGGESTS = "RSQLite",
      FIXTURE_OPTIONAL = "true"))
  run("hidden-backend", c(MARGINPLYR_HIDE_SUGGESTS = "duckdb", FIXTURE_OPTIONAL = "true"))
  run("required-load-error", c(FIXTURE_ACTION = "load-error",
      MARGINPLYR_REQUIRED_DUCKDB_JSON = "true"), cases = FALSE)

  files <- list.files(file.path(root, "tests/testthat"), pattern = "^test-.*[.]R$",
                      full.names = TRUE)
  json <- files[vapply(files, function(path) {
    any(grepl("duckdb_test_json_extension(", readLines(path), fixed = TRUE))
  }, logical(1))]
  stopifnot(length(json) > 0L, all(vapply(json, function(path) {
    identical(parse(path, keep.source = FALSE)[[1L]], quote(prepare_duckdb_json_tests()))
  }, logical(1))))
  cat("Local DuckDB JSON preparation fixtures passed.\n")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Supply one prepared JSON extension path.", call. = FALSE)
  run_local_duckdb_json_fixtures(args[[1L]])
}
