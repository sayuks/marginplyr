# Exercise the CI setup command and a fresh, offline native JSON consumer.
# Downloads belong to setup; the consumer disables automatic extension fetching.
main <- function() {
  workspace <- tempfile("json-provision-fixture-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  runner <- file.path(workspace, "runner space's 日本語")
  dir.create(runner)
  env_file <- file.path(workspace, "github-env")
  writeLines("EXISTING_ENTRY=retained", env_file)
  rscript <- file.path(R.home("bin"), "Rscript")
  command <- ".github/scripts/provision-duckdb-json.R"
  env <- Sys.getenv()
  env[c("RUNNER_TEMP", "GITHUB_ENV")] <- c(runner, env_file)
  for (missing in c("RUNNER_TEMP", "GITHUB_ENV")) {
    incomplete <- env
    incomplete[[missing]] <- ""
    result <- processx::run(
      rscript, command, env = incomplete, error_on_status = FALSE,
      timeout = 60000, cleanup_tree = TRUE
    )
    stopifnot(result$status != 0L, grepl(missing, result$stderr, fixed = TRUE),
              identical(readLines(env_file), "EXISTING_ENTRY=retained"))
  }
  result <- processx::run(rscript, command, env = env, error_on_status = FALSE,
                          timeout = 180000, cleanup_tree = TRUE)
  cat(result$stdout, result$stderr, sep = "")
  stopifnot(result$status == 0L)
  entries <- readLines(env_file, encoding = "UTF-8", warn = FALSE)
  stopifnot(identical(entries[[1L]], "EXISTING_ENTRY=retained"))
  key <- "MARGINPLYR_DUCKDB_JSON_EXTENSION="
  entry <- entries[startsWith(entries, key)]
  stopifnot(length(entry) == 1L)
  path <- substring(entry, nchar(key) + 1L)
  root <- paste0(normalizePath(runner, winslash = "/"), "/")
  stopifnot(file.exists(path), !grepl("\\", path, fixed = TRUE),
            startsWith(path, root))
  consumer <- file.path(workspace, "consumer.R")
  writeLines(c(
    "source('tests/testthat/helper-duckdb.R')",
    "con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, config = list(",
    "  autoinstall_known_extensions = 'false',",
    "  autoload_known_extensions = 'false')))",
    "duckdb_test_json_extension(con)",
    "print(utils::packageVersion('duckdb'))",
    "print(DBI::dbGetQuery(con, 'PRAGMA version'))",
    "value <- DBI::dbGetQuery(con, paste(",
    "  \"SELECT json_extract_string(\",",
    "  \"'{\\\"answer\\\":42}', '$.answer') AS answer\"))",
    "stopifnot(identical(value$answer, '42'))",
    "DBI::dbDisconnect(con, shutdown = TRUE)",
    "cat('Fresh offline JSON consumer passed.\\n')"
  ), consumer, useBytes = TRUE)
  env[["MARGINPLYR_DUCKDB_JSON_EXTENSION"]] <- path
  result <- processx::run(rscript, consumer, env = env, error_on_status = FALSE,
                          timeout = 60000, cleanup_tree = TRUE)
  cat(result$stdout, result$stderr, sep = "")
  stopifnot(result$status == 0L)
  cat("DuckDB JSON provisioning command fixtures passed.\n")
}
main()
