# CI prepares JSON in job-owned storage; package tests only load its explicit path.
provision <- function() {
  runner <- Sys.getenv("RUNNER_TEMP")
  env_file <- Sys.getenv("GITHUB_ENV")
  if (!nzchar(runner) || !dir.exists(runner)) {
    stop("RUNNER_TEMP must name an existing directory.")
  }
  if (!nzchar(env_file) || !dir.exists(dirname(env_file))) {
    stop("GITHUB_ENV must name a file in an existing directory.")
  }
  directory <- tempfile("duckdb-json-", tmpdir = runner)
  if (!dir.create(directory)) stop("Could not create isolated extension storage.")
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  source("tools/duckdb-json.R", local = TRUE)
  con <- duckdb_json_connection()
  native <- tryCatch(duckdb_json_identity(con),
    finally = DBI::dbDisconnect(con, shutdown = TRUE))
  paths <- duckdb_json_cache_paths(native$version, native$platform, directory)
  # The unique job-owned directory needs no shared-cache locking dependency.
  duckdb_json_publish(paths, native)
  path <- duckdb_json_validate(paths$binary)
  if (!startsWith(path, paste0(directory, "/"))) {
    stop("DuckDB JSON extension is outside the isolated extension directory.")
  }
  # Environment files require UTF-8; the absolute path survives child cwd changes.
  env_output <- file(env_file, open = "at", encoding = "UTF-8")
  on.exit(close(env_output), add = TRUE)
  writeLines(paste0("MARGINPLYR_DUCKDB_JSON_EXTENSION=", path), env_output)
  message("Provisioned DuckDB ", as.character(utils::packageVersion("duckdb")),
          " JSON extension: ", path)
}
provision()
