# Install the running DuckDB engine's JSON extension into this job's temporary
# storage. Package tests only load the explicit path exported to later steps.
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
  if (!dir.create(directory)) {
    stop("Could not create isolated extension storage.")
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, config = list(
    extension_directory = directory,
    autoinstall_known_extensions = "false",
    autoload_known_extensions = "false"
  )))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # HTTPS INSTALL would first need httpfs on engines without built-in HTTPS.
  # R downloads only JSON; DuckDB still verifies and installs the local binary.
  version <- DBI::dbGetQuery(con, "PRAGMA version")$library_version
  platform <- DBI::dbGetQuery(con, "PRAGMA platform")$platform
  url <- paste0("https://extensions.duckdb.org/", version, "/", platform,
                "/json.duckdb_extension.gz")
  archive <- file.path(directory, "json.duckdb_extension.gz")
  utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  binary <- file.path(directory, "json.duckdb_extension")
  compressed <- readBin(archive, "raw", n = file.info(archive)$size)
  writeBin(memDecompress(compressed, type = "gzip"), binary)
  DBI::dbExecute(con, paste("INSTALL", DBI::dbQuoteString(con, binary)))
  unlink(c(archive, binary))
  DBI::dbExecute(con, "LOAD json")
  extension <- DBI::dbGetQuery(con, paste(
    "SELECT installed, loaded, install_path FROM duckdb_extensions()",
    "WHERE extension_name = 'json'"
  ))
  if (nrow(extension) != 1L || !isTRUE(extension$installed) ||
        !isTRUE(extension$loaded) || !file.exists(extension$install_path)) {
    stop("DuckDB JSON extension was not installed and loaded from a file.")
  }
  path <- normalizePath(extension$install_path, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(directory, "/")) || grepl("[\r\n]", path)) {
    stop("DuckDB JSON extension is outside the isolated extension directory.")
  }
  # GitHub environment files require UTF-8, and forward slashes preserve Windows
  # paths when later R processes read the value as an ordinary string.
  env_output <- file(env_file, open = "at", encoding = "UTF-8")
  on.exit(close(env_output), add = TRUE)
  writeLines(paste0("MARGINPLYR_DUCKDB_JSON_EXTENSION=", path), env_output)
  message("Provisioned DuckDB ", as.character(utils::packageVersion("duckdb")),
          " JSON extension: ", path)
}
provision()
