# Disposable local PostgreSQL fixture for dated research, never caller data.
with_postgres <- function(code) {
  workspace <- tempfile("marginplyr-pg-", tmpdir = "/tmp")
  dir.create(workspace)
  cluster <- file.path(workspace, "data")
  socket <- file.path(workspace, "socket")
  dir.create(socket)
  started <- FALSE
  con <- NULL
  on.exit({
    if (!is.null(con)) DBI::dbDisconnect(con)
    if (started) processx::run("pg_ctl", c("-D", cluster, "-m", "immediate", "-w", "stop"))
    unlink(workspace, recursive = TRUE)
  }, add = TRUE)
  processx::run("initdb", c("-D", cluster, "-A", "trust", "--no-locale", "-E", "UTF8"))
  processx::run("pg_ctl", c("-D", cluster, "-l", file.path(workspace, "server.log"),
    "-o", paste("-F -k", socket, "-c listen_addresses=''"), "-w", "start"))
  started <- TRUE
  con <- DBI::dbConnect(RPostgres::Postgres(), host = socket, dbname = "postgres")
  code(con)
}
