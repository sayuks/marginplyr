# Prepares signed JSON binaries outside test cases and leases their shared cache.
duckdb_json_repository <- function() "https://extensions.duckdb.org"

duckdb_json_default_cache <- function() {
  file.path(tools::R_user_dir("marginplyr", "cache"), "duckdb-json")
}

# Resolve paths before passing them across R processes with different workspaces.
duckdb_json_absolute <- function(path, must_work = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || grepl("[\r\n]", path)) {
    stop("DuckDB JSON paths must be nonempty single-line strings.", call. = FALSE)
  }
  path <- path.expand(path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    path <- file.path(getwd(), path)
  }
  if (must_work || file.exists(path) || dir.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = must_work))
  }
  # Resolve existing parents too, so a newly created /tmp path keeps one lock key.
  suffix <- character()
  parent <- path
  while (!dir.exists(parent) && !file.exists(parent)) {
    next_parent <- dirname(parent)
    if (identical(parent, next_parent)) break
    suffix <- c(basename(parent), suffix)
    parent <- next_parent
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  do.call(file.path, as.list(c(parent, suffix)))
}

# A native version/platform key cannot escape the managed cache hierarchy.
duckdb_json_component <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", value)) {
    stop("Invalid DuckDB JSON ", label, ".", call. = FALSE)
  }
  value
}

duckdb_json_cache_paths <- function(version, platform,
                                  cache = duckdb_json_default_cache()) {
  version <- duckdb_json_component(version, "version")
  platform <- duckdb_json_component(platform, "platform")
  cache <- duckdb_json_absolute(cache)
  directory <- file.path(cache, version, platform)
  paths <- list(directory = directory,
       binary = file.path(directory, "json.duckdb_extension"),
       lock = file.path(cache, ".locks", version, paste0(platform, ".lock")))
  # Managed entry components are not aliases for another entry or outside file.
  for (path in c(directory, paths$binary, dirname(paths$lock), paths$lock)) {
    link <- Sys.readlink(path)
    if ((!is.na(link) && nzchar(link)) ||
        !identical(path, duckdb_json_absolute(path))) {
      stop("DuckDB JSON managed cache contains a path alias: ", path,
           call. = FALSE)
    }
  }
  paths
}

# Connections never acquire extensions implicitly or use DuckDB's shared home.
duckdb_json_connection <- function(directory = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("DuckDB JSON preparation requires the DBI and duckdb R packages.",
         call. = FALSE)
  }
  config <- list(
    autoinstall_known_extensions = "false",
    autoload_known_extensions = "false"
  )
  if (!is.null(directory)) {
    config$extension_directory <- duckdb_json_absolute(directory, must_work = TRUE)
  }
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, config = config))
}

duckdb_json_identity <- function(con) {
  list(
    version = duckdb_json_component(
      DBI::dbGetQuery(con, "PRAGMA version")$library_version, "version"),
    platform = duckdb_json_component(
      DBI::dbGetQuery(con, "PRAGMA platform")$platform, "platform")
  )
}

# One OS lock backs all same-process read leases for a cache entry.
duckdb_json_locks <- new.env(parent = emptyenv())

duckdb_json_lock <- function(path, exclusive, timeout = Inf, create = TRUE,
                             holder) {
  key <- duckdb_json_absolute(path)
  token <- new.env(parent = emptyenv())
  token$key <- key
  token$released <- FALSE
  token$registered <- FALSE
  provisional <- NULL
  handed <- FALSE
  on.exit({
    if (!handed) {
      if (token$registered) duckdb_json_unlock(token)
      else if (!is.null(provisional)) filelock::unlock(provisional)
    }
  }, add = TRUE)
  if (exists(key, envir = duckdb_json_locks, inherits = FALSE)) {
    held <- get(key, envir = duckdb_json_locks, inherits = FALSE)
    if (exclusive || held$exclusive) {
      stop("DuckDB JSON cache is in use by this R process.", call. = FALSE)
    }
    suspendInterrupts({
      held$count <- held$count + 1L
      token$registered <- TRUE
      holder$lock <- token
      handed <- TRUE
    })
  } else {
    if (!requireNamespace("filelock", quietly = TRUE)) {
      stop("DuckDB JSON preparation requires the filelock development tool.",
           call. = FALSE)
    }
    if (create) dir.create(dirname(key), recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(key)) || (!create && !file.exists(key))) {
      stop("DuckDB JSON cache is not prepared. Run Rscript tools/setup-duckdb-json.R.",
           call. = FALSE)
    }
    held <- new.env(parent = emptyenv())
    held$exclusive <- exclusive
    held$count <- 1L
    suspendInterrupts({
      # Waiting is interruptible; ownership publication is protected afterwards.
      provisional <- allowInterrupts(
        filelock::lock(key, exclusive = exclusive, timeout = timeout)
      )
      if (is.null(provisional)) {
        stop("DuckDB JSON cache is in use; it cannot be removed.", call. = FALSE)
      }
      held$lock <- provisional
      assign(key, held, envir = duckdb_json_locks)
      token$registered <- TRUE
      holder$lock <- token
      handed <- TRUE
    })
  }
  token
}

duckdb_json_unlock <- function(token) {
  if (is.null(token) || isTRUE(token$released)) return(invisible(NULL))
  suspendInterrupts({
    token$released <- TRUE
    held <- get(token$key, envir = duckdb_json_locks, inherits = FALSE)
    held$count <- held$count - 1L
    if (held$count == 0L) {
      filelock::unlock(held$lock)
      rm(list = token$key, envir = duckdb_json_locks)
    }
  })
  invisible(NULL)
}

# Releasing is explicit; finalization only recovers an abandoned caller lease.
duckdb_json_release <- function(lease) {
  if (!is.null(lease)) duckdb_json_unlock(lease$lock)
  invisible(NULL)
}

duckdb_json_lease <- function(path, identity, lock = NULL) {
  if (!is.null(lock)) reg.finalizer(lock, duckdb_json_unlock, onexit = TRUE)
  c(list(path = path, lock = lock), identity)
}

# LOAD keeps DuckDB's signature/compatibility checks; the control proves JSON use.
duckdb_json_validate <- function(path) {
  path <- duckdb_json_absolute(path, must_work = TRUE)
  if (dir.exists(path)) stop("DuckDB JSON extension must be a file.", call. = FALSE)
  con <- duckdb_json_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste("LOAD", DBI::dbQuoteString(con, path)))
  answer <- DBI::dbGetQuery(con,
    "SELECT json_extract_string('{\"answer\":42}', '$.answer') AS answer")
  if (!identical(answer$answer, "42")) {
    stop("DuckDB JSON control query did not return 42.", call. = FALSE)
  }
  path
}

# The setup boundary is the only network operation; fixtures replace this seam.
duckdb_json_download <- function(url, destination) {
  utils::download.file(url, destination, mode = "wb", method = "libcurl", quiet = TRUE)
}

duckdb_json_rename <- function(from, to) file.rename(from, to)

# The caller owns either the entry's exclusive lock or a private CI directory.
duckdb_json_stages <- function(paths, remove = FALSE) {
  parent <- dirname(paths$directory)
  prefix <- paste0(".", basename(paths$directory), "-stage-")
  stages <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  stages <- stages[startsWith(basename(stages), prefix)]
  for (stage in stages) {
    if (!identical(stage, duckdb_json_absolute(stage, TRUE))) {
      stop("DuckDB JSON staging contains a path alias: ", stage, call. = FALSE)
    }
  }
  if (remove && length(stages) && unlink(stages, recursive = TRUE) != 0L) {
    stop("Cannot remove interrupted DuckDB JSON staging files.", call. = FALSE)
  }
  stages
}

# Restore a predecessor left by a process that ended during publication.
duckdb_json_recover <- function(paths) {
  stages <- duckdb_json_stages(paths)
  backups <- file.path(stages, "previous.duckdb_extension")
  backups <- backups[file.exists(backups)]
  if (!file.exists(paths$binary) && length(backups)) {
    if (length(backups) != 1L) {
      stop("DuckDB JSON publication has ambiguous retained predecessors.",
           call. = FALSE)
    }
    suspendInterrupts({
      if (!duckdb_json_rename(backups[[1L]], paths$binary)) {
        stop("Cannot restore interrupted DuckDB JSON publication.", call. = FALSE)
      }
      duckdb_json_stages(paths, remove = TRUE)
    })
  }
  invisible(NULL)
}

# Rollback runs under the same exclusive ownership as the interrupted publish.
duckdb_json_restore <- function(paths, previous, stage) {
  displaced <- file.path(stage, "uncommitted.duckdb_extension")
  had_candidate <- file.exists(paths$binary)
  if (had_candidate && !duckdb_json_rename(paths$binary, displaced)) return(FALSE)
  if (duckdb_json_rename(previous, paths$binary)) return(TRUE)
  if (had_candidate) duckdb_json_rename(displaced, paths$binary)
  FALSE
}

# A complete signed installation is staged on the destination filesystem.
duckdb_json_publish <- function(paths, identity) {
  dir.create(dirname(paths$directory), recursive = TRUE, showWarnings = FALSE)
  duckdb_json_stages(paths, remove = TRUE)
  stage <- tempfile(paste0(".", identity$platform, "-stage-"),
                    tmpdir = dirname(paths$directory))
  if (!dir.create(stage)) stop("Cannot create DuckDB JSON staging directory.")
  previous <- file.path(stage, "previous.duckdb_extension")
  published <- FALSE
  on.exit(suspendInterrupts({
    if (!published && file.exists(previous) &&
        !duckdb_json_restore(paths, previous, stage)) {
      stop("Cannot restore DuckDB JSON predecessor retained at ", previous,
           call. = FALSE)
    }
    if (unlink(stage, recursive = TRUE) != 0L) {
      stop("Cannot remove DuckDB JSON staging directory: ", stage, call. = FALSE)
    }
  }), add = TRUE)
  archive <- file.path(stage, "json.duckdb_extension.gz")
  binary <- file.path(stage, "json.duckdb_extension")
  url <- paste0(duckdb_json_repository(), "/", identity$version, "/",
                identity$platform, "/json.duckdb_extension.gz")
  result <- duckdb_json_download(url, archive)
  if (is.numeric(result) && length(result) == 1L && result != 0L) {
    stop("DuckDB JSON download failed.", call. = FALSE)
  }
  compressed <- readBin(archive, "raw", n = file.info(archive)$size)
  writeBin(memDecompress(compressed, type = "gzip"), binary)
  installed <- local({
    con <- duckdb_json_connection(stage)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbExecute(con, paste("INSTALL", DBI::dbQuoteString(con, binary)))
    installed <- DBI::dbGetQuery(con, paste(
      "SELECT install_path FROM duckdb_extensions() WHERE extension_name = 'json'"
    ))$install_path
    if (length(installed) != 1L || !file.exists(installed)) {
      stop("DuckDB JSON installation produced no binary.", call. = FALSE)
    }
    installed <- duckdb_json_absolute(installed, must_work = TRUE)
    if (!startsWith(installed, paste0(duckdb_json_absolute(stage, TRUE), "/"))) {
      stop("DuckDB JSON installation escaped its staging directory.", call. = FALSE)
    }
    installed
  })
  duckdb_json_validate(installed)
  candidate <- file.path(stage, "validated.duckdb_extension")
  if (!file.copy(installed, candidate)) stop("Cannot stage validated DuckDB JSON.")
  dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
  suspendInterrupts({
    # Moving a predecessor first also supports Windows rename semantics.
    if (file.exists(paths$binary) && !duckdb_json_rename(paths$binary, previous)) {
      stop("Cannot replace the managed DuckDB JSON binary.", call. = FALSE)
    }
    if (!duckdb_json_rename(candidate, paths$binary)) {
      stop("Cannot publish the validated DuckDB JSON binary.", call. = FALSE)
    }
    published <- TRUE
  })
  invisible(paths$binary)
}

# Retains a shared lease; only automatic preparation may repair managed entries.
duckdb_json_acquire <- function(install = TRUE,
                               path = Sys.getenv("MARGINPLYR_DUCKDB_JSON_EXTENSION", ""),
                               cache = duckdb_json_default_cache()) {
  if (!is.logical(install) || length(install) != 1L || is.na(install)) {
    stop("install must be one nonmissing logical value.", call. = FALSE)
  }
  con <- duckdb_json_connection()
  native <- tryCatch(duckdb_json_identity(con),
    finally = DBI::dbDisconnect(con, shutdown = TRUE))
  paths <- duckdb_json_cache_paths(native$version, native$platform, cache)
  holder <- new.env(parent = emptyenv())
  holder$lock <- NULL
  success <- FALSE
  on.exit(if (!success) duckdb_json_unlock(holder$lock), add = TRUE)
  explicit <- is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("DuckDB JSON explicit path must be one string.", call. = FALSE)
  }
  if (explicit) {
    path <- duckdb_json_absolute(path, must_work = TRUE)
    managed <- identical(path, duckdb_json_absolute(paths$binary))
    lock <- if (managed) {
      duckdb_json_lock(paths$lock, FALSE, create = FALSE, holder = holder)
    } else NULL
    duckdb_json_validate(path)
    lease <- duckdb_json_lease(path, native, lock)
    success <- TRUE
    return(lease)
  }
  if (!install && !file.exists(paths$binary)) {
    stop("DuckDB JSON cache is not prepared. Run Rscript tools/setup-duckdb-json.R.",
         call. = FALSE)
  }
  lock <- duckdb_json_lock(paths$lock, FALSE, create = install, holder = holder)
  problem <- tryCatch({ duckdb_json_validate(paths$binary); NULL }, error = identity)
  if (!is.null(problem)) {
    duckdb_json_unlock(lock)
    lock <- NULL
    if (!install) {
      stop("DuckDB JSON cache is unusable: ", conditionMessage(problem),
           ". Run Rscript tools/setup-duckdb-json.R.", call. = FALSE)
    }
    lock <- duckdb_json_lock(paths$lock, TRUE, holder = holder)
    duckdb_json_recover(paths)
    # A preceding producer may have published while this process waited.
    problem <- tryCatch({ duckdb_json_validate(paths$binary); NULL }, error = identity)
    if (!is.null(problem)) {
      tryCatch(duckdb_json_publish(paths, native), error = function(cnd) {
        stop("DuckDB JSON preparation failed: ", conditionMessage(cnd), call. = FALSE)
      })
    }
    duckdb_json_unlock(lock)
    lock <- NULL
    lock <- duckdb_json_lock(paths$lock, FALSE, holder = holder)
    # Cleanup can acquire the gap between writer release and reader acquisition.
    duckdb_json_validate(paths$binary)
  }
  lease <- duckdb_json_lease(duckdb_json_absolute(paths$binary, TRUE), native, lock)
  success <- TRUE
  lease
}

# Explicit cleanup never removes stable lock files or an entry with active readers.
duckdb_json_cleanup <- function(version, platform, cache = duckdb_json_default_cache()) {
  paths <- duckdb_json_cache_paths(version, platform, cache)
  if (!dir.exists(paths$directory) && !file.exists(paths$lock) &&
      !length(duckdb_json_stages(paths))) {
    return(invisible(FALSE))
  }
  holder <- new.env(parent = emptyenv())
  holder$lock <- NULL
  on.exit(duckdb_json_unlock(holder$lock), add = TRUE)
  duckdb_json_lock(paths$lock, TRUE, timeout = 0, holder = holder)
  had_entry <- dir.exists(paths$directory) || length(duckdb_json_stages(paths)) > 0L
  duckdb_json_stages(paths, remove = TRUE)
  if (unlink(paths$directory, recursive = TRUE) != 0L || dir.exists(paths$directory)) {
    stop("Cannot remove the managed DuckDB JSON cache entry.", call. = FALSE)
  }
  invisible(had_entry)
}
