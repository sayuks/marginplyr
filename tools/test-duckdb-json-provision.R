# Exercise preparation, persistent reuse and owned-cache concurrency with real
# signed JSON binaries. Only the initial CI preparation downloads from the web.
main <- function() {
  workspace <- tempfile("json-provision-fixture-")
  dir.create(workspace)
  cleanup_workspace <- TRUE
  on.exit(if (cleanup_workspace) unlink(workspace, recursive = TRUE), add = TRUE)
  workspace <- normalizePath(workspace, winslash = "/", mustWork = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  processes <- list()
  on.exit({
    for (process in processes) {
      if (process$is_alive()) process$kill_tree()
      process$wait(timeout = 10000L)
      if (process$is_alive()) cleanup_workspace <- FALSE
    }
    if (!cleanup_workspace) stop("Could not collect JSON fixture children.", call. = FALSE)
  }, add = TRUE, after = FALSE)
  env <- Sys.getenv()
  env[["MARGINPLYR_DUCKDB_JSON_EXTENSION"]] <- ""
  run <- function(command, args = character(), status = 0L, values = env,
                  timeout = 60000L) {
    result <- processx::run(rscript, c(command, args), env = values,
      error_on_status = FALSE, timeout = timeout, cleanup_tree = TRUE)
    output <- paste(result$stdout, result$stderr, sep = "")
    matched <- if (is.null(status)) result$status != 0L else identical(result$status, status)
    if (!matched) stop("Unexpected fixture status ", result$status, ":\n", output,
                       call. = FALSE)
    result
  }
  fingerprint <- function(path) {
    list(hash = unname(tools::md5sum(path)), modified = as.numeric(file.info(path)$mtime))
  }
  downloads <- function(path) {
    if (file.exists(path)) readLines(path, warn = FALSE) else character()
  }

  runner <- file.path(workspace, "runner space's 日本語")
  dir.create(runner)
  env_file <- file.path(workspace, "github-env")
  writeLines("EXISTING_ENTRY=retained", env_file)
  ci_env <- env
  ci_env[c("RUNNER_TEMP", "GITHUB_ENV")] <- c(runner, env_file)
  command <- ".github/scripts/provision-duckdb-json.R"
  for (missing in c("RUNNER_TEMP", "GITHUB_ENV")) {
    incomplete <- ci_env
    incomplete[[missing]] <- ""
    result <- run(command, status = NULL, values = incomplete)
    stopifnot(grepl(missing, result$stderr, fixed = TRUE),
              identical(readLines(env_file), "EXISTING_ENTRY=retained"))
  }
  for (args in list("--unknown", "--cache", c("--cache", file.path(workspace, "custom-cache")),
                   "--remove", c("--remove", "v1"))) {
    run("tools/setup-duckdb-json.R", args, status = NULL)
  }
  stopifnot(!dir.exists(file.path(workspace, "custom-cache")))
  result <- run(command, values = ci_env, timeout = 180000L)
  cat(result$stdout, result$stderr, sep = "")
  entries <- readLines(env_file, encoding = "UTF-8", warn = FALSE)
  stopifnot(identical(entries[[1L]], "EXISTING_ENTRY=retained"))
  key <- "MARGINPLYR_DUCKDB_JSON_EXTENSION="
  entry <- entries[startsWith(entries, key)]
  stopifnot(length(entry) == 1L)
  binary <- substring(entry, nchar(key) + 1L)
  stopifnot(file.exists(binary), !grepl("\\", binary, fixed = TRUE),
            startsWith(binary, paste0(normalizePath(runner, winslash = "/"), "/")))
  expected_hash <- unname(tools::md5sum(binary))
  archive <- file.path(workspace, "signed-json.gz")
  bytes <- readBin(binary, "raw", n = file.info(binary)$size)
  writeBin(memCompress(bytes, type = "gzip"), archive)

  consumer <- file.path(workspace, "consumer.R")
  writeLines(r"---(
source('tests/testthat/helper-optional-backends.R')
source('tests/testthat/helper-duckdb.R')
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, config = list(
  autoinstall_known_extensions = 'false', autoload_known_extensions = 'false')))
duckdb_test_json_extension(con)
value <- DBI::dbGetQuery(con,
  "SELECT json_extract_string('{\"answer\":42}', '$.answer') AS answer")
stopifnot(identical(value$answer, '42'))
DBI::dbDisconnect(con, shutdown = TRUE)
cat('Fresh offline JSON consumer passed.\n')
)---", consumer, useBytes = TRUE)
  consumer_env <- env
  consumer_env[["MARGINPLYR_DUCKDB_JSON_EXTENSION"]] <- binary
  result <- run(consumer, values = consumer_env)
  cat(result$stdout, result$stderr, sep = "")

  worker <- file.path(workspace, "worker.R")
  writeLines(r"---(
main <- function() {
  args <- readRDS(commandArgs(trailingOnly = TRUE)[[1L]])
  module <- new.env(parent = globalenv())
  sys.source('tools/duckdb-json.R', envir = module)
  repair_started <- FALSE
  wait_for_file <- function(path) {
    deadline <- Sys.time() + 30
    while (!file.exists(path)) {
      if (Sys.time() >= deadline) stop('Fixture release was not received.')
      Sys.sleep(0.02)
    }
  }
  module$duckdb_json_download <- function(url, destination) {
    stopifnot(startsWith(url, 'https://extensions.duckdb.org/'))
    repair_started <<- TRUE
    write(url, file = args$log, append = TRUE)
    if (args$download %in% c('fail', 'interrupt', 'pause')) {
      writeBin(charToRaw('incomplete download'), destination)
    }
    if (args$download == 'fail') stop('fixture download failed')
    if (args$download == 'interrupt') {
      stop(structure(list(message = 'fixture preparation interrupted', call = NULL),
        class = c('interrupt', 'condition')))
    }
    if (args$download == 'pause') {
      writeLines('download started', args$download_ready)
      wait_for_file(args$download_release)
    }
    stopifnot(file.copy(args$archive, destination, overwrite = TRUE))
    invisible(destination)
  }
  if (!is.null(args$entered)) {
    requireNamespace('filelock', quietly = TRUE)
    probe <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    DBI::dbDisconnect(probe, shutdown = TRUE)
    writeLines('acquisition entered', args$entered)
  }
  if (args$mode == 'publish-boundary') {
    paths <- module$duckdb_json_cache_paths(args$version, args$platform, args$cache)
    original <- list(hash = unname(tools::md5sum(paths$binary)),
      modified = as.numeric(file.info(paths$binary)$mtime))
    real_validate <- module$duckdb_json_validate
    cache_checks <- 0L
    module$duckdb_json_validate <- function(path) {
      if (identical(path, paths$binary) && !repair_started) {
        cache_checks <<- cache_checks + 1L
        stop('fixture cache validation requires repair')
      }
      real_validate(path)
    }
    real_rename <- module$duckdb_json_rename
    injected <- native_continued <- candidate_published <- FALSE
    module$duckdb_json_rename <- function(from, to) {
      result <- real_rename(from, to)
      candidate <- identical(basename(from), 'validated.duckdb_extension') &&
        identical(to, paths$binary)
      if (isTRUE(result) && candidate) candidate_published <<- TRUE
      selected <- if (args$publish_phase == 'previous') {
        identical(basename(to), 'previous.duckdb_extension')
      } else candidate
      if (isTRUE(result) && selected && !injected) {
        injected <<- TRUE
        writeLines('publication boundary reached', args$injection_mark)
        if (args$publish_signal == 'pause') {
          writeLines('previous binary moved', args$publish_ready)
          wait_for_file(args$publish_release)
        } else if (args$publish_signal == 'error') {
          stop('fixture publication error')
        } else if (args$publish_signal == 'native') {
          rlang::interrupt()
          native_continued <<- TRUE
        } else {
          stop(structure(list(message = 'fixture publication interrupted', call = NULL),
            class = c('interrupt', 'condition')))
        }
      }
      result
    }
    lease <- NULL
    on.exit(if (!is.null(lease)) module$duckdb_json_release(lease), add = TRUE)
    outcome <- tryCatch({
      lease <- module$duckdb_json_acquire(install = TRUE, path = '', cache = args$cache)
      module$duckdb_json_release(lease)
      lease <- NULL
      list(kind = 'returned')
    }, error = function(cnd) list(kind = 'error', message = conditionMessage(cnd)),
       interrupt = function(cnd) list(kind = 'interrupt', message = conditionMessage(cnd)))
    expected <- if (args$publish_signal == 'error') 'error' else 'interrupt'
    stopifnot(injected, cache_checks >= 1L, identical(outcome$kind, expected),
              file.exists(paths$binary),
              identical(unname(tools::md5sum(paths$binary)), original$hash),
              length(module$duckdb_json_stages(paths)) == 0L)
    real_validate(paths$binary)
    if (args$publish_signal == 'native') {
      stopifnot(native_continued, candidate_published)
    } else {
      stopifnot(identical(as.numeric(file.info(paths$binary)$mtime), original$modified))
    }
    # Cleanup is observable proof that the failed acquisition released its lease.
    module$duckdb_json_cleanup(args$version, args$platform, cache = args$cache)
    stopifnot(!file.exists(paths$binary))
    saveRDS(list(hash = original$hash, kind = outcome$kind,
      native_continued = native_continued, candidate_published = candidate_published),
      args$output)
  } else if (args$mode == 'cleanup') {
    module$duckdb_json_cleanup(args$version, args$platform, cache = args$cache)
    saveRDS(TRUE, args$output)
  } else if (args$mode == 'refcounts') {
    first <- second <- NULL
    on.exit({
      if (!is.null(first)) module$duckdb_json_release(first)
      if (!is.null(second)) module$duckdb_json_release(second)
    }, add = TRUE)
    first <- module$duckdb_json_acquire(install = FALSE, path = '', cache = args$cache)
    second <- module$duckdb_json_acquire(install = FALSE, path = '', cache = args$cache)
    version <- first$version
    platform <- first$platform
    busy <- function() {
      result <- tryCatch(module$duckdb_json_cleanup(version, platform, cache = args$cache),
                         error = identity)
      stopifnot(inherits(result, 'error'),
                grepl('in use', conditionMessage(result), fixed = TRUE))
    }
    busy()
    module$duckdb_json_release(first)
    first <- NULL
    busy()
    module$duckdb_json_release(second)
    second <- NULL
    module$duckdb_json_cleanup(version, platform, cache = args$cache)
    saveRDS(TRUE, args$output)
  } else {
    lease <- module$duckdb_json_acquire(
      install = args$install, path = args$path, cache = args$cache)
    on.exit(module$duckdb_json_release(lease), add = TRUE)
    saveRDS(list(path = lease$path, version = lease$version,
      platform = lease$platform, hash = unname(tools::md5sum(lease$path)),
      modified = as.numeric(file.info(lease$path)$mtime)), args$output)
    if (!is.null(args$lease_ready)) {
      writeLines('lease held', args$lease_ready)
      wait_for_file(args$lease_release)
    }
  }
  cat('JSON cache fixture worker passed.\n')
}
main()
)---", worker, useBytes = TRUE)
  new_args <- function(cache, ...) {
    values <- list(mode = "acquire", cache = cache, archive = archive,
      path = "", install = TRUE, download = "copy", log = file.path(cache, "downloads.log"),
      output = tempfile("result-", tmpdir = workspace, fileext = ".rds"))
    extra <- list(...)
    values[names(extra)] <- extra
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    values
  }
  argument_file <- function(args) {
    path <- tempfile("arguments-", tmpdir = workspace, fileext = ".rds")
    saveRDS(args, path)
    path
  }
  run_worker <- function(args, status = 0L) {
    result <- run(worker, argument_file(args), status = status)
    value <- if (file.exists(args$output)) readRDS(args$output) else NULL
    list(value = value, result = result)
  }
  start_worker <- function(args) {
    process <- processx::process$new(rscript, c(worker, argument_file(args)),
      env = env, stdout = "|", stderr = "2>&1", cleanup_tree = TRUE,
      supervise = TRUE)
    processes[[length(processes) + 1L]] <<- process
    process
  }
  finish_worker <- function(process, status = 0L, killed = FALSE) {
    process$wait(timeout = 60000L)
    timed_out <- process$is_alive()
    if (timed_out) {
      process$kill_tree()
      process$wait(timeout = 10000L)
    }
    if (process$is_alive()) stop("Could not collect a stopped JSON fixture.", call. = FALSE)
    # kill_tree closes stdio; killed workers are checked by status and markers.
    output <- if (killed || timed_out) "" else process$read_all_output()
    matched <- if (is.null(status)) process$get_exit_status() != 0L else
      identical(process$get_exit_status(), status)
    if (timed_out || !matched) stop("Pending JSON fixture failed:\n", output,
                                  call. = FALSE)
    invisible(output)
  }
  wait_for_marker <- function(process, path) {
    deadline <- Sys.time() + 30
    while (!file.exists(path)) {
      if (!process$is_alive()) {
        finish_worker(process)
        stop("JSON fixture exited without its marker.", call. = FALSE)
      }
      if (Sys.time() >= deadline) stop("JSON fixture marker was not received.", call. = FALSE)
      process$poll_io(50L)
    }
  }

  cache <- file.path(workspace, "persistent cache's 日本語")
  initial <- new_args(cache)
  first <- run_worker(initial)$value
  stopifnot(identical(first$hash, expected_hash), length(downloads(initial$log)) == 1L)
  before <- fingerprint(first$path)
  hit <- new_args(cache, install = FALSE, download = "fail")
  reused <- run_worker(hit)$value
  stopifnot(identical(fingerprint(first$path), before),
            identical(reused$path, first$path), length(downloads(hit$log)) == 1L)
  stopifnot(identical(run_worker(new_args(cache, download = "fail"))$value$hash,
                      expected_hash), length(downloads(hit$log)) == 1L)
  explicit <- new_args(cache, path = first$path, download = "fail")
  stopifnot(identical(run_worker(explicit)$value$path, first$path))
  missing <- new_args(cache, path = file.path(workspace, "missing.duckdb_extension"))
  run_worker(missing, status = NULL)
  corrupt <- file.path(workspace, "external-corrupt.duckdb_extension")
  writeLines("not a signed extension", corrupt)
  corrupt_before <- fingerprint(corrupt)
  run_worker(new_args(cache, path = corrupt), status = NULL)
  stopifnot(identical(fingerprint(corrupt), corrupt_before),
            identical(fingerprint(first$path), before), length(downloads(hit$log)) == 1L)
  cat("JSON cache reuse and explicit-path failures passed.\n")

  for (component in c(first$version, ".locks")) {
    name <- if (component == ".locks") "lock" else "version"
    alias <- new_args(file.path(workspace, paste0(name, "-parent-alias")))
    outside <- file.path(workspace, paste0(name, "-outside-cache"))
    dir.create(outside)
    sentinel <- file.path(outside, "retained.txt")
    writeLines("outside files are not cache entries", sentinel)
    retained <- fingerprint(sentinel)
    modified <- as.numeric(file.info(outside)$mtime)
    linked <- suppressWarnings(file.symlink(outside, file.path(alias$cache, component)))
    if (!linked && .Platform$OS.type == "windows") {
      cat("JSON parent-alias fixture unavailable without Windows symlink permission.\n")
      next
    }
    stopifnot(linked)
    result <- run_worker(alias, status = NULL)$result
    stopifnot(grepl("path alias", paste(result$stdout, result$stderr), fixed = TRUE),
              identical(list.files(outside, all.files = TRUE, no.. = TRUE), "retained.txt"),
              identical(fingerprint(sentinel), retained),
              identical(as.numeric(file.info(outside)$mtime), modified),
              length(downloads(alias$log)) == 0L)
  }
  cat("JSON managed paths reject aliased parents before creating children.\n")

  empty <- file.path(workspace, "read-only-empty")
  absent <- new_args(empty, install = FALSE)
  result <- run_worker(absent, status = NULL)$result
  stopifnot(grepl("setup-duckdb-json.R", paste(result$stdout, result$stderr), fixed = TRUE),
            length(downloads(absent$log)) == 0L)
  failed <- new_args(file.path(workspace, "failed-download"), download = "fail")
  run_worker(failed, status = NULL)
  expected <- file.path(failed$cache, first$version, first$platform, "json.duckdb_extension")
  stopifnot(!file.exists(expected), length(downloads(failed$log)) == 1L)
  interrupted <- new_args(file.path(workspace, "interrupted-download"), download = "interrupt")
  run_worker(interrupted, status = NULL)
  stopifnot(!file.exists(file.path(interrupted$cache, first$version, first$platform,
                                  "json.duckdb_extension")),
            length(downloads(interrupted$log)) == 1L)
  writeLines("damaged managed cache", first$path)
  repair <- new_args(cache)
  stopifnot(identical(run_worker(repair)$value$hash, expected_hash),
            length(downloads(repair$log)) == 2L)
  writeLines("damaged managed cache again", first$path)
  damaged <- fingerprint(first$path)
  run_worker(new_args(cache, download = "fail"), status = NULL)
  stopifnot(!file.exists(first$path) || identical(fingerprint(first$path), damaged),
            length(downloads(repair$log)) == 3L)
  run_worker(new_args(cache))
  cat("JSON preparation failures, interruption and repair passed.\n")

  seed_signed_cache <- function(directory) {
    path <- file.path(directory, first$version, first$platform, "json.duckdb_extension")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(binary, path))
    stopifnot(Sys.setFileTime(path, as.POSIXct("2000-01-01", tz = "UTC")))
    path
  }
  remaining_stages <- function(directory) {
    paths <- list.dirs(file.path(directory, first$version),
                      recursive = FALSE, full.names = TRUE)
    paths[startsWith(basename(paths), paste0(".", first$platform, "-stage-"))]
  }
  publication_args <- function(directory, phase, signal) {
    new_args(directory, mode = "publish-boundary", version = first$version,
      platform = first$platform, publish_phase = phase, publish_signal = signal,
      injection_mark = tempfile("publication-injected-", tmpdir = workspace))
  }
  for (phase in c("previous", "candidate")) {
    for (signal in c("error", "synthetic", "native")) {
      directory <- file.path(workspace, paste("publish", phase, signal, sep = "-"))
      original <- seed_signed_cache(directory)
      original_hash <- unname(tools::md5sum(original))
      args <- publication_args(directory, phase, signal)
      observed <- run_worker(args)$value
      stopifnot(file.exists(args$injection_mark),
                identical(observed$hash, original_hash),
                length(downloads(args$log)) == 1L,
                length(remaining_stages(directory)) == 0L,
                !file.exists(original))
    }
  }
  cat("JSON publication errors and interrupts preserve prior valid bytes.\n")

  for (ambiguous in c(FALSE, TRUE)) {
    directory <- file.path(workspace, paste0("publication-crash-", ambiguous))
    original <- seed_signed_cache(directory)
    original_state <- fingerprint(original)
    args <- publication_args(directory, "previous", "pause")
    args$publish_ready <- tempfile("previous-moved-", tmpdir = workspace)
    args$publish_release <- tempfile("never-release-publication-", tmpdir = workspace)
    producer <- start_worker(args)
    wait_for_marker(producer, args$publish_ready)
    stopifnot(!file.exists(original))
    producer$kill_tree()
    finish_worker(producer, status = NULL, killed = TRUE)
    previous <- list.files(file.path(directory, first$version),
      pattern = "^previous[.]duckdb_extension$", recursive = TRUE, full.names = TRUE,
      all.files = TRUE)
    stopifnot(length(previous) == 1L,
              identical(fingerprint(previous[[1L]]), original_state))
    if (ambiguous) {
      duplicate <- file.path(dirname(dirname(previous[[1L]])),
        paste0(".", first$platform, "-stage-duplicate"), "previous.duckdb_extension")
      dir.create(dirname(duplicate))
      stopifnot(file.copy(previous[[1L]], duplicate, copy.date = TRUE))
      previous <- c(previous, duplicate)
      preserved <- lapply(previous, fingerprint)
      run_worker(new_args(directory, download = "fail"), status = NULL)
      stopifnot(!file.exists(original), all(file.exists(previous)),
                identical(lapply(previous, fingerprint), preserved),
                length(downloads(args$log)) == 1L)
    } else {
      recovered <- run_worker(new_args(directory, download = "fail"))$value
      stopifnot(identical(fingerprint(recovered$path), original_state),
                length(downloads(args$log)) == 1L,
                length(remaining_stages(directory)) == 0L)
    }
  }
  cat("Interrupted publication recovers one backup and preserves ambiguous backups.\n")

  references <- file.path(workspace, "lease-references")
  reference_seed <- run_worker(new_args(references))$value
  old <- file.path(references, "v0.0.0", "fixture_platform", "retained.txt")
  dir.create(dirname(old), recursive = TRUE)
  writeLines("older environments retain their cache", old)
  run_worker(new_args(references, mode = "refcounts", install = FALSE))
  stopifnot(!file.exists(reference_seed$path), file.exists(old))
  holder_args <- new_args(cache, install = FALSE,
    lease_ready = file.path(workspace, "lease-ready"),
    lease_release = file.path(workspace, "lease-release"))
  holder <- start_worker(holder_args)
  wait_for_marker(holder, holder_args$lease_ready)
  cleanup <- new_args(cache, mode = "cleanup", version = first$version,
                      platform = first$platform)
  busy <- run_worker(cleanup, status = NULL)$result
  stopifnot(grepl("in use", paste(busy$stdout, busy$stderr), fixed = TRUE),
            file.exists(first$path))
  writeLines("release", holder_args$lease_release)
  finish_worker(holder)
  run_worker(cleanup)
  stopifnot(!file.exists(first$path))
  cat("JSON shared leases, retention and in-use cleanup refusal passed.\n")

  concurrent <- file.path(workspace, "concurrent")
  producer_args <- new_args(concurrent, download = "pause",
    download_ready = file.path(workspace, "producer-ready"),
    download_release = file.path(workspace, "producer-release"))
  producer <- start_worker(producer_args)
  wait_for_marker(producer, producer_args$download_ready)
  public <- file.path(concurrent, first$version, first$platform, "json.duckdb_extension")
  stopifnot(!file.exists(public))
  follower_args <- new_args(concurrent, entered = file.path(workspace, "follower-entered"))
  follower <- start_worker(follower_args)
  wait_for_marker(follower, follower_args$entered)
  follower$poll_io(500L)
  stopifnot(follower$is_alive(), length(downloads(producer_args$log)) == 1L,
            !file.exists(public))
  writeLines("publish", producer_args$download_release)
  finish_worker(producer)
  finish_worker(follower)
  producer_result <- readRDS(producer_args$output)
  follower_result <- readRDS(follower_args$output)
  stopifnot(identical(producer_result$path, follower_result$path),
            identical(follower_result$hash, expected_hash),
            length(downloads(producer_args$log)) == 1L)
  cat("Concurrent JSON producers share one validated publication.\n")

  abandoned <- file.path(workspace, "abandoned-producer")
  abandoned_args <- new_args(abandoned, download = "pause",
    download_ready = file.path(workspace, "abandoned-ready"),
    download_release = file.path(workspace, "never-released"))
  stopped <- start_worker(abandoned_args)
  wait_for_marker(stopped, abandoned_args$download_ready)
  stopped$kill_tree()
  finish_worker(stopped, status = NULL, killed = TRUE)
  abandoned_public <- file.path(abandoned, first$version, first$platform,
                                "json.duckdb_extension")
  stopifnot(!file.exists(abandoned_public))
  recovered <- run_worker(new_args(abandoned))$value
  stopifnot(identical(recovered$hash, expected_hash),
            length(downloads(abandoned_args$log)) == 2L)
  cat("Stopped JSON producer leaves no partial public cache.\n")
  source("tools/test-local-duckdb-json.R", local = TRUE)
  run_local_duckdb_json_fixtures(binary)
  cat("DuckDB JSON provisioning command fixtures passed.\n")
}
main()
