# Local full-check observations only; ADR 0040 owns the private-hook decision.
# No timing result changes dispatch or the package-check verdict.

# Reports an optimization failure without raising a package-test condition.
review_ready_timing_disabled <- function(reason) {
  try(cat("Review-ready timing disabled: ", reason, "\n", sep = ""), silent = TRUE)
  invisible(FALSE)
}

# Requires the calibrated version and complete queue_task body before tracing it.
review_ready_timing_supported <- function(namespace = asNamespace("testthat"),
                                         version = as.character(utils::packageVersion("testthat"))) {
  expected <- quote({
    withr::local_envvar(TESTTHAT_IS_PARALLEL = "true")
    snapshotter <- SubprocessSnapshotReporter$new(snap_dir = "_snaps")
    withr::local_options(testthat.snapshotter = snapshotter)
    reporters <- list(SubprocessReporter$new(), snapshotter)
    multi <- MultiReporter$new(reporters = reporters)
    with_reporter(multi, test_one_file(path, env = the$testing_env,
        shuffle = shuffle))
    NULL
  })
  target <- get("queue_task", namespace)
  identical(version, "3.3.2") &&
    identical(formals(target), as.pairlist(alist(path = , shuffle = FALSE))) &&
    identical(body(target), expected)
}

# Installs worker-local entry/exit recording; helpers/setup run before queue_task.
# test_one_file returns after file cleanup, before this exit observation.
review_ready_timing_install <- function() {
  directory <- Sys.getenv("MARGINPLYR_REVIEW_TIMING")
  if (!nzchar(directory)) return(invisible(FALSE))
  tryCatch(withCallingHandlers({
    if (!review_ready_timing_supported()) stop("unsupported testthat version or queue_task shape")
    pid <- Sys.getpid()
    log <- file.path(directory, paste0(pid, ".tsv"))
    disabled <- FALSE
    record <- function(kind, path) {
      if (disabled) return(invisible(NULL))
      tryCatch(withCallingHandlers({
        cat(paste(kind, basename(path), pid,
          sprintf("%.9f", as.numeric(Sys.time())),
          sprintf("%.9f", proc.time()[["elapsed"]]), sep = "\t"),
          "\n", file = log, append = TRUE, sep = "")
      }, warning = function(cnd) stop(conditionMessage(cnd))), error = function(cnd) {
        disabled <<- TRUE
        review_ready_timing_disabled(conditionMessage(cnd))
        try(suppressWarnings(writeLines(conditionMessage(cnd),
          file.path(directory, paste0(pid, ".disabled")))), silent = TRUE)
      })
      invisible(NULL)
    }
    saveRDS(list(pid = pid, session = utils::sessionInfo(),
      supported_testthat = "3.3.2", boundary = "queue_task"),
      file.path(directory, paste0(pid, ".worker.rds")))
    invisible(utils::capture.output(trace("queue_task", where = asNamespace("testthat"),
      print = FALSE,
      tracer = substitute(RECORD("start", path), list(RECORD = record)),
      exit = substitute(RECORD("end", path), list(RECORD = record)))))
    invisible(TRUE)
  }, warning = function(cnd) stop(conditionMessage(cnd))), error = function(cnd) {
    review_ready_timing_disabled(conditionMessage(cnd))
    try(suppressWarnings(writeLines(conditionMessage(cnd),
      file.path(directory, paste0(Sys.getpid(), ".disabled")))), silent = TRUE)
    invisible(FALSE)
  })
}

# Captures pinned file bytes and dependency versions before the ordinary check.
review_ready_timing_prepare <- function(source_root, directory, metadata) {
  tryCatch(withCallingHandlers({
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    test_dir <- file.path(source_root, "tests", "testthat")
    discovered <- testthat::find_test_scripts(test_dir, full.names = FALSE)
    inputs <- c("DESCRIPTION", paste0("tests/",
      list.files(file.path(source_root, "tests"), recursive = TRUE, all.files = TRUE)))
    paths <- file.path(source_root, inputs)
    contents <- lapply(paths, function(path) readBin(path, "raw", n = file.info(path)$size))
    names(contents) <- inputs
    dependencies <- utils::installed.packages()[, c("Package", "Version", "Built"), drop = FALSE]
    saveRDS(list(version = 1L, sha = metadata$sha, started_utc = metadata$started_utc,
      discovered = discovered, contents = contents, md5 = stats::setNames(tools::md5sum(paths), inputs),
      dependencies = dependencies, session = utils::sessionInfo(),
      supported_testthat = "3.3.2", boundary = "queue_task", intended_workers = 8L),
      file.path(directory, "manifest.rds"))
    TRUE
  }, warning = function(cnd) stop(conditionMessage(cnd))),
    error = function(cnd) review_ready_timing_disabled(conditionMessage(cnd)))
}

# Validates raw observations against discovery and the invocation's terminal result.
# Consumers also require the sibling result.rds to remain successful at this SHA.
review_ready_timing_evidence <- function(directory, terminal, transcript) {
  manifest <- readRDS(file.path(directory, "manifest.rds"))
  disabled <- list.files(directory, "[.]disabled$", full.names = TRUE)
  if (length(disabled)) stop(paste(unique(unlist(lapply(disabled, readLines))), collapse = "; "))
  announcements <- grep("^Starting [0-9]+ test process(es)?[.]$", trimws(transcript), value = TRUE)
  workers <- if (length(announcements) == 1L) {
    as.integer(sub("^Starting ([0-9]+).*", "\\1", announcements))
  } else NA_integer_
  logs <- list.files(directory, "^[0-9]+[.]tsv$", full.names = TRUE)
  records <- lapply(logs, function(log) {
    bytes <- readBin(log, "raw", n = file.info(log)$size)
    if (!length(bytes) || !identical(bytes[[length(bytes)]], as.raw(10L))) {
      stop("unterminated worker observation")
    }
    strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1L]]
  })
  lines <- unlist(records, use.names = FALSE)
  if (!length(lines)) stop("missing worker observations")
  fields <- strsplit(lines, "\t", fixed = TRUE)
  valid_shape <- length(fields) > 0L && all(lengths(fields) == 5L)
  if (!valid_shape) stop("missing or malformed worker observations")
  events <- as.data.frame(do.call(rbind, fields), stringsAsFactors = FALSE)
  names(events) <- c("event", "file", "pid", "wall", "elapsed")
  events$log_pid <- as.numeric(rep(sub("[.]tsv$", "", basename(logs)), lengths(records)))
  for (name in c("pid", "wall", "elapsed")) events[[name]] <- suppressWarnings(as.numeric(events[[name]]))
  if (!all(events$event %in% c("start", "end")) ||
      !all(events$file %in% manifest$discovered) ||
      !all(is.finite(as.matrix(events[c("pid", "wall", "elapsed")]))) ||
      any(events$pid != events$log_pid | events$pid <= 0 | events$pid != floor(events$pid)) ||
      any(events$wall < 0 | events$elapsed < 0)) {
    stop("invalid worker observation values")
  }
  results <- utils::read.delim(file.path(directory, "file-results.tsv"),
    header = FALSE, col.names = c("file", "skip", "other"), stringsAsFactors = FALSE)
  reasons <- character()
  if (!identical(terminal$status, "passed") || !identical(terminal$sha, manifest$sha) ||
      !identical(terminal$heavy_check, "passed")) reasons <- c(reasons, "full invocation did not pass")
  if (!identical(workers, 8L)) reasons <- c(reasons, "actual worker count is not eight")
  if (nrow(results) != length(manifest$discovered) || anyDuplicated(results$file) ||
      !setequal(results$file, manifest$discovered) ||
      !is.numeric(results$skip) || !is.numeric(results$other) ||
      !all(is.finite(as.matrix(results[c("skip", "other")]))) ||
      any(as.matrix(results[c("skip", "other")]) < 0) ||
      any(as.matrix(results[c("skip", "other")]) %% 1 != 0)) stop("incomplete file results")
  files <- lapply(manifest$discovered, function(file) {
    pair <- events[events$file == file, ]
    valid <- nrow(pair) == 2L && identical(pair$event, c("start", "end")) &&
      length(unique(pair$pid)) == 1L && pair$pid[[1L]] > 0 &&
      pair$wall[[2L]] >= pair$wall[[1L]] && pair$elapsed[[2L]] >= pair$elapsed[[1L]]
    result <- results[results$file == file, ]
    all_skipped <- result$skip > 0 && result$other == 0
    data.frame(file = file, valid_pair = valid, all_skipped = all_skipped,
      empty = result$skip + result$other == 0,
      duration = if (valid) diff(pair$elapsed) else NA_real_)
  })
  files <- do.call(rbind, files)
  if (!all(files$valid_pair)) reasons <- c(reasons, "incomplete or duplicate start/end pairs")
  pids <- unique(events$pid)
  for (pid in pids) {
    worker <- readRDS(file.path(directory, paste0(pid, ".worker.rds")))
    if (!identical(worker$pid, as.integer(pid)) ||
        !identical(worker$supported_testthat, "3.3.2")) stop("invalid worker identity")
  }
  files$eligible <- !length(reasons) & files$valid_pair & !files$all_skipped & !files$empty
  list(version = 1L, sha = manifest$sha, actual_workers = workers,
    eligible = !length(reasons), reasons = reasons, files = files,
    observations = events, terminal = terminal, manifest = "manifest.rds")
}

# Timing failures are visible and never substitute for the complete check outcome.
review_ready_timing_finish <- function(directory, terminal, transcript) {
  tryCatch(withCallingHandlers({
    evidence <- review_ready_timing_evidence(directory, terminal, transcript)
    saveRDS(evidence, file.path(directory, "evidence.rds"))
    if (!evidence$eligible) review_ready_timing_disabled(paste(evidence$reasons, collapse = "; "))
    cat("Review-ready timing evidence: ", file.path(directory, "evidence.rds"),
      "; eligible: ", evidence$eligible, "\n", sep = "")
  }, warning = function(cnd) stop(conditionMessage(cnd))), error = function(cnd) {
    review_ready_timing_disabled(conditionMessage(cnd))
    try(suppressWarnings(saveRDS(list(version = 1L, sha = terminal$sha, eligible = FALSE,
      reasons = conditionMessage(cnd)), file.path(directory, "evidence.rds"))), silent = TRUE)
  })
  invisible(NULL)
}

review_ready_timing_install
