# Reads completed-file events written by the check's testthat reporter.
# Progress is observational; the check process alone supplies the verdict.
review_ready_progress_snapshot <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  if (!length(lines) || !grepl("^total\t[0-9]+$", lines[[1L]])) return(NULL)
  total <- as.integer(sub("^total\t", "", lines[[1L]]))
  files <- unique(sub("^done\t", "", lines[grepl("^done\t.*[.][rR]$", lines)]))
  list(
    total = total, completed = length(files), last = tail(files, 1L),
    finished = "finished" %in% lines
  )
}

# Reads the current check transcript, including after R CMD check renames it.
review_ready_test_transcript <- function(check_dir) {
  logs <- list.files(check_dir, "^testthat[.]Rout([.]fail)?$",
    recursive = TRUE, full.names = TRUE)
  for (log in logs) {
    # R CMD check can rename the transcript to .fail while it is observed.
    lines <- tryCatch(suppressWarnings({
      bytes <- readBin(log, "raw", n = file.info(log)$size)
      ends <- which(bytes == as.raw(10L))
      # The writer may not have completed its last line yet.
      if (length(ends)) {
        text <- rawToChar(bytes[seq_len(tail(ends, 1L))])
        sub("\r$", "", strsplit(text, "\n", fixed = TRUE)[[1L]])
      } else {
        character()
      }
    }), error = function(cnd) character())
    if (length(lines)) return(lines)
  }
  character()
}

# Locates a native worker diagnostic with its preceding worker output, if any.
# Ordinary test failures remain with the standard check reporter.
review_ready_worker_diagnostic_start <- function(lines) {
  markers <- grep(paste(
    "testthat subprocess (failed to start|exited in file)",
    "At least one parallel worker failed to run teardown", sep = "|"
  ), lines)
  if (!length(markers)) return(NULL)
  errors <- grep("^Error([ :]|$)", lines[seq_len(markers[[1L]])])
  start <- if (length(errors)) tail(errors, 1L) else markers[[1L]]
  while (start > 1L && grepl("^$|^> [^:]+:", lines[[start - 1L]])) {
    start <- start - 1L
  }
  start
}

# Runs the same source-tarball check while forwarding output and periodic
# completed-file counts. The process object is retained through terminal wait.
review_ready_progress_check <- function(
  path, args, build_args, check_dir, error_on, env, repos,
  process_factory = rcmdcheck::rcmdcheck_process$new
) {
  stopifnot(identical(error_on, "never"))
  dir.create(check_dir, recursive = TRUE, showWarnings = FALSE)
  progress <- file.path(check_dir, "test-progress.tsv")
  env[["MARGINPLYR_REVIEW_PROGRESS"]] <- progress
  process <- process_factory(
    path = path, args = args, build_args = build_args,
    check_dir = check_dir, env = env, repos = repos
  )
  on.exit({
    if (process$is_alive()) process$kill()
  }, add = TRUE)
  started <- NULL
  displayed <- -Inf
  finished <- FALSE
  announced <- FALSE
  diagnostic_start <- NULL
  reported_until <- 0L
  transcript_read_at <- -Inf
  repeat {
    process$poll_io(250L)
    lines <- process$read_output_lines()
    if (length(lines)) cat(lines, sep = "\n")
    alive <- process$is_alive()
    snapshot <- review_ready_progress_snapshot(progress)
    now <- proc.time()[["elapsed"]]
    if (now - transcript_read_at >= 1 || !alive ||
        (!announced && !is.null(snapshot))) {
      transcript_read_at <- now
      transcript <- review_ready_test_transcript(check_dir)
      if (!announced) {
        announcement <- grep("^Starting [0-9]+ test process(es)?[.]$",
          trimws(transcript), value = TRUE)
        if (length(announcement)) {
          cat(announcement[[1L]], "\n", sep = "")
          flush.console()
          announced <- TRUE
        }
      }
      if (is.null(diagnostic_start)) {
        diagnostic_start <- review_ready_worker_diagnostic_start(transcript)
      }
      if (!is.null(diagnostic_start) && length(transcript) > reported_until) {
        if (reported_until == 0L) cat("\nTest worker diagnostic:\n")
        first <- max(diagnostic_start, reported_until + 1L)
        cat(transcript[seq.int(first, length(transcript))], sep = "\n")
        flush.console()
        reported_until <- length(transcript)
      }
    }
    if (!is.null(snapshot) && !finished) {
      if (is.null(started)) started <- now
      if (now - displayed >= 5 || snapshot$finished) {
        elapsed <- floor(now - started)
        percent <- if (snapshot$total) {
          100 * snapshot$completed / snapshot$total
        } else {
          0
        }
        last <- if (length(snapshot$last)) snapshot$last else "none"
        cat(sprintf(
          "[tests %02d:%02d] %d/%d files completed (%.1f%%); last: %s\n",
          elapsed %/% 60, elapsed %% 60,
          snapshot$completed, snapshot$total, percent, last
        ))
        flush.console()
        displayed <- now
      }
      finished <- snapshot$finished
    }
    if (!alive) break
  }
  process$wait()
  lines <- process$read_output_lines()
  if (length(lines)) cat(lines, sep = "\n")
  process$parse_results()
}
