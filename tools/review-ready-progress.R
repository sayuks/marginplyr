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

# Reads testthat's native startup message from its actual check transcript.
# The selected worker count can be lower than TESTTHAT_CPUS.
review_ready_test_start_message <- function(check_dir) {
  logs <- list.files(check_dir, "^testthat[.]Rout([.]fail)?$",
    recursive = TRUE, full.names = TRUE)
  for (log in logs) {
    # R CMD check can rename the transcript to .fail while it is observed.
    lines <- tryCatch(suppressWarnings(readLines(log, warn = FALSE)),
      error = function(cnd) character())
    messages <- grep("^Starting [0-9]+ test process(es)?[.]$", trimws(lines),
      value = TRUE)
    if (length(messages)) return(messages[[1L]])
  }
  character()
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
  repeat {
    process$poll_io(250L)
    lines <- process$read_output_lines()
    if (length(lines)) cat(lines, sep = "\n")
    alive <- process$is_alive()
    snapshot <- review_ready_progress_snapshot(progress)
    now <- proc.time()[["elapsed"]]
    if (!announced && (!is.null(snapshot) || !alive)) {
      announcement <- review_ready_test_start_message(check_dir)
      if (length(announcement)) {
        cat(announcement, "\n", sep = "")
        flush.console()
        announced <- TRUE
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
