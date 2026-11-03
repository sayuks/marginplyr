# Serializes live FIFO coordination; capacity remains the existing heavy lock.

# Acquires one interruptible lock, treating an unexpected timeout as failure.
review_ready_fifo_lock <- function(path) {
  link <- Sys.readlink(path)
  if (file.exists(path) || (!is.na(link) && nzchar(link))) review_ready_fifo_file(path)
  lock <- filelock::lock(path, timeout = Inf)
  if (is.null(lock)) stop("Cannot acquire Review-ready FIFO lock: ", path, call. = FALSE)
  lock
}

# Writes the counter and live records atomically while metadata is locked.
review_ready_fifo_save <- function(queue, state) {
  temporary <- file.path(queue, "state-new.rds")
  if (file.exists(temporary) || (!is.na(Sys.readlink(temporary)) && nzchar(Sys.readlink(temporary)))) {
    review_ready_fifo_file(temporary)
    if (unlink(temporary) != 0L) stop("Cannot remove interrupted FIFO state write.")
  }
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(state, temporary)
  if (!file.rename(temporary, file.path(queue, "state.rds"))) {
    stop("Cannot publish Review-ready FIFO state.", call. = FALSE)
  }
}

# Decodes the OS process identity carried by an invocation's unique guard name.
review_ready_fifo_owner <- function(guard) {
  pattern <- "^request-([0-9a-f]{32})-([0-9]+)-([0-9]+[.][0-9]+)[.]lock$"
  pieces <- regmatches(guard, regexec(pattern, guard))[[1L]]
  if (length(pieces) != 4L) stop("Invalid Review-ready FIFO guard identity.", call. = FALSE)
  pid <- suppressWarnings(as.integer(pieces[[3L]]))
  birth <- suppressWarnings(as.numeric(pieces[[4L]]))
  if (is.na(pid) || pid <= 0L || !is.finite(birth) || birth <= 0) {
    stop("Invalid Review-ready FIFO OS owner.", call. = FALSE)
  }
  ps::ps_handle(pid, time = as.POSIXct(birth, origin = "1970-01-01", tz = "UTC"))
}

# Refuses linked, missing and non-file coordination targets before using them.
review_ready_fifo_file <- function(path) {
  link <- Sys.readlink(path)
  if ((!is.na(link) && nzchar(link)) || !file.exists(path) || isTRUE(file.info(path)$isdir)) {
    stop("Unsafe or missing Review-ready FIFO file: ", path, call. = FALSE)
  }
  invisible(NULL)
}

# Validates all state before a caller may prune or attempt admission.
review_ready_fifo_read <- function(queue) {
  path <- file.path(queue, "state.rds")
  review_ready_fifo_file(path)
  state <- readRDS(path)
  number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x >= 0 && x <= 2^53 - 1 && x == floor(x)
  if (!is.list(state) || !identical(names(state), c("version", "counter", "requests")) ||
      !identical(state$version, 1L) || !number(state$counter) || !is.list(state$requests)) {
    stop("Incompatible Review-ready FIFO state.", call. = FALSE)
  }
  for (request in state$requests) {
    if (!is.list(request) || !identical(names(request), c("sequence", "guard", "phase")) ||
        !number(request$sequence) || request$sequence < 1 || request$sequence > state$counter ||
        !is.character(request$guard) || length(request$guard) != 1L || is.na(request$guard) ||
        !is.character(request$phase) || length(request$phase) != 1L || is.na(request$phase) ||
        !request$phase %in% c("waiting", "active")) {
      stop("Invalid Review-ready FIFO request.", call. = FALSE)
    }
    review_ready_fifo_owner(request$guard)
    review_ready_fifo_file(file.path(queue, request$guard))
  }
  sequences <- vapply(state$requests, `[[`, numeric(1), "sequence")
  guards <- vapply(state$requests, `[[`, character(1), "guard")
  phases <- vapply(state$requests, `[[`, character(1), "phase")
  if (anyDuplicated(sequences) || anyDuplicated(guards) || sum(phases == "active") > 1L ||
      is.unsorted(sequences, strictly = TRUE) ||
      (any(phases == "active") && phases[[1L]] != "active")) {
    stop("Inconsistent Review-ready FIFO order.", call. = FALSE)
  }
  state
}

# A zombie has ended even when its parent has not collected its exit status.
review_ready_fifo_alive <- function(owner) {
  if (!ps::ps_is_running(owner)) return(FALSE)
  tryCatch(!identical(ps::ps_status(owner), "zombie"), error = function(cnd) {
    if (!ps::ps_is_running(owner)) return(FALSE)
    stop(cnd)
  })
}

# Prunes only ended OS identities whose guards can positively be acquired.
# Live owners are never probed: filelock 1.0.3 leaks a descriptor on contention.
review_ready_fifo_prune <- function(queue, state) {
  guards <- list.files(queue, pattern = "^request-", all.files = TRUE)
  referenced <- vapply(state$requests, `[[`, character(1), "guard")
  for (guard in guards) {
    path <- file.path(queue, guard)
    review_ready_fifo_file(path)
    owner <- review_ready_fifo_owner(guard)
    if (review_ready_fifo_alive(owner)) {
      if (!guard %in% referenced) {
        stop("Ambiguous live unregistered Review-ready FIFO guard: ", guard, call. = FALSE)
      }
      next
    }
    ownership <- filelock::lock(path, timeout = 0)
    if (is.null(ownership)) {
      stop("Ambiguous Review-ready FIFO guard ownership: ", guard, call. = FALSE)
    }
    tryCatch({
      state$requests <- Filter(function(x) x$guard != guard, state$requests)
      review_ready_fifo_save(queue, state)
      if (unlink(path) != 0L || file.exists(path)) stop("Cannot remove inactive FIFO guard.")
    }, finally = filelock::unlock(ownership))
  }
  # A crash after publishing removal leaves an unreferenced guard for the next prune.
  invisible(state)
}

# Removes this invocation while it still owns its guard, under serialized metadata.
review_ready_fifo_unregister <- function(request) {
  metadata <- review_ready_fifo_lock(file.path(request$queue, "metadata.lock"))
  on.exit(filelock::unlock(metadata), add = TRUE)
  state <- review_ready_fifo_read(request$queue)
  state$requests <- Filter(function(x) x$guard != request$name, state$requests)
  review_ready_fifo_save(request$queue, state)
  if (unlink(file.path(request$queue, request$name)) != 0L) stop("Cannot remove owned FIFO guard.")
}

# Holds a guard before publishing acceptance and allocates order in that publication.
review_ready_fifo_register <- function(queue, request) {
  existing <- file.exists(file.path(queue, "metadata.lock"))
  metadata <- review_ready_fifo_lock(file.path(queue, "metadata.lock"))
  on.exit(filelock::unlock(metadata), add = TRUE)
  path <- file.path(queue, "state.rds")
  if (!file.exists(path)) {
    if (existing || length(list.files(queue, pattern = "^request-"))) {
      stop("Missing Review-ready FIFO state at an existing coordination target.", call. = FALSE)
    }
    review_ready_fifo_save(queue, list(version = 1L, counter = 0, requests = list()))
  }
  state <- review_ready_fifo_prune(queue, review_ready_fifo_read(queue))
  if (state$counter >= 2^53 - 1) stop("Review-ready FIFO sequence exhausted.", call. = FALSE)
  random <- file("/dev/urandom", "rb")
  nonce <- tryCatch(readBin(random, "raw", 16L), finally = close(random))
  if (length(nonce) != 16L) stop("Cannot allocate a unique FIFO identity.")
  request$name <- paste0("request-", paste(sprintf("%02x", as.integer(nonce)), collapse = ""),
    "-", Sys.getpid(), "-", sprintf("%.6f", as.numeric(ps::ps_create_time())), ".lock")
  target <- file.path(queue, request$name)
  if (file.exists(target)) stop("Review-ready FIFO identity collision.")
  request$guard <- review_ready_fifo_lock(target)
  state$counter <- state$counter + 1
  request$sequence <- state$counter
  state$requests[[length(state$requests) + 1L]] <- list(sequence = request$sequence,
    guard = request$name, phase = "waiting")
  review_ready_fifo_save(queue, state)
  cat("FIFO accepted sequence ", sprintf("%.0f", request$sequence), ".\n", sep = "")
  flush.console()
}

# Releases capacity only after the check caller has collected its owned children.
review_ready_fifo_release <- function(request) {
  suspendInterrupts({
    on.exit({
      if (!is.null(request$guard)) filelock::unlock(request$guard)
      if (!is.null(request$heavy)) filelock::unlock(request$heavy)
    }, add = TRUE)
    if (!is.null(request$guard)) review_ready_fifo_unregister(request)
  })
  invisible(NULL)
}

# Only the oldest live waiter may block once on the stable heavy-capacity target.
review_ready_heavy_lock <- function() {
  directory <- file.path(review_ready_account_home(), ".local", "state", "marginplyr")
  queue <- file.path(directory, "review-ready-fifo")
  dir.create(queue, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  link <- Sys.readlink(queue)
  if (!is.na(link) && nzchar(link)) stop("Review-ready FIFO directory must not be linked.")
  request <- new.env(parent = emptyenv())
  request$queue <- queue
  request$guard <- request$heavy <- NULL
  handed_off <- FALSE
  on.exit(if (!handed_off) review_ready_fifo_release(request), add = TRUE)
  review_ready_fifo_register(queue, request)
  started <- proc.time()[["elapsed"]]
  previous <- NULL
  cat("\nWaiting for heavy package-check capacity.\n")
  repeat {
    metadata <- review_ready_fifo_lock(file.path(queue, "metadata.lock"))
    earlier <- tryCatch({
      state <- review_ready_fifo_prune(queue, review_ready_fifo_read(queue))
      sum(vapply(state$requests, function(x) x$phase == "waiting" &&
        x$sequence < request$sequence, logical(1)))
    }, finally = filelock::unlock(metadata))
    if (!identical(earlier, previous)) {
      cat("FIFO sequence ", sprintf("%.0f", request$sequence), ": ", earlier,
        " earlier live waiter(s).\n", sep = "")
      flush.console()
      previous <- earlier
    }
    if (earlier == 0L) break
    Sys.sleep(0.2)
  }
  # Release metadata before this single interruptible capacity wait.
  request$heavy <- filelock::lock(file.path(directory, "review-ready-heavy.lock"), timeout = Inf)
  if (is.null(request$heavy)) stop("Cannot acquire heavy package-check capacity.", call. = FALSE)
  metadata <- review_ready_fifo_lock(file.path(queue, "metadata.lock"))
  tryCatch({
    state <- review_ready_fifo_prune(queue, review_ready_fifo_read(queue))
    index <- which(vapply(state$requests, function(x) x$guard == request$name, logical(1)))
    waiting <- which(vapply(state$requests, function(x) x$phase == "waiting", logical(1)))
    if (length(index) != 1L || !length(waiting) || index != waiting[[1L]]) {
      stop("Review-ready FIFO admission ownership changed.", call. = FALSE)
    }
    if (any(vapply(state$requests, function(x) x$phase == "active", logical(1)))) {
      stop("Review-ready FIFO active ownership conflicts with free capacity.", call. = FALSE)
    }
    state$requests[[index]]$phase <- "active"
    review_ready_fifo_save(queue, state)
  }, finally = filelock::unlock(metadata))
  cat("Heavy package-check capacity acquired. FIFO sequence ", sprintf("%.0f", request$sequence),
    "; elapsed wait ", sprintf("%.3f", proc.time()[["elapsed"]] - started), " seconds.\n", sep = "")
  flush.console()
  handed_off <- TRUE
  request
}
