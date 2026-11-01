# Shared primitives for the repository runner. No package loading or check stage
# belongs in this file; it is also loaded by waiting and recovery managers.
rr_protocol <- 4L
rr_schema <- 2L
rr_week <- 7 * 24 * 60 * 60
rr_units <- c("build-full", "build-documentation", "generation", "documentation-assertions")
rr_metadata <- c(
  CODEX_TOOL_CALL_ID = "exec-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
  CODEX_NETWORK_PROXY_ATTRIBUTION = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
  CLAUDE_CODE_SESSION_ID = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
  CLAUDE_PID = "[1-9][0-9]*"
)
rr_stop <- function(...) stop(..., call. = FALSE)
rr_say <- function(...) { cat(..., "\n", sep = ""); flush.console() }
rr_now <- function() as.numeric(Sys.time())
rr_hex <- function(x) paste(format(as.raw(x)), collapse = "")
rr_hash <- function(x) rr_hex(openssl::sha256(x))
rr_id <- function() rr_hex(openssl::rand_bytes(16))
rr_match <- function(pattern, text) grepl(paste0("^(?:", pattern, ")$"), text, perl = TRUE)
# Virtual process files can report size zero while their stream is nonempty.
rr_read <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  chunks <- list()
  repeat {
    chunk <- readBin(connection, "raw", n = 1048576L)
    if (!length(chunk)) break
    chunks[[length(chunks) + 1L]] <- chunk
  }
  do.call(c, c(list(raw()), chunks))
}
rr_text <- function(path) rawToChar(rr_read(path))

# v2 has length-prefixed UTF-8 strings, sorted unique map keys, explicit lists,
# booleans/null, and finite round-trippable numbers. It never authenticates RDS.
rr_bytes <- function(value) {
  encode <- function(x) {
    if (is.null(x)) return("z")
    if (is.list(x)) {
      keys <- names(x)
      if (is.null(keys)) return(paste0("a", length(x), ":", paste(vapply(x, encode, ""), collapse = "")))
      if (anyDuplicated(keys) || anyNA(keys) || !all(nzchar(keys))) rr_stop("Duplicate or empty encoding key.")
      order <- order(enc2utf8(keys), method = "radix")
      parts <- vapply(order, function(i) paste0(encode(keys[[i]]), encode(x[[i]])), "")
      return(paste0("o", length(x), ":", paste(parts, collapse = "")))
    }
    if (length(x) != 1L) return(encode(as.list(unname(x))))
    if (is.na(x)) rr_stop("Encoding requires finite nonmissing values.")
    if (is.character(x)) {
      x <- enc2utf8(x)
      if (is.na(iconv(x, "UTF-8", "UTF-8"))) rr_stop("Invalid UTF-8 encoding.")
      return(paste0("s", nchar(x, type = "bytes"), ":", x))
    }
    if (is.logical(x)) return(if (x) "t" else "f")
    if (is.numeric(x) && is.finite(x)) return(paste0("n", trimws(formatC(if (x == 0) 0 else x,
      format = "g", digits = 17L, decimal.mark = ".")), ";"))
    rr_stop("Encoding requires finite JSON values.")
  }
  charToRaw(paste0("marginplyr-review-ready/v2:", encode(value)))
}
rr_digest <- function(value) rr_hash(rr_bytes(value))
rr_same <- function(left, right) identical(rr_digest(left), rr_digest(right))
rr_json <- function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA))
rr_read_json <- function(path) {
  value <- jsonlite::fromJSON(rr_text(path), simplifyVector = FALSE)
  # Encoding rejects duplicate keys and unsupported values after parsing.
  rr_bytes(value)
  value
}

# A closed complete replacement is published by one same-directory rename.
# OS crash/power-loss durability is deliberately outside ADR 0039's guarantee.
rr_write <- function(path, value, raw = FALSE) {
  temporary <- tempfile(".replace-", tmpdir = dirname(path))
  on.exit(unlink(temporary, expand = FALSE), add = TRUE)
  if (!raw && basename(path) == "record.json" && identical(value$version, 3L)) {
    value <- jsonlite::fromJSON(rr_json(value), simplifyVector = FALSE)
    value <- list(record = value, checksum = rr_digest(value))
  }
  bytes <- if (raw) value else charToRaw(rr_json(value))
  connection <- file(temporary, "wb")
  tryCatch(writeBin(bytes, connection), finally = close(connection))
  Sys.chmod(temporary, "0600")
  if (!file.rename(temporary, path)) rr_stop("Cannot publish state: ", path)
  invisible(value)
}

rr_setup <- function() {
  if (!Sys.info()[["sysname"]] %in% c("Darwin", "Linux")) rr_stop("Review-ready requires macOS or Linux.")
  packages <- c("processx", "ps", "filelock", "openssl", "askpass", "sys", "jsonlite")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) rr_stop("Install Review-ready development dependencies: ", paste(missing, collapse = ", "))
  if (!ps::ps_is_supported()) rr_stop("Unsupported process inspection; no check was run.")
  if (!nzchar(openssl::openssl_config()$version)) rr_stop("OpenSSL runtime is unavailable.")
  rr_processes()
  rr_boot()
  invisible(TRUE)
}

# ~user is resolved through the account database, independently of HOME/XDG.
rr_account_home <- function() {
  value <- path.expand(paste0("~", ps::ps_username()))
  if (!startsWith(value, "/") || !dir.exists(value)) rr_stop("OS account home is unavailable.")
  normalizePath(value, mustWork = TRUE)
}
rr_uid <- function() unname(ps::ps_uids()[["real"]])
rr_state <- function() file.path(rr_account_home(), ".local/state/marginplyr/review-ready")
rr_mkdir <- function(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, mode = "0700")) rr_stop("Cannot create ", path)
  invisible(normalizePath(path, mustWork = TRUE))
}
rr_lock <- function(path, wait = FALSE) {
  filelock::lock(path, timeout = if (wait) Inf else 0)
}
rr_with_lock <- function(path, code) {
  lock <- rr_lock(path, TRUE)
  on.exit(filelock::unlock(lock), add = TRUE)
  force(code)
}

rr_run <- function(command, args = character(), root = getwd(), env = getOption("marginplyr.review.env"), error = TRUE) {
  result <- processx::run(command, args, wd = root, env = env, error_on_status = FALSE, echo = FALSE)
  if (error && result$status != 0L) rr_stop(command, " failed: ", trimws(result$stderr))
  invisible(result)
}
rr_git <- function(root, ...) invisible(trimws(rr_run("git", c("--no-replace-objects", "-C", root, ...))$stdout, which = "right"))
rr_in_environment <- function(values, code) {
  if (is.null(values)) return(force(code))
  previous <- Sys.getenv()
  on.exit({
    Sys.unsetenv(setdiff(names(Sys.getenv()), names(previous)))
    do.call(Sys.setenv, as.list(previous))
  }, add = TRUE)
  Sys.unsetenv(setdiff(names(previous), names(values)))
  do.call(Sys.setenv, as.list(values))
  force(code)
}
rr_git_raw <- function(root, ...) {
  command <- paste(vapply(c("git", "--no-replace-objects", "-C", root, ...), shQuote, ""), collapse = " ")
  connection <- rr_in_environment(getOption("marginplyr.review.env"), pipe(command, "rb"))
  closed <- FALSE
  on.exit(if (!closed) close(connection), add = TRUE)
  chunks <- list()
  repeat {
    chunk <- readBin(connection, "raw", n = 1024L * 1024L)
    if (!length(chunk)) break
    chunks[[length(chunks) + 1L]] <- chunk
  }
  status <- close(connection); closed <- TRUE
  if (!is.null(status) && status != 0L) rr_stop("Git object unavailable.")
  do.call(c, c(list(raw()), chunks))
}
rr_split0 <- function(bytes) {
  if (!length(bytes)) return(character())
  if (tail(bytes, 1L) != as.raw(0)) rr_stop("Unterminated Git records.")
  boundaries <- which(bytes == as.raw(0))
  starts <- c(1L, head(boundaries, -1L) + 1L)
  vapply(seq_along(boundaries), function(i) {
    if (starts[[i]] == boundaries[[i]]) "" else rawToChar(bytes[seq.int(starts[[i]], boundaries[[i]] - 1L)])
  }, "")
}
rr_tree <- function(root, sha) {
  entries <- rr_split0(rr_git_raw(root, "ls-tree", "-rz", sha))
  rows <- lapply(entries, function(entry) {
    boundary <- regexpr("\t", entry, fixed = TRUE)[[1L]]
    if (boundary < 1L) rr_stop("Malformed Git tree.")
    fields <- strsplit(substr(entry, 1L, boundary - 1L), " ", fixed = TRUE)[[1L]]
    list(name = substring(entry, boundary + 1L), mode = fields[[1L]], kind = fields[[2L]], oid = fields[[3L]])
  })
  stats::setNames(rows, vapply(rows, `[[`, "", "name"))
}

# One cat-file batch avoids a process launch for every registered source blob.
rr_blobs <- function(root, oids) {
  oids <- unique(oids)
  if (!length(oids)) return(list())
  temporary <- getOption("marginplyr.review.tmp", tempdir())
  input <- tempfile("git-batch-input-", tmpdir = temporary)
  output <- tempfile("git-batch-output-", tmpdir = temporary)
  on.exit(unlink(c(input, output), expand = FALSE), add = TRUE)
  writeLines(oids, input)
  result <- processx::run("git", c("--no-replace-objects", "-C", root, "cat-file", "--batch"), stdin = input,
    stdout = output, stderr = "|", env = getOption("marginplyr.review.env"), error_on_status = FALSE)
  if (result$status != 0L) rr_stop("Unavailable Git object batch: ", result$stderr)
  bytes <- rr_read(output); offset <- 1L; blobs <- list()
  for (oid in oids) {
    boundary <- which(bytes[seq.int(offset, length(bytes))] == as.raw(10))[[1L]] + offset - 1L
    header <- strsplit(rawToChar(bytes[seq.int(offset, boundary - 1L)]), " ", fixed = TRUE)[[1L]]
    if (length(header) != 3L || header[[1L]] != oid || header[[2L]] != "blob") rr_stop("Unsupported Git object response.")
    size <- as.integer(header[[3L]])
    if (is.na(size) || size < 0L || boundary + size + 1L > length(bytes) || bytes[[boundary + size + 1L]] != as.raw(10)) rr_stop("Incomplete Git object response.")
    blobs[[oid]] <- if (size) bytes[seq.int(boundary + 1L, boundary + size)] else raw()
    offset <- boundary + size + 2L
  }
  blobs
}
rr_identity <- function(root = getwd()) {
  root <- normalizePath(root, mustWork = TRUE)
  if (!identical(root, normalizePath(rr_git(root, "rev-parse", "--show-toplevel")))) rr_stop("Run from the target repository root.")
  if (!identical(read.dcf(file.path(root, "DESCRIPTION"))[[1L, "Package"]], "marginplyr")) rr_stop("The target is not marginplyr.")
  sha <- rr_git(root, "rev-parse", "HEAD")
  if (!rr_match("[0-9a-f]{40}", sha)) rr_stop("A committed SHA is required.")
  if (nzchar(rr_git(root, "status", "--porcelain=v1", "--untracked-files=all"))) rr_stop("A clean committed HEAD is required.")
  list(root = root, sha = sha)
}

# Path validation precedes every owned-tree deletion, including parent links.
rr_owned_path <- function(path, root) {
  root <- normalizePath(root, mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) rr_stop("Deletion outside owned root refused: ", path)
  relative <- substring(path, nchar(root) + 2L)
  pieces <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  if (any(pieces %in% c("", ".", ".."))) rr_stop("Invalid owned path.")
  current <- root
  for (piece in pieces) {
    current <- file.path(current, piece)
    link <- Sys.readlink(current)
    if (!is.na(link) && nzchar(link)) rr_stop("Owned path crosses a symlink: ", current)
  }
  path
}
rr_remove <- function(path, root) {
  rr_owned_path(path, root)
  if (unlink(path, recursive = TRUE, force = TRUE, expand = FALSE) != 0L || file.exists(path) || dir.exists(path)) rr_stop("Owned cleanup failed: ", path)
  invisible(TRUE)
}

rr_process <- function(handle = ps::ps_handle()) list(pid = ps::ps_pid(handle), birth = sprintf("%.6f", as.numeric(ps::ps_create_time(handle))))
rr_alive <- function(item) {
  if (!is.list(item) || !is.numeric(item$pid) || length(item$pid) != 1L || item$pid <= 0 ||
      !is.character(item$birth) || length(item$birth) != 1L || !rr_match("[0-9]+[.][0-9]{6}", item$birth)) rr_stop("Invalid process identity.")
  tryCatch({
    handle <- ps::ps_handle(item$pid, as.POSIXct(as.numeric(item$birth), origin = "1970-01-01", tz = "UTC"))
    ps::ps_is_running(handle) && !ps::ps_status(handle) %in% c("zombie", "dead")
  }, no_such_process = function(e) FALSE)
}
# Reads same-account metadata after the caller captures the handle's birth.
rr_process_details <- function(handle, group) {
  if (ps::ps_uids(handle)[["real"]] != rr_uid() || ps::ps_status(handle) %in% c("zombie", "dead")) return(NULL)
  list(parent = ps::ps_ppid(handle), group = group, env = unclass(ps::ps_environ(handle)))
}
rr_processes <- function() {
  table <- list()
  groups <- strsplit(trimws(rr_run("ps", c("-axo", "pid=,pgid="))$stdout), "\n", fixed = TRUE)[[1L]]
  groups <- lapply(groups, function(line) as.integer(strsplit(trimws(line), "[[:space:]]+")[[1L]]))
  groups <- stats::setNames(vapply(groups, `[[`, integer(1), 2L), vapply(groups, function(row) as.character(row[[1L]]), ""))
  for (pid in ps::ps_pids()) {
    identity <- NULL; handle <- NULL
    for (attempt in seq_len(3L)) {
      row <- tryCatch({
        if (is.null(identity)) {
          handle <- ps::ps_handle(pid)
          identity <- rr_process(handle)
        }
        details <- rr_process_details(handle, unname(groups[as.character(pid)]))
        if (!is.null(details)) c(list(identity = identity), details) else NULL
      }, no_such_process = function(e) NULL, error = function(e) e)
      if (!inherits(row, "error")) break
      # PID absence covers exits before birth capture. Otherwise the captured
      # identity must stop or a bounded reread must restore complete metadata.
      stopped <- tryCatch(if (is.null(identity)) !(pid %in% ps::ps_pids()) else !rr_alive(identity),
        error = function(e) FALSE)
      if (stopped) { row <- NULL; break }
      if (attempt < 3L) Sys.sleep(0.02)
    }
    if (inherits(row, "error")) rr_stop("Process ownership/environment inspection unavailable for PID ", pid, ": ", conditionMessage(row))
    if (!is.null(row)) table[[as.character(pid)]] <- row
  }
  if (is.null(table[[as.character(Sys.getpid())]])) rr_stop("Incomplete process inspection.")
  table
}
rr_owned <- function(table, record) {
  found <- names(Filter(function(row) identical(unname(row$env["MARGINPLYR_REVIEW_REQUEST"]), record$token), table))
  for (item in record$children) {
    row <- table[[as.character(item$pid)]]
    if (!is.null(row) && identical(row$identity, item)) found <- union(found, as.character(item$pid))
  }
  if (!is.null(record$group)) {
    members <- names(Filter(function(row) identical(row$group, record$group), table))
    if (length(members) && !length(intersect(members, found))) rr_stop("A process group has no confirmed owned member; shutdown is uncertain.")
    found <- union(found, members)
  }
  repeat {
    children <- names(Filter(function(row) as.character(row$parent) %in% found, table))
    next_found <- union(found, children)
    if (identical(next_found, found)) break
    found <- next_found
  }
  found
}
rr_boot <- function() {
  value <- if (Sys.info()[["sysname"]] == "Darwin") rr_run("/usr/sbin/sysctl", c("-n", "kern.bootsessionuuid"))$stdout else
    rr_text("/proc/sys/kernel/random/boot_id")
  value <- tolower(trimws(value))
  if (!rr_match("[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}", value)) rr_stop("OS boot identity unavailable.")
  value
}
rr_child_env <- function(values = Sys.getenv()) {
  for (name in names(rr_metadata)) {
    if (name %in% names(values) && rr_match(rr_metadata[[name]], values[[name]])) values <- values[names(values) != name]
  }
  values
}

# Compile the pinned signal receiver during setup, before entering waiting.
# Its source and output are request-owned and removed after unloading it.
rr_signals <- function(source, directory, environment = Sys.getenv()) {
  rr_mkdir(directory)
  name <- "review_ready_signals"
  file <- file.path(directory, paste0(name, ".c"))
  if (!file.copy(source, file)) rr_stop("Pinned signal receiver unavailable.")
  library <- file.path(directory, paste0(name, .Platform$dynlib.ext))
  environment <- c(environment, R_MAKEVARS_USER = "/dev/null", R_MAKEVARS_SITE = "/dev/null")
  environment <- environment[!duplicated(names(environment), fromLast = TRUE)]
  rr_run(file.path(R.home("bin"), "R"), c("CMD", "SHLIB", "-o", library, file), directory, environment)
  dll <- dyn.load(library)
  methods <- getDLLRegisteredRoutines(dll)$.Call
  .Call(methods$elapsed_time)
  .Call(methods$install_signals)
  list(poll = function() .Call(methods$poll_signals), clock = function() .Call(methods$elapsed_time), close = function() {
    .Call(methods$restore_signals)
    dyn.unload(library)
  })
}
