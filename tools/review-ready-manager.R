# Request lifecycle and Full admission for the repository-owned R runner.

# Validates ownership independently of wall-clock age. The collector and stage
# planner separately refuse future or otherwise untrustworthy ages.
rr_read_record <- function(path) {
  envelope <- rr_read_json(path)
  if (!is.list(envelope$record) || !is.character(envelope$checksum) ||
      !identical(envelope$checksum, rr_digest(envelope$record))) rr_stop("Damaged or unsupported request record; use a compatible runner: ", path)
  record <- envelope$record
  if (!identical(record$version, 3L) || !identical(record$protocol, rr_protocol)) rr_stop("Unknown request version; use a compatible runner for recovery: ", path)
  valid <- is.list(record) && rr_match("[0-9a-f]{32}", record$id) && identical(record$id, basename(dirname(path))) &&
    rr_match("[0-9a-f]{32}", record$token) && rr_match("[0-9a-f]{40}", record$sha) &&
    is.character(record$root) && startsWith(record$root, "/") &&
    is.character(record$boot) && rr_match("[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}", record$boot) &&
    record$state %in% c("preparing", "waiting", "acquiring", "running", "promoting", "cancelling", "recovery", "terminal") &&
    record$capacity %in% c("Full", "exempt") && is.logical(record$owned_capacity) && is.logical(record$may_own_full) &&
    is.numeric(record$epoch) && record$epoch >= 1L && is.logical(record$authorized) && is.logical(record$gate_started) &&
    is.list(record$children) && is.list(record$selection) && identical(record$selection$submitted, record$sha) &&
    record$selection$path %in% c("Full", "Documentation", "Repository") &&
    identical(record$workspace, file.path(dirname(path), "work")) &&
    (is.null(record$group) || (is.numeric(record$group) && length(record$group) == 1L && record$group > 0L)) &&
    (!record$owned_capacity || (record$may_own_full && record$capacity == "Full")) &&
    (record$capacity != "exempt" || !record$may_own_full) &&
    (record$state != "running" || record$capacity != "Full" || record$owned_capacity) &&
    (record$state != "terminal" || (identical(record$cleanup, "confirmed") && rr_valid_time(record$finished, Inf) &&
      is.numeric(record$outcome) && length(record$outcome) == 1L && is.finite(record$outcome))) &&
    identical(rr_full_required(record$selection), record$capacity == "Full") &&
    setequal(names(record$selection$plan$units), rr_required_units(record$selection))
  if (!isTRUE(valid)) rr_stop("Invalid request ownership metadata: ", path)
  rr_alive(record$manager)
  for (item in record$children) rr_alive(item)
  for (entry in record$selection$plan$units) {
    if (!entry$action %in% c("execute", "reuse")) rr_stop("Invalid execution plan.")
    if (entry$action == "reuse" && (!rr_match("[0-9a-f]{32}", entry$request) ||
        !rr_match("[0-9a-f]{40}", entry$submitted) || !entry$unit %in% rr_units)) rr_stop("Invalid execution origin.")
  }
  record
}
rr_requests <- function(state, active = TRUE) {
  root <- file.path(state, "requests")
  paths <- if (dir.exists(root)) list.dirs(root, recursive = FALSE, full.names = TRUE) else character()
  results <- lapply(paths, function(directory) {
    path <- file.path(directory, "record.json")
    tryCatch(list(path = path, record = rr_read_record(path)), error = function(e) list(path = path, error = conditionMessage(e)))
  })
  if (file.exists(file.path(state, "request.json"))) results <- c(results, list(list(
    path = file.path(state, "request.json"), id = "legacy",
    error = "Legacy Full ownership requires the installed generation's compatible control command.")))
  Filter(function(item) !active || !is.null(item$error) || item$record$state != "terminal", results)
}

rr_recover <- function(state, request, automatic = FALSE) {
  candidates <- rr_requests(state)
  ids <- vapply(candidates, function(item) if (is.null(item$id)) basename(dirname(item$path)) else item$id, "")
  if (is.null(request) && !length(ids)) {
    rr_say("No unfinished request to recover. No check was run.")
    return(0L)
  }
  if (is.null(request) || !rr_match("[0-9a-f]{32}", request) ||
      !file.exists(file.path(state, "requests", request, "record.json"))) {
    rr_say("Recovery requires one explicit known request ID; candidates: ", rr_json(as.list(ids)), ". No check was run; no state changed.")
    return(4L)
  }
  directory <- rr_owned_path(file.path(state, "requests", request), state)
  path <- file.path(directory, "record.json")
  record <- rr_read_record(path)
  if (record$state == "terminal") {
    rr_say("Request ", request, " is already terminal. No check was run; no state changed.")
    return(0L)
  }
  lock <- rr_lock(file.path(directory, "manager.lock"))
  if (is.null(lock)) {
    if (!automatic) rr_say("An active manager holds request ownership. No check was run.")
    return(4L)
  }
  on.exit(filelock::unlock(lock), add = TRUE)
  capacity <- if (record$may_own_full) rr_lock(file.path(state, "admission.lock")) else NULL
  if (record$may_own_full && is.null(capacity)) {
    if (!automatic) rr_say("Full capacity is held; recovery refused. No check was run.")
    return(4L)
  }
  on.exit(if (!is.null(capacity)) filelock::unlock(capacity), add = TRUE)
  if (identical(record$boot, rr_boot())) {
    for (observation in 1:2) {
      if (rr_alive(record$manager) || length(rr_owned(rr_processes(), record))) {
        if (!automatic) rr_say("Recovery refused: owned processing remains for ", request, ". No check was run.")
        return(4L)
      }
      if (observation == 1L) Sys.sleep(0.2)
    }
  }
  if (is.null(record$gate_outcome)) record$gate_outcome <- 130L
  rr_write(path, record)
  rr_observe(state, path, record, rr_worker_result(record))
  rr_cleanup(record, dirname(path))
  record$state <- "terminal"; record$outcome <- 130L; record$cleanup <- "confirmed"
  record$owned_capacity <- FALSE; record$finished <- rr_now()
  rr_write(path, record)
  rr_say("Recovered interrupted request ", request, "; confirmed shutdown and cleanup. No check was run.")
  0L
}
rr_auto_recover <- function(state) {
  for (item in rr_requests(state)) {
    if (!is.null(item$error)) { rr_say("Protected recovery state: ", item$path, "; ", item$error); next }
    if (!rr_alive(item$record$manager)) tryCatch(rr_recover(state, item$record$id, TRUE),
      error = function(e) rr_say("Automatic recovery deferred: ", item$record$id, "; ", conditionMessage(e)))
  }
}
rr_cleanup <- function(record, directory) {
  for (child in c("work", "tmp", "cache")) rr_remove(file.path(directory, child), directory)
}

rr_selection_transport <- function(record) {
  selection <- record$selection
  paste0("Path: ", selection$path, "\nCommit: ", record$sha, "\nRequest: ", record$token,
    "\nProtocol: 4\nCapacity: ", record$capacity, "\nVerifiers: ", paste(unlist(selection$verifiers), collapse = " "),
    "\nBase: ", if (is.null(selection$base)) "" else selection$base,
    "\nReused: ", paste(rr_reused(selection), collapse = " "), "\n")
}

# The private file mailbox uses complete atomic messages authenticated by the
# request token. An epoch and one-use message ID reject stale/replayed replies.
rr_rpc <- function(operation, reason = "") {
  path <- Sys.getenv("MARGINPLYR_REVIEW_RECORD")
  record <- rr_read_record(path)
  token <- Sys.getenv("MARGINPLYR_REVIEW_REQUEST")
  if (!identical(token, record$token) || !identical(Sys.getenv("MARGINPLYR_REVIEW_EPOCH"), as.character(record$epoch))) rr_stop("Mismatched supervisor channel.")
  id <- rr_id()
  directory <- file.path(record$workspace, "control")
  message <- list(protocol = rr_protocol, request = record$id, sha = record$sha, epoch = record$epoch,
    expected = "running", id = id, operation = operation, reason = reason)
  if (operation == "authorize") message$transport <- rr_hash(rr_read(Sys.getenv("MARGINPLYR_REVIEW_SELECTION")))
  rr_write(file.path(directory, paste0(id, ".request.json")), rr_seal(message, charToRaw(token)))
  library <- rr_owned_path(file.path(record$workspace, "signals", paste0("review_ready_signals", .Platform$dynlib.ext)), record$workspace)
  dll <- dyn.load(library)
  on.exit(dyn.unload(library), add = TRUE)
  method <- getDLLRegisteredRoutines(dll)$.Call$elapsed_time
  clock <- function() .Call(method)
  deadline <- clock() + 15
  reply <- file.path(directory, paste0(id, ".reply.json"))
  while (!file.exists(reply)) {
    if (!rr_alive(record$manager) || clock() > deadline) rr_stop("Supervisor reply unavailable.")
    Sys.sleep(0.05)
  }
  value <- rr_unseal(rr_read_json(reply), charToRaw(token))
  if (!identical(value$id, id) || !isTRUE(value$ok)) rr_stop("Supervisor refused operation: ", value$error)
  value
}
rr_control <- function(path, record, cancelled) {
  directory <- file.path(record$workspace, "control")
  files <- list.files(directory, "[.]request[.]json$", full.names = TRUE)
  promote <- FALSE
  for (file in files) {
    id <- sub("[.]request[.]json$", "", basename(file))
    reply <- list(id = id, ok = FALSE, error = "Invalid supervisor request.")
    tryCatch({
      message <- rr_unseal(rr_read_json(file), charToRaw(record$token))
      if (!identical(message$id, id) || !identical(message$protocol, rr_protocol) ||
          !identical(message$request, record$id) || !identical(message$sha, record$sha) ||
          !identical(message$epoch, record$epoch) || !identical(message$expected, "running") ||
          record$state != "running" || cancelled || file.exists(file.path(directory, paste0(id, ".reply.json")))) rr_stop("Stale, replayed or cancelled supervisor request.")
      operation <- message$operation
      if (operation == "begin-gate") {
        if (record$gate_started || isTRUE(record$selection$roxygen_pending)) rr_stop("Gate start is not eligible.")
        record$gate_started <- TRUE
      } else if (operation == "authorize") {
        if (!record$gate_started || record$authorized || (rr_full_required(record$selection) && !record$owned_capacity) ||
            !identical(message$transport, rr_hash(charToRaw(rr_selection_transport(record))))) rr_stop("Gate authorization refused.")
        record$authorized <- TRUE
        reply$path <- record$selection$path; reply$capacity <- record$capacity
      } else if (operation == "resolve-documentation") {
        if (record$gate_started || !isTRUE(record$selection$roxygen_pending) ||
            !identical(rr_text(file.path(record$workspace, "roxygen-path")), "Documentation\n")) rr_stop("Documentation proof refused.")
        record$selection$roxygen_pending <- FALSE
      } else if (operation == "discard-reuse") {
        if (record$gate_started || record$capacity != "Full" || !record$owned_capacity) rr_stop("Discarding reuse requires pre-gate Full ownership.")
        record$selection$plan <- rr_execute_plan(record$selection, "Reuse invalidated before validation")
      } else if (operation == "promote") {
        if (record$gate_started || record$capacity != "exempt" || !is.character(message$reason) ||
            !nzchar(message$reason) || nchar(message$reason) > 2000L) rr_stop("Promotion refused.")
        record$state <- "promoting"
        record$promotions <- c(record$promotions, list(list(epoch = record$epoch, reason = message$reason,
          at = rr_now(), source = record$selection$path, target = "Full")))
        promote <- TRUE
      } else rr_stop("Unsupported supervisor operation.")
      rr_write(path, record)
      reply$ok <- TRUE; reply$error <- NULL
    }, error = function(e) { reply$error <<- conditionMessage(e) })
    rr_write(file.path(directory, paste0(id, ".reply.json")), rr_seal(reply, charToRaw(record$token)))
    unlink(file)
  }
  list(record = record, promote = promote)
}

rr_acquire <- function(state, path, record, cancelled, clock) {
  record$state <- "waiting"
  rr_write(path, record)
  displayed <- -Inf
  repeat {
    if (cancelled()) return(list(record = record, status = 130L))
    current <- tryCatch(rr_identity(record$root), error = function(e) NULL)
    if (is.null(current) || !identical(current$sha, record$sha)) return(list(record = record, status = 3L))
    lock <- rr_lock(file.path(state, "admission.lock"))
    if (!is.null(lock)) {
      blocker <- Filter(function(item) !is.null(item$error) ||
        (item$record$id != record$id && item$record$may_own_full), rr_requests(state))
      if (length(blocker)) {
        filelock::unlock(lock)
        rr_say("Full recovery required: ", blocker[[1L]]$path)
        return(list(record = record, status = 4L))
      }
      record$state <- "acquiring"; record$may_own_full <- TRUE
      rr_write(path, record)
      record$owned_capacity <- TRUE
      rr_write(path, record)
      return(list(record = record, lock = lock, status = 0L))
    }
    if (clock() - displayed >= 5) {
      rr_say("Waiting for Review-ready capacity; request ", record$id, "; commit ", record$sha)
      displayed <- clock()
    }
    Sys.sleep(0.15)
  }
}

rr_supervise <- function(path, record, cancelled, environment, clock) {
  rr_check_authority(record)
  record$state <- "running"; record$authorized <- FALSE
  rr_write(path, record)
  directory <- dirname(path)
  environment[["MARGINPLYR_REVIEW_EPOCH"]] <- as.character(record$epoch)
  output <- file.path(directory, "logs", paste0("epoch-", record$epoch, ".log"))
  entry <- file.path(record$workspace, "authority", "tools", "review-ready.R")
  job <- processx::process$new(file.path(R.home("bin"), "Rscript"), c(entry, "--worker", path),
    env = environment, stdout = output, stderr = "2>&1", cleanup = TRUE, cleanup_tree = FALSE,
    supervise = TRUE, wd = record$root)
  on.exit(if (job$is_alive()) job$kill(), add = TRUE)
  record$group <- job$get_pid()
  record$children <- c(record$children, list(rr_process(job$as_ps_handle())))
  rr_write(path, record)
  rr_say("Admitted Review-ready commit: ", record$sha, "; request: ", record$id,
    "; path: ", record$selection$path, "; capacity: ", record$capacity)
  offset <- 0; stopping <- NULL; promoting <- FALSE
  drain <- function() {
    if (!file.exists(output)) return()
    bytes <- rr_read(output)
    if (length(bytes) > offset) cat(rawToChar(bytes[seq.int(offset + 1L, length(bytes))]))
    offset <<- length(bytes)
    flush.console()
  }
  repeat {
    drain()
    table <- rr_processes()
    owned <- rr_owned(table, record)
    identities <- lapply(table[owned], `[[`, "identity")
    for (item in identities) {
      if (!any(vapply(record$children, identical, logical(1), y = item))) record$children <- c(record$children, list(item))
    }
    rr_write(path, record)
    control <- rr_control(path, record, cancelled())
    record <- control$record; promoting <- promoting || control$promote
    if ((cancelled() || promoting) && is.null(stopping)) {
      stopping <- clock()
      record$state <- if (cancelled()) "cancelling" else "promoting"
      rr_write(path, record)
      rr_say("Stopping owned processing; request: ", record$id)
    }
    if (!is.null(stopping)) {
      signal <- if (clock() - stopping < 2) 15L else 9L
      for (item in identities) if (rr_alive(item)) tryCatch(ps::ps_send_signal(ps::ps_handle(item$pid,
        as.POSIXct(as.numeric(item$birth), origin = "1970-01-01", tz = "UTC")), signal), no_such_process = function(e) NULL)
    }
    if (!job$is_alive() && !length(owned) && !length(rr_owned(rr_processes(), record))) {
      job$wait(); drain()
      status <- if (cancelled()) 130L else if (promoting) 0L else job$get_exit_status()
      if (status == 0L && !promoting && !record$authorized) status <- 2L
      return(list(record = record, status = as.integer(if (status < 0L) 128L - status else status), promote = promoting && !cancelled()))
    }
    if (!is.null(stopping) && clock() - stopping > 10) rr_stop("Owned shutdown is unconfirmed; recovery required.")
    Sys.sleep(0.1)
  }
}

rr_worker_result <- function(record) {
  path <- file.path(record$workspace, "worker-result.json")
  if (file.exists(path)) return(rr_read_json(path))
  before <- file.path(record$workspace, "worker-before.json")
  report <- file.path(record$workspace, "stage-report.json")
  list(before = if (file.exists(before)) rr_read_json(before) else NULL, after = NULL,
    report = if (file.exists(report)) rr_read_json(report) else list(), repository = record$repository)
}
rr_execution_entries <- function(record, worker) {
  units <- list()
  report <- worker$report
  if (is.null(worker$before) || !identical(report$request, record$token)) return(units)
  for (unit in names(report$units)) {
    status <- report$units[[unit]]
    if (!unit %in% names(worker$before$sources) || !status %in% c("passed", "running") ||
        !identical(record$selection$plan$units[[unit]]$action, "execute")) next
    invalid <- !rr_same(worker$before, worker$after) ||
      (startsWith(unit, "build-") && status == "passed" && !rr_valid_check(report$check, unit))
    entry <- list(source = worker$before$sources[[unit]], environment = worker$before$environment$digest,
      environment_contract = worker$before$environment,
      status = if (invalid) "unavailable" else if (status == "passed") "passed" else "failed")
    if (startsWith(unit, "build-")) { entry$check <- report$check; entry$timestamps <- report$timestamps }
    units[[unit]] <- entry
  }
  units
}
rr_publish <- function(state, record, payload) {
  rr_with_lock(file.path(state, "evidence.lock"), {
    value <- rr_store_read(state, create = TRUE)
    history <- rr_store_history(state, value)
    for (wanted in names(payload$reused)) {
      reference <- payload$reused[[wanted]]
      origins <- Filter(function(entry) entry$payload$kind == "evidence" && entry$payload$request == reference$request, history)
      if (length(origins) != 1L || !identical(origins[[1L]]$payload$submitted, reference$submitted) ||
          !rr_origin_valid(origins[[1L]], reference$unit, wanted, history)) rr_stop("Pinned execution origin expired or was superseded before issuance.")
    }
    duplicate <- Filter(function(entry) entry$payload$request == record$id && entry$payload$kind == payload$kind, value$records)
    if (length(duplicate)) return(duplicate[[1L]]$payload)
    payload$sequence <- value$next_sequence
    value$next_sequence <- value$next_sequence + 1L
    value$records <- c(value$records, list(list(payload = payload,
      available = as.list(if (payload$kind == "evidence") names(payload$units) else character()))))
    tryCatch(rr_store_write(state, value), error = function(e) {
      stop(errorCondition(paste("Interrupted evidence publication:", conditionMessage(e)),
        class = "marginplyr_review_publication_error"))
    })
    payload
  })
}
rr_payload <- function(record, kind, units, reused = list()) {
  list(schema = rr_schema, kind = kind, request = record$id, user = rr_uid(), repository = record$repository,
    submitted = record$sha, selection = record$selection, supervisor = record$launcher,
    finished = if (kind == "evidence") record$finished else rr_now(),
    outcome = if (kind == "evidence") record$outcome else record$gate_outcome, units = units, reused = reused)
}
rr_observe <- function(state, path, record, worker) {
  if (!record$authorized) return(NULL)
  units <- rr_execution_entries(record, worker)
  if (is.null(worker$before) || !identical(worker$report$request, record$token)) {
    executed <- names(Filter(function(unit) unit$action == "execute", record$selection$plan$units))
    units <- stats::setNames(lapply(executed, function(unit) list(status = "unavailable", source = NULL, environment = NULL)), executed)
  }
  rr_publish(state, record, rr_payload(record, "observation", units))
}
rr_finish <- function(state, path, record, status) {
  worker <- rr_worker_result(record)
  record$gate_outcome <- status
  rr_write(path, record)
  observation <- tryCatch(rr_observe(state, path, record, worker), error = function(e) {
    if (inherits(e, "marginplyr_review_publication_error")) stop(e)
    rr_say("Stage observation unavailable: ", conditionMessage(e))
    NULL
  })
  if (status == 0L && record$authorized && is.null(observation) && length(rr_reused(record$selection))) status <- 1L
  completed <- unlist(worker$report$completed, use.names = FALSE)
  artifacts <- setdiff(unlist(record$selection$verifiers), if ("generation" %in% rr_reused(record$selection)) "tools/review-ready-generated.R")
  required <- c("Package spelling", "jarl", "package-aware lintr", if (length(artifacts)) paste("Artifact verification:", artifacts))
  if (record$selection$path != "Repository") required <- c(required, "Test dependency syntax", "Required test Suggests")
  executed <- names(Filter(function(unit) unit$action == "execute", record$selection$plan$units))
  complete <- identical(worker$report$request, record$token) && all(required %in% completed) &&
    all(vapply(executed, function(unit) identical(worker$report$units[[unit]], "passed"), logical(1)))
  if (status == 0L && !complete && length(rr_reused(record$selection))) status <- 1L
  rr_cleanup(record, dirname(path))
  record$state <- "terminal"; record$cleanup <- "confirmed"; record$outcome <- status
  record$finished <- rr_now(); record$owned_capacity <- FALSE
  rr_write(path, record)
  if (!is.null(observation) && !is.null(worker$before) && rr_same(worker$before, worker$after)) {
    units <- rr_execution_entries(record, worker)
    for (name in names(units)) units[[name]]$observation_sequence <- observation$sequence
    if (status != 0L || complete) tryCatch({
      payload <- rr_payload(record, "evidence", units, Filter(function(unit) unit$action == "reuse", record$selection$plan$units))
      payload$observation <- observation$sequence; payload$completed <- worker$report$completed; payload$omitted <- worker$report$omitted
      rr_publish(state, record, payload)
    }, error = function(e) {
      if (inherits(e, "marginplyr_review_publication_error")) stop(e)
      rr_say("Terminal evidence unavailable: ", conditionMessage(e))
      if (status == 0L && length(rr_reused(record$selection))) { status <<- 1L; record$outcome <<- 1L; rr_write(path, record) }
    })
  } else if (status == 0L && length(rr_reused(record$selection))) {
    status <- 1L; record$outcome <- status; rr_write(path, record)
  }
  rr_say("Review-ready terminal outcome: ", status, "; request: ", record$id, "; checked commit: ", record$sha,
    "; selected path: ", record$selection$path, "; capacity: ", record$capacity, "; cleanup: confirmed")
  status
}

# Load the accepted manager directly from pinned Git objects. No source file
# from the candidate can grant limited-workload capabilities to itself.
rr_authority <- function(root, main) {
  environment <- new.env(parent = globalenv())
  for (name in c("core", "registry", "evidence", "manager")) {
    path <- paste0("tools/review-ready-", name, ".R")
    bytes <- tryCatch(rr_git_raw(root, "show", paste0(main, ":", path)), error = function(e) {
      rr_stop("Pinned local main ", main, " has no accepted R manager (", path,
        "). Incorporate the runner implementation into main before cutover; use the installed CLI for bootstrap verification.")
    })
    connection <- textConnection(rawToChar(bytes))
    tryCatch(source(connection, local = environment), finally = close(connection))
  }
  if (!identical(environment$rr_protocol, rr_protocol)) rr_stop("Pinned main manager is incompatible; update or backport the supported runner.")
  environment
}

# Copy accepted helpers from blobs, bypassing archive attributes and filters.
# A materialized worker must execute the generation the manager pinned.
rr_materialize_authority <- function(root, main, directory) {
  patterns <- unique(c(rr_runner_sources, unlist(rr_core_sources), names(rr_stage_contracts),
    unlist(lapply(rr_stage_contracts, function(stage) stage[[2L]]))))
  capabilities <- rr_capabilities(root, main, patterns)
  tree <- rr_tree(root, main)[names(capabilities)]
  blobs <- rr_blobs(root, vapply(tree, `[[`, "", "oid"))
  manifest <- list()
  rr_mkdir(directory)
  for (name in names(tree)) {
    path <- file.path(directory, name)
    rr_mkdir(dirname(path))
    rr_write(path, blobs[[tree[[name]]$oid]], raw = TRUE)
    Sys.chmod(path, if (tree[[name]]$mode == "100755") "0755" else "0644")
    manifest[[name]] <- list(mode = tree[[name]]$mode, digest = rr_hash(blobs[[tree[[name]]$oid]]))
  }
  manifest
}
rr_check_authority <- function(record) {
  directory <- file.path(record$workspace, "authority")
  if (!is.list(record$authority) || !all(rr_runner_sources %in% names(record$authority))) rr_stop("Pinned manager manifest is unavailable.")
  for (name in names(record$authority)) {
    path <- rr_owned_path(file.path(directory, name), directory)
    expected <- record$authority[[name]]
    mode <- if (expected$mode == "100755") strtoi("755", 8L) else strtoi("644", 8L)
    if (!identical(as.integer(file.info(path)$mode), mode) || !identical(rr_hash(rr_read(path)), expected$digest)) rr_stop("Pinned manager source changed: ", name)
  }
  invisible()
}

rr_entry <- function(args, runner_root) {
  rr_setup()
  state <- rr_mkdir(rr_state())
  if (identical(args, "--gate-proof")) {
    proof <- rr_rpc("authorize")
    rr_say("Path: ", proof$path, "\nCapacity: ", proof$capacity)
    return(0L)
  }
  if (length(args) == 2L && args[[1L]] == "--worker" && nzchar(Sys.getenv("MARGINPLYR_REVIEW_REQUEST"))) return(rr_worker(args[[2L]]))
  if (identical(args, "--status")) {
    items <- lapply(rr_requests(state), function(item) {
      if (!is.null(item$error)) return(list(id = if (is.null(item$id)) basename(dirname(item$path)) else item$id,
        state = "recovery", error = item$error, capacity = "unknown; Full protected"))
      record <- item$record
      list(id = record$id, sha = record$sha, root = record$root, state = if (rr_alive(record$manager)) record$state else "recovery",
        capacity = record$capacity, selection = record$selection, cleanup = record$cleanup)
    })
    rr_say(rr_json(list(requests = items, operation = "status only; No check was run.")))
    return(0L)
  }
  if (length(args) %in% 1:2 && args[[1L]] == "--recover")
    return(rr_recover(state, if (length(args) == 2L) args[[2L]] else NULL))
  if (length(args)) rr_stop("Use no arguments, --status, or --recover REQUEST. No check was run.")
  context <- c("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE", "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_NAMESPACE", "GIT_REPLACE_REF_BASE", "GIT_ATTR_SOURCE", "GIT_GRAFT_FILE")
  if (any(nzchar(Sys.getenv(context)))) rr_stop("Cannot obtain the target's accepted main authority under Git context overrides; clear them before checking.")
  identity <- rr_identity()
  if (!identical(normalizePath(runner_root, mustWork = TRUE), identity$root))
    rr_stop("Invoke the target worktree's tracked tools/review-ready.R from its root; no check was run.")
  main <- tryCatch(rr_git(identity$root, "rev-parse", "--verify", "refs/heads/main^{commit}"),
    error = function(e) rr_stop("Missing local main authority. Update or restore local main; no check was run."))
  authority <- rr_authority(identity$root, main)
  authority$rr_main(identity, main)
}

rr_main <- function(identity, main) {
  state <- rr_mkdir(rr_state())
  delivery <- file.path(state, "delivery.json")
  if (!file.exists(delivery) || !identical(rr_read_json(delivery)$protocol, rr_protocol))
    rr_stop("R-runner cutover is not established. Drain the installed CLI and run Rscript tools/migrate-review-ready.R after main accepts this implementation.")
  # The old generation's journals must be drained before normal R requests.
  if (file.exists(file.path(state, "request.json"))) rr_stop("Legacy Full ownership remains; drain/recover it with the installed CLI before cutover.")
  rr_auto_recover(state)
  tryCatch(rr_maintenance(state), error = function(e) rr_say("Evidence maintenance unavailable; reuse disabled: ", conditionMessage(e)))
  id <- rr_id()
  token <- rr_id(); boot <- rr_boot(); manager <- rr_process(); started <- rr_now()
  directory <- rr_mkdir(file.path(state, "requests", id))
  lock <- rr_lock(file.path(directory, "manager.lock"))
  if (is.null(lock)) rr_stop("Cannot acquire new request ownership.")
  on.exit(filelock::unlock(lock), add = TRUE)
  root <- identity$root; sha <- identity$sha
  selection <- list(submitted = sha, main = main, base = NULL, path = "Full",
    reasons = list("Preparing accepted workload registration."), verifiers = rr_reference_checks)
  selection$plan <- rr_execute_plan(selection)
  record <- list(version = 3L, protocol = rr_protocol, id = id, token = token, sha = sha, root = root,
    boot = boot, manager = manager, group = NULL, children = list(), started = started,
    selection = selection, state = "preparing", capacity = "Full", may_own_full = FALSE, owned_capacity = FALSE,
    epoch = 1L, authorized = FALSE, gate_started = FALSE, promotions = list(), workspace = file.path(directory, "work"),
    cleanup = "pending", repository = NULL, main = main, core_changed = FALSE, launcher = NULL)
  path <- file.path(directory, "record.json")
  # Even setup child processes have recovery metadata before they can start.
  rr_write(path, record)
  rr_say("Review-ready request: ", id, "; management record: ", path)
  capacity <- NULL; signals <- NULL; finalized <- FALSE
  on.exit({
    if (!is.null(signals)) signals$close()
    if (!finalized) {
      latest <- tryCatch(rr_read_record(path), error = function(e) NULL)
      if (!is.null(latest)) {
        latest$state <- "recovery"; latest$cleanup <- "unconfirmed"
        try(rr_write(path, latest), silent = TRUE)
      }
      rr_say("Protected unfinished request: ", id, "; recover explicitly after confirmed shutdown.")
    }
    if (!is.null(capacity)) filelock::unlock(capacity)
  }, add = TRUE)
  for (child in c("logs", "diagnostics", "work", "tmp", "cache")) rr_mkdir(file.path(directory, child))
  previous_options <- options(marginplyr.review.tmp = file.path(directory, "tmp"))
  on.exit(options(previous_options), add = TRUE)
  authority <- file.path(record$workspace, "authority")
  environment <- rr_render_env(directory)
  environment[["MARGINPLYR_REVIEW_REQUEST"]] <- record$token
  environment[["MARGINPLYR_REVIEW_RECORD"]] <- path
  environment[["MARGINPLYR_REVIEW_COMMIT"]] <- sha
  environment[["MARGINPLYR_REVIEW_SUPERVISOR"]] <- file.path(authority, "tools/review-ready.R")
  previous_environment <- options(marginplyr.review.env = environment)
  on.exit(options(previous_environment), add = TRUE)
  selection <- rr_select(root, sha, main)
  selection$plan <- rr_execute_plan(selection)
  trust <- unique(c(rr_runner_sources, unlist(rr_core_sources), "tools/test-review-ready-runner.R"))
  core_changed <- !identical(rr_capabilities(root, main, trust), rr_capabilities(root, sha, trust))
  if (core_changed) {
    selection$path <- "Full"; selection$roxygen_pending <- FALSE
    selection$verifiers <- as.list(union(unlist(selection$verifiers), "tools/test-review-ready-runner.R"))
    selection$reasons <- c(selection$reasons, list("Trust core differs: pinned main owns the outer Full supervisor and gate."))
    selection$plan <- rr_execute_plan(selection, "Changed trust core")
  }
  record$selection <- selection
  record$capacity <- if (rr_full_required(selection)) "Full" else "exempt"
  record$core_changed <- core_changed
  record$repository <- rr_repository(root)
  record$launcher <- rr_digest(rr_capabilities(root, main, rr_runner_sources))
  record$authority <- rr_materialize_authority(root, main, authority)
  rr_write(path, record)
  rr_check_authority(record)
  # The accepted native source is setup-only and has no package/check workload.
  signals <- rr_signals(file.path(authority, "tools/review-ready-signals.c"), file.path(record$workspace, "signals"), environment)
  parent <- rr_process(ps::ps_handle(ps::ps_ppid()))
  cancelled <- function() signals$poll() != 0L || !rr_alive(parent)
  if (!core_changed) {
    # Probe in the same private environment as the worker. Never load marginplyr
    # or run a verifier while deciding actual-work capacity.
    previous <- Sys.getenv()
    changed <- names(environment)
    on.exit({ Sys.unsetenv(setdiff(changed, names(previous))); do.call(Sys.setenv, as.list(previous[intersect(changed, names(previous))])) }, add = TRUE)
    do.call(Sys.setenv, as.list(environment))
    record$selection$plan <- rr_plan(root, sha, record$selection, state)
    do.call(Sys.setenv, as.list(previous[intersect(changed, names(previous))]))
    Sys.unsetenv(setdiff(changed, names(previous)))
    reason <- rr_eligibility(root, sha, record$selection, main)
    if (!is.null(reason)) {
      record$selection$path <- "Full"; record$selection$roxygen_pending <- FALSE
      record$selection$reasons <- c(record$selection$reasons, list(reason))
      record$selection$plan <- rr_execute_plan(record$selection, reason)
    }
  }
  record$capacity <- if (rr_full_required(record$selection)) "Full" else "exempt"
  rr_write(path, record)
  rr_say("Review-ready selection: ", rr_json(record$selection))
  rr_mkdir(file.path(record$workspace, "control"))
  repeat {
    status <- 0L
    if (record$capacity == "Full") {
      admitted <- rr_acquire(state, path, record, cancelled, signals$clock)
      record <- admitted$record; capacity <- admitted$lock; status <- admitted$status
    } else {
      current <- tryCatch(rr_identity(root), error = function(e) NULL)
      if (is.null(current) || !identical(current$sha, sha)) status <- 3L
      if (cancelled()) status <- 130L
    }
    if (status == 0L) {
      result <- rr_supervise(path, record, cancelled, environment, signals$clock)
      record <- result$record; status <- result$status
      if (result$promote) {
        record$selection$path <- "Full"; record$selection$roxygen_pending <- FALSE
        record$selection$plan <- rr_execute_plan(record$selection, "Authenticated Full promotion")
        record$capacity <- "Full"; record$state <- "waiting"; record$epoch <- record$epoch + 1L
        record$group <- NULL; record$children <- list(); record$authorized <- FALSE; record$gate_started <- FALSE
        rr_write(path, record)
        rr_remove(file.path(record$workspace, "source"), directory)
        next
      }
    }
    signals$close(); signals <- NULL
    status <- rr_finish(state, path, record, status)
    finalized <- TRUE
    tryCatch(rr_maintenance(state), error = function(e) rr_say("Old history maintenance deferred: ", conditionMessage(e)))
    return(status)
  }
}

rr_worker <- function(path) {
  record <- rr_read_record(path)
  if (!identical(record$token, Sys.getenv("MARGINPLYR_REVIEW_REQUEST")) ||
      !identical(as.character(record$epoch), Sys.getenv("MARGINPLYR_REVIEW_EPOCH")) ||
      !identical(record$manager$pid, ps::ps_ppid()) || record$state != "running" ||
      !rr_alive(record$manager) || (record$capacity == "Full" && !record$owned_capacity)) rr_stop("Worker lacks matching live supervision.")
  rr_check_authority(record)
  state <- dirname(dirname(dirname(path)))
  checkout <- file.path(record$workspace, "source")
  rr_run("git", c("clone", "--quiet", "--shared", "--no-checkout", record$root, checkout))
  rr_git(checkout, "checkout", "--quiet", "--detach", record$sha)
  rr_git(checkout, "remote", "set-url", "origin", record$repository)
  selection <- record$selection
  rr_renderer()
  rr_normalize_environment(rr_child_env())
  if (record$capacity == "exempt") {
    reason <- rr_eligibility(checkout, record$sha, selection, record$main)
    if (!is.null(reason)) { rr_rpc("promote", reason); repeat Sys.sleep(0.1) }
  }
  if (isTRUE(selection$roxygen_pending)) {
    result <- file.path(record$workspace, "roxygen-path")
    proof <- processx::run(file.path(R.home("bin"), "Rscript"),
      c("tools/review-ready-roxygen.R", selection$base, record$sha, result), wd = checkout,
      env = rr_child_env(), echo = TRUE, error_on_status = FALSE)
    if (proof$status != 0L || !file.exists(result)) rr_stop("Documentation proof failed; no Full fallback.")
    if (identical(rr_text(result), "Full\n")) { rr_rpc("promote", "R documentation proof requires Full"); repeat Sys.sleep(0.1) }
    if (!identical(rr_text(result), "Documentation\n")) rr_stop("Invalid documentation proof result.")
    rr_rpc("resolve-documentation")
    record <- rr_read_record(path); selection <- record$selection
  }
  if (!rr_revalidate(checkout, record$sha, selection, state)) {
    rr_rpc(if (record$capacity == "Full") "discard-reuse" else "promote", "Stage evidence invalidated before gate start")
    if (record$capacity != "Full") repeat Sys.sleep(0.1)
    record <- rr_read_record(path); selection <- record$selection
  }
  units <- rr_required_units(selection)
  before <- tryCatch(if (length(units)) rr_snapshot(checkout, record$sha, units) else
      list(environment = list(digest = NULL), sources = list()), error = function(e) { rr_say("Evidence identity unavailable: ", conditionMessage(e)); NULL })
  if (length(rr_reused(selection)) && !rr_same(before, selection$plan$snapshot)) rr_stop("Evidence changed before validation.")
  if (!is.null(before)) rr_write(file.path(record$workspace, "worker-before.json"), before)
  transport <- file.path(record$workspace, "selection.dcf")
  rr_write(transport, charToRaw(rr_selection_transport(record)), raw = TRUE)
  Sys.setenv(MARGINPLYR_REVIEW_SELECTION = transport, MARGINPLYR_REVIEW_STAGE_REPORT = file.path(record$workspace, "stage-report.json"))
  rr_rpc("begin-gate")
  # The accepted outer gate owns the plan even when candidate gate code changes.
  authority <- file.path(record$workspace, "authority")
  gate <- file.path(record$workspace, "gate.R")
  code <- c(sprintf("authority <- %s", deparse(authority)),
    'for (file in c(".github/scripts/cran-note-policy.R", "tools/dependency-requirements.R", "tools/required-test-suggests.R", "tools/review-ready-progress.R", "tools/review-ready-check-lib.R")) source(file.path(authority, file))',
    sprintf('quit(status = review_ready_check_cli(character(), %s, selection = read_review_ready_selection(), diagnostic_root = %s, accepted_root = authority), save = "no")', deparse(checkout), deparse(file.path(dirname(path), "diagnostics"))))
  writeLines(code, gate)
  result <- processx::run(file.path(R.home("bin"), "Rscript"), gate, wd = checkout, env = rr_child_env(), echo = TRUE, error_on_status = FALSE)
  status <- as.integer(result$status)
  after <- tryCatch(if (length(units)) rr_snapshot(checkout, record$sha, units) else before, error = function(e) NULL)
  if (length(rr_reused(selection)) && (!rr_same(before, after) || !rr_revalidate(checkout, record$sha, selection, state))) status <- 1L
  report <- file.path(record$workspace, "stage-report.json")
  worker <- list(before = before, after = after, status = status, repository = record$repository,
    report = if (file.exists(report)) rr_read_json(report) else list())
  rr_write(file.path(record$workspace, "worker-result.json"), worker)
  status
}
