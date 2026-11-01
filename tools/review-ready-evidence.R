# Authenticated evidence storage for ADRs 0038/0039. A generation contains its
# complete inventory and keys; one pointer rename publishes the transaction.
rr_seal <- function(payload, key) {
  rr_bytes(payload)
  payload <- jsonlite::fromJSON(rr_json(payload), simplifyVector = FALSE)
  list(encoding = 2L, payload = payload, integrity = rr_hex(openssl::sha256(rr_bytes(payload), key = key)))
}

# Fresh random-key double HMAC blinds the compared MAC on every verification.
# The comparison cannot reveal a prefix of the original authenticator; see the
# authentication contract in tools/review-ready-runner.md.
rr_unseal <- function(envelope, key) {
  rr_bytes(envelope)
  if (!identical(envelope$encoding, 2L) || !is.character(envelope$integrity) ||
      !rr_match("[0-9a-f]{64}", envelope$integrity)) rr_stop("Invalid evidence authentication envelope.")
  expected <- rr_seal(envelope$payload, key)$integrity
  blind <- openssl::rand_bytes(32)
  actual_blind <- openssl::sha256(charToRaw(envelope$integrity), key = blind)
  expected_blind <- openssl::sha256(charToRaw(expected), key = blind)
  if (!identical(unclass(actual_blind), unclass(expected_blind))) rr_stop("Evidence authentication failed.")
  envelope$payload
}

rr_store_root <- function(state) file.path(state, "evidence-v2")
rr_valid_time <- function(time, now = rr_now()) is.numeric(time) && length(time) == 1L && is.finite(time) && time > 0 && time <= now

# Call only while holding evidence.lock. A missing pointer with existing
# generations is an interrupted initial publication, never an empty baseline.
rr_store_read <- function(state, create = FALSE) {
  root <- rr_store_root(state)
  pointer <- file.path(root, "current.json")
  if (!file.exists(pointer)) {
    if (dir.exists(root) && length(list.files(root, pattern = "^g-"))) {
      requests <- rr_requests(state, active = FALSE)
      if (!create || any(vapply(requests, function(item) !is.null(item$error) || isTRUE(item$record$authorized), logical(1))))
        rr_stop("Incomplete evidence publication; recovery required.")
      # With no published pointer and no authorized execution, these are only
      # interrupted initial-setup outputs; no older success can be restored.
      for (path in list.dirs(root, recursive = FALSE, full.names = TRUE)) {
        if (rr_match("g-[0-9a-f]{32}", basename(path))) rr_remove(path, root)
      }
    }
    if (!create) rr_stop("No R-runner evidence baseline.")
    return(list(schema = rr_schema, key_created = rr_now(), next_sequence = 1L, records = list(), key = openssl::rand_bytes(32)))
  }
  link <- rr_read_json(pointer)
  if (!rr_match("g-[0-9a-f]{32}", link$generation) || !rr_match("[0-9a-f]{64}", link$digest)) rr_stop("Invalid evidence generation pointer.")
  directory <- rr_owned_path(file.path(root, link$generation), root)
  path <- file.path(directory, "key.bin")
  metadata <- file.info(path)
  if (is.na(metadata$uid) || metadata$uid != rr_uid() || as.integer(metadata$mode) != strtoi("600", 8L) ||
      nzchar(Sys.readlink(path))) rr_stop("Invalid private evidence key owner/mode.")
  key <- rr_read(path)
  if (length(key) != 32L) rr_stop("Invalid private evidence key length.")
  bytes <- rr_read(file.path(directory, "index.json"))
  if (!identical(rr_hash(bytes), link$digest)) rr_stop("Incomplete evidence generation.")
  value <- rr_unseal(jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE), key)
  if (!identical(value$schema, rr_schema) || !rr_valid_time(value$key_created) ||
      !is.numeric(value$next_sequence) || value$next_sequence < 1 || !is.list(value$records)) rr_stop("Invalid evidence inventory.")
  previous <- 0
  identities <- character()
  for (entry in value$records) {
    payload <- entry$payload
    identity <- paste(payload$request, payload$kind)
    if (!rr_match("[0-9a-f]{32}", payload$request) || !payload$kind %in% c("evidence", "observation") ||
        !identical(payload$schema, rr_schema) || !identical(payload$user, rr_uid()) ||
        !is.numeric(payload$sequence) || payload$sequence <= previous || identity %in% identities ||
        !all(unlist(entry$available) %in% names(payload$units))) rr_stop("Invalid authenticated evidence provenance/order.")
    journal <- rr_read_record(file.path(state, "requests", payload$request, "record.json"))
    if (!identical(journal$sha, payload$submitted) || !identical(journal$selection, payload$selection) ||
        !identical(journal$launcher, payload$supervisor) || !isTRUE(journal$authorized)) rr_stop("Evidence differs from its authorized request.")
    if (payload$kind == "observation") {
      if (!identical(journal$gate_outcome, payload$outcome)) rr_stop("Evidence differs from its stage observation.")
    } else if (journal$state != "terminal" || journal$cleanup != "confirmed" || isTRUE(journal$owned_capacity) ||
        !identical(journal$outcome, payload$outcome) || !identical(journal$finished, payload$finished)) rr_stop("Unconfirmed terminal evidence.")
    previous <- payload$sequence
    identities <- c(identities, identity)
  }
  if (value$next_sequence <= previous) rr_stop("Invalid evidence high-water sequence.")
  for (entry in value$records) {
    payload <- entry$payload
    for (unit in names(payload$units)) {
      stage <- payload$units[[unit]]
      if (!unit %in% rr_units || !stage$status %in% c("passed", "failed", "unavailable") ||
          !identical(payload$selection$plan$units[[unit]]$action, "execute") ||
          (stage$status != "unavailable" && (!rr_match("[0-9a-f]{64}", stage$source) || !rr_match("[0-9a-f]{64}", stage$environment)))) rr_stop("Invalid observed stage provenance.")
    }
    if (payload$kind == "evidence") {
      observations <- Filter(function(item) item$payload$kind == "observation" && item$payload$request == payload$request, value$records)
      if (length(observations) != 1L || !identical(observations[[1L]]$payload$sequence, payload$observation)) rr_stop("Missing executing observation.")
      observed <- observations[[1L]]$payload
      for (unit in names(payload$units)) {
        stage <- payload$units[[unit]]; original <- observed$units[[unit]]
        if (!identical(stage$observation_sequence, observed$sequence) ||
            !rr_same(stage[c("source", "environment", "status")], original[c("source", "environment", "status")])) rr_stop("Execution differs from its observation.")
      }
      for (unit in names(payload$reused)) {
        origin <- payload$reused[[unit]]
        if (!rr_match("[0-9a-f]{32}", origin$request) || origin$request == payload$request ||
            !rr_match("[0-9a-f]{40}", origin$submitted) || !rr_compatible_unit(origin$unit, unit) ||
            !rr_same(origin, payload$selection$plan$units[[unit]])) rr_stop("Invalid inherited execution origin.")
        retained <- Filter(function(item) item$payload$kind == "evidence" && item$payload$request == origin$request, value$records)
        if (length(retained)) {
          source <- retained[[1L]]$payload
          if (length(retained) != 1L || !identical(source$submitted, origin$submitted) ||
              source$sequence >= payload$observation || !identical(source$outcome, 0L) ||
              !identical(source$units[[origin$unit]]$status, "passed")) rr_stop("Cyclic or non-executing inherited origin.")
        }
      }
    }
  }
  value$key <- key
  value$generation <- link$generation
  value
}

# One pointer publication covers evidence changes and key renewal together.
# Readers hold the same lock, so none can observe half a retirement/rotation.
rr_store_write <- function(state, value) {
  root <- rr_mkdir(rr_store_root(state))
  generation <- paste0("g-", rr_id())
  directory <- rr_mkdir(file.path(root, generation))
  key <- value$key
  value$key <- NULL
  value$generation <- NULL
  rr_write(file.path(directory, "key.bin"), key, raw = TRUE)
  rr_write(file.path(directory, "index.json"), rr_seal(value, key))
  published <- list(generation = generation, digest = rr_hash(rr_read(file.path(directory, "index.json"))))
  rr_write(file.path(root, "current.json"), published)
  # Orphans are fully unpublished generations; the lock excludes every reader.
  for (path in setdiff(list.dirs(root, recursive = FALSE, full.names = TRUE), directory)) {
    if (rr_match("g-[0-9a-f]{32}", basename(path))) tryCatch(rr_remove(path, root), error = function(e) rr_say("Old evidence deletion deferred: ", conditionMessage(e)))
  }
  invisible(value)
}

rr_compatible_unit <- function(candidate, wanted) candidate == wanted || (candidate == "build-full" && wanted == "build-documentation")
rr_required_units <- function(selection) {
  units <- if (selection$path == "Repository") character() else if (selection$path == "Full") "build-full" else "build-documentation"
  if (selection$path == "Documentation") units <- c(units, "documentation-assertions")
  if ("tools/review-ready-generated.R" %in% unlist(selection$verifiers)) units <- c(units, "generation")
  units
}
rr_reused <- function(selection) names(Filter(function(unit) identical(unit$action, "reuse"), selection$plan$units))
rr_full_required <- function(selection) selection$path == "Full" && !"build-full" %in% rr_reused(selection)
rr_execute_plan <- function(selection, reason = "Required execution") {
  units <- rr_required_units(selection)
  list(schema = rr_schema, units = stats::setNames(lapply(units, function(unit) list(action = "execute")), units),
    snapshot = NULL, reasons = list(reason))
}

rr_vetoes <- function(negative, origin, candidate, wanted) {
  if (!is.null(negative$repository) && !identical(negative$repository, origin$repository)) return(FALSE)
  successful <- origin$units[[candidate]]
  sequence <- successful$observation_sequence
  if (is.null(sequence)) sequence <- origin$sequence
  any(vapply(names(negative$units), function(unit) {
    failure <- negative$units[[unit]]
    order <- failure$observation_sequence
    if (is.null(order)) order <- negative$sequence
    rr_compatible_unit(unit, wanted) && failure$status %in% c("failed", "unavailable") && order >= sequence &&
      (is.null(failure$source) || identical(failure$source, successful$source)) &&
      (is.null(failure$environment) || identical(failure$environment, successful$environment))
  }, logical(1)))
}

# An expired inherited unit does not remove a request's own independent units.
# Before dropping a negative observation, retire every success it still vetoes.
rr_store_retire <- function(value, expired, now = rr_now(), observations = list()) {
  retired <- Filter(function(entry) entry$payload$request %in% expired, c(value$records, observations))
  remaining <- Filter(function(entry) !entry$payload$request %in% expired, value$records)
  for (i in seq_along(remaining)) {
    entry <- remaining[[i]]
    if (entry$payload$kind != "evidence") next
    eligible <- unlist(entry$available, use.names = FALSE)
    for (unit in eligible) {
      if (any(vapply(retired, function(negative) rr_vetoes(negative$payload, entry$payload, unit, unit), logical(1)))) eligible <- setdiff(eligible, unit)
    }
    remaining[[i]]$available <- as.list(eligible)
  }
  value$records <- remaining
  if (!rr_valid_time(value$key_created, now)) rr_stop("Untrustworthy evidence key creation time.")
  if (now >= value$key_created + rr_week) {
    value$key <- openssl::rand_bytes(32)
    value$key_created <- now
  }
  value
}

rr_history <- function(state, now = rr_now()) {
  expired <- character()
  requests <- file.path(state, "requests")
  if (!dir.exists(requests)) return(expired)
  for (directory in list.dirs(requests, recursive = FALSE, full.names = TRUE)) {
    path <- file.path(directory, "record.json")
    tryCatch({
      record <- rr_read_record(path)
      if (record$state != "terminal" || record$cleanup != "confirmed" || isTRUE(record$owned_capacity)) next
      if (!rr_valid_time(record$finished, now)) { rr_say("History age unavailable: ", record$id); next }
      if (now < record$finished + rr_week || rr_alive(record$manager)) next
      lock <- rr_lock(file.path(directory, "manager.lock"))
      if (is.null(lock)) next
      stopped <- !length(rr_owned(rr_processes(), record))
      filelock::unlock(lock)
      if (stopped) expired <- c(expired, record$id)
    }, error = function(e) rr_say("Protected request history: ", basename(directory), "; ", conditionMessage(e)))
  }
  expired
}

rr_maintenance <- function(state, now = rr_now()) {
  expired <- rr_history(state, now)
  retired <- rr_mkdir(file.path(state, "retired"))
  rr_with_lock(file.path(state, "evidence.lock"), {
    value <- rr_store_read(state, create = TRUE)
    value <- rr_store_retire(value, expired, now, rr_store_history(state, value))
    rr_store_write(state, value)
    # Rename a safely retired request before recursive deletion. A partial
    # deletion can no longer be mistaken for active, damaged control metadata.
    for (request in expired) {
      path <- rr_owned_path(file.path(state, "requests", request), state)
      target <- file.path(retired, request)
      if (!file.rename(path, target)) rr_say("Old history deletion deferred: ", request)
    }
  })
  for (path in list.dirs(retired, recursive = FALSE, full.names = TRUE)) {
    if (rr_match("[0-9a-f]{32}", basename(path))) tryCatch(rr_remove(path, retired),
      error = function(e) rr_say("Old history deletion deferred: ", conditionMessage(e)))
  }
  rr_prune_legacy(state, now)
  invisible()
}

# Migration attributes terminal legacy histories individually. Shared caches,
# original worktrees, manual captures and OS crash reports have no route here.
rr_prune_legacy <- function(state, now = rr_now()) {
  root <- file.path(state, "legacy-history")
  if (!dir.exists(root)) return(invisible())
  for (directory in list.dirs(root, recursive = FALSE, full.names = TRUE)) {
    tryCatch({
      history <- rr_read_json(file.path(directory, "history.json"))
      if (!identical(history$kind, "legacy-review-ready") || !identical(history$request, basename(directory)) ||
          !rr_match("[0-9a-f]{32}", history$request) || !rr_match("[0-9a-f]{40}", history$sha) ||
          !identical(history$cleanup, "confirmed") || !rr_valid_time(history$finished, now)) rr_stop("Unattributed legacy history.")
      if (now >= history$finished + rr_week) {
        roots <- file.path(rr_account_home(), c("Library/Caches/org.R-project.R/R/marginplyr/review-ready-failures",
          ".cache/R/marginplyr/review-ready-failures"))
        for (bundle in unlist(history$diagnostics)) {
          boundary <- roots[vapply(roots, function(path) startsWith(bundle, paste0(path, "/")), logical(1))]
          if (length(boundary) != 1L) rr_stop("Unregistered historical diagnostic boundary.")
          if (!dir.exists(bundle)) next
          metadata <- file.path(bundle, "metadata.txt")
          if (!file.exists(metadata) || !paste("Commit:", history$sha) %in% readLines(metadata, warn = FALSE)) rr_stop("Legacy diagnostic provenance changed.")
          rr_remove(bundle, boundary)
        }
        rr_remove(directory, root)
      }
    }, error = function(e) rr_say("Legacy history deletion deferred: ", conditionMessage(e)))
  }
  if (!length(list.dirs(root, recursive = FALSE, full.names = TRUE))) {
    for (name in c("evidence.key", "evidence-index.json", "evidence-index.new", "retirement.json")) {
      path <- file.path(root, name)
      if (file.exists(path)) tryCatch(rr_remove(path, root), error = function(e) rr_say("Legacy control deletion deferred: ", conditionMessage(e)))
    }
    for (name in c("legacy-control.py", "legacy-launcher.disabled")) {
      path <- file.path(state, name)
      if (file.exists(path)) tryCatch(rr_remove(path, state), error = function(e) rr_say("Legacy control deletion deferred: ", conditionMessage(e)))
    }
  }
  invisible()
}

# Read under the issuance lock; synthetic unavailable observations fence every
# authorized execution not yet observed, including a dead manager's spawn gap.
rr_store_history <- function(state, value) {
  history <- value$records
  observed <- vapply(Filter(function(entry) entry$payload$kind == "observation", history), function(entry) entry$payload$request, "")
  for (directory in list.dirs(file.path(state, "requests"), recursive = FALSE, full.names = TRUE)) {
    record <- rr_read_record(file.path(directory, "record.json"))
    if (isTRUE(record$authorized) && !record$id %in% observed) {
      units <- names(Filter(function(unit) unit$action == "execute", record$selection$plan$units))
      history <- c(history, list(list(available = list(), payload = list(kind = "observation", request = record$id,
        repository = record$repository, submitted = record$sha, outcome = 1L, sequence = value$next_sequence,
        units = stats::setNames(lapply(units, function(unit) list(status = "unavailable", source = NULL, environment = NULL)), units)))))
    }
  }
  history
}

rr_origin_valid <- function(entry, unit, wanted, history, now = rr_now()) {
  payload <- entry$payload
  origin <- payload$units[[unit]]
  payload$kind == "evidence" && identical(payload$outcome, 0L) && unit %in% unlist(entry$available) &&
    rr_valid_time(payload$finished, now) && now < payload$finished + rr_week &&
    !is.null(origin) && identical(origin$status, "passed") && rr_compatible_unit(unit, wanted) &&
    !any(vapply(history, function(negative) rr_vetoes(negative$payload, payload, unit, wanted), logical(1)))
}

rr_clock_valid <- function(clock, inventory, commit_time, now = rr_now()) {
  known <- c("4.6.1", "2efcaa5190c038e60834bd5c4e502c82", "34ee7dea5eac7ba7138bd3c60651faee")
  if (!identical(unname(unlist(clock[c("r", "checker", "timeout")])), known) ||
      !is.list(inventory) || !length(inventory$paths) || !rr_valid_time(inventory$observed, now)) return(FALSE)
  values <- c(unlist(inventory$paths), commit_time)
  is.numeric(clock$leeway) && length(clock$leeway) == 1L && is.finite(clock$leeway) && clock$leeway > 0 &&
    all(is.finite(values) & values <= now + clock$leeway)
}

rr_valid_check <- function(check, unit) {
  if (!is.list(check)) return(FALSE)
  args <- as.list(c("--as-cran", if (unit == "build-documentation") "--no-tests"))
  valid <- function(attempt) is.list(attempt) && identical(attempt$status, 0L) && identical(attempt$timeout, FALSE) &&
    identical(attempt$errors, 0L) && identical(attempt$warnings, 0L) && identical(attempt$notes, check$notes)
  if (!identical(check$args, args) || !valid(check) || !is.list(check$notes) ||
      !all(unlist(check$notes) == "allowed-note") || !length(check$attempts) %in% 1:2) return(FALSE)
  if (length(check$attempts) == 2L) {
    first <- check$attempts[[1L]]
    if (!isTRUE(check$retry_verified) || is.null(check$first_failure) || is.null(check$retry_identity) ||
        !identical(first$status, 1L) || !identical(first$timeout, FALSE) ||
        !identical(first$errors, 1L) || !identical(first$warnings, 0L)) return(FALSE)
  }
  valid(tail(check$attempts, 1L)[[1L]])
}

rr_plan <- function(root, sha, selection, state, observe = rr_snapshot, now = rr_now()) {
  result <- rr_execute_plan(selection)
  units <- rr_required_units(selection)
  if (!length(units)) return(result)
  tryCatch(rr_with_lock(file.path(state, "evidence.lock"), {
    value <- rr_store_read(state)
    history <- rr_store_history(state, value)
    origins <- rev(Filter(function(entry) entry$payload$kind == "evidence", history))
    repo <- rr_repository(root)
    source <- rr_source_inputs(root, sha)
    if (!any(vapply(origins, function(entry) identical(entry$payload$repository, repo) &&
        any(vapply(entry$payload$units, function(unit) identical(unit$source, source), logical(1))), logical(1)))) return(result)
    snapshot <- observe(root, sha, units)
    result$snapshot <- snapshot
    commit_time <- as.numeric(rr_git(root, "show", "-s", "--format=%ct", sha))
    for (wanted in units) {
      for (entry in origins) {
        origin <- entry$payload
        if (!identical(origin$repository, repo) || rr_run("git", c("-C", root, "merge-base", "--is-ancestor", origin$submitted, sha), error = FALSE)$status != 0L) next
        for (unit in names(origin$units)) {
          actual <- origin$units[[unit]]
          if (!rr_origin_valid(entry, unit, wanted, history, now) ||
              !identical(actual$source, snapshot$sources[[wanted]]) ||
              !identical(actual$environment, snapshot$environment$digest)) next
          if (startsWith(wanted, "build-") && (!rr_valid_check(actual$check, unit) ||
              !rr_clock_valid(snapshot$environment$clock, actual$timestamps, commit_time, now))) next
          result$units[[wanted]] <- list(action = "reuse", request = origin$request, unit = unit, submitted = origin$submitted)
          break
        }
        if (result$units[[wanted]]$action == "reuse") break
      }
    }
    result
  }), error = function(e) {
    result <- rr_execute_plan(selection, paste("Reuse unavailable:", conditionMessage(e)))
    result
  })
}
rr_revalidate <- function(root, sha, selection, state) {
  if (!length(rr_reused(selection))) return(TRUE)
  plan <- rr_plan(root, sha, selection, state)
  rr_same(plan$units, selection$plan$units) && rr_same(plan$snapshot, selection$plan$snapshot)
}

rr_repository <- function(root) {
  remote <- rr_git(root, "remote", "get-url", "origin")
  remote <- sub("^(https?://)[^/@]+@", "\\1", remote)
  remote <- sub("[?#].*$", "", remote)
  sub("[.]git$", "", sub("/$", "", sub("^git@github.com:", "https://github.com/", remote)))
}
rr_non_inputs <- c("AGENTS[.]md|CLAUDE[.]md|CONTEXT[.]md|CODE_OF_CONDUCT[.]md",
  "(design|investigation)/(?:[^/]+/)*[^/]+[.]md|tools/[^/]+[.]md",
  "[.]github/workflows/(R-CMD-check|altdoc|document|lint|postgres-transactions|release-matrix|rhub|test-coverage)[.]yaml",
  "[.]github/scripts/summarize-check-log[.]R",
  "altdoc/quarto_website[.]yml|codecov[.]yml|[.]codex/config[.]toml|[.]claude/settings[.]json")
rr_source_inputs <- function(root, sha) {
  tree <- rr_tree(root, sha)
  needed <- Filter(function(entry) basename(entry$name) == ".gitattributes" || entry$name == rr_expectation ||
    (entry$mode == "100644" && any(vapply(rr_non_inputs, rr_match, logical(1), text = entry$name))), tree)
  blobs <- rr_blobs(root, vapply(needed, `[[`, "", "oid"))
  inputs <- list()
  for (name in sort(names(tree), method = "radix")) {
    entry <- tree[[name]]
    if (!entry$mode %in% c("100644", "100755") || entry$kind != "blob") rr_stop("Unsupported committed source type: ", name)
    if (basename(name) == ".gitattributes" && length(blobs[[entry$oid]])) rr_stop("Unregistered committed Git attributes.")
    if (entry$mode == "100644" && any(vapply(rr_non_inputs, rr_match, logical(1), text = name))) {
      if (rr_ordinary_text(blobs[[entry$oid]])) next
    }
    if (entry$mode == "100644" && name == rr_expectation) {
      protected <- rr_protected_expectation(blobs[[entry$oid]])
      if (!is.null(protected)) entry$oid <- rr_hash(protected)
    }
    inputs[[length(inputs) + 1L]] <- unname(entry[c("name", "mode", "kind", "oid")])
  }
  rr_digest(inputs)
}
rr_file_identity <- function(path) {
  before <- file.info(path)
  if (is.na(before$size) || before$isdir) rr_stop("External file unavailable: ", path)
  connection <- file(path, "rb")
  hash <- tryCatch(rr_hash(connection), finally = close(connection))
  after <- file.info(path)
  fields <- c("mode", "size", "mtime", "ctime", "uid", "gid")
  if (!identical(before[fields], after[fields])) rr_stop("External input changed while identifying it: ", path)
  list(path = path, resolved = normalizePath(path, mustWork = TRUE), mode = as.integer(after$mode),
    size = after$size, mtime = as.numeric(after$mtime), ctime = as.numeric(after$ctime), digest = hash)
}
rr_native_file <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  rr_hex(readBin(connection, "raw", n = 4L)) %in% c("7f454c46", "cffaedfe", "feedfacf", "cefaedfe", "feedface", "cafebabe", "bebafeca")
}
rr_directory_identity <- function(root) {
  rows <- list(); natives <- character(); seen <- character()
  visit <- function(path, relative) {
    resolved <- normalizePath(path, mustWork = TRUE)
    info <- file.info(path)
    if (info$isdir) {
      if (resolved %in% seen) rr_stop("Cyclic external directory: ", path)
      seen <<- c(seen, resolved)
      rows[[length(rows) + 1L]] <<- list(relative, "directory", resolved, as.integer(info$mode))
      for (child in sort(list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE), method = "radix")) visit(child, paste0(relative, "/", basename(child)))
      after <- file.info(path)
      if (!identical(info$mtime, after$mtime) || !identical(info$ctime, after$ctime)) rr_stop("External directory changed: ", path)
      seen <<- setdiff(seen, resolved)
    } else {
      rows[[length(rows) + 1L]] <<- list(relative, rr_file_identity(path))
      if ((grepl("[.](so|dylib)$", path) || file.access(path, 1L) == 0L) && rr_native_file(path)) natives <<- union(natives, resolved)
    }
  }
  visit(root, ".")
  list(digest = rr_digest(rows), natives = natives)
}

# Unknown loader-relative resolutions disable reuse instead of guessing which
# native dependency or runtime asset directory supplied the running code.
rr_linkage <- function(natives, roots, executables) {
  seen <- character(); identities <- list(); bottles <- character()
  roots <- unique(normalizePath(roots, mustWork = TRUE))
  mac <- Sys.info()[["sysname"]] == "Darwin"
  if (mac) identities <- list(rr_file_identity("/System/Library/CoreServices/SystemVersion.plist"))
  pending <- natives
  rpaths <- new.env(parent = emptyenv())
  runpaths <- function(binary) {
    if (!exists(binary, rpaths, inherits = FALSE)) {
      output <- rr_run("/usr/bin/otool", c("-l", binary))$stdout
      lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
      at <- grep("cmd LC_RPATH$", trimws(lines)) + 2L
      assign(binary, sub("^\\s*path (.*?) \\(offset.*$", "\\1", lines[at], perl = TRUE), rpaths)
    }
    get(binary, rpaths, inherits = FALSE)
  }
  while (length(setdiff(pending, seen))) {
    batch <- sort(setdiff(pending, seen)); pending <- character(); seen <- union(seen, batch)
    dependencies <- character()
    for (binary in batch) {
      if (mac) {
        ids <- strsplit(rr_run("/usr/bin/otool", c("-D", binary))$stdout, "\n", fixed = TRUE)[[1L]]
        output <- strsplit(rr_run("/usr/bin/otool", c("-L", binary))$stdout, "\n", fixed = TRUE)[[1L]]
        names <- sub(" \\(compatibility.*$", "", trimws(output[startsWith(output, "\t")]))
        for (name in setdiff(names, c(ids, basename(binary)))) {
          name <- sub("^@loader_path/", paste0(dirname(binary), "/"), name)
          if (startsWith(name, "@executable_path/")) {
            candidates <- unique(file.path(dirname(c(executables, binary)), substring(name, 18L)))
            candidates <- unique(normalizePath(candidates[file.exists(candidates)], mustWork = TRUE))
            if (length(candidates) != 1L) rr_stop("Unregistered executable-relative dependency: ", name)
            name <- candidates
          }
          if (startsWith(name, "@rpath/")) {
            candidates <- character()
            for (owners in list(unique(c(executables, binary)), unique(c(natives, seen)))) {
              for (owner in owners) {
                paths <- gsub("@loader_path", dirname(owner), runpaths(owner), fixed = TRUE)
                for (executable in executables) {
                  expanded <- gsub("@executable_path", dirname(executable), paths, fixed = TRUE)
                  possible <- file.path(expanded[startsWith(expanded, "/")], substring(name, 8L))
                  candidates <- union(candidates, normalizePath(possible[file.exists(possible)], mustWork = TRUE))
                }
              }
              if (length(candidates)) break
            }
            if (length(candidates) != 1L) rr_stop("Unregistered native loader resolution: ", name)
            name <- candidates
          }
          if (!startsWith(name, "/")) rr_stop("Unregistered native dependency: ", name)
          if (!grepl("^(/usr/lib/|/System/Library/)", name)) dependencies <- union(dependencies, name)
        }
      } else {
        output <- rr_run("ldd", binary, error = FALSE)
        text <- paste(output$stdout, output$stderr)
        if ((output$status != 0L && !grepl("not a dynamic executable|statically linked", text)) || grepl("not found", text)) rr_stop("Unavailable native linkage: ", binary)
        matches <- regmatches(text, gregexpr("(?:=>\\s+|^\\s*)(/[^\\s]+)", text, perl = TRUE))[[1L]]
        dependencies <- union(dependencies, sub("^.*?(/[^\\s]+)$", "\\1", trimws(matches), perl = TRUE))
      }
    }
    for (dependency in sort(dependencies)) {
      if (!file.exists(dependency)) { identities <- c(identities, list(list(dependency, "absent"))); next }
      path <- normalizePath(dependency, mustWork = TRUE)
      inside <- any(vapply(roots, function(root) path == root || startsWith(path, paste0(root, "/")), logical(1)))
      if (!inside && !path %in% seen) {
        bottle <- regmatches(path, regexpr("^/(?:opt/homebrew|usr/local)/Cellar/[^/]+/[^/]+", path, perl = TRUE))
        bottles <- union(bottles, bottle[nzchar(bottle)])
        identities <- c(identities, list(rr_file_identity(path)))
        if (rr_native_file(path)) pending <- union(pending, path)
      }
    }
  }
  for (bottle in sort(bottles)) identities <- c(identities, list(list(bottle, rr_directory_identity(bottle)$digest)))
  rr_digest(identities)
}

rr_git_environment <- function(root) {
  rr_comparison_inputs(root)
  fields <- rr_split0(rr_git_raw(root, "config", "--show-scope", "--show-origin", "--null", "--list"))
  if (length(fields) %% 3L) rr_stop("Unknown Git configuration inventory.")
  values <- list(); files <- character()
  for (i in seq.int(1L, length(fields), by = 3L)) {
    scope <- fields[[i]]; origin <- fields[[i + 1L]]; item <- fields[[i + 2L]]
    name <- tolower(sub("\n.*$", "", item))
    if (name %in% c("core.hookspath", "core.fsmonitor", "init.templatedir", "diff.external") ||
        startsWith(name, "tar.") || grepl("^diff[.].*[.](command|textconv)$", name)) rr_stop("Unregistered executable/archive Git configuration: ", name)
    if (!scope %in% c("local", "worktree")) {
      values <- c(values, list(list(scope, name, rr_digest(item))))
      if (startsWith(origin, "file:")) files <- union(files, path.expand(substring(origin, 6L)))
    }
  }
  home <- rr_account_home()
  xdg <- Sys.getenv("XDG_CONFIG_HOME", file.path(home, ".config"))
  execution <- dirname(dirname(rr_git(root, "--exec-path")))
  files <- union(files, c(file.path(home, ".gitconfig"), file.path(xdg, "git/config"), "/etc/gitconfig", file.path(execution, "etc/gitconfig")))
  files <- union(files, Sys.getenv(c("GIT_CONFIG_GLOBAL", "GIT_CONFIG_SYSTEM")))
  attributes <- c(file.path(xdg, "git/attributes"), "/etc/gitattributes", file.path(execution, "etc/gitattributes"))
  query <- rr_run("git", c("-C", root, "config", "--path", "--get-all", "core.attributesFile"), error = FALSE)
  if (!query$status %in% c(0L, 1L)) rr_stop("Git attributes configuration unavailable.")
  if (nzchar(trimws(query$stdout))) attributes <- union(attributes, strsplit(trimws(query$stdout), "\n", fixed = TRUE)[[1L]])
  info <- rr_git(root, "rev-parse", "--path-format=absolute", "--git-path", "info/attributes")
  if (file.exists(info)) rr_stop("Unregistered repository-local Git attributes.")
  if (any(file.exists(attributes) & file.info(attributes)$size > 0L, na.rm = TRUE)) rr_stop("Unregistered external Git attributes.")
  paths <- sort(unique(c(files[nzchar(files)], attributes)))
  if (!all(startsWith(paths, "/"))) rr_stop("Unregistered relative Git configuration input.")
  list(configuration = values, files = lapply(paths, function(path) if (file.exists(path)) rr_file_identity(path) else list(path, "absent")))
}

rr_snapshot <- function(root, sha, units) {
  source <- rr_source_inputs(root, sha)
  list(environment = rr_environment(root), sources = stats::setNames(as.list(rep(source, length(units))), units))
}

# Request storage roles are independent of renderer versions and cache layouts.
# Fresh private caches are outputs, not reusable inputs. Only these exact role
# assignments are normalized; arbitrary HOME/cache overrides remain inputs.
rr_render_env <- function(directory, values = rr_child_env()) {
  unsupported <- c("R_USER", "R_PROFILE", "R_PROFILE_USER", "R_ENVIRON", "R_ENVIRON_USER",
    "QUARTO_PATH", "QUARTO_DENO", "QUARTO_PANDOC", "QUARTO_DART_SASS", "QUARTO_USERDATA", "QUARTO_CACHE_DIR")
  supplied <- unsupported[unsupported %in% names(values) & nzchar(values[unsupported]) & !is.na(values[unsupported])]
  if (length(supplied)) rr_stop("Unsupported private renderer configuration: ", paste(supplied, collapse = ", "))
  roles <- c(HOME = "cache/home", XDG_CACHE_HOME = "cache/xdg", XDG_CONFIG_HOME = "cache/config",
    XDG_DATA_HOME = "cache/data", XDG_STATE_HOME = "cache/state", XDG_RUNTIME_DIR = "cache/runtime",
    TMPDIR = "tmp", TMP = "tmp", TEMP = "tmp", DENO_DIR = "cache/deno",
    R_USER_CACHE_DIR = "cache/r", R_USER_DATA_DIR = "cache/r-data", R_USER_CONFIG_DIR = "cache/r-config")
  for (name in names(roles)) {
    path <- file.path(directory, roles[[name]])
    rr_mkdir(path)
    values[[name]] <- path
  }
  libraries <- normalizePath(.libPaths(), mustWork = TRUE)
  values[["R_LIBS"]] <- paste(libraries, collapse = .Platform$path.sep)
  values[["MARGINPLYR_REVIEW_LIBRARY_PATHS"]] <- values[["R_LIBS"]]
  values[["MARGINPLYR_REVIEW_ISOLATION"]] <- directory
  # R's Renviron copies R_PAPERSIZE to R_PAPERSIZE_USER on child startup.
  # Record that effective value before comparing manager and worker snapshots.
  if ("R_PAPERSIZE" %in% names(values)) values[["R_PAPERSIZE_USER"]] <- values[["R_PAPERSIZE"]]
  values[["TESTTHAT_PARALLEL"]] <- "false"
  values[["TESTTHAT_CPUS"]] <- "1"
  values[["PYTHONDONTWRITEBYTECODE"]] <- "1"
  values <- values[!names(values) %in% c("R_SESSION_TMPDIR", "R_TEMP_DIR")]
  values
}
rr_normalize_environment <- function(values) {
  directory <- values[["MARGINPLYR_REVIEW_ISOLATION"]]
  if (is.null(directory) || !startsWith(directory, "/")) rr_stop("Unregistered renderer isolation.")
  expected <- rr_render_env(directory, values)
  roles <- c("HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME", "XDG_RUNTIME_DIR",
    "TMPDIR", "TMP", "TEMP", "DENO_DIR", "R_USER_CACHE_DIR", "R_USER_DATA_DIR", "R_USER_CONFIG_DIR")
  for (name in roles) {
    if (!identical(values[[name]], expected[[name]])) rr_stop("Unregistered private renderer destination: ", name)
    values[[name]] <- paste0("<request>/", substring(expected[[name]], nchar(directory) + 2L))
  }
  internal <- c("MARGINPLYR_REVIEW_REQUEST", "MARGINPLYR_REVIEW_SELECTION", "MARGINPLYR_REVIEW_RECORD",
    "MARGINPLYR_REVIEW_COMMIT", "MARGINPLYR_REVIEW_EPOCH", "MARGINPLYR_REVIEW_SUPERVISOR",
    "MARGINPLYR_REVIEW_STAGE_REPORT", "MARGINPLYR_REVIEW_BASE", "MARGINPLYR_REVIEW_ISOLATION",
    "PWD", "OLDPWD", "SHLVL", "_", "R_SESSION_TMPDIR")
  # processx 3.9 marks each child tree with this ps-generated transport. It
  # remains in children for shutdown; unknown names/values remain inputs.
  trees <- names(values)[rr_match("PROCESSX_PS[0-9a-f]+_[0-9]+", names(values)) & values == "YES"]
  as.list(values[!names(values) %in% c(internal, trees)])
}
# Queries renderer capabilities and storage in the request's private environment.
rr_renderer_query <- function(command, args, directory, values) {
  tryCatch({
    result <- processx::run(command, args, wd = directory, env = values,
      error_on_status = FALSE, timeout = 15, cleanup_tree = TRUE)
    if (result$status != 0L) rr_stop(trimws(paste(result$stderr, result$stdout)))
    result
  }, error = function(e) rr_stop("Quarto capability query ", paste(args, collapse = " "), ": ", conditionMessage(e)))
}
rr_renderer_location <- function(path, directory, role, label, must_exist = FALSE) {
  boundary <- file.path(directory, role)
  valid <- is.character(path) && length(path) == 1L && !is.na(path) &&
    (identical(path, boundary) || startsWith(path, paste0(boundary, "/")))
  if (!isTRUE(valid)) rr_stop(label, " isolation failed: ", rr_json(path), "; expected a location inside ", boundary)
  tryCatch({
    rr_owned_path(path, directory)
    if (file.exists(path) && !dir.exists(path)) rr_stop("The reported location is not a directory: ", path)
    normalizePath(path, mustWork = must_exist)
  }, error = function(e) rr_stop(label, " isolation failed: ", conditionMessage(e)))
}
rr_renderer <- function(values = rr_child_env()) {
  quarto <- unname(Sys.which("quarto"))
  if (!nzchar(quarto)) rr_stop("Renderer isolation requires Quarto.")
  quarto <- normalizePath(quarto, mustWork = TRUE)
  directory <- values[["MARGINPLYR_REVIEW_ISOLATION"]]
  if (is.null(directory) || !startsWith(directory, "/")) rr_stop("Quarto capability inspection requires a private request environment.")
  directory <- normalizePath(directory, mustWork = TRUE)
  work <- rr_mkdir(file.path(directory, "work"))
  probe <- rr_mkdir(tempfile("renderer-capabilities-", tmpdir = work))
  on.exit(rr_remove(probe, directory), add = TRUE)
  query <- function(args) rr_renderer_query(quarto, args, probe, values)
  executable <- rr_file_identity(quarto)
  version <- trimws(query("--version")$stdout)
  if (!nzchar(version)) rr_stop("Quarto did not report its actual version for evidence.")
  help <- query(c("render", "--help"))$stdout
  if (!all(vapply(c("--to", "--output"), function(flag) grepl(paste0(flag, "([[:space:]=<]|$)"), help), logical(1))))
    rr_stop("Quarto render lacks required --to or --output functionality.")
  pandoc <- trimws(query(c("pandoc", "--version"))$stdout)
  if (!nzchar(pandoc)) rr_stop("Quarto cannot report its selected Pandoc implementation.")
  pandoc_lines <- strsplit(pandoc, "\n", fixed = TRUE)[[1L]]
  data_line <- grep("^User data directory: ", pandoc_lines)
  if (length(data_line) != 1L) rr_stop("Quarto-selected Pandoc does not expose its user data location; private storage cannot be established.")
  pandoc_data <- rr_renderer_location(sub("^User data directory: ", "", pandoc_lines[[data_line]]),
    directory, "cache", "Pandoc data")
  pandoc_lines[[data_line]] <- paste0("User data directory: <request>/", substring(pandoc_data, nchar(directory) + 2L))
  pandoc <- paste(pandoc_lines, collapse = "\n")
  info <- file.path(probe, "quarto-info.json")
  query(c("check", "info", "--output", info, "--no-strict"))
  if (!file.exists(info)) rr_stop("Quarto check info does not expose resolved cache storage as JSON.")
  metadata <- tryCatch(rr_read_json(info), error = function(e) rr_stop("Quarto cache metadata unavailable: ", conditionMessage(e)))
  if (!is.list(metadata) || !is.list(metadata$info)) rr_stop("Quarto cache metadata lacks info.cacheDir; private storage cannot be established.")
  cache <- rr_renderer_location(metadata$info$cacheDir, directory, "cache", "Quarto cache")
  script <- file.path(probe, "runtime.ts")
  writeLines(c(
    'const temporary = await Deno.makeTempDir();',
    'const result = await new Deno.Command(Deno.execPath(), {args: ["info", "--json"], stdout: "piped", stderr: "piped"}).output();',
    'if (!result.success) throw new Error("Deno cannot report its cache location");',
    'const info = JSON.parse(new TextDecoder().decode(result.stdout));',
    'console.log(JSON.stringify({version: Deno.version.deno, executable: Deno.execPath(), cache: info.denoDir,',
    '  temporary, home: Deno.env.get("HOME"), libraries: Deno.env.get("R_LIBS")}));'), script)
  runtime <- tryCatch(jsonlite::fromJSON(query(c("run", script))$stdout, simplifyVector = FALSE),
    error = function(e) rr_stop("Quarto TypeScript/Deno capabilities unavailable: ", conditionMessage(e)))
  if (!is.list(runtime)) rr_stop("Quarto TypeScript/Deno metadata lacks runtime and storage locations.")
  deno_cache <- rr_renderer_location(runtime$cache, directory, "cache", "Deno cache")
  temporary <- rr_renderer_location(runtime$temporary, directory, "tmp", "Deno temporary storage", TRUE)
  home <- rr_renderer_location(runtime$home, directory, "cache", "Quarto HOME", TRUE)
  if (!identical(home, normalizePath(values[["HOME"]], mustWork = TRUE)) ||
      !identical(runtime$libraries, values[["R_LIBS"]])) rr_stop("Quarto runtime changed the private HOME or ordered R library environment.")
  if (!is.character(runtime$version) || length(runtime$version) != 1L || !nzchar(runtime$version) ||
      !is.character(runtime$executable) || length(runtime$executable) != 1L || !startsWith(runtime$executable, "/"))
    rr_stop("Quarto runtime did not report the selected Deno version and executable.")
  deno <- normalizePath(runtime$executable, mustWork = TRUE)
  rr_remove(temporary, directory)
  if (!rr_same(executable, rr_file_identity(quarto))) rr_stop("Quarto executable changed during capability inspection.")
  js <- file.path(dirname(quarto), "quarto.js")
  if (!file.exists(js)) js <- NULL
  storage <- list(quarto = cache, deno = deno_cache, pandoc = pandoc_data, temporary = file.path(directory, "tmp"), home = home)
  identity <- list(version = version, executable = executable,
    script = if (!is.null(js)) rr_file_identity(js) else NULL,
    deno = list(version = runtime$version, executable = rr_file_identity(deno)), pandoc = pandoc,
    storage = lapply(storage, function(path) paste0("<request>/", substring(path, nchar(directory) + 2L))))
  list(executable = quarto, js = js, root = dirname(dirname(quarto)), version = version,
    deno = deno, storage = storage, identity = identity)
}
rr_environment <- function(root) {
  git <- rr_git_environment(root)
  renderer <- rr_renderer()
  values <- rr_child_env()
  loader <- c("LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH")
  probe_environment <- values[!names(values) %in% loader]
  probe <- jsonlite::fromJSON(rr_run(file.path(R.home("bin"), "Rscript"),
    c("tools/review-ready-environment.R", names(rr_metadata)), root, probe_environment)$stdout, simplifyVector = FALSE)
  actual <- Sys.getenv(loader)
  defaults <- unlist(probe$loader_defaults)[loader]
  if (any(nzchar(actual) & (is.na(defaults) | actual != defaults))) rr_stop("Unregistered caller loader override.")
  if (length(probe$unregistered)) rr_stop("Unregistered external inputs: ", paste(unlist(probe$unregistered), collapse = ", "))
  audit <- list(version = 4L, names = as.list(names(rr_metadata)),
    present = as.list(names(rr_metadata)[nzchar(values[names(rr_metadata)]) & !is.na(values[names(rr_metadata)])]))
  if (!identical(probe$metadata_audit, audit)) rr_stop("Unavailable execution metadata audit.")
  files <- list(); natives <- character(); executables <- character(); directories <- list()
  for (name in c("Rscript", "R", "git", "jarl", "quarto", "actionlint", "python3")) {
    command <- unname(Sys.which(name))
    if (!nzchar(command)) rr_stop("Unavailable environment tool identity: ", name)
    files <- c(files, list(rr_file_identity(command)))
    if (rr_native_file(command)) {
      natives <- union(natives, normalizePath(command)); executables <- union(executables, normalizePath(command))
    }
  }
  if (!is.null(renderer$deno) && rr_native_file(renderer$deno)) {
    natives <- union(natives, renderer$deno); executables <- union(executables, renderer$deno)
  }
  # Python remains an external artifact-verifier runtime; no Python supervisor
  # or evidence implementation is invoked by the R runner.
  python <- rr_run(unname(Sys.which("python3")), c("-c",
    "import json,sys,sysconfig; print(json.dumps([sys.base_prefix,sysconfig.get_path('stdlib'),sysconfig.get_path('platstdlib')]))"))$stdout
  python_roots <- unique(unlist(jsonlite::fromJSON(python)))
  roots <- unique(c(unlist(probe$libraries), unlist(probe$runtime_roots), python_roots))
  for (path in roots) {
    identified <- rr_directory_identity(path)
    directories <- c(directories, list(list(path, identified$digest)))
    natives <- union(natives, identified$natives)
  }
  for (path in unlist(probe$configuration)) files <- c(files, list(if (file.exists(path)) rr_file_identity(path) else list(path, "absent")))
  # Private request paths in the probe have the same registered roles as env.
  isolation <- values[["MARGINPLYR_REVIEW_ISOLATION"]]
  if (is.null(isolation)) rr_stop("Private renderer environment required.")
  normalized_probe <- probe
  normalized_probe$configuration <- lapply(probe$configuration, function(path) {
    if (startsWith(path, paste0(isolation, "/"))) paste0("<request>/", substring(path, nchar(isolation) + 2L)) else path
  })
  normalize_paths <- function(value) {
    if (is.list(value)) return(lapply(value, normalize_paths))
    if (is.character(value)) return(vapply(value, function(text) {
      if (startsWith(text, paste0(isolation, "/"))) paste0("<request>/", substring(text, nchar(isolation) + 2L)) else text
    }, "", USE.NAMES = FALSE))
    value
  }
  list(digest = rr_digest(list(probe = normalized_probe, renderer = renderer$identity,
    files = normalize_paths(files), directories = directories, git = normalize_paths(git),
    linkage = rr_linkage(natives, roots, union(executables, unlist(probe$executables))),
    variables = rr_digest(rr_normalize_environment(values)), platform = as.list(Sys.info()[c("sysname", "release", "version", "machine")]))),
    clock = probe$clock, r = probe$r, libraries = probe$libraries, metadata_audit = audit, renderer = renderer$identity)
}
