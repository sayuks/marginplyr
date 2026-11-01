# Process contracts for the repository-owned runner. All state is disposable;
# the account-home adapter is replaced in this harness, never by a CLI option.
args <- commandArgs(trailingOnly = TRUE)
candidate <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else getwd()
source(file.path(candidate, "tools", "review-ready-core.R"))
source(file.path(candidate, "tools", "review-ready-registry.R"))
source(file.path(candidate, "tools", "review-ready-evidence.R"))
source(file.path(candidate, "tools", "review-ready-manager.R"))
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
fixture_root <- dirname(dirname(normalizePath(entry, mustWork = TRUE)))

fail <- function(text) stop(text, call. = FALSE)
expect <- function(value, text) if (!isTRUE(value)) { cat(text, "\n"); fail("Runner contract failed.") }
rejects <- function(code, pattern) {
  result <- tryCatch({ force(code); NULL }, error = identity)
  expect(inherits(result, "error") && grepl(pattern, conditionMessage(result)), pattern)
}
source(file.path(fixture_root, "tools/test-review-ready-inherited.R"))
source(file.path(fixture_root, "tools/test-review-ready-capabilities.R"))
source(file.path(fixture_root, "tools/test-review-ready-migration.R"))
source(file.path(fixture_root, "tools/test-review-ready-renderer-compatibility.R"))

# These known answers come from SHA-256 and RFC 4231, independently of the store.
expect(identical(rr_hash(charToRaw("abc")),
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"), "SHA-256 known answer")
expect(identical(rr_hex(openssl::sha256(charToRaw("Hi There"), key = as.raw(rep(11, 20)))),
  "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"), "HMAC known answer")
expect(identical(rr_bytes(list(b = 2, a = "x")), rr_bytes(list(a = "x", b = 2))),
  "map encoding is independent of insertion order")
expect(identical(rawToChar(rr_bytes(1.25)), "marginplyr-review-ready/v2:n1.25;"), "Canonical decimal encoding")
rejects(rr_bytes(stats::setNames(list(1, 2), c("a", "a"))), "Duplicate")
rejects(rr_bytes(list(a = Inf)), "finite")
rejects(rr_seal(list(a = Inf), openssl::rand_bytes(32)), "finite")
key <- openssl::rand_bytes(32)
envelope <- rr_seal(list(schema = 2L, value = "kept"), key)
expect(identical(rr_unseal(envelope, key)$value, "kept"), "authentication roundtrip")
duplicate <- c(envelope, list(integrity = envelope$integrity))
rejects(rr_unseal(duplicate, key), "Duplicate")
rejects(rr_unseal(envelope, openssl::rand_bytes(32)), "authentication")
envelope$payload$value <- "changed"
rejects(rr_unseal(envelope, key), "authentication")
cat("Repository runner authentication contracts passed.\n")

run_store_contracts <- function() {
  state <- tempfile("runner-store-")
  dir.create(state)
  state <- normalizePath(state)
  on.exit(unlink(state, recursive = TRUE), add = TRUE)
  rr_mkdir(file.path(state, "requests"))
  now <- rr_now()
  source_a <- paste(rep("a", 64), collapse = "")
  source_c <- paste(rep("c", 64), collapse = "")
  environment <- paste(rep("e", 64), collapse = "")
  make <- function(outcome = 0L, finished = now - 10, reuse = NULL) {
    id <- rr_id(); directory <- rr_mkdir(file.path(state, "requests", id))
    sha <- paste(rep(substr(id, 1L, 1L), 40), collapse = "")
    selection <- list(submitted = sha, main = sha, base = sha, path = "Full", verifiers = list())
    if (!is.null(reuse)) selection$verifiers <- list("tools/review-ready-generated.R")
    selection$plan <- rr_execute_plan(selection)
    if (!is.null(reuse)) selection$plan$units[["build-full"]] <- reuse
    record <- list(version = 3L, protocol = rr_protocol, id = id, token = rr_id(), sha = sha, root = state,
      boot = "00000000-0000-0000-0000-000000000000", manager = list(pid = 99999999L, birth = "1.000000"),
      group = NULL, children = list(), started = now - 200, selection = selection, state = "terminal",
      capacity = if (is.null(reuse)) "Full" else "exempt", may_own_full = is.null(reuse), owned_capacity = FALSE,
      epoch = 1L, authorized = TRUE, gate_started = TRUE, promotions = list(), workspace = file.path(directory, "work"),
      cleanup = "confirmed", repository = "https://github.com/sayuks/marginplyr", main = sha,
      launcher = paste(rep("f", 64), collapse = ""), gate_outcome = outcome, outcome = outcome, finished = finished)
    path <- file.path(directory, "record.json")
    rr_write(path, record)
    list(path = path, record = rr_read_record(path))
  }
  stage <- function(source, status = "passed") list(source = source, environment = environment, status = status)
  observe <- function(request, units) rr_publish(state, request$record, rr_payload(request$record, "observation", units))
  terminal <- function(request, observation, units, reuse = list()) {
    for (unit in names(units)) units[[unit]]$observation_sequence <- observation$sequence
    payload <- rr_payload(request$record, "evidence", units, reuse)
    payload$observation <- observation$sequence
    rr_publish(state, request$record, payload)
  }
  a <- make(finished = now - 80)
  a_observation <- observe(a, list(`build-full` = stage(source_a)))
  b <- make(1L, finished = now - 90)
  b_observation <- observe(b, list(`build-full` = stage(source_a, "failed")))
  terminal(b, b_observation, list(`build-full` = stage(source_a, "failed")))
  # The success is published later, but its execution observation came first.
  terminal(a, a_observation, list(`build-full` = stage(source_a)))
  c <- make(finished = now - 30)
  c_observation <- observe(c, list(`build-full` = stage(source_c)))
  terminal(c, c_observation, list(`build-full` = stage(source_c)))
  read <- function() rr_with_lock(file.path(state, "evidence.lock"), rr_store_read(state))
  value <- read()
  origin <- function(id, records = value$records) Filter(function(entry) entry$payload$kind == "evidence" && entry$payload$request == id, records)[[1L]]
  expect(!rr_origin_valid(origin(a$record$id), "build-full", "build-full", value$records), "Observation order vetoes delayed success cleanup")
  expect(rr_origin_valid(origin(c$record$id), "build-full", "build-full", value$records), "Unrelated source remains reusable")
  retired <- rr_store_retire(value, b$record$id)
  expect(!"build-full" %in% unlist(origin(a$record$id, retired$records)$available), "Retiring a failure also retires the earlier success it vetoes")
  expect("build-full" %in% unlist(origin(c$record$id, retired$records)$available), "Retirement retains unrelated valid evidence")
  reference <- list(action = "reuse", request = c$record$id, unit = "build-full", submitted = c$record$sha)
  d <- make(reuse = reference)
  d_observation <- observe(d, list(generation = stage(source_c)))
  terminal(d, d_observation, list(generation = stage(source_c)), list(`build-full` = reference))
  value <- read()
  mixed <- rr_store_retire(value, c$record$id)
  expect("generation" %in% unlist(origin(d$record$id, mixed$records)$available), "Expired inherited origin preserves the request's own executed unit")
  expect(!any(vapply(mixed$records, function(entry) entry$payload$request == c$record$id, logical(1))), "Expired origin leaves no permanent inventory tombstone")
  unknown <- list(payload = list(repository = NULL, sequence = value$next_sequence, units = list(
    `build-full` = list(status = "unavailable", source = NULL, environment = NULL))))
  expect(!rr_origin_valid(origin(c$record$id), "build-full", "build-full", c(value$records, list(unknown))), "Unknown-input observation fences earlier success")
  unknown$payload$request <- rr_id()
  retired_unknown <- rr_store_retire(value, unknown$payload$request, observations = list(unknown))
  expect(!"build-full" %in% unlist(origin(c$record$id, retired_unknown$records)$available),
    "Retiring an unobserved request cannot resurrect a success it fenced")
  root <- rr_store_root(state)
  pointer <- rr_read(file.path(root, "current.json"))
  link <- rr_read_json(file.path(root, "current.json"))
  key_path <- file.path(root, link$generation, "key.bin")
  original_key <- rr_read(key_path)
  Sys.chmod(key_path, "0644")
  rejects(read(), "owner/mode")
  Sys.chmod(key_path, "0600")
  rr_write(key_path, openssl::rand_bytes(32), raw = TRUE)
  rejects(read(), "authentication")
  rr_write(key_path, original_key, raw = TRUE)
  unlink(file.path(root, "current.json"))
  rejects(rr_with_lock(file.path(state, "evidence.lock"), rr_store_read(state, create = TRUE)), "Incomplete evidence publication")
  rr_write(file.path(root, "current.json"), pointer, raw = TRUE)
  renewed <- read()
  renewed$key_created <- now - rr_week
  renewed <- rr_store_retire(renewed, character(), now)
  rr_with_lock(file.path(state, "evidence.lock"), rr_store_write(state, renewed))
  after <- read()
  expect(!identical(original_key, after$key) && rr_same(value$records, after$records), "Physical key renewal reauthenticates the same ordered records")
  expect(length(list.dirs(root, recursive = FALSE)) == 1L, "Published generation compacts obsolete keys/inventories")
  damaged <- rr_read(a$path)
  envelope <- rr_read_json(a$path); envelope$record$boot <- "11111111-1111-1111-1111-111111111111"
  rr_write(a$path, envelope)
  rejects(rr_read_record(a$path), "Damaged")
  rr_write(a$path, damaged, raw = TRUE)
  missing <- tempfile("missing-journal-", tmpdir = state)
  expect(file.rename(b$path, missing), "Temporarily remove an indexed failure journal")
  rejects(read(), "cannot open")
  expect(any(vapply(rr_requests(state), function(item) identical(item$path, b$path) && !is.null(item$error), logical(1))),
    "A missing ownership journal remains protected recovery state")
  expect(file.rename(missing, b$path), "Restore the indexed failure journal")
  unknown_record <- a$record; unknown_record$protocol <- rr_protocol + 1L
  rr_write(a$path, unknown_record)
  unknown_bytes <- rr_read(a$path)
  rejects(rr_recover(state, a$record$id), "Unknown request version")
  expect(identical(rr_read(a$path), unknown_bytes), "Unknown versions refuse recovery mutations")
  rr_write(a$path, a$record)
  wrong_user <- read(); wrong_user$records[[1L]]$payload$user <- rr_uid() + 1L
  rr_with_lock(file.path(state, "evidence.lock"), rr_store_write(state, wrong_user))
  rejects(read(), "provenance/order")
  rr_with_lock(file.path(state, "evidence.lock"), rr_store_write(state, after))
  forged <- after
  submitted <- paste(rep("0", 40), collapse = "")
  for (i in seq_along(forged$records)) if (forged$records[[i]]$payload$request == d$record$id) {
    forged$records[[i]]$payload$selection$plan$units[["build-full"]]$submitted <- submitted
    if (forged$records[[i]]$payload$kind == "evidence") forged$records[[i]]$payload$reused[["build-full"]]$submitted <- submitted
  }
  changed <- d$record; changed$selection$plan$units[["build-full"]]$submitted <- submitted
  rr_write(d$path, changed)
  rr_with_lock(file.path(state, "evidence.lock"), rr_store_write(state, forged))
  rejects(read(), "inherited origin")
  rr_write(d$path, d$record)
  rr_with_lock(file.path(state, "evidence.lock"), rr_store_write(state, after))
  incomplete <- rr_finish
  environment(incomplete) <- new.env(parent = environment(rr_finish))
  snapshot <- list(sources = list(), environment = list(digest = "fixture"))
  environment(incomplete)$rr_worker_result <- function(...) list(before = snapshot, after = snapshot, report = list())
  environment(incomplete)$rr_observe <- function(...) list(sequence = 1L)
  environment(incomplete)$rr_cleanup <- function(...) invisible()
  environment(incomplete)$rr_publish <- function(...) invisible()
  original <- rr_read(d$path)
  invisible(capture.output({ outcome <- incomplete(state, d$path, d$record, 0L) }))
  expect(outcome == 1L && rr_read_record(d$path)$outcome == 1L,
    "Incomplete mandatory reports cannot terminally pass through reused work")
  rr_write(d$path, original, raw = TRUE)
  later <- make(1L)
  later_observation <- observe(later, list(`build-full` = stage(source_c, "failed")))
  terminal(later, later_observation, list(`build-full` = stage(source_c, "failed")))
  rejects(rr_publish(state, d$record, rr_payload(d$record, "evidence", list(), list(`build-full` = reference))), "superseded")
  future <- make(finished = now + 3600)
  expect(!length(rr_requests(state)), "Clock rollback does not turn completed history into active ownership")
  expect(!future$record$id %in% rr_history(state, now), "Future history remains protected from age-based deletion")
  interrupted <- make(1L)
  interrupted$record$state <- "running"; interrupted$record$owned_capacity <- TRUE
  interrupted$record$cleanup <- "pending"
  rr_write(interrupted$path, interrupted$record)
  publisher <- rr_publish
  environment(publisher) <- new.env(parent = environment(rr_publish))
  environment(publisher)$rr_store_write <- function(...) stop("Fixture replacement interrupted")
  error <- tryCatch(publisher(state, interrupted$record,
    rr_payload(interrupted$record, "observation", list(`build-full` = stage(source_a, "failed")))), error = identity)
  expect(inherits(error, "marginplyr_review_publication_error"), "An interrupted write differs from an unavailable cache read")
  finish <- rr_finish
  environment(finish) <- new.env(parent = environment(rr_finish))
  environment(finish)$rr_observe <- function(...) stop(error)
  cleaned <- FALSE
  environment(finish)$rr_cleanup <- function(...) { cleaned <<- TRUE }
  rejects(finish(state, interrupted$path, interrupted$record, 1L), "Interrupted evidence publication")
  retained <- rr_read_record(interrupted$path)
  expect(!cleaned && retained$state == "running" && retained$owned_capacity,
    "Interrupted observation writes preserve ownership before cleanup")
  recovery <- rr_recover
  environment(recovery) <- new.env(parent = environment(rr_recover))
  environment(recovery)$rr_boot <- function() interrupted$record$boot
  inspections <- 0L
  environment(recovery)$rr_processes <- function() {
    inspections <<- inspections + 1L
    if (inspections == 2L) stop("Fixture final inspection unavailable")
    list()
  }
  before_recovery <- rr_read(interrupted$path)
  rejects(recovery(state, interrupted$record$id), "Fixture final inspection unavailable")
  expect(inspections == 2L && identical(before_recovery, rr_read(interrupted$path)),
    "A failed final shutdown observation cannot release ownership or rewrite its journal")
  prior_boot <- make(1L)
  prior_boot$record$state <- "recovery"; prior_boot$record$cleanup <- "unconfirmed"
  prior_boot$record$authorized <- FALSE; prior_boot$record$owned_capacity <- TRUE
  rr_write(prior_boot$path, prior_boot$record)
  invisible(capture.output({ recovered <- rr_recover(state, prior_boot$record$id) }))
  expect(recovered == 0L && rr_read_record(prior_boot$path)$outcome == 130L,
    "Recovery after an earlier OS boot confirms interruption without running a check")
  cat("Repository runner observation, retirement, mixed-origin and key-store contracts passed.\n")
}
run_store_contracts()

run_marker_contract <- function() {
  token <- rr_id()
  environment <- Sys.getenv()
  environment[["MARGINPLYR_REVIEW_REQUEST"]] <- token
  child <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", "Sys.sleep(60)"),
    env = environment, cleanup = FALSE, supervise = FALSE)
  on.exit({ if (child$is_alive()) child$kill(); child$wait() }, add = TRUE)
  record <- list(token = token, children = list(), group = NULL)
  deadline <- rr_now() + 5
  repeat {
    table <- rr_processes()
    if (as.character(child$get_pid()) %in% rr_owned(table, record) || rr_now() >= deadline) break
    Sys.sleep(0.05)
  }
  expect(as.character(child$get_pid()) %in% rr_owned(table, record),
    "An unregistered detached child remains owned through its request marker")
}
run_marker_contract()

run_process_contracts <- function() {
fixture <- tempfile("repository-runner-contract-", tmpdir = Sys.getenv("TMPDIR", tempdir()))
dir.create(fixture)
fixture <- normalizePath(fixture, mustWork = TRUE)
jobs <- list()
on.exit({
  for (job in jobs) if (job$is_alive()) { job$signal(2L); job$wait(15000); if (job$is_alive()) job$kill() }
  table <- rr_processes()
  if (exists("state") && dir.exists(file.path(state, "requests"))) {
    for (item in rr_requests(state, active = FALSE)) if (!is.null(item$record)) {
      for (attempt in 1:100) {
        owned <- rr_owned(table, item$record)
        if (!length(owned)) break
        for (row in table[owned]) if (rr_alive(row$identity)) ps::ps_send_signal(ps::ps_handle(row$identity$pid), 9L)
        Sys.sleep(0.05)
        table <- rr_processes()
      }
      if (length(owned)) rr_stop("Fixture processing remains; workspace retained: ", fixture)
    }
  }
  # A fault may leave an unreadable journal. The private fixture boundary still
  # attributes its live children; stop them before deleting their control tree.
  for (attempt in 1:100) {
    table <- rr_processes()
    owned <- Filter(function(row) {
      path <- unname(row$env["MARGINPLYR_REVIEW_RECORD"])
      length(path) == 1L && !is.na(path) && startsWith(path, paste0(fixture, "/"))
    }, table)
    if (!length(owned)) break
    for (row in owned) if (rr_alive(row$identity)) ps::ps_send_signal(ps::ps_handle(row$identity$pid,
      as.POSIXct(as.numeric(row$identity$birth), origin = "1970-01-01", tz = "UTC")), 9L)
    Sys.sleep(0.05)
  }
  if (length(owned)) rr_stop("Fixture children remain; workspace retained: ", fixture)
  unlink(fixture, recursive = TRUE)
}, add = TRUE)
repo <- file.path(fixture, "repo")
home <- file.path(fixture, "account")
dir.create(repo); dir.create(home)
archive <- file.path(fixture, "source.tar")
rr_git(candidate, "archive", "--format=tar", paste0("--output=", archive), "HEAD")
utils::untar(archive, exdir = repo)
for (path in c(rr_runner_sources, "tools/test-review-ready-runner.R", "tools/test-review-ready-inherited.R", "tools/test-review-ready-capabilities.R", "tools/test-review-ready-migration.R", "tools/test-review-ready-renderer-compatibility.R", "tools/review-ready-environment.R", "tools/review-ready-check.R", "tools/review-ready-check-lib.R")) {
  file.copy(file.path(candidate, path), file.path(repo, path), overwrite = TRUE)
}
cat("\nrr_account_home <- function() ", deparse(home), "\n", file = file.path(repo, "tools/review-ready-core.R"), append = TRUE)
cat('\nrr_environment <- function(root) { mutable <- Sys.getenv("RR_FIXTURE_MUTABLE"); list(digest = rr_digest(list(input = Sys.getenv("RR_FIXTURE_INPUT", "same"), mutable = if (nzchar(mutable)) rr_hash(rr_read(mutable)) else NULL)), clock = list(r = "4.6.1", checker = "2efcaa5190c038e60834bd5c4e502c82", timeout = "34ee7dea5eac7ba7138bd3c60651faee", leeway = 300), r = "4.6.1", libraries = list()) }\n',
  file = file.path(repo, "tools/review-ready-evidence.R"), append = TRUE)
cat(paste0('\nrr_renderer <- function() {\n',
  ' invalid <- Sys.getenv("RR_FIXTURE_RENDERER_INVALID"); if (nzchar(invalid) && file.exists(invalid)) rr_stop("Fixture renderer storage became incompatible")\n',
  ' writeLines("Metadata query output", file.path(Sys.getenv("MARGINPLYR_REVIEW_ISOLATION"), "cache", "renderer-capability.fixture"))\n',
  ' if (Sys.getenv("RR_FIXTURE_MUTATE_BEFORE") == "yes" && Sys.getenv("MARGINPLYR_REVIEW_EPOCH") == "1") writeLines("Changed before validation", Sys.getenv("RR_FIXTURE_MUTABLE"))\n',
  ' if (Sys.getenv("RR_FIXTURE_WORKER_ACTION") == "promote" && Sys.getenv("MARGINPLYR_REVIEW_EPOCH") == "1") {\n',
  '  child <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", "Sys.sleep(60)"), cleanup = FALSE, supervise = FALSE)\n',
  '  writeLines(as.character(child$get_pid()), Sys.getenv("RR_FIXTURE_PREPARATION_PID"))\n',
  '  rr_rpc("promote", "Fixture preparation requires Full"); repeat Sys.sleep(.1)\n',
  ' }; list(fixture = TRUE)\n}\n'), file = file.path(repo, "tools/review-ready-evidence.R"), append = TRUE)
cat(paste0('\nrr_plan_fixture_original <- rr_plan\n',
  'rr_plan <- function(root, sha, selection, ...) {\n',
  ' if (Sys.getenv("RR_FIXTURE_FORCE_EXECUTION") == "yes") return(rr_execute_plan(selection))\n',
  ' rr_plan_fixture_original(root, sha, selection, ...)\n}\n'), file = file.path(repo, "tools/review-ready-manager.R"), append = TRUE)
cat(paste0('\nrr_write_fixture_original <- rr_write\n',
  'rr_write <- function(path, value, raw = FALSE) {\n',
  ' result <- rr_write_fixture_original(path, value, raw)\n',
  ' trigger <- Sys.getenv("RR_FIXTURE_PUBLICATION_TRIGGER")\n',
  ' if (nzchar(trigger) && file.exists(trigger) && basename(path) == "index.json") {\n',
  '  writeLines(path, paste0(trigger, ".ready")); rr_say("Fixture publication replacement prepared"); repeat Sys.sleep(.1)\n',
  ' }; invisible(result)\n}\n'), file = file.path(repo, "tools/review-ready-manager.R"), append = TRUE)
cat(paste0('\nrr_cleanup_fixture_original <- rr_cleanup\n',
  'rr_cleanup <- function(record, directory) {\n',
  ' if (Sys.getenv("RR_FIXTURE_CLEANUP_FAIL") == "yes") rr_stop("Fixture cleanup failure")\n',
  ' release <- Sys.getenv("RR_FIXTURE_CLEANUP_RELEASE")\n',
  ' if (nzchar(release)) { rr_say("Fixture disposable cleanup waiting"); while (!file.exists(release)) Sys.sleep(.05) }\n',
  ' rr_cleanup_fixture_original(record, directory)\n}\n'), file = file.path(repo, "tools/review-ready-manager.R"), append = TRUE)
cat(paste0('\nrr_now_fixture_original <- rr_now\n',
  'rr_now <- function() { trigger <- Sys.getenv("RR_FIXTURE_CLOCK_ROLLBACK"); ',
  'rr_now_fixture_original() - if (nzchar(trigger) && file.exists(trigger)) 3600 else 0 }\n'),
  file = file.path(repo, "tools/review-ready-manager.R"), append = TRUE)
# This is an external package-check stand-in, never a real Full result. The
# actual R entry/manager, Git, OS locks, signals, records and authentication run.
writeLines(c(readLines(file.path(candidate, "tools/review-ready-check-lib.R")),
  'rr_fixture_read_selection <- read_review_ready_selection',
  'read_review_ready_selection <- function() {',
  '  transport <- Sys.getenv("RR_FIXTURE_TRANSPORT")',
  '  if (transport == "missing") unlink(Sys.getenv("MARGINPLYR_REVIEW_SELECTION"))',
  '  if (transport == "replaced") writeLines("Path: Repository", Sys.getenv("MARGINPLYR_REVIEW_SELECTION"))',
  '  rr_fixture_read_selection()',
  '  jsonlite::fromJSON(Sys.getenv("MARGINPLYR_REVIEW_RECORD"), simplifyVector = FALSE)$record$selection',
  '}',
  'review_ready_check_cli <- function(args, expected_root, selection, diagnostic_root, accepted_root) {',
  '  force(selection)',
  '  cached <- list.files(file.path(Sys.getenv("MARGINPLYR_REVIEW_ISOLATION"), "cache"), all.files = TRUE, recursive = TRUE, full.names = TRUE)',
  '  if (any(!dir.exists(cached))) stop("Validation started with populated renderer caches")',
  '  record <- jsonlite::fromJSON(Sys.getenv("MARGINPLYR_REVIEW_RECORD"), simplifyVector = FALSE)$record',
  '  survivor <- Sys.getenv("RR_FIXTURE_SURVIVOR")',
  '  if (nzchar(survivor)) { child <- processx::process$new("/bin/sleep", "60", cleanup = FALSE, supervise = FALSE); writeLines(as.character(child$get_pid()), survivor) }',
  '  detached <- Sys.getenv("RR_FIXTURE_DETACHED")',
  '  if (nzchar(detached)) {',
  '    code <- paste0("args <- commandArgs(TRUE); ",',
  '      if (Sys.getenv("RR_FIXTURE_RESISTANT") == "yes") "dll <- dyn.load(args[[3L]]); invisible(.Call(getDLLRegisteredRoutines(dll)$.Call$install_signals)); " else "",',
  '      "writeLines(as.character(Sys.getpid()), args[[1L]]); while (!file.exists(args[[2L]])) Sys.sleep(.05)")',
  '    child <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", code, detached, paste0(detached, ".release"),',
  '      file.path(record$workspace, "signals", paste0("review_ready_signals", .Platform$dynlib.ext))), cleanup = FALSE, supervise = FALSE)',
  '    while (!file.exists(detached)) Sys.sleep(.05)',
  '  }',
  '  cat("FIXTURE GATE STARTED\\n"); flush.console()',
  '  release <- Sys.getenv("RR_FIXTURE_RELEASE")',
  '  if (nzchar(release)) { while (!file.exists(release)) Sys.sleep(0.05) } else Sys.sleep(as.numeric(Sys.getenv("RR_FIXTURE_SECONDS", "0.1")))',
  '  units <- names(Filter(function(unit) unit$action == "execute", selection$plan$units))',
  '  attempt <- list(status=0L, timeout=FALSE, errors=0L, warnings=0L, notes=list())',
  '  report <- list(request = record$token, units = stats::setNames(as.list(rep("passed",length(units))), units),',
  '    completed=as.list(c("Package spelling", "jarl", "package-aware lintr", "Test dependency syntax", "Required test Suggests", paste("Artifact verification:", unlist(selection$verifiers)))), omitted=list(),',
  '    timestamps=list(observed=as.numeric(Sys.time()), paths=list(DESCRIPTION=as.numeric(Sys.time()))),',
  '    check=c(attempt,list(args=as.list(c("--as-cran",if(selection$path=="Documentation") "--no-tests")),attempts=list(attempt))))',
  '  if (Sys.getenv("RR_FIXTURE_GATE_STATUS") == "1" && length(units)) report$units[[units[[1L]]]] <- "running"',
  '  writeLines(jsonlite::toJSON(report, auto_unbox=TRUE, null="null", digits=NA), Sys.getenv("MARGINPLYR_REVIEW_STAGE_REPORT"))',
  '  invalid <- Sys.getenv("RR_FIXTURE_RENDERER_INVALID"); if (nzchar(invalid)) file.create(invalid)',
  '  if (Sys.getenv("RR_FIXTURE_MUTATE_AFTER") == "yes") writeLines("Changed during validation", Sys.getenv("RR_FIXTURE_MUTABLE"))',
  '  trigger <- Sys.getenv("RR_FIXTURE_PUBLICATION_TRIGGER"); if (nzchar(trigger)) file.create(trigger)',
  '  cat("FIXTURE GATE FINISHED\\n")',
  '  as.integer(Sys.getenv("RR_FIXTURE_GATE_STATUS", "0"))',
  '}'
), file.path(repo, "tools/review-ready-check-lib.R"))
rr_run("git", c("init", "-q", "-b", "main"), repo)
rr_git(repo, "config", "user.name", "Runner fixture")
rr_git(repo, "config", "user.email", "fixture@example.invalid")
rr_git(repo, "remote", "add", "origin", "https://github.com/sayuks/marginplyr.git")
rr_git(repo, "add", ".")
rr_git(repo, "commit", "-qm", "Fixture accepted R authority")
rr_git(repo, "checkout", "-qb", "candidate")
writeLines("fixture package change", file.path(repo, "NEW-INPUT"))
rr_git(repo, "add", ".")
rr_git(repo, "commit", "-qm", "Fixture Full obligation")
invoke <- function(args = character(), extra = character(), root = repo) {
  environment <- c(Sys.getenv(), extra)
  environment <- environment[!duplicated(names(environment), fromLast = TRUE)]
  rr_run(file.path(R.home("bin"), "Rscript"), c("tools/review-ready.R", args), root, environment, error = FALSE)
}
state <- rr_mkdir(file.path(home, ".local/state/marginplyr/review-ready"))
rr_write(file.path(state, "delivery.json"), list(protocol = rr_protocol, fixture = TRUE))
first <- invoke()
expect(first$status == 0L && grepl("terminal outcome: 0", first$stdout), paste("Full fixture lifecycle:", first$stdout, first$stderr))
delegation_environment <- Sys.getenv()
delegation_environment <- delegation_environment[!startsWith(names(delegation_environment), "MARGINPLYR_REVIEW_")]
delegated <- rr_run(file.path(R.home("bin"), "Rscript"), "tools/review-ready-check.R", repo,
  env = delegation_environment, error = FALSE)
expect(delegated$status == 0L && grepl("terminal outcome: 0", delegated$stdout),
  paste("The former direct R gate delegates to the repository entry after cutover:", delegated$stdout, delegated$stderr))
requests <- rr_requests(state, active = FALSE)
requests <- Filter(function(item) identical(item$record$selection$plan$units[["build-full"]]$action, "execute"), requests)
expect(length(requests) == 1L && requests[[1L]]$record$cleanup == "confirmed", "Confirmed request cleanup")
record <- requests[[1L]]$record
expect(!dir.exists(record$workspace) && !dir.exists(file.path(dirname(requests[[1L]]$path), "cache")), "Temporary/cache cleanup")
store <- rr_with_lock(file.path(state,"evidence.lock"), rr_store_read(state))
own_records <- Filter(function(entry) identical(entry$payload$request, record$id), store$records)
executions <- Filter(function(entry) entry$payload$kind == "evidence" && "build-full" %in% names(entry$payload$units), store$records)
expect(length(own_records) == 2L && length(executions) == 1L,
  "Authenticated observation and terminal origin; delegation reuse adds no execution")
writeLines("# fixture prose", file.path(repo, "design", "fixture-followup.md"))
rr_git(repo, "add", ".")
rr_git(repo, "commit", "-qm", "Fixture prose follow-up")
second <- invoke()
expect(second$status == 0L && grepl('"action":"reuse"', second$stdout, fixed=TRUE), paste("Actual origin reuse fixture:",second$stdout,second$stderr))
status <- invoke("--status")
expect(status$status == 0L && grepl("No check was run", status$stdout), "Status is not check evidence")
entry_root <- rr_mkdir(file.path(fixture, "other-entry"))
rr_mkdir(file.path(entry_root, "tools"))
for (path in rr_runner_sources) file.copy(file.path(repo, path), file.path(entry_root, path))
other_entry <- rr_run(file.path(R.home("bin"), "Rscript"), c(file.path(entry_root, "tools/review-ready.R")), repo,
  error = FALSE)
expect(other_entry$status == 2L && grepl("target worktree's tracked", other_entry$stdout, fixed = TRUE),
  "A different checkout cannot supply the target's entry generation")
empty_recovery <- invoke("--recover")
expect(empty_recovery$status == 0L && grepl("No check was run", empty_recovery$stdout), "Empty recovery is not check evidence")
terminal_recovery <- invoke(c("--recover", record$id))
expect(terminal_recovery$status == 0L && grepl("No check was run", terminal_recovery$stdout), "Terminal recovery is not check evidence")
unknown <- invoke("--skip")
expect(unknown$status != 0L && grepl("Use no arguments", unknown$stdout), "No caller-selected skip")

accepted <- rr_git(repo, "rev-parse", "main")
submitted <- rr_git(repo, "rev-parse", "HEAD")
rr_git(repo, "rm", "--quiet", "tools/review-ready-progress.R")
rr_git(repo, "commit", "-qm", "Missing required candidate source")
missing_source <- invoke()
unfinished_setup <- rr_requests(state)
expect(missing_source$status == 2L && length(unfinished_setup) == 1L && !is.null(unfinished_setup[[1L]]$record),
  "Missing candidate contract sources retain recoverable setup ownership")
setup_recovery <- invoke(c("--recover", unfinished_setup[[1L]]$record$id))
expect(setup_recovery$status == 0L && grepl("No check was run", setup_recovery$stdout, fixed = TRUE),
  "Missing-source setup can be recovered without the missing helper")
rr_git(repo, "checkout", "--quiet", "--detach", submitted)
empty <- rr_select(repo, accepted, accepted)
expect(empty$path == "Full" && identical(empty$base, accepted), "Empty valid difference keeps its established base under Full")
rr_git(repo, "update-ref", "-d", "refs/heads/main")
missing <- invoke()
expect(missing$status == 2L && grepl("Missing local main authority", missing$stdout), "Missing authority never self-approves")
rr_git(repo, "update-ref", "refs/heads/main", accepted)
writeLines("dirty", file.path(repo, "UNTRACKED"))
expect(invoke()$status == 2L && invoke("--status")$status == 0L, "Dirty HEAD refuses checks but permits status")
unlink(file.path(repo, "UNTRACKED"))
inspection_bin <- rr_mkdir(file.path(fixture, "no-inspection"))
writeLines(c("#!/bin/sh", "exit 1"), file.path(inspection_bin, "ps"))
Sys.chmod(file.path(inspection_bin, "ps"), "0755")
before_inspection <- length(rr_requests(state, active = FALSE))
unsupported <- invoke(extra = c(PATH = paste(inspection_bin, Sys.getenv("PATH"), sep = .Platform$path.sep)))
expect(unsupported$status == 2L && !grepl("FIXTURE GATE STARTED", unsupported$stdout, fixed = TRUE) &&
    length(rr_requests(state, active = FALSE)) == before_inspection, "Unavailable process inspection fails setup before journaled work")
rr_git(repo, "checkout", "--quiet", "--detach")
detached <- invoke()
expect(detached$status == 0L && grepl('"request":"', detached$stdout, fixed = TRUE), "Detached committed revision is eligible")

clone <- file.path(fixture, "clone")
rr_run("git", c("clone", "--quiet", repo, clone))
rr_git(clone, "remote", "set-url", "origin", "https://github.com/sayuks/marginplyr.git")
rr_git(clone, "config", "user.name", "Runner fixture")
rr_git(clone, "config", "user.email", "fixture@example.invalid")
rr_git(clone, "update-ref", "refs/heads/main", accepted)
cross_clone <- invoke(root = clone)
expect(cross_clone$status == 0L && grepl(paste0('"request":"', record$id, '"'), cross_clone$stdout, fixed = TRUE), "Cross-clone reuse retains original execution")
clone_head <- rr_git(clone, "rev-parse", "HEAD")
rr_git(clone, "checkout", "--quiet", "--detach", accepted)
rr_git(clone, "read-tree", "--reset", "-u", clone_head)
rr_git(clone, "commit", "-qm", "Same tree on a sibling without executing ancestor provenance")
nonancestor <- invoke(root = clone)
expect(nonancestor$status == 0L && grepl('"build-full":{"action":"execute"}', nonancestor$stdout, fixed = TRUE),
  paste("An identical tree without the executing ancestor cannot reuse its origin:", nonancestor$stdout, nonancestor$stderr))
rr_git(clone, "checkout", "--quiet", "--detach", clone_head)
rr_git(clone, "remote", "set-url", "origin", "https://github.com/fixture/another-repository.git")
different_repository <- invoke(root = clone)
expect(different_repository$status == 0L && grepl('"build-full":{"action":"execute"}', different_repository$stdout, fixed = TRUE),
  "A different repository cannot inherit an executing origin")
rr_git(clone, "remote", "set-url", "origin", "https://github.com/sayuks/marginplyr.git")
for (transport in c("missing", "replaced")) {
  damaged <- invoke(extra = c(RR_FIXTURE_TRANSPORT = transport))
  expect(damaged$status != 0L && !grepl("FIXTURE GATE STARTED", damaged$stdout, fixed = TRUE),
    paste("Invalid transport cannot authorize or fall back to Full:", transport))
}
renderer_invalid <- invoke(extra = c(RR_FIXTURE_INPUT = "post-renderer-incompatible",
  RR_FIXTURE_RENDERER_INVALID = file.path(fixture, "incompatible-renderer")))
expect(renderer_invalid$status != 0L && grepl("renderer storage became incompatible", renderer_invalid$stdout, fixed = TRUE),
  "Renderer isolation becoming incompatible during execution cannot produce a passing invocation")
mutable <- file.path(fixture, "mutable-environment")
writeLines("Original", mutable)
mutable_env <- c(RR_FIXTURE_INPUT = "mutable-environment", RR_FIXTURE_MUTABLE = mutable)
expect(invoke(extra = mutable_env)$status == 0L, "Establish a mutable-environment execution origin")
changed_during <- invoke(extra = c(mutable_env, RR_FIXTURE_MUTATE_AFTER = "yes"))
expect(changed_during$status == 1L && !grepl('"target":"Full"', changed_during$stdout, fixed = TRUE),
  "An environment change during reused validation fails without replacing the gate with Full")
writeLines("Original", mutable)
changed_before <- invoke(extra = c(mutable_env, RR_FIXTURE_MUTATE_BEFORE = "yes"))
changed_id <- regmatches(changed_before$stdout, regexec("Review-ready request: ([0-9a-f]{32});", changed_before$stdout))[[1L]][[2L]]
changed_record <- rr_read_record(file.path(state, "requests", changed_id, "record.json"))
expect(changed_before$status == 0L && changed_record$epoch == 2L && length(changed_record$promotions) == 1L,
  "Pre-gate loss of reused eligibility strengthens the same request to admitted Full execution")
baseline <- invoke(extra = c(RR_FIXTURE_INPUT = "negative-observation"))
expect(baseline$status == 0L, "A new environment establishes its own fixture origin")
failure <- invoke(extra = c(RR_FIXTURE_INPUT = "negative-observation", RR_FIXTURE_FORCE_EXECUTION = "yes", RR_FIXTURE_GATE_STATUS = "1",
  RR_FIXTURE_RENDERER_INVALID = file.path(fixture, "renderer-after-failed-gate")))
expect(failure$status == 1L, "A failed execution keeps its failure outcome")
after_failure <- invoke(extra = c(RR_FIXTURE_INPUT = "negative-observation"))
expect(after_failure$status == 0L && grepl('"build-full":{"action":"execute"}', after_failure$stdout, fixed = TRUE),
  "A later same-input failed stage vetoes the older pass")

start <- function(root = repo, seconds = "20", input = "cancel", release = "", survivor = "", action = "", preparation = "", publication = "", cleanup = "", detached = "", resistant = "", rollback = "") {
  environment <- c(Sys.getenv(), RR_FIXTURE_SECONDS = seconds, RR_FIXTURE_INPUT = input, RR_FIXTURE_RELEASE = release,
    RR_FIXTURE_SURVIVOR = survivor, RR_FIXTURE_WORKER_ACTION = action, RR_FIXTURE_PREPARATION_PID = preparation,
    RR_FIXTURE_PUBLICATION_TRIGGER = publication, RR_FIXTURE_CLEANUP_RELEASE = cleanup,
    RR_FIXTURE_DETACHED = detached, RR_FIXTURE_RESISTANT = resistant, RR_FIXTURE_CLOCK_ROLLBACK = rollback)
  environment <- environment[!duplicated(names(environment), fromLast = TRUE)]
  log <- file.path(fixture, paste0("job-", length(jobs), ".log"))
  job <- processx::process$new(file.path(R.home("bin"), "Rscript"), "tools/review-ready.R",
    wd = root, env = environment, stdout = log, stderr = "2>&1", cleanup = TRUE)
  jobs[[length(jobs) + 1L]] <<- job
  list(job = job, log = log)
}
until <- function(run, pattern, timeout = 45) {
  deadline <- rr_now() + timeout
  while (rr_now() < deadline && run$job$is_alive()) {
    if (file.exists(run$log) && grepl(pattern, rr_text(run$log), fixed = TRUE)) return(invisible())
    Sys.sleep(0.05)
  }
  cat(if (file.exists(run$log)) rr_text(run$log), "\n")
  fail(paste("Missing fixture observation:", pattern))
}
finish <- function(run, status) {
  run$job$wait(45000)
  expect(identical(run$job$get_exit_status(), status), paste("Fixture terminal exit:", rr_text(run$log)))
}
cached_cancel <- start(input = "same", release = file.path(fixture, "never-cached-release"))
until(cached_cancel, "FIXTURE GATE STARTED")
cached_record <- Filter(function(item) !is.null(item$record) && item$record$state == "running", rr_requests(state))[[1L]]$record
expect(cached_record$capacity == "exempt" && cached_record$selection$plan$units[["build-full"]]$action == "reuse",
  "Cached Full work runs under independent supervision")
cached_cancel$job$signal(15L)
finish(cached_cancel, 130L)
after_cached_cancel <- invoke()
expect(after_cached_cancel$status == 0L && grepl(paste0('"request":"', cached_record$selection$plan$units[["build-full"]]$request, '"'), after_cached_cancel$stdout, fixed = TRUE),
  "Reused cancellation retains exit 130 and the original executing origin")
failed_cleanup <- invoke(extra = c(RR_FIXTURE_INPUT = "same", RR_FIXTURE_FORCE_EXECUTION = "yes",
  RR_FIXTURE_GATE_STATUS = "1", RR_FIXTURE_CLEANUP_FAIL = "yes"))
expect(failed_cleanup$status == 2L, "Cleanup failure retains unfinished ownership")
cleanup_record <- Filter(function(item) !is.null(item$record) && item$record$state == "recovery", rr_requests(state))[[1L]]$record
expect(cleanup_record$owned_capacity && cleanup_record$cleanup == "unconfirmed", "Failed current cleanup cannot release confirmed Full ownership")
cleanup_recovery <- invoke(c("--recover", cleanup_record$id), root = clone)
expect(cleanup_recovery$status == 0L && grepl("No check was run", cleanup_recovery$stdout, fixed = TRUE),
  "Recovery completes failed cleanup without a successful execution")
after_cleanup <- invoke()
expect(after_cleanup$status == 0L && grepl('"build-full":{"action":"execute"}', after_cleanup$stdout, fixed = TRUE),
  "A failed stage survives cleanup failure and recovery to veto the earlier pass")
cleanup_release <- file.path(fixture, "cleanup-release")
cleaning <- start(seconds = ".1", input = "cleanup-holder", cleanup = cleanup_release)
until(cleaning, "Fixture disposable cleanup waiting")
live_record <- Filter(function(item) !is.null(item$record) && item$record$state == "running", rr_requests(state))[[1L]]$record
expect(invoke(c("--recover", live_record$id), root = clone)$status == 4L && cleaning$job$is_alive(),
  "Recovery refuses a live manager without signalling it")
cleanup_waiter <- start(seconds = ".1", input = "cleanup-waiter")
until(cleanup_waiter, "Waiting for Review-ready capacity")
expect(!grepl("FIXTURE GATE STARTED", rr_text(cleanup_waiter$log), fixed = TRUE), "Full capacity remains held through disposable-source cleanup")
file.create(cleanup_release)
finish(cleaning, 0L); finish(cleanup_waiter, 0L)
cancel <- start()
until(cancel, "FIXTURE GATE STARTED")
cancel$job$signal(15L)
finish(cancel, 130L)
expect(grepl("cleanup: confirmed", rr_text(cancel$log), fixed=TRUE), "SIGTERM cancellation confirms cleanup")
detached_path <- file.path(fixture, "detached.pid")
detached <- start(seconds = ".1", input = "detached-parent-exit", detached = detached_path)
until(detached, "FIXTURE GATE FINISHED")
detached$job$wait(500)
slot <- rr_lock(file.path(state, "admission.lock"))
if (!is.null(slot)) filelock::unlock(slot)
expect(detached$job$is_alive() && is.null(slot), "A normal parent exit retains Full capacity while its detached child writes")
file.create(paste0(detached_path, ".release"))
finish(detached, 0L)
sentinel <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", "Sys.sleep(60)"), cleanup = TRUE)
jobs[[length(jobs) + 1L]] <- sentinel
resistant_path <- file.path(fixture, "resistant.pid")
rollback <- file.path(fixture, "rollback-clock")
resistant <- start(seconds = "20", input = "resistant-child", detached = resistant_path, resistant = "yes", rollback = rollback)
until(resistant, "FIXTURE GATE STARTED")
resistant_pid <- as.integer(readLines(resistant_path))
resistant$job$signal(15L)
until(resistant, "Stopping owned processing")
file.create(rollback)
finish(resistant, 130L)
resistant_stopped <- tryCatch(ps::ps_status(ps::ps_handle(resistant_pid)) %in% c("zombie", "dead"), no_such_process = function(e) TRUE)
expect(resistant_stopped && sentinel$is_alive(), "Backward wall clocks cannot delay stopping TERM-resistant owned work or signal an unrelated process")
sentinel$kill(); sentinel$wait()

# Concurrent real managers use one Full lock; supported prose is independent.
rr_git(clone, "checkout", "--quiet", "-b", "prose", accepted)
writeLines("# Repository fixture", file.path(clone, "design", "fixture-only.md"))
rr_git(clone, "add", "."); rr_git(clone, "commit", "-qm", "Repository fixture")
release <- file.path(fixture, "release")
active <- start(input = "concurrency", release = release)
until(active, "FIXTURE GATE STARTED")
rr_git(repo, "update-ref", "refs/heads/main", rr_git(repo, "rev-parse", "HEAD"))
waiting <- start(seconds = "0.1", input = "concurrency")
until(waiting, "Waiting for Review-ready capacity")
limited <- invoke(root = clone)
expect(limited$status == 0L && grepl('"path":"Repository"', limited$stdout, fixed=TRUE), "Repository overlaps Full without a second Full slot")
waiting$job$signal(1L)
finish(waiting, 130L)
file.create(release)
finish(active, 0L)
active_record <- Filter(function(item) !is.null(item$record) && identical(item$record$manager$pid, active$job$get_pid()), rr_requests(state, active = FALSE))[[1L]]$record
expect(identical(active_record$main, accepted) && identical(active_record$selection$base, accepted),
  "A later main update cannot replace an admitted request's pinned authority or comparison")
rr_git(repo, "update-ref", "refs/heads/main", accepted)

promotion_release <- file.path(fixture, "promotion-release")
preparation <- file.path(fixture, "preparation.pid")
promoted <- start(root = clone, input = "promotion-active", release = promotion_release,
  action = "promote", preparation = preparation)
until(promoted, "FIXTURE GATE STARTED")
promotion_record <- Filter(function(item) !is.null(item$record) && item$record$state == "running", rr_requests(state))[[1L]]$record
expect(promotion_record$epoch == 2L && length(promotion_record$promotions) == 1L && promotion_record$owned_capacity,
  "Promotion preserves the request and acquires real Full capacity")
preparation_pid <- as.integer(readLines(preparation))
preparation_stopped <- tryCatch(ps::ps_status(ps::ps_handle(preparation_pid)) %in% c("zombie", "dead"), no_such_process = function(e) TRUE)
expect(preparation_stopped, "Promotion collects preparation children before Full validation")
promoted_waiter <- start(root = clone, input = "promotion-waiter", action = "promote",
  preparation = file.path(fixture, "waiter-preparation.pid"))
until(promoted_waiter, "Waiting for Review-ready capacity")
waiting_handle <- promoted_waiter$job$as_ps_handle()
cpu <- ps::ps_cpu_times(waiting_handle)
memory <- ps::ps_memory_info(waiting_handle)
rr_say("R waiting overhead: RSS bytes ", unname(memory[["rss"]]), "; CPU seconds ", sum(cpu[c("user", "system")]),
  "; elapsed seconds ", rr_now() - as.numeric(ps::ps_create_time(waiting_handle)), "; no package gate started while waiting.")
expect(!grepl("FIXTURE GATE STARTED", rr_text(promoted_waiter$log), fixed = TRUE), "Promoted requests share one Full slot")
promoted_waiter$job$signal(15L)
finish(promoted_waiter, 130L)
stale <- start(root = clone, input = "promotion-stale", action = "promote",
  preparation = file.path(fixture, "stale-preparation.pid"))
until(stale, "Waiting for Review-ready capacity")
writeLines("Dirty while waiting", file.path(clone, "UNTRACKED"))
finish(stale, 3L)
unlink(file.path(clone, "UNTRACKED"))
clean_stale <- start(root = clone, input = "promotion-clean-stale", action = "promote",
  preparation = file.path(fixture, "clean-stale-preparation.pid"))
until(clean_stale, "Waiting for Review-ready capacity")
clean_head <- rr_git(clone, "rev-parse", "HEAD")
rr_git(clone, "checkout", "--quiet", "--detach", accepted)
finish(clean_stale, 3L)
rr_git(clone, "checkout", "--quiet", "--detach", clean_head)
file.create(promotion_release)
finish(promoted, 0L)

publication_trigger <- file.path(fixture, "publication-trigger")
publishing <- start(input = "interrupted-publication", seconds = ".1", publication = publication_trigger)
until(publishing, "Fixture publication replacement prepared")
pending <- Filter(function(item) !is.null(item$record) && item$record$state != "terminal", rr_requests(state))[[1L]]$record
publishing$job$kill(); publishing$job$wait(5000)
expect(pending$authorized && pending$owned_capacity && file.exists(paste0(publication_trigger, ".ready")),
  "A complete unpublished replacement retains authorized ownership after manager death")
for (attempt in 1:100) {
  table <- rr_processes(); owned <- rr_owned(table, pending)
  if (!length(owned)) break
  for (row in table[owned]) if (rr_alive(row$identity)) ps::ps_send_signal(ps::ps_handle(row$identity$pid), 9L)
  Sys.sleep(0.05)
}
expect(!length(owned), "Interrupted publisher has confirmed stopped children")
repaired <- invoke(c("--recover", pending$id), root = clone)
expect(repaired$status == 0L && grepl("No check was run", repaired$stdout, fixed = TRUE),
  paste("Recovery completes the interrupted observation transaction:", repaired$stdout, repaired$stderr))
recovered_record <- rr_read_record(file.path(state, "requests", pending$id, "record.json"))
expect(recovered_record$outcome == 130L, "Recovery does not issue a successful execution origin")

original_record <- rr_read(requests[[1L]]$path)
corrupt <- rr_read_json(requests[[1L]]$path)
corrupt$checksum <- paste(rep("0", 64), collapse = "")
rr_write(requests[[1L]]$path, corrupt)
independent <- invoke(root = clone)
expect(independent$status == 0L && grepl('"path":"Repository"', independent$stdout, fixed=TRUE),
  "Damaged Full recovery state leaves independently supervised Repository work eligible")
blocked <- invoke()
expect(blocked$status == 4L, "Damaged ownership protects Full capacity")
rr_write(requests[[1L]]$path, original_record, raw = TRUE)

rr_git(clone, "checkout", "--quiet", "-b", "documentation", accepted)
cat("\nFixture documentation.\n", file = file.path(clone, "NEWS.md"), append = TRUE)
rr_git(clone, "add", "."); rr_git(clone, "commit", "-qm", "Documentation fixture")
documentation <- invoke(root = clone)
expect(documentation$status == 0L && grepl('"path":"Documentation"', documentation$stdout, fixed=TRUE) &&
    grepl('"build-documentation"', documentation$stdout, fixed=TRUE),
  paste("Documentation selects its own independent package boundary:", documentation$stdout, documentation$stderr))

parent_log <- file.path(fixture, "management-parent.log")
parent_environment <- c(Sys.getenv(), RR_FIXTURE_INPUT = "management-parent", RR_FIXTURE_SECONDS = "60")
parent_environment <- parent_environment[!duplicated(names(parent_environment), fromLast = TRUE)]
parent_code <- paste0('args <- commandArgs(TRUE); child <- processx::process$new(file.path(R.home("bin"), "Rscript"), ',
  '"tools/review-ready.R", wd=args[[1L]], stdout=args[[2L]], stderr="2>&1", cleanup=FALSE, supervise=FALSE); repeat Sys.sleep(.05)')
parent <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", parent_code, repo, parent_log),
  env = parent_environment, cleanup = TRUE)
jobs[[length(jobs) + 1L]] <- parent
parent_run <- list(job = parent, log = parent_log)
until(parent_run, "FIXTURE GATE STARTED")
parent_record <- Filter(function(item) !is.null(item$record) && item$record$state == "running", rr_requests(state))[[1L]]
parent$kill(); parent$wait()
deadline <- rr_now() + 45
repeat {
  observed <- rr_read_record(parent_record$path)
  if (observed$state == "terminal" || rr_now() >= deadline) break
  Sys.sleep(0.1)
}
expect(observed$state == "terminal" && observed$outcome == 130L && observed$cleanup == "confirmed" &&
    !length(rr_owned(rr_processes(), observed)), "Management-parent termination cancels and collects the request's owned work")

death <- start(input = "manager-death", release = file.path(fixture, "never-release"), survivor = file.path(fixture, "survivor.pid"))
until(death, "FIXTURE GATE STARTED")
death$job$kill()
death$job$wait(5000)
unfinished <- Filter(function(item) !is.null(item$record) && item$record$state != "terminal", rr_requests(state))
expect(length(unfinished) == 1L, "Manager death retains one recoverable request")
interrupted <- unfinished[[1L]]$record
expect(invoke(c("--recover", interrupted$id), root = clone)$status != 0L, "Surviving owned child prevents recovery")
expect(invoke()$status == 4L, "A killed manager's surviving work blocks another Full admission")
for (attempt in 1:100) {
  table <- rr_processes(); owned <- rr_owned(table, interrupted)
  if (!length(owned)) break
  for (row in table[owned]) ps::ps_send_signal(ps::ps_handle(row$identity$pid), 9L)
  Sys.sleep(0.05)
}
expect(!length(owned), "Fixture owner collects every interrupted child")
disappeared <- file.path(fixture, "disappeared")
expect(file.rename(repo, disappeared), "Remove originating checkout before recovery")
recovery <- invoke(c("--recover", interrupted$id), root = clone)
expect(recovery$status == 0L && grepl("No check was run", recovery$stdout), paste("Recovery without originating worktree:", recovery$stdout, recovery$stderr))
expect(file.rename(disappeared, repo), "Restore fixture checkout")

protected <- file.path(fixture, "protected")
dir.create(protected); writeLines("kept", file.path(protected, "sentinel"))
link <- file.path(fixture, "link")
expect(file.symlink(protected, link), "Create deletion-boundary fixture")
rejects(rr_remove(link, fixture), "symlink")
expect(file.exists(file.path(protected, "sentinel")), "Deletion leaves unrelated target untouched")
unlink(link)

attributes <- rr_git(repo, "rev-parse", "--path-format=absolute", "--git-path", "info/attributes")
writeLines("tools/review-ready-core.R export-ignore", attributes)
materialized <- file.path(fixture, "accepted-sources", "authority")
manifest <- rr_materialize_authority(repo, accepted, materialized)
expect(identical(rr_hash(rr_read(file.path(materialized, "tools/review-ready-core.R"))),
    rr_hash(rr_git_raw(repo, "show", paste0(accepted, ":tools/review-ready-core.R")))),
  "Pinned manager bytes bypass external archive attributes")
unlink(attributes)
authority_record <- list(workspace = dirname(materialized), authority = manifest)
rr_check_authority(authority_record)
authority_core <- file.path(materialized, "tools/review-ready-core.R")
cat("\n# Changed after pinning.\n", file = authority_core, append = TRUE)
rejects(rr_check_authority(authority_record), "Pinned manager source changed")
rr_materialize_authority(repo, accepted, materialized)
Sys.chmod(authority_core, "0755")
rejects(rr_check_authority(authority_record), "Pinned manager source changed")

request_environment <- Sys.getenv()
request_environment[["MARGINPLYR_REVIEW_REQUEST"]] <- "setup-child-ownership"
previous_environment <- options(marginplyr.review.env = request_environment)
expect(identical(trimws(rr_run(file.path(R.home("bin"), "Rscript"),
    c("-e", 'cat(Sys.getenv("MARGINPLYR_REVIEW_REQUEST"))'))$stdout), "setup-child-ownership"),
  "Setup subprocesses inherit persisted request ownership")
original_request <- Sys.getenv("MARGINPLYR_REVIEW_REQUEST", unset = NA_character_)
expect(identical(rr_in_environment(request_environment, Sys.getenv("MARGINPLYR_REVIEW_REQUEST")), "setup-child-ownership") &&
    identical(Sys.getenv("MARGINPLYR_REVIEW_REQUEST", unset = NA_character_), original_request),
  "Binary-pipe setup restores supervisor environment")
options(previous_environment)

# At the exact expiry boundary an origin is unavailable even before collection.
value <- rr_with_lock(file.path(state, "evidence.lock"), rr_store_read(state))
origins <- Filter(function(entry) entry$payload$kind == "evidence" && entry$payload$request == record$id, value$records)
origin <- origins[[1L]]
expect(rr_origin_valid(origin, "build-full", "build-full", list(), origin$payload$finished + rr_week - 1), "Origin before seven-day boundary")
expect(!rr_origin_valid(origin, "build-full", "build-full", list(), origin$payload$finished + rr_week), "Origin expires at seven-day boundary")
renewal <- value
renewal$key_created <- rr_now() - rr_week
old_key <- renewal$key
renewal <- rr_store_retire(renewal, character())
expect(!identical(old_key, renewal$key) && rr_same(renewal$records, value$records), "Key renewal preserves evidence and original deadlines")
run_migration_contracts(repo, home, state)
cat("Repository runner authority, cross-clone, cancellation, concurrency and expiry contracts passed.\n")
cat("Repository runner process lifecycle and reuse contracts passed.\n")
}
# Capture the primary fault before on.exit cleanup can replace its diagnostic.
withCallingHandlers(run_process_contracts(), error = function(e) {
  rr_say("Runner process contract error: ", conditionMessage(e))
  traceback(0L, max.lines = 20L)
})
