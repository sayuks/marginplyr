# Process contracts for the repository-owned runner. All state is disposable;
# the account-home adapter is replaced in this harness, never by a CLI option.
args <- commandArgs(trailingOnly = TRUE)
candidate <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else getwd()
source(file.path(candidate, "tools", "review-ready-core.R"))
source(file.path(candidate, "tools", "review-ready-registry.R"))
source(file.path(candidate, "tools", "review-ready-evidence.R"))
source(file.path(candidate, "tools", "review-ready-manager.R"))

fail <- function(text) stop(text, call. = FALSE)
expect <- function(value, text) if (!isTRUE(value)) { cat(text, "\n"); fail("Runner contract failed.") }
rejects <- function(code, pattern) {
  result <- tryCatch({ force(code); NULL }, error = identity)
  expect(inherits(result, "error") && grepl(pattern, conditionMessage(result)), pattern)
}

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
key <- openssl::rand_bytes(32)
envelope <- rr_seal(list(schema = 2L, value = "kept"), key)
expect(identical(rr_unseal(envelope, key)$value, "kept"), "authentication roundtrip")
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
  cat("Repository runner observation, retirement, mixed-origin and key-store contracts passed.\n")
}
run_store_contracts()

run_process_contracts <- function() {
fixture <- tempfile("repository-runner-contract-", tmpdir = Sys.getenv("TMPDIR", tempdir()))
dir.create(fixture)
fixture <- normalizePath(fixture, mustWork = TRUE)
jobs <- list()
on.exit({
  for (job in jobs) if (job$is_alive()) { job$signal(2L); job$wait(15000); if (job$is_alive()) job$kill() }
  if (exists("state") && dir.exists(file.path(state, "requests"))) {
    for (item in rr_requests(state, active = FALSE)) if (!is.null(item$record)) {
      for (attempt in 1:100) {
        table <- rr_processes()
        owned <- rr_owned(table, item$record)
        if (!length(owned)) break
        for (row in table[owned]) if (rr_alive(row$identity)) ps::ps_send_signal(ps::ps_handle(row$identity$pid), 9L)
        Sys.sleep(0.05)
      }
      if (length(owned)) rr_stop("Fixture processing remains; workspace retained: ", fixture)
    }
  }
  unlink(fixture, recursive = TRUE)
}, add = TRUE)
repo <- file.path(fixture, "repo")
home <- file.path(fixture, "account")
dir.create(repo); dir.create(home)
archive <- file.path(fixture, "source.tar")
rr_git(candidate, "archive", "--format=tar", paste0("--output=", archive), "HEAD")
utils::untar(archive, exdir = repo)
for (path in c(rr_runner_sources, "tools/test-review-ready-runner.R", "tools/review-ready-environment.R", "tools/review-ready-check-lib.R")) {
  file.copy(file.path(candidate, path), file.path(repo, path), overwrite = TRUE)
}
cat("\nrr_account_home <- function() ", deparse(home), "\n", file = file.path(repo, "tools/review-ready-core.R"), append = TRUE)
cat('\nrr_environment <- function(root) list(digest = rr_digest(Sys.getenv("RR_FIXTURE_INPUT", "same")), clock = list(r = "4.6.1", checker = "2efcaa5190c038e60834bd5c4e502c82", timeout = "34ee7dea5eac7ba7138bd3c60651faee", leeway = 300), r = "4.6.1", libraries = list())\n',
  file = file.path(repo, "tools/review-ready-evidence.R"), append = TRUE)
cat('\nrr_renderer <- function() list(fixture = TRUE)\n', file = file.path(repo, "tools/review-ready-evidence.R"), append = TRUE)
# This is an external package-check stand-in, never a real Full result. The
# actual R entry/manager, Git, OS locks, signals, records and authentication run.
writeLines(c(
  'read_review_ready_selection <- function() {',
  '  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(Sys.getenv("MARGINPLYR_REVIEW_SUPERVISOR")), "--gate-proof"))',
  '  if (status) stop("Proof refused")',
  '  jsonlite::fromJSON(Sys.getenv("MARGINPLYR_REVIEW_RECORD"), simplifyVector = FALSE)$record$selection',
  '}',
  'review_ready_check_cli <- function(args, expected_root, selection, diagnostic_root, accepted_root) {',
  '  record <- jsonlite::fromJSON(Sys.getenv("MARGINPLYR_REVIEW_RECORD"), simplifyVector = FALSE)$record',
  '  survivor <- Sys.getenv("RR_FIXTURE_SURVIVOR")',
  '  if (nzchar(survivor)) { child <- processx::process$new("/bin/sleep", "60", cleanup = FALSE, supervise = FALSE); writeLines(as.character(child$get_pid()), survivor) }',
  '  cat("FIXTURE GATE STARTED\\n"); flush.console()',
  '  release <- Sys.getenv("RR_FIXTURE_RELEASE")',
  '  if (nzchar(release)) { while (!file.exists(release)) Sys.sleep(0.05) } else Sys.sleep(as.numeric(Sys.getenv("RR_FIXTURE_SECONDS", "0.1")))',
  '  units <- names(Filter(function(unit) unit$action == "execute", selection$plan$units))',
  '  attempt <- list(status=0L, timeout=FALSE, errors=0L, warnings=0L, notes=list())',
  '  report <- list(request = record$token, units = stats::setNames(as.list(rep("passed",length(units))), units),',
  '    completed=as.list(c("Package spelling", "jarl", "package-aware lintr", "Test dependency syntax", "Required test Suggests", paste("Artifact verification:", unlist(selection$verifiers)))), omitted=list(),',
  '    timestamps=list(observed=as.numeric(Sys.time()), paths=list(DESCRIPTION=as.numeric(Sys.time()))),',
  '    check=c(attempt,list(args=as.list(c("--as-cran",if(selection$path=="Documentation") "--no-tests")),attempts=list(attempt))))',
  '  writeLines(jsonlite::toJSON(report, auto_unbox=TRUE, null="null", digits=NA), Sys.getenv("MARGINPLYR_REVIEW_STAGE_REPORT"))',
  '  cat("FIXTURE GATE FINISHED\\n")',
  '  0L',
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
requests <- rr_requests(state, active = FALSE)
expect(length(requests) == 1L && requests[[1L]]$record$cleanup == "confirmed", "Confirmed request cleanup")
record <- requests[[1L]]$record
expect(!dir.exists(record$workspace) && !dir.exists(file.path(dirname(requests[[1L]]$path), "cache")), "Temporary/cache cleanup")
store <- rr_with_lock(file.path(state,"evidence.lock"), rr_store_read(state))
expect(length(store$records) == 2L, "Authenticated observation and terminal origin")
writeLines("# fixture prose", file.path(repo, "design", "fixture-followup.md"))
rr_git(repo, "add", ".")
rr_git(repo, "commit", "-qm", "Fixture prose follow-up")
second <- invoke()
expect(second$status == 0L && grepl('"action":"reuse"', second$stdout, fixed=TRUE), paste("Actual origin reuse fixture:",second$stdout,second$stderr))
status <- invoke("--status")
expect(status$status == 0L && grepl("No check was run", status$stdout), "Status is not check evidence")
unknown <- invoke("--skip")
expect(unknown$status != 0L && grepl("Use no arguments", unknown$stdout), "No caller-selected skip")

accepted <- rr_git(repo, "rev-parse", "main")
rr_git(repo, "update-ref", "-d", "refs/heads/main")
missing <- invoke()
expect(missing$status == 2L && grepl("Missing local main authority", missing$stdout), "Missing authority never self-approves")
rr_git(repo, "update-ref", "refs/heads/main", accepted)
writeLines("dirty", file.path(repo, "UNTRACKED"))
expect(invoke()$status == 2L && invoke("--status")$status == 0L, "Dirty HEAD refuses checks but permits status")
unlink(file.path(repo, "UNTRACKED"))
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

start <- function(root = repo, seconds = "20", input = "cancel", release = "", survivor = "") {
  environment <- c(Sys.getenv(), RR_FIXTURE_SECONDS = seconds, RR_FIXTURE_INPUT = input, RR_FIXTURE_RELEASE = release,
    RR_FIXTURE_SURVIVOR = survivor)
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
cancel <- start()
until(cancel, "FIXTURE GATE STARTED")
cancel$job$signal(15L)
finish(cancel, 130L)
expect(grepl("cleanup: confirmed", rr_text(cancel$log), fixed=TRUE), "SIGTERM cancellation confirms cleanup")

# Concurrent real managers use one Full lock; supported prose is independent.
rr_git(clone, "checkout", "--quiet", "-b", "prose", accepted)
writeLines("# Repository fixture", file.path(clone, "design", "fixture-only.md"))
rr_git(clone, "add", "."); rr_git(clone, "commit", "-qm", "Repository fixture")
release <- file.path(fixture, "release")
active <- start(input = "concurrency", release = release)
until(active, "FIXTURE GATE STARTED")
waiting <- start(seconds = "0.1", input = "concurrency")
until(waiting, "Waiting for Review-ready capacity")
limited <- invoke(root = clone)
expect(limited$status == 0L && grepl('"path":"Repository"', limited$stdout, fixed=TRUE), "Repository overlaps Full without a second Full slot")
waiting$job$signal(1L)
finish(waiting, 130L)
file.create(release)
finish(active, 0L)

death <- start(input = "manager-death", release = file.path(fixture, "never-release"), survivor = file.path(fixture, "survivor.pid"))
until(death, "FIXTURE GATE STARTED")
death$job$kill()
death$job$wait(5000)
unfinished <- Filter(function(item) !is.null(item$record) && item$record$state != "terminal", rr_requests(state))
expect(length(unfinished) == 1L, "Manager death retains one recoverable request")
interrupted <- unfinished[[1L]]$record
expect(invoke(c("--recover", interrupted$id), root = clone)$status != 0L, "Surviving owned child prevents recovery")
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
cat("Repository runner authority, cross-clone, cancellation, concurrency and expiry contracts passed.\n")
cat("Repository runner process lifecycle and reuse contracts passed.\n")
}
run_process_contracts()
