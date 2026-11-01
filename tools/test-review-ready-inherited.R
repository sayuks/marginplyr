# Inherited ADR 0038 decisions exercised against the R implementation loaded by
# the accepted runner fixture. This file is a fixture dependency, not an entry.
run_inherited_value_contracts <- function() {
  if (Sys.info()[["sysname"]] == "Linux") {
    expect(nzchar(trimws(rr_text("/proc/sys/kernel/random/boot_id"))),
      "Zero-size procfs metadata does not truncate the available boot identity")
  }
  clock <- list(r = "4.6.1", checker = "2efcaa5190c038e60834bd5c4e502c82",
    timeout = "34ee7dea5eac7ba7138bd3c60651faee", leeway = 300)
  timestamps <- list(observed = 1000, paths = list(DESCRIPTION = 1300))
  expect(rr_clock_valid(clock, timestamps, 1300, 1000), "Clock leeway includes its exact boundary")
  expect(!rr_clock_valid(clock, timestamps, 1300.001, 1000) &&
    !rr_clock_valid(clock, timestamps, 1000, 999), "Future timestamps and clock rollback refuse reuse")
  unknown <- clock; unknown$r <- "unknown"
  expect(!rr_clock_valid(unknown, timestamps, 1000, 1000), "Unknown R checker refuses timestamp reuse")
  unknown <- timestamps; unknown$paths <- list()
  expect(!rr_clock_valid(clock, unknown, 1000, 1000), "An empty timestamp inventory is unavailable")
  clock$leeway <- NaN
  expect(!rr_clock_valid(clock, timestamps, 1000, 1000), "Nonfinite clock leeway is unavailable")

  attempt <- list(status = 0L, timeout = FALSE, errors = 0L, warnings = 0L, notes = list())
  check <- c(attempt, list(args = list("--as-cran"), attempts = list(attempt)))
  expect(rr_valid_check(check, "build-full"), "Exact clean Full check is reusable")
  expect(!rr_valid_check(check, "build-documentation"), "Documentation requires the exact no-tests boundary")
  documentation <- check; documentation$args <- list("--as-cran", "--no-tests")
  expect(rr_valid_check(documentation, "build-documentation") && !rr_valid_check(documentation, "build-full"),
    "Documentation never supplies a Full execution")
  unknown <- check; unknown$notes <- list("unknown-note")
  unknown$attempts[[1L]]$notes <- unknown$notes
  expect(!rr_valid_check(unknown, "build-full"), "Unknown NOTE identifiers are not reusable")
  retry <- check
  retry$attempts <- list(modifyList(attempt, list(status = 1L, errors = 1L)), attempt)
  expect(!rr_valid_check(retry, "build-full"), "A second attempt alone does not authorize retry evidence")
  retry$retry_verified <- TRUE; retry$first_failure <- "/retained/diagnostics"
  retry$retry_identity <- list(hash = "same-tarball")
  expect(rr_valid_check(retry, "build-full"), "Authorized retry preserves its original failure and identity")
  retry$attempts[[1L]]$warnings <- 1L
  expect(!rr_valid_check(retry, "build-full"), "Retry does not admit a first-attempt warning")

  known <- c(CODEX_TOOL_CALL_ID = "exec-12345678-abcd-abcd-abcd-123456789abc",
    CODEX_NETWORK_PROXY_ATTRIBUTION = "12345678-abcd-abcd-abcd-123456789abc",
    CLAUDE_CODE_SESSION_ID = "12345678-abcd-abcd-abcd-123456789abc", CLAUDE_PID = "1234")
  expect(!length(rr_child_env(known)), "Only known-format execution metadata is removed")
  unknown <- known; unknown[] <- "unknown-configuration"
  expect(identical(rr_child_env(unknown), unknown), "Unknown values of execution IDs remain inputs")
  configuration <- c(CODEX_REVIEW_BEHAVIOR = "original", CLAUDE_ENV_FILE = "/settings",
    CLAUDE_CODE_SHELL_PREFIX = "wrapper", HTTPS_PROXY = "proxy", MARGINPLYR_REVIEW_REQUEST = "transport")
  expect(identical(rr_child_env(configuration), configuration), "Configuration and supervisor transport remain in children")
  for (name in c("QUARTO_DART_SASS", "QUARTO_CACHE_DIR", "R_PROFILE_USER")) {
    rejects(rr_render_env(tempfile("unregistered-renderer-"), stats::setNames("/external", name)), name)
  }
  state <- rr_state()
  modified <- Sys.getenv(); modified[["HOME"]] <- "/private/tmp/unrelated-account-home"
  modified[["XDG_STATE_HOME"]] <- "/private/tmp/unrelated-state"
  expect(identical(rr_in_environment(modified, rr_state()), state), "HOME and cache profiles cannot partition OS-account admission")

  observer <- rr_environment
  environment(observer) <- new.env(parent = environment(rr_environment))
  environment(observer)$rr_git_environment <- function(...) list()
  environment(observer)$rr_renderer <- function(...) list()
  for (version in c(1L, 2L, 3L)) {
    probe <- list(unregistered = list(), loader_defaults = as.list(Sys.getenv(c("LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH"))),
      metadata_audit = list(version = version, names = as.list(names(rr_metadata)), present = list()))
    environment(observer)$rr_run <- function(...) list(stdout = rr_json(probe))
    rejects(observer(candidate), "execution metadata audit")
  }

  current <- rr_read(file.path(candidate, rr_expectation))
  broken <- charToRaw(gsub('"devtools", ', "", rawToChar(current), fixed = TRUE))
  expect(!identical(current, broken) && identical(rr_protected_expectation(current), rr_protected_expectation(broken)),
    "The 1107 dependency correction preserves protected assertion bytes")
  weakened <- charToRaw(sub("if (!identical(actual, expected))", "if (FALSE)", rawToChar(current), fixed = TRUE))
  expect(!identical(rr_protected_expectation(current), rr_protected_expectation(weakened)),
    "Weakening the strict dependency assertion changes the contract")
  expect(!identical(rr_protected_expectation(current), rr_protected_expectation(c(current, charToRaw("\nstop('extra')\n")))),
    "Unrelated verifier code cannot use expectation normalization")
  formatted <- charToRaw(sub('"altdoc", "askpass",', '"altdoc",\n    "askpass",', rawToChar(current), fixed = TRUE))
  expect(identical(rr_protected_expectation(current), rr_protected_expectation(formatted)),
    "Literal dependency-vector whitespace preserves its protected contract")

  identity <- list(pid = Sys.getpid(), birth = "1.000000")
  table <- list(list(identity = identity, parent = 1L, group = 321L, env = c(MARGINPLYR_REVIEW_REQUEST = "other")))
  names(table) <- as.character(Sys.getpid())
  record <- list(token = "owned", children = list(modifyList(identity, list(birth = "2.000000"))), group = NULL)
  expect(!length(rr_owned(table, record)), "A reused PID is not an owned process")
  record$children <- list(identity)
  expect(identical(rr_owned(table, record), as.character(Sys.getpid())),
    "A recorded detached identity stays owned without its environment marker")
  record$children <- list()
  record$group <- 321L
  rejects(rr_owned(table, record), "no confirmed owned member")
  rejects(rr_alive(list(pid = -1L, birth = "invalid")), "Invalid process identity")
  inspect <- rr_processes
  environment(inspect) <- new.env(parent = environment(rr_processes))
  environment(inspect)$rr_uid <- function() -1L
  rejects(inspect(), "Incomplete process inspection")
  cat("Inherited clock, NOTE/retry, metadata, expectation and PID contracts passed.\n")
}
run_inherited_value_contracts()

run_selection_contracts <- function() {
  root <- tempfile("runner-selection-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  write <- function(path, text) {
    rr_mkdir(dirname(file.path(root, path)))
    writeLines(text, file.path(root, path))
  }
  git <- function(...) rr_git(root, ...)
  commit <- function() { git("add", "."); git("commit", "-qm", "Selection fixture"); git("rev-parse", "HEAD") }
  rr_run("git", c("init", "-q", "-b", "main"), root)
  git("config", "user.name", "Selection fixture"); git("config", "user.email", "fixture@example.invalid")
  git("config", "core.filemode", "true")
  write("DESCRIPTION", "Package: marginplyr")
  write("R/behavior.R", "behavior <- function() 1")
  write("design/note.md", "Base prose")
  write("NEWS.md", "Base news")
  write("tools/review-ready-progress-reporter.R", "# Base reporter")
  write(rr_expectation, readLines(file.path(candidate, rr_expectation)))
  base <- commit()
  select <- function(changes, expected, routes = character()) {
    git("checkout", "--quiet", "--detach", "-f", base)
    for (path in names(changes)) write(path, changes[[path]])
    sha <- commit()
    result <- rr_select(root, sha, base)
    expect(identical(result$path, expected) && identical(result$base, base) && all(routes %in% unlist(result$verifiers)),
      paste("Cumulative selection and routes:", paste(names(changes), collapse = ", ")))
    result
  }
  for (path in c("AGENTS.md", "CLAUDE.md", "CONTEXT.md", "CODE_OF_CONDUCT.md", "design/nested/note.md",
      "investigation/note.md", "tools/operations.md")) select(stats::setNames(list("Changed prose"), path), "Repository")
  select(list("design/agents/local-checks.md" = "Changed policy"), "Repository",
    c("tools/test-review-ready-documentation.R", "tools/test-review-ready-roxygen.R"))
  for (path in c("README.md", "README.qmd", "man/topic.Rd", "vignettes/guide.qmd", "vignettes/refs.bib",
      "vignettes/style.csl", "man/figures/icon.svg")) select(stats::setNames(list("Documentation"), path),
    "Documentation", "tools/review-ready-generated.R")
  select(list("NEWS.md" = "Changed news"), "Documentation")
  for (path in c("vignettes/unknown.Rmd", "vignettes/nested/guide.qmd", "vignettes/unknown.txt", "LICENSE",
      "investigation/executable.R", ".github/unsupported.yml")) select(stats::setNames(list("Unknown input"), path), "Full")
  select(list("codecov.yml" = "# configuration", ".github/workflows/lint.yaml" = "# workflow",
    "R/behavior.R" = "behavior <- function() 2", "UNKNOWN" = "Package input"), "Full",
    c(".github/scripts/verify-coverage-policy.R", "tools/verify-ci-artifacts.py", "tools/review-ready-generated.R"))
  for (path in c(".github/workflows/lint.yaml", ".codex/config.toml", ".claude/settings.json"))
    select(stats::setNames(list("# configuration"), path), "Repository", "tools/verify-ci-artifacts.py")
  reporter <- select(list("tools/review-ready-progress-reporter.R" = "# Changed reporter"), "Full",
    c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R", "tools/test-review-ready-progress.R"))
  expect(rr_source_inputs(root, reporter$submitted) != rr_source_inputs(root, base), "The progress reporter is an executing stage input")
  roxygen <- select(list("R/behavior.R" = c("#' A comment", "behavior <- function() 1")), "Documentation",
    "tools/review-ready-generated.R")
  expect(isTRUE(roxygen$roxygen_pending), "R edits await supervised proof")
  select(list("R/new.R" = "new <- function() 1"), "Full", "tools/review-ready-generated.R")
  git("checkout", "--quiet", "--detach", "-f", base)
  git("rm", "--quiet", "NEWS.md")
  removed <- commit()
  expect(rr_select(root, removed, base)$path == "Documentation", "Documentation deletions retain their obligation")
  git("checkout", "--quiet", "--detach", "-f", base)
  git("mv", "design/note.md", "design/name\nwith tab\t.md")
  renamed <- commit()
  expect(rr_select(root, renamed, base)$path == "Repository", "NUL records preserve unusual names and both rename sides")
  git("checkout", "--quiet", "--detach", "-f", base)
  Sys.chmod(file.path(root, "design/note.md"), "0755")
  mode <- commit()
  expect(rr_select(root, mode, base)$path == "Full", "Executable prose is Full")
  expect(rr_source_inputs(root, mode) != rr_source_inputs(root, base), "Executable prose is not an excluded source input")
  git("checkout", "--quiet", "--detach", "-f", base)
  writeBin(as.raw(c(255, 0, 1)), file.path(root, "design/note.md"))
  binary <- commit()
  expect(rr_select(root, binary, base)$path == "Full", "Binary and invalid UTF-8 prose remain Full")
  expect(rr_source_inputs(root, binary) != rr_source_inputs(root, base), "Binary prose is not an excluded source input")
  git("checkout", "--quiet", "--detach", "-f", base)
  write("R/behavior.R", "behavior <- function() 2")
  package <- commit()
  write("design/note.md", "Follow-up prose")
  mixed <- commit()
  expect(rr_select(root, mixed, base)$path == "Documentation", "Earlier R changes remain pending cumulative proof")
  write("R/behavior.R", "behavior <- function() 1")
  reverted <- commit()
  expect(rr_select(root, reverted, base)$path == "Repository", "Reverted code does not remain in the final-tree difference")
  expect(rr_source_inputs(root, reverted) == rr_source_inputs(root, base), "Audited prose is a stage non-input")
  expect(rr_source_inputs(root, package) != rr_source_inputs(root, base), "Package implementation bytes are stage inputs")
  write("DESCRIPTION", c("Package: marginplyr", "Version: 1.0"))
  metadata <- commit()
  expect(rr_source_inputs(root, metadata) != rr_source_inputs(root, base), "All DESCRIPTION fields are source inputs")
  expect(file.symlink("../unregistered-external", file.path(root, "linked")), "Create a committed source link")
  linked <- commit()
  rejects(rr_source_inputs(root, linked), "source type")
  expect(rr_select(root, linked, base)$path == "Full", "Committed links cannot receive limited selection")
  git("checkout", "--quiet", "--detach", "-f", metadata)
  git("update-index", "--add", "--cacheinfo", paste0("160000,", base, ",submodule"))
  submodule <- git("commit", "-qm", "Submodule fixture")
  rejects(rr_source_inputs(root, git("rev-parse", "HEAD")), "source type")
  git("checkout", "--quiet", "--detach", "-f", metadata)
  write(".gitattributes", "R/behavior.R export-ignore")
  attributes <- commit()
  rejects(rr_source_inputs(root, attributes), "Git attributes")
  git("replace", base, package)
  overlay <- rr_select(root, reverted, base)
  expect(overlay$path == "Full" && is.null(overlay$base), "Replacement refs cannot hide a cumulative change")
  git("replace", "-d", base)
  unknown <- rr_select(root, reverted, paste(rep("f", 40), collapse = ""))
  expect(unknown$path == "Full" && is.null(unknown$base) && "tools/verify-ci-artifacts.py" %in% unlist(unknown$verifiers),
    "Unavailable comparisons retain unresolved artifact verification")
  empty <- rr_select(root, base, base)
  expect(empty$path == "Full" && identical(empty$base, base), "Empty known differences retain Full and their real base")
  unavailable <- rr_select
  environment(unavailable) <- new.env(parent = environment(rr_select))
  for (bytes in list(charToRaw("unterminated"), c(charToRaw("unclassifiable"), as.raw(0)))) {
    environment(unavailable)$rr_git_raw <- function(...) bytes
    result <- unavailable(root, reverted, base)
    expect(result$path == "Full" && is.null(result$base) && "tools/verify-ci-artifacts.py" %in% unlist(result$verifiers),
      "Malformed raw comparisons retain unresolved verification without inventing a base")
  }
  environment(unavailable)$rr_git <- function(...) paste(base, base, sep = "\n")
  result <- unavailable(root, reverted, base)
  expect(result$path == "Full" && is.null(result$base), "Ambiguous merge-bases cannot provide a verification base")

  external <- file.path(root, "installed-library")
  rr_mkdir(external); writeLines("Version: 1.0", file.path(external, "DESCRIPTION"))
  code <- file.path(external, "implementation")
  writeLines("original", code)
  before <- rr_directory_identity(external)
  writeLines("changed", code)
  expect(!rr_same(before, rr_directory_identity(external)), "Same-version installed bytes invalidate identity")
  writeLines("original", code)
  Sys.setFileTime(code, Sys.time() + 2)
  expect(!rr_same(before, rr_directory_identity(external)), "Restoring bytes does not erase change metadata")
  cat("Inherited cumulative selection, route, source and external-input contracts passed.\n")
}
run_selection_contracts()

run_control_contracts <- function() {
  directory <- tempfile("runner-control-")
  rr_mkdir(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  control <- rr_mkdir(file.path(directory, "work/control"))
  path <- file.path(directory, "record.json")
  initial <- list(version = 3L, id = rr_id(), token = rr_id(), sha = paste(rep("a", 40), collapse = ""), epoch = 1L,
    workspace = file.path(directory, "work"), state = "running", capacity = "Full", owned_capacity = TRUE,
    gate_started = FALSE, authorized = FALSE,
    selection = list(path = "Full", submitted = paste(rep("a", 40), collapse = ""), verifiers = list(), base = NULL,
      plan = list(units = list(`build-full` = list(action = "execute")))))
  send <- function(record = initial, operation = "authorize", mutate = identity, key = charToRaw(record$token), cancelled = FALSE) {
    unlink(list.files(control, full.names = TRUE))
    id <- rr_id()
    message <- list(protocol = rr_protocol, request = record$id, sha = record$sha, epoch = record$epoch,
      expected = "running", id = id, operation = operation, reason = "Fixture promotion",
      transport = rr_hash(charToRaw(rr_selection_transport(record))))
    rr_write(file.path(control, paste0(id, ".request.json")), rr_seal(mutate(message), key))
    result <- rr_control(path, record, cancelled)
    reply <- rr_unseal(rr_read_json(file.path(control, paste0(id, ".reply.json"))), charToRaw(record$token))
    list(record = result$record, reply = reply, promote = result$promote)
  }
  expect(!send()$reply$ok, "Live supervision cannot authorize before gate start")
  begun <- send(operation = "begin-gate")
  expect(begun$reply$ok && begun$record$gate_started, "Pre-gate state authorizes one gate start")
  authorized <- send(begun$record)
  expect(authorized$reply$ok && authorized$record$authorized, "Exact authenticated transport authorizes the gate")
  expect(!send(authorized$record)$reply$ok, "Duplicate gate authorization is refused")
  for (field in c("sha", "request", "epoch", "expected", "protocol", "id", "transport")) {
    result <- send(begun$record, mutate = function(message) { message[[field]] <- "mismatch"; message })
    expect(!result$reply$ok, paste("Mismatched control field is refused:", field))
  }
  expect(!send(begun$record, key = openssl::rand_bytes(32))$reply$ok, "Wrong-key control messages cannot authorize Full")
  expect(!send(begun$record, cancelled = TRUE)$reply$ok, "Cancellation fences authorization")
  limited <- initial; limited$capacity <- "exempt"; limited$owned_capacity <- FALSE
  promoted <- send(limited, "promote")
  expect(promoted$reply$ok && promoted$promote && promoted$record$state == "promoting", "Pre-gate promotion requests Full ownership")
  limited$gate_started <- TRUE
  expect(!send(limited, "promote")$reply$ok, "Started validation cannot be replaced by Full promotion")
  expect(!send(begun$record, "discard-reuse")$reply$ok, "Started validation cannot discard reused obligations")
  expect(!send(initial, "resolve-documentation")$reply$ok, "Absent documentation proof cannot resolve its selection")
  pending <- initial
  pending$selection$path <- "Documentation"; pending$selection$roxygen_pending <- TRUE
  pending$selection$base <- paste(rep("b", 40), collapse = "")
  pending$selection$verifiers <- list("tools/verify-ci-artifacts.py")
  writeLines("Documentation", file.path(initial$workspace, "roxygen-path"))
  resolved <- send(pending, "resolve-documentation")
  expected <- pending; expected$selection$roxygen_pending <- FALSE
  expect(resolved$reply$ok && rr_same(resolved$record, expected),
    "Resolving documentation preserves request, token, epoch, pinned base, plan and CI routes")
  cat("Inherited authenticated control, transport and promotion boundaries passed.\n")
}
run_control_contracts()

run_registered_contracts <- function() {
  code <- paste("import importlib.machinery,json,sys;",
    "m=importlib.machinery.SourceFileLoader('baseline',sys.argv[1]).load_module();",
    "print(json.dumps(dict(core=m.CORE_SOURCES,documentation=m.DOCUMENTATION_SOURCES,stages=m.STAGE_CONTRACTS)))")
  baseline <- jsonlite::fromJSON(rr_run("python3", c("-c", code, file.path(candidate, "tools/review-ready-admission.py")))$stdout,
    simplifyVector = FALSE)
  expect(identical(baseline$core, rr_core_sources) && identical(baseline$documentation, rr_documentation_sources),
    "Inherited core and Documentation helper registration is complete")
  for (stage in setdiff(names(baseline$stages), c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R")))
    expect(identical(baseline$stages[[stage]], rr_stage_contracts[[stage]]), paste("Preserved workload contract:", stage))
  expect(all(c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R") %in% names(rr_stage_contracts)),
    "Main authority materializes both replacement fixture entry points")
  cat("Every inherited workload and helper-source contract matches the R registry.\n")
}
run_registered_contracts()

run_site_authority_contract <- function() {
  fixture <- tempfile("runner-site-authority-")
  accepted <- rr_mkdir(file.path(fixture, "accepted/.github/scripts"))
  archived <- rr_mkdir(file.path(fixture, "archive/.github/scripts"))
  previous <- getwd()
  on.exit({ setwd(previous); unlink(fixture, recursive = TRUE) }, add = TRUE)
  writeLines('cat("ACCEPTED HELPER\\n")', file.path(accepted, "ci-helpers.R"))
  writeLines(c('source(".github/scripts/ci-helpers.R")', 'cat("ACCEPTED VERIFIER\\n")'), file.path(accepted, "verify-site.R"))
  writeLines('stop("Candidate helper selected")', file.path(archived, "ci-helpers.R"))
  writeLines('stop("Candidate verifier selected")', file.path(archived, "verify-site.R"))
  definitions <- parse(file = file.path(candidate, "tools/verify-site-artifacts.R"))
  environment <- new.env(parent = globalenv())
  for (value in definitions) if (is.call(value) && identical(value[[1L]], as.name("<-")) &&
      identical(value[[2L]], as.name("verify_rendered_site"))) eval(value, environment)
  setwd(dirname(dirname(archived)))
  output <- capture.output(environment$verify_rendered_site(dirname(dirname(accepted))))
  expect(all(c("ACCEPTED HELPER", "ACCEPTED VERIFIER") %in% output),
    "Internal site archives cannot substitute candidate verification sources")
  protected <- file.path(fixture, "protected")
  writeLines("Keep unrelated bytes", protected)
  unlink(file.path(archived, "ci-helpers.R"))
  expect(file.symlink(protected, file.path(archived, "ci-helpers.R")), "Create a helper destination link")
  rejects(environment$verify_rendered_site(dirname(dirname(accepted))), "symlink")
  expect(identical(readLines(protected), "Keep unrelated bytes"), "A helper destination link cannot overwrite an outside file")
  cat("Accepted site verifier and helper survive inner archiving; linked destinations are refused.\n")
}
run_site_authority_contract()

run_deletion_contracts <- function() {
  fixture <- rr_mkdir(tempfile("runner-literal-deletion-"))
  on.exit(unlink(fixture, recursive = TRUE, expand = FALSE), add = TRUE)
  for (name in c("owned[1]", "owned?", "owned*")) {
    directory <- rr_mkdir(file.path(fixture, rr_id()))
    owned <- rr_mkdir(file.path(directory, name))
    unrelated <- rr_mkdir(file.path(directory, "owned1/work"))
    rr_mkdir(file.path(owned, "work"))
    writeLines("Unrelated bytes", file.path(unrelated, "sentinel"))
    rr_remove(file.path(owned, "work"), owned)
    expect(!dir.exists(file.path(owned, "work")) && file.exists(file.path(unrelated, "sentinel")),
      paste("Validated deletion paths are literal, never glob patterns:", name))
  }
  cat("Owned deletion treats wildcard characters literally and preserves unrelated paths.\n")
}
run_deletion_contracts()
