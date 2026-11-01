#!/usr/bin/env Rscript
# Drained delivery cutover for ADR 0039. This command never cancels another
# owner's work. Run only after the replacement's acceptance evidence is ready.
for (name in c("core", "registry", "evidence", "manager")) source(file.path("tools", paste0("review-ready-", name, ".R")))

rr_migrate <- function() {
  if (length(commandArgs(TRUE))) rr_stop("Migration takes no arguments.")
  rr_setup()
  identity <- rr_identity()
  main <- rr_git(identity$root, "rev-parse", "--verify", "refs/heads/main^{commit}")
  rr_authority(identity$root, main)
  sources <- unique(c(rr_runner_sources, "tools/migrate-review-ready.R", "tools/test-review-ready-runner.R"))
  if (!identical(rr_capabilities(identity$root, main, sources), rr_capabilities(identity$root, identity$sha, sources)))
    rr_stop("Migration must use the accepted local main implementation.")
  state <- rr_mkdir(rr_state())
  lock <- rr_lock(file.path(state, "delivery.lock"))
  if (is.null(lock)) rr_stop("Another delivery migration is active; no state changed.")
  on.exit(filelock::unlock(lock), add = TRUE)
  command <- file.path(rr_account_home(), ".local/bin/marginplyr-review-ready")
  legacy <- rr_mkdir(file.path(state, "legacy-history"))
  control <- file.path(state, "legacy-control.py")
  retired <- "# retired by repository-runner protocol: 4"
  if (file.exists(command) && !grepl(retired, rr_text(command), fixed = TRUE)) {
    output <- rr_run(command, "--status")$stdout
    status <- jsonlite::fromJSON(output, simplifyVector = FALSE)
    if (length(status$requests)) rr_stop("Legacy requests remain. Their owners must finish/cancel and confirm cleanup before migration.")
    saved <- file.path(state, "legacy-launcher.disabled")
    rr_write(saved, rr_read(command), raw = TRUE)
    # The retained adapter supports control only, never a check invocation.
    writeLines(c("import importlib.machinery, sys",
      "if sys.argv[1:2] not in (['--status'], ['--recover']): sys.exit('Legacy check entry retired; use the repository R entry.')",
      paste0("module = importlib.machinery.SourceFileLoader('retired_review_ready', ",
        as.character(jsonlite::toJSON(saved, auto_unbox = TRUE)), ").load_module()"),
      "sys.exit(module.main(sys.argv[1:]))"), control)
    Sys.chmod(control, "0600")
    wrapper <- c("#!/bin/sh", retired,
      'case "$1" in',
      paste0('  --status|--recover) exec python3 ', shQuote(control), ' "$@" ;;'),
      '  *) echo "Installed check entry retired. From the target root use Rscript tools/review-ready.R." >&2; exit 2 ;;',
      "esac")
    replacement <- tempfile(".retired-", tmpdir = dirname(command))
    writeLines(wrapper, replacement)
    Sys.chmod(replacement, "0755")
    if (!file.rename(replacement, command)) rr_stop("Cannot retire the installed check entry.")
  }
  if (file.exists(control)) {
    status <- jsonlite::fromJSON(rr_run(command, "--status")$stdout, simplifyVector = FALSE)
    if (length(status$requests)) rr_stop("A legacy request entered during cutover. Its owner must finish/cancel and recover it; new R checks remain disabled.")
  }
  # A process that loaded the old launcher before replacement may not yet have
  # journaled a request. Inspect its argv as well as the durable legacy state.
  for (row in rr_processes()) {
    if (row$identity$pid == Sys.getpid()) next
    argv <- tryCatch(ps::ps_cmdline(ps::ps_handle(row$identity$pid)), no_such_process = function(e) character())
    if (command %in% argv) rr_stop("A legacy launcher is still alive (PID ", row$identity$pid, "); cutover remains pending.")
  }
  if (file.exists(file.path(state, "request.json"))) rr_stop("Legacy Full recovery state remains; no cutover marker was published.")
  table <- rr_processes()
  root <- file.path(state, "requests")
  if (dir.exists(root)) for (directory in list.dirs(root, recursive = FALSE, full.names = TRUE)) {
    rr_owned_path(directory, state)
    path <- file.path(directory, "record.json")
    old <- rr_read_json(path)
    if (!is.null(old$record)) {
      current <- rr_read_record(path)
      if (current$state != "terminal") rr_stop("Unfinished R request remains: ", current$id)
      next
    }
    if (!identical(old$version, 2L) || !identical(old$protocol, 3L) ||
        !identical(old$id, basename(directory)) || !rr_match("[0-9a-f]{32}", old$id) ||
        !identical(old$state, "terminal") || !identical(old$cleanup, "confirmed") ||
        !identical(old$owned_capacity, FALSE) || !rr_valid_time(old$finished))
      rr_stop("Unattributed/damaged legacy history requires inspection: ", directory)
    legacy_alive <- function(item) {
      row <- table[[as.character(item$pid)]]
      if (is.null(row)) return(FALSE)
      previous <- Sys.getlocale("LC_TIME")
      on.exit(Sys.setlocale("LC_TIME", previous), add = TRUE)
      Sys.setlocale("LC_TIME", "C")
      birth <- format(as.POSIXct(as.numeric(row$identity$birth), origin = "1970-01-01"), "%a %b %e %H:%M:%S %Y")
      identical(gsub(" +", " ", birth), item$birth)
    }
    if (legacy_alive(old$manager) || any(vapply(old$children, legacy_alive, logical(1))) ||
        any(vapply(table, function(row) identical(unname(row$env["MARGINPLYR_REVIEW_REQUEST"]), old$token), logical(1))))
      rr_stop("Legacy terminal handoff/owned processing remains: ", old$id)
    destination <- file.path(legacy, old$id)
    diagnostics <- character()
    output <- file.path(directory, "output.log")
    if (file.exists(output)) {
      lines <- readLines(output, warn = FALSE)
      candidates <- sub("^Review-ready failure diagnostics: ", "", lines[startsWith(lines, "Review-ready failure diagnostics: ")])
      roots <- file.path(rr_account_home(), c("Library/Caches/org.R-project.R/R/marginplyr/review-ready-failures",
        ".cache/R/marginplyr/review-ready-failures"))
      for (bundle in unique(candidates)) {
        boundary <- roots[vapply(roots, function(root) startsWith(bundle, paste0(root, "/")), logical(1))]
        if (length(boundary) != 1L || !dir.exists(bundle)) next
        rr_owned_path(bundle, boundary)
        metadata <- file.path(bundle, "metadata.txt")
        if (!file.exists(metadata) || !paste("Commit:", old$sha) %in% readLines(metadata, warn = FALSE)) {
          rr_say("Unattributed legacy diagnostics left for inspection: ", bundle)
          next
        }
        diagnostics <- union(diagnostics, bundle)
      }
    }
    rr_write(file.path(directory, "history.json"), list(kind = "legacy-review-ready", request = old$id,
      sha = old$sha, finished = old$finished, cleanup = "confirmed", diagnostics = as.list(diagnostics)))
    if (!file.rename(directory, destination)) rr_stop("Cannot attribute legacy history: ", old$id)
  }
  # Old authenticated results are historical data, never new R execution origins.
  for (name in c("evidence.key", "evidence-index.json", "evidence-index.new")) {
    path <- file.path(state, name)
    if (file.exists(path) && !file.rename(path, file.path(legacy, name))) rr_stop("Cannot retire legacy evidence state: ", name)
  }
  rr_write(file.path(legacy, "retirement.json"), list(kind = "legacy-protocol", finished = rr_now(), cleanup = "confirmed"))
  rr_write(file.path(state, "delivery.json"), list(protocol = rr_protocol, main = main, completed = rr_now(),
    legacy_checks = "retired", sessions = "refresh instructions before next invocation"))
  rr_say("Drained cutover established. Refresh every participating session; use Rscript tools/review-ready.R from each target root.")
}
status <- tryCatch({ rr_migrate(); 0L }, error = function(e) { rr_say("Migration stopped: ", conditionMessage(e)); 2L })
quit(status = status, save = "no")
