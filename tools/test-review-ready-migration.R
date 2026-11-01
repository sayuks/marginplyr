# Migration contracts use a private account and a stand-in legacy command.
# The real accepted-source comparisons, locks, process inspection and moves run.
run_migration_contracts <- function(repo, home, state) {
  migration <- new.env(parent = environment())
  for (value in parse(file.path(candidate, "tools/migrate-review-ready.R"))) {
    if (is.call(value) && identical(value[[1L]], as.name("<-")) &&
        identical(value[[2L]], as.name("rr_migrate"))) eval(value, migration)
  }
  migration$commandArgs <- function(...) character()
  migration$rr_identity <- function() list(root = repo, sha = rr_git(repo, "rev-parse", "HEAD"))
  migration$rr_account_home <- function() home
  migration$rr_state <- function() state
  command <- file.path(rr_mkdir(file.path(home, ".local/bin")), "marginplyr-review-ready")
  active <- file.path(home, "legacy-active")
  writeLines(c("#!/usr/bin/env python3", "import json, pathlib, sys",
    "def main(args):",
    paste0("    print(json.dumps({'requests': ['legacy-active'] if pathlib.Path(", rr_json(active), ").exists() else []}))"),
    "    return 0", "if __name__ == '__main__': sys.exit(main(sys.argv[1:]))"), command)
  Sys.chmod(command, "0755")
  delivery <- file.path(state, "delivery.json")
  unlink(delivery)
  file.create(active)
  original <- rr_read(command)
  rejects(migration$rr_migrate(), "Legacy requests remain")
  expect(!file.exists(delivery) && identical(rr_read(command), original),
    "Active legacy requests prevent retirement and publication of R delivery")
  unlink(active)
  id <- rr_id()
  directory <- rr_mkdir(file.path(state, "requests", id))
  record <- list(version = 2L, protocol = 3L, id = id, token = rr_id(), sha = rr_git(repo, "rev-parse", "HEAD"),
    state = "terminal", cleanup = "confirmed", owned_capacity = FALSE, finished = rr_now(),
    manager = list(pid = 99999999L, birth = "Thu Jan  1 00:00:00 1970"), children = list())
  rr_write(file.path(directory, "record.json"), modifyList(record, list(cleanup = "unconfirmed")))
  rejects(migration$rr_migrate(), "Unattributed/damaged legacy history")
  expect(!file.exists(delivery) && dir.exists(directory), "Unconfirmed legacy history remains protected during a partial cutover")
  rr_write(file.path(directory, "record.json"), record)
  legacy_evidence <- file.path(state, "evidence-index.json")
  writeLines("Legacy evidence is never an R execution", legacy_evidence)
  existing <- if (dir.exists(rr_store_root(state))) rr_with_lock(file.path(state, "evidence.lock"), rr_store_read(state)) else NULL
  invisible(capture.output(migration$rr_migrate()))
  retired <- file.path(state, "legacy-history", id)
  expect(file.exists(delivery) && rr_read_json(delivery)$protocol == rr_protocol &&
      !dir.exists(directory) && rr_read_json(file.path(retired, "history.json"))$request == id,
    "A drained cutover individually attributes terminal legacy history before enabling R delivery")
  expect(!file.exists(legacy_evidence) && file.exists(file.path(state, "legacy-history/evidence-index.json")),
    "Legacy authenticated inventory becomes attributed historical data")
  if (!is.null(existing)) {
    current <- rr_with_lock(file.path(state, "evidence.lock"), rr_store_read(state))
    expect(rr_same(existing$records, current$records), "Migration never imports Python results as R execution origins")
  }
  expect(rr_run(command, error = FALSE)$status == 2L && rr_run(command, "--status", error = FALSE)$status == 0L,
    "The retired installed command preserves control and refuses normal checks")
  expect(dir.exists(repo) && file.exists(file.path(repo, "DESCRIPTION")), "Migration leaves the originating Git checkout intact")
  cat("Drained migration, protected legacy state and retired-entry contracts passed.\n")
}
