# Separate-process acceptance worker. The supervisor sends SIGINT only after a
# flushed checkpoint with this process's identity; snapshots precede assertions.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1L]])
directory <- normalizePath(args[[2L]])
configuration <- jsonlite::read_json(file.path(directory, "case.json"),
                                      simplifyVector = TRUE)
setwd(root)
pkgload::load_all(quiet = TRUE)
library(testthat)
source("tests/testthat/helper-optional-backends.R")
source("tests/testthat/helper-sqlite-interruption.R")
write_json <- function(value, name) {
  pending <- file.path(directory, paste0(name, ".pending"))
  jsonlite::write_json(value, pending, auto_unbox = TRUE,
                       pretty = TRUE, null = "null", na = "null")
  if (!file.rename(pending, file.path(directory, name))) {
    stop("Cannot publish checkpoint evidence")
  }
}
condition_info <- function(condition) {
  list(class = class(condition), message = conditionMessage(condition),
       parent = if (!is.null(condition$parent)) condition_info(condition$parent),
       cleanup = if (!is.null(condition$cleanup)) condition_info(condition$cleanup))
}
write_json(list(
  pid = Sys.getpid(), snapshot = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  R = R.version.string, OS = as.list(Sys.info()),
  dependencies = stats::setNames(lapply(c("DBI", "RSQLite", "dbplyr", "dplyr",
                                         "rlang", "testthat", "pkgload"), function(pkg) {
    as.character(utils::packageVersion(pkg))
  }), c("DBI", "RSQLite", "dbplyr", "dplyr", "rlang", "testthat", "pkgload"))
), "environment.json")
deliver <- switch(configuration$mode,
  sigint = function() {
    write_json(list(pid = Sys.getpid(), checkpoint = configuration$checkpoint,
                    interrupts_suspended = .Internal(interruptsSuspended()),
                    reached_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
               "reached.json")
    # A pending interrupt must be able to wait out a protected handoff. The
    # supervisor's resume file also lets that handoff return and deliver it.
    # Sys.sleep() can deliver SIGINT even inside suspendInterrupts() on this R;
    # a reached-checkpoint barrier must not override the transition's protection.
    while (!file.exists(file.path(directory, "resume"))) {
      invisible(NULL)
    }
    # An eligible checkpoint must deliver the signal here, before fast result
    # preparation can reach release. Protected handoffs defer it instead.
    if (!.Internal(interruptsSuspended())) {
      Sys.sleep(0)
    }
  },
  controlled = rlang::interrupt,
  error = function() stop("ordinary checkpoint control"),
  healthy = function() invisible(NULL)
)
results <- test_that("supervised SQLite interruption acceptance", {
  sqlite_interrupt_fixture(function(fixture) {
    con <- fixture$con
    if (configuration$outer) {
      DBI::dbBegin(con)
      DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
    }
    write_json(list(state = fixture$before, input = fixture$input,
                    expected = fixture$expected), "before.json")
    state <- sqlite_interrupt_hook(configuration$checkpoint, deliver = deliver)
    on.exit(sqlite_interrupt_unhook(state), add = TRUE)
    out <- NULL
    outcome <- tryCatch({
      captured <- tryCatch({
        out <- dplyr::compute(
          fixture$query,
          name = if (configuration$schema == "main") "report" else fixture$destination,
          temporary = configuration$schema == "temp", overwrite = configuration$overwrite,
          indexes = if (configuration$checkpoint == "index") list("g") else list(),
          in_transaction = configuration$flag
        )
        list(kind = "success")
      }, interrupt = function(err) list(kind = "interrupt", condition = err),
         error = function(err) list(kind = "error", condition = err))
      # Flush a deferred signal before any observation. R need not deliver it at
      # the exact instruction restoring interrupt eligibility after a handoff.
      Sys.sleep(0)
      captured
    }, interrupt = function(err) list(kind = "interrupt", condition = err),
       error = function(err) list(kind = "error", condition = err))
    if (!is.null(outcome$condition)) {
      outcome$condition <- condition_info(outcome$condition)
    }
    # No probe, retry, caller decision, test rollback or disposal precedes this.
    write_json(list(
      outcome = outcome, hit = state$hit, savepoint = state$savepoint,
      transacting = RSQLite::sqliteIsTransacting(con), state = sqlite_interrupt_state(con),
      source = DBI::dbReadTable(con, "source"), sentinel = DBI::dbReadTable(con, "sentinel"),
      destination_exists = DBI::dbExistsTable(con, fixture$destination),
      destination = if (DBI::dbExistsTable(con, fixture$destination)) {
        DBI::dbReadTable(con, fixture$destination)
      },
      observer_sentinel = DBI::dbReadTable(fixture$observer, "sentinel"),
      observer_destination = if (configuration$schema != "temp" &&
                                  DBI::dbExistsTable(fixture$observer, fixture$destination)) {
        DBI::dbReadTable(fixture$observer, fixture$destination)
      }
    ), "immediate.json")
    completed <- configuration$checkpoint == "release" || configuration$mode == "healthy"
    if (completed) {
      expect_true(state$hit)
      expect_identical(outcome$kind,
                       if (configuration$mode == "healthy") "success" else "interrupt")
      expect_equal(DBI::dbReadTable(con, fixture$destination), fixture$expected)
      expect_identical(DBI::dbGetQuery(con, paste0(
        "SELECT name, type FROM pragma_table_info('report', '",
        configuration$schema, "')"
      )), data.frame(name = c("g", "sid", if (configuration$expansion) "v" else "z"),
                     type = c("TEXT", "INT", if (configuration$expansion) "REAL" else "")))
      expect_identical(DBI::dbReadTable(con, "source"), fixture$input)
      expect_identical(DBI::dbReadTable(con, "sentinel")$value,
                       if (configuration$outer) c("baseline", "caller") else "baseline")
      expect_identical(RSQLite::sqliteIsTransacting(con), configuration$outer)
      expect_error(DBI::dbExecute(con, paste("ROLLBACK TO", state$savepoint)),
                   "no such savepoint")
      if (!configuration$outer) {
        expect_equal(DBI::dbReadTable(fixture$observer, fixture$destination), fixture$expected)
      } else {
        expect_identical(DBI::dbReadTable(fixture$observer, fixture$destination),
                         data.frame(old = 42L))
      }
    } else {
      if (!configuration$checkpoint %in% c("rollback", "cleanup_release")) {
        expect_identical(outcome$kind,
                         if (configuration$mode == "error") "error" else "interrupt")
      }
      expect_sqlite_restored(fixture, state, outer = configuration$outer)
    }
    sqlite_interrupt_unhook(state)
    if (configuration$outer) {
      if (configuration$commit) DBI::dbCommit(con) else DBI::dbRollback(con)
      expect_false(RSQLite::sqliteIsTransacting(con))
      expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                       if (configuration$commit) c("baseline", "caller") else "baseline")
      reader <- if (configuration$schema == "temp") con else fixture$observer
      if (completed && configuration$commit) {
        expect_equal(DBI::dbReadTable(reader, fixture$destination), fixture$expected)
      } else if (configuration$overwrite) {
        expect_identical(DBI::dbReadTable(reader, fixture$destination),
                         data.frame(old = 42L))
      } else {
        expect_false(DBI::dbExistsTable(reader, fixture$destination))
      }
    } else {
      expect_true(DBI::dbBegin(con))
      expect_true(DBI::dbCommit(con))
      retry <- dplyr::compute(fixture$query, name = "recovered", temporary = FALSE,
                              in_transaction = configuration$flag)
      expect_equal(as.data.frame(dplyr::collect(retry)), fixture$expected)
      expect_equal(DBI::dbReadTable(fixture$observer, "recovered"), fixture$expected)
    }
    write_json(list(outcome = outcome, transacting = RSQLite::sqliteIsTransacting(con),
                    caller_commit = configuration$commit, completed = completed), "final.json")
  }, schema = configuration$schema, overwrite = configuration$overwrite,
     sorted = configuration$sorted, expansion = configuration$expansion)
})
write_json(list(passed = isTRUE(results)), "verdict.json")
if (!isTRUE(results)) stop("SQLite interruption acceptance failed")
