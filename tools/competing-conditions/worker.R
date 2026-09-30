# Record raw public outcomes before probes, rendering, or caller decisions.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1L]])
directory <- normalizePath(args[[2L]])
setwd(root)
configuration <- jsonlite::read_json(file.path(directory, "case.json"),
                                      simplifyVector = TRUE)
pkgload::load_all(quiet = TRUE)
library(testthat)
source("tests/testthat/helper-optional-backends.R")
source("tests/testthat/helper-sqlite-interruption.R")
write_json <- function(value, name) {
  pending <- file.path(directory, paste0(name, ".pending"))
  jsonlite::write_json(value, pending, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  if (!file.rename(pending, file.path(directory, name))) stop("Checkpoint write failed")
}
write_json(list(
  pid = Sys.getpid(), R = R.version.string,
  OS = as.list(Sys.info()[c("sysname", "release", "version", "machine")]),
  dependencies = stats::setNames(lapply(
    c("DBI", "RSQLite", "dbplyr", "dplyr", "rlang", "testthat", "pkgload"),
    function(pkg) as.character(utils::packageVersion(pkg))
  ), c("DBI", "RSQLite", "dbplyr", "dplyr", "rlang", "testthat", "pkgload"))
), "environment.json")
notification <- new.env(parent = emptyenv())
notification$effects <- numeric()
notification$replayed <- list()
notification$checkpoint <- NULL
deliver <- function() {
  write_json(c(list(pid = Sys.getpid(), operation = configuration$operation,
                    reached_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
               notification$checkpoint()), "reached.json")
  switch(configuration$mode,
    sigint = {
      while (!file.exists(file.path(directory, "resume"))) invisible(NULL)
      Sys.sleep(0)
    },
    controlled = rlang::interrupt(),
    healthy = NULL
  )
}
capture <- function(expr) {
  tryCatch(list(kind = "value", value = expr),
           interrupt = function(cnd) list(kind = "interrupt", condition = cnd),
           error = function(cnd) list(kind = "error", condition = cnd))
}
results <- test_that("reached competing-condition checkpoint acceptance", {
  if (configuration$operation == "summary") {
    old <- options(warn = configuration$warn)
    on.exit(options(old), add = TRUE)
    input <- data.frame(g = c("a", "b"), v = c(2, 5))
    original <- input
    notification$checkpoint <- function() list(effects = notification$effects)
    branch <- function(v) {
      if (length(v) == 2L) {
        deliver()
      } else {
        notification$effects <- c(notification$effects, sum(v))
        if (configuration$warnings) warning("earlier warning")
      }
      sum(v)
    }
    raw <- capture(withCallingHandlers(
      summarize_with_margins(input, z = branch(.data$v), .grouping = rollup("g")),
      warning = function(cnd) {
        notification$replayed[[length(notification$replayed) + 1L]] <- cnd
      }
    ))
    saveRDS(raw, file.path(directory, "raw.rds"))
    probe <- capture(Sys.sleep(0))
    saveRDS(list(raw = raw, probe = probe, effects = notification$effects,
                 replayed = notification$replayed,
                 input = input, warn = getOption("warn")),
            file.path(directory, "immediate.rds"))
    expect_identical(probe$kind, "value")
    expect_identical(input, original)
    expect_identical(notification$effects, c(2, 5))
    expect_identical(getOption("warn"), configuration$warn)
    if (configuration$mode == "healthy") {
      expect_identical(raw$kind, if (configuration$warnings &&
                                     configuration$warn == 2L) "error" else "value")
    } else {
      expect_identical(raw$kind, "interrupt")
      expect_false(inherits(raw$condition, "error"))
      if (configuration$warnings) {
        expect_length(notification$replayed, 1L)
        if (configuration$warn == 2L) {
          expect_s3_class(raw$condition$interrupt, "interrupt")
          expect_s3_class(raw$condition$replay_error, "error")
          expect_match(conditionMessage(raw$condition$replay_error),
                       "converted from warning")
        }
      }
    }
    options(old)
    expect_equal(summarize_with_margins(input, z = sum(.data$v),
                                       .grouping = rollup("g"))$z, c(2, 5, 7))
  } else {
    sqlite_interrupt_fixture(function(fixture) {
      con <- fixture$con
      if (configuration$outer) {
        DBI::dbBegin(con)
        DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
      }
      cause <- rlang::error_cnd(message = "execution checkpoint failure",
                                parent = simpleError("original execution cause"))
      state <- sqlite_interrupt_hook("before_cleanup_release", deliver = deliver,
                                     execution_error = cause)
      on.exit(sqlite_interrupt_unhook(state), add = TRUE)
      notification$checkpoint <- function() list(inserted = state$inserted,
                                    rolled_back = state$rolled_back,
                                    counts = as.list(state$counts))
      raw <- capture(dplyr::compute(fixture$query, name = "report",
                                    temporary = FALSE, overwrite = TRUE,
                                    in_transaction = configuration$flag))
      saveRDS(raw, file.path(directory, "raw.rds"))
      probe <- capture(Sys.sleep(0))
      saveRDS(list(raw = raw, probe = probe, before = fixture$before,
                   state = sqlite_interrupt_state(con),
                   report = DBI::dbReadTable(con, "report"),
                   sentinel = DBI::dbReadTable(con, "sentinel"),
                   observer_report = DBI::dbReadTable(fixture$observer, "report"),
                   observer_sentinel = DBI::dbReadTable(fixture$observer, "sentinel"),
                   transacting = RSQLite::sqliteIsTransacting(con), counts = state$counts),
              file.path(directory, "immediate.rds"))
      expect_identical(probe$kind, "value")
      expect_identical(state$counts, c(rollback = 1L, release = 1L))
      expect_sqlite_restored(fixture, state, configuration$outer)
      if (configuration$mode == "healthy") {
        expect_identical(raw$kind, "error")
        expect_identical(raw$condition, cause)
      } else {
        expect_identical(raw$kind, "interrupt")
        expect_identical(raw$condition$parent, cause)
        expect_false(inherits(raw$condition, "error"))
      }
      sqlite_interrupt_unhook(state)
      if (configuration$outer) {
        if (configuration$commit) DBI::dbCommit(con) else DBI::dbRollback(con)
        expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                         if (configuration$commit) c("baseline", "caller") else "baseline")
      }
      expect_true(DBI::dbBegin(con))
      expect_true(DBI::dbCommit(con))
    })
  }
})
write_json(list(passed = isTRUE(results)), "verdict.json")
if (!isTRUE(results)) stop("Competing-condition acceptance failed")
