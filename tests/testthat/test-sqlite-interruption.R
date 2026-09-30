# The selected contract explicitly requires immediate DBI observations at the
# public compute boundary; retry alone cannot prove savepoint release (#755).
test_that("SQLite interrupted mutations restore destinations before retry", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    for (schema in c("main", "temp", "other")) {
      for (overwrite in c(FALSE, TRUE)) {
        checkpoints <- c("create", "insert", "analyze", "result")
        if (overwrite) {
          checkpoints <- c("drop", checkpoints)
        }
        # Ordinary dbplyr does not support indexes on qualified destinations.
        if (schema == "main") {
          checkpoints <- c(checkpoints, "index")
        }
        for (checkpoint in checkpoints) {
          sqlite_interrupt_fixture(function(fixture) {
            state <- sqlite_interrupt_hook(checkpoint)
            on.exit(sqlite_interrupt_unhook(state), add = TRUE)
            condition <- tryCatch(
              dplyr::compute(
                fixture$query,
                name = if (schema == "main") "report" else fixture$destination,
                temporary = schema == "temp", overwrite = overwrite,
                indexes = if (checkpoint == "index") list("g") else list(),
                in_transaction = flag
              ),
              interrupt = identity, error = identity
            )
            expect_s3_class(condition, "interrupt")
            expect_false(inherits(condition, "error"))
            expect_sqlite_restored(fixture, state)
            sqlite_interrupt_unhook(state)
            expect_true(DBI::dbBegin(fixture$con))
            expect_true(DBI::dbCommit(fixture$con))
            out <- dplyr::compute(fixture$query, name = "recovered",
                                  temporary = FALSE, in_transaction = flag)
            expect_equal(as.data.frame(dplyr::collect(out)), fixture$expected)
            expect_equal(DBI::dbReadTable(fixture$observer, "recovered"),
                         fixture$expected)
          }, schema = schema, overwrite = overwrite)
        }
      }
    }
  }
})

test_that("SQLite interruption preserves caller work and decisions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    for (commit in c(FALSE, TRUE)) {
      for (checkpoint in c("drop", "create", "insert", "analyze", "result")) {
        sqlite_interrupt_fixture(function(fixture) {
          con <- fixture$con
          DBI::dbBegin(con)
          DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
          state <- sqlite_interrupt_hook(checkpoint)
          on.exit(sqlite_interrupt_unhook(state), add = TRUE)
          condition <- tryCatch(
            dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                           overwrite = TRUE, in_transaction = flag),
            interrupt = identity, error = identity
          )
          expect_s3_class(condition, "interrupt")
          expect_sqlite_restored(fixture, state, outer = TRUE)
          sqlite_interrupt_unhook(state)
          if (commit) {
            expect_true(DBI::dbCommit(con))
          } else {
            expect_true(DBI::dbRollback(con))
          }
          expect_false(RSQLite::sqliteIsTransacting(con))
          expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                           if (commit) c("baseline", "caller") else "baseline")
          expect_identical(DBI::dbReadTable(fixture$observer, "report"),
                           data.frame(old = 42L))
          expect_identical(sqlite_interrupt_state(con), fixture$before)
        })
      }
    }
  }
})

test_that("unsorted summaries and expansions recover on interruption", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    for (sorted in c(FALSE, TRUE)) {
      for (expansion in c(FALSE, TRUE)) {
        sqlite_interrupt_fixture(function(fixture) {
          state <- sqlite_interrupt_hook("result")
          on.exit(sqlite_interrupt_unhook(state), add = TRUE)
          condition <- tryCatch(
            dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                           overwrite = TRUE, in_transaction = flag),
            interrupt = identity, error = identity
          )
          expect_s3_class(condition, "interrupt")
          expect_sqlite_restored(fixture, state)
          sqlite_interrupt_unhook(state)
          out <- dplyr::compute(fixture$query, name = "recovered",
                                temporary = FALSE, in_transaction = flag)
          rows <- as.data.frame(dplyr::collect(out))
          expect_equal(rows[order(rows$sid, rows$g), ], fixture$expected)
          expect_equal(DBI::dbReadTable(fixture$observer, "recovered"),
                       fixture$expected)
        }, sorted = sorted, expansion = expansion)
      }
    }
  }
})

test_that("cleanup failures identify their step and triggering interrupt", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (step in c("rollback", "release")) {
    sqlite_interrupt_fixture(function(fixture) {
      state <- sqlite_interrupt_hook("insert", cleanup_failure = step)
      on.exit(sqlite_interrupt_unhook(state), add = TRUE)
      condition <- tryCatch(
        dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                       overwrite = TRUE),
        interrupt = identity, error = identity
      )
      expect_true(state$hit)
      expect_s3_class(condition, "error")
      expect_match(conditionMessage(condition),
                   paste("savepoint", step, "failed"))
      expect_match(conditionMessage(condition), paste("injected", step))
      expect_s3_class(condition$parent, "interrupt")
      expect_identical(condition$cleanup, state$cleanup_failure)
      expect_true(RSQLite::sqliteIsTransacting(fixture$con))
      if (step == "release") {
        expect_identical(DBI::dbReadTable(fixture$con, "report"),
                         data.frame(old = 42L))
      } else {
        expect_equal(DBI::dbReadTable(fixture$con, "report"), fixture$expected)
      }
      # After observing incomplete cleanup, the test repairs its database.
      sqlite_interrupt_unhook(state)
      DBI::dbRollback(fixture$con)
    })
  }
})

test_that("interrupt before SQLite acquisition changes no database state", {
  skip_if_suggest_absent("RSQLite", "DBI")
  sqlite_interrupt_fixture(function(fixture) {
    state <- sqlite_interrupt_hook("before")
    on.exit(sqlite_interrupt_unhook(state), add = TRUE)
    condition <- tryCatch(
      dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                     overwrite = TRUE),
      interrupt = identity, error = identity
    )
    expect_s3_class(condition, "interrupt")
    expect_null(state$savepoint)
    expect_sqlite_restored(fixture, state)
  })
})

test_that("unusable SQLite state reports incomplete interrupt recovery", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (invalid in c(FALSE, TRUE)) {
    sqlite_interrupt_fixture(function(fixture) {
      con <- fixture$con
      DBI::dbBegin(con)
      DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
      state <- sqlite_interrupt_hook("insert", deliver = function() {
        if (invalid) {
          DBI::dbDisconnect(con)
        } else {
          # Simulate backend whole-transaction abort, not native cancellation.
          DBI::dbExecute(con, "ROLLBACK")
        }
        rlang::interrupt()
      })
      on.exit(sqlite_interrupt_unhook(state), add = TRUE)
      condition <- tryCatch(
        dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                       overwrite = TRUE),
        interrupt = identity, error = identity
      )
      expect_true(state$hit)
      expect_match(conditionMessage(condition), "savepoint rollback failed")
      expect_s3_class(condition$parent, "interrupt")
      expect_s3_class(condition$cleanup, "error")
      expect_match(conditionMessage(condition),
                   conditionMessage(condition$cleanup),
                   fixed = TRUE)
      expect_identical(DBI::dbReadTable(fixture$observer, "report"),
                       data.frame(old = 42L))
      expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                       "baseline")
      if (invalid) {
        expect_false(DBI::dbIsValid(con))
      } else {
        expect_false(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbReadTable(con, "sentinel")$value, "baseline")
      }
    })
  }
})

test_that("caller ownership survives each destination kind", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    for (commit in c(FALSE, TRUE)) {
      for (schema in c("main", "temp", "other")) {
        for (overwrite in c(FALSE, TRUE)) {
          sqlite_interrupt_fixture(function(fixture) {
            con <- fixture$con
            DBI::dbBegin(con)
            DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
            state <- sqlite_interrupt_hook("insert")
            on.exit(sqlite_interrupt_unhook(state), add = TRUE)
            condition <- tryCatch(
              dplyr::compute(
                fixture$query, name = fixture$destination,
                temporary = schema == "temp", overwrite = overwrite,
                in_transaction = flag
              ),
              interrupt = identity, error = identity
            )
            expect_s3_class(condition, "interrupt")
            expect_sqlite_restored(fixture, state, outer = TRUE)
            sqlite_interrupt_unhook(state)
            if (commit) {
              expect_true(DBI::dbCommit(con))
            } else {
              expect_true(DBI::dbRollback(con))
            }
            expect_false(RSQLite::sqliteIsTransacting(con))
            expect_identical(sqlite_interrupt_state(con), fixture$before)
            expect_identical(
              DBI::dbReadTable(fixture$observer, "sentinel")$value,
              if (commit) c("baseline", "caller") else "baseline"
            )
            if (overwrite) {
              expect_identical(DBI::dbReadTable(con, fixture$destination),
                               data.frame(old = 42L))
            } else {
              expect_false(DBI::dbExistsTable(con, fixture$destination))
            }
          }, schema = schema, overwrite = overwrite, sorted = FALSE)
        }
      }
    }
  }
})
