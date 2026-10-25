# The option transitions are narrower than a backend callback can target.
# Shadow only the diagnostic renderer's options binding to inject R interrupts
# at its real stores; preflight's separate lifecycle setting is untouched.
# The public SQLite Margin call and subsequent warnings are the test seam.
with_render_option_interrupt <- function(transition, settings, code) {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(c(list(marginplyr.audit_sql = TRUE, warn = 0), settings))
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "render_option_source"
  )
  state <- new.env(parent = emptyenv())
  state$stores <- 0L
  hit <- FALSE
  change_options <- function(...) {
    state$stores <- state$stores + 1L
    if (!hit && transition == "restore" && state$stores == 2L) {
      hit <<- TRUE
      rlang::interrupt()
    }
    previous <- base::options(...)
    if (!hit && transition == "quiet" && state$stores == 1L) {
      hit <<- TRUE
      rlang::interrupt()
    }
    previous
  }
  renderer <- render_sent_query_sql
  environment(renderer) <- list2env(
    list(options = change_options), parent = environment(renderer)
  )
  testthat::local_mocked_bindings(render_sent_query_sql = renderer)
  check <- function() {
    summarize_with_margins(
      remote, total = sum(.data$v),
      .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
    )
  }
  code(check)
  expect_true(hit)
}

test_that("cancelled option transitions restore subsequent warnings", {
  settings <- list(
    rlib_warning_verbosity = "default", lifecycle_verbosity = "default"
  )
  for (transition in c("quiet", "restore")) {
    with_render_option_interrupt(transition, settings, function(check) {
      delivered <- list()
      caught <- tryCatch(withCallingHandlers({
        check()
        Sys.sleep(0)
      }, interrupt = function(cnd) {
        delivered[[length(delivered) + 1L]] <<- cnd
      }), interrupt = identity)
      expect_s3_class(caught, "interrupt")
      expect_identical(delivered, list(caught))
      expect_identical(lapply(names(settings), getOption), unname(settings))
      warnings <- character()
      withCallingHandlers({
        rlang::warn("rlang after cancellation", .frequency = "once",
                    .frequency_id = tempfile())
        lifecycle::deprecate_warn(
          "0.0.1", "render_option_marker()", id = tempfile(), always = TRUE
        )
      }, warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      })
      expect_length(warnings, 2L)
      expect_true(any(grepl("rlang after cancellation", warnings,
                            fixed = TRUE)))
      expect_true(any(grepl("render_option_marker", warnings, fixed = TRUE)))
    })
  }
})

test_that("cancellation during option restoration finishes cleanup", {
  cases <- list(
    list(rlib_warning_verbosity = NULL, lifecycle_verbosity = NULL),
    list(rlib_warning_verbosity = "verbose", lifecycle_verbosity = "warning"),
    list(rlib_warning_verbosity = "quiet", lifecycle_verbosity = "error")
  )
  for (settings in cases) {
    for (transition in c("quiet", "restore")) {
      with_render_option_interrupt(transition, settings, function(check) {
        delivered <- list()
        caught <- tryCatch(withCallingHandlers({
          check()
          # R may defer delivery until the next interrupt check.
          Sys.sleep(0)
        }, interrupt = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
        }), interrupt = identity)
        expect_s3_class(caught, "interrupt")
        expect_identical(delivered, list(caught))
        expect_identical(lapply(names(settings), getOption), unname(settings))
        saved <- last_sent_queries(details = TRUE)
        expect_identical(nrow(saved), 1L)
        expect_identical(saved$status, "prepared")
        expect_identical(last_sent_queries(), saved[c("purpose", "sql")])
        expect_identical(last_sent_queries(FALSE, TRUE), saved)
        expect_s3_class(check(), "tbl_sql")
        expect_identical(lapply(names(settings), getOption), unname(settings))
        expect_identical(saved$status, "prepared")
        expect_identical(nrow(last_sent_queries()), 2L)
      })
    }
  }
})
