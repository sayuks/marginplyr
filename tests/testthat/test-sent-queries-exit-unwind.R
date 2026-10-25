# A write checkpoint reaches the real restoration body without replacing it.
# Private state is used only to place SIGINT inside cleanup and to isolate a
# failed restoration; observations use the public verbs and accessor (#810).
with_cleanup_signal <- function(binding, matches, code) {
  saved <- as.list(sent_queries, all.names = TRUE)
  value <- sent_queries[[binding]]
  fired <- FALSE
  rm(list = binding, envir = sent_queries)
  makeActiveBinding(binding, function(replacement) {
    if (missing(replacement)) {
      return(value)
    }
    if (!fired && matches(replacement)) {
      fired <<- TRUE
      tools::pskill(Sys.getpid(), signal = 2L)
      # A loop reaches R's interrupt checks without a system event loop that
      # can deliver SIGINT even inside suspendInterrupts() on macOS.
      for (i in seq_len(10000L)) invisible(i)
    }
    value <<- replacement
    invisible(NULL)
  }, sent_queries)
  on.exit(suspendInterrupts({
    rm(list = ls(sent_queries, all.names = TRUE), envir = sent_queries)
    list2env(saved, envir = sent_queries)
  }), add = TRUE)
  suspendInterrupts(code(function() fired))
}

# Self-directed SIGINT is POSIX-only. The condition/ownership tests below run
# on every platform; tools/check-sent-query-interrupt.R adds bounded native
# stress without a checkpoint or a replacement package function.
if (identical(.Platform$OS.type, "unix")) {
  test_that("an interrupt during outer cleanup releases each entry point", {
    old <- options(marginplyr.audit_sql = TRUE)
    on.exit(options(old), add = TRUE)
    input <- data.frame(g = "a", v = 1)
    remote <- dbplyr::tbl_lazy(input)
    verbs <- list(summarize_with_margins, summarise_with_margins,
                  expand_with_margins, nest_with_margins,
                  nest_by_with_margins, inspect_grouping)
    inspect_grouping(input)
    for (verb in verbs) {
      with_cleanup_signal(
        "active",
        function(x) identical(x, FALSE),
        function(fired) {
          caught <- tryCatch(
            allowInterrupts({
              verb(input, .grouping = rollup(g))
              for (i in seq_len(10000L)) invisible(i)
            }),
            interrupt = identity
          )
          expect_true(fired())
          expect_s3_class(caught, "interrupt")
          expect_identical(nrow(last_sent_queries()), 0L)
          # Repeated replacements also check that the saved audit flag belongs
          # to the independent call, rather than to the interrupted owner.
          for (audited in c(FALSE, TRUE, TRUE)) {
            options(marginplyr.audit_sql = audited)
            fresh <- summarize_with_margins(
              remote, fresh = sum(v, na.rm = TRUE)
            )
            options(marginplyr.audit_sql = !audited)
            if (audited) {
              rows <- last_sent_queries(details = TRUE)
              expect_identical(rows$purpose, "result")
              expect_identical(rows$status, "prepared")
              expect_identical(
                rows$sql, as.character(dbplyr::sql_render(fresh))
              )
            } else {
              expect_error(last_sent_queries(), "marginplyr.audit_sql",
                           class = "marginplyr_error")
            }
          }
          options(marginplyr.audit_sql = TRUE)
        }
      )
    }
  })

  test_that("an interrupt during child cleanup restores its actual parent", {
    old <- options(marginplyr.audit_sql = TRUE)
    on.exit(options(old), add = TRUE)
    remote <- dbplyr::tbl_lazy(data.frame(g = "a", v = 1))
    factory <- function() {
      before <- last_sent_queries(details = TRUE)
      parent_record <- sent_queries$record
      with_cleanup_signal(
        "record",
        function(x) identical(x, parent_record),
        function(fired) {
          caught <- tryCatch(
            allowInterrupts({
              summarize_with_margins(remote, child = sum(v, na.rm = TRUE))
              for (i in seq_len(10000L)) invisible(i)
            }),
            interrupt = identity
          )
          expect_true(fired())
          expect_s3_class(caught, "interrupt")
          expect_identical(last_sent_queries(details = TRUE), before)
        }
      )
      rollup(g)
    }
    result <- summarize_with_margins(
      remote, parent = sum(v, na.rm = TRUE), .grouping = factory()
    )
    rows <- last_sent_queries(details = TRUE)
    expect_identical(rows$purpose, "result")
    expect_identical(rows$sql, as.character(dbplyr::sql_render(result)))
    expect_identical(rows$status, "prepared")
  })
}

test_that("interrupted SQL reentry preserves conditions and audit settings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  outer <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "interrupt_outer")
  inner <- dplyr::copy_to(con, data.frame(g = "b", v = 2), "interrupt_inner")
  collect <- dplyr::collect
  leaf <- structure(list(message = "owned operation interrupted", call = NULL),
                    class = c("owned_interrupt", "interrupt", "condition"))
  check <- function(source) {
    summarize_with_margins(
      source, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .check_margin_label = TRUE
    )
  }
  # The backend boundary can raise the same condition on every platform.
  # SIGINT during the ownership write is exercised separately above.
  run <- function(parent_audited, child_audited, catch_child) {
    options(marginplyr.audit_sql = parent_audited)
    in_child <- FALSE
    before <- NULL
    child_seen <- NULL
    resumed <- FALSE
    testthat::local_mocked_bindings(collect = function(x, ...) {
      if (in_child) {
        stop(leaf)
      }
      if (parent_audited) {
        before <<- last_sent_queries(details = TRUE)
      }
      options(marginplyr.audit_sql = child_audited)
      in_child <<- TRUE
      call_child <- function() {
        withCallingHandlers(check(inner), interrupt = function(cnd) {
          expect_identical(cnd, leaf)
          if (child_audited) {
            child_seen <<- last_sent_queries(details = TRUE)
          } else {
            expect_error(last_sent_queries(), "marginplyr.audit_sql",
                         class = "marginplyr_error")
          }
        })
      }
      if (catch_child) {
        expect_identical(tryCatch(call_child(), interrupt = identity), leaf)
      } else {
        call_child()
      }
      in_child <<- FALSE
      resumed <<- TRUE
      if (parent_audited) {
        expect_identical(last_sent_queries(details = TRUE), before)
      } else {
        expect_error(last_sent_queries(), "marginplyr.audit_sql",
                     class = "marginplyr_error")
      }
      collect(x, ...)
    }, .package = "dplyr")
    result <- tryCatch(check(outer), interrupt = identity)
    expect_identical(resumed, catch_child)
    expect_identical(getOption("marginplyr.audit_sql"), child_audited)
    if (child_audited) {
      expect_identical(child_seen$status, "invoking")
      expect_match(child_seen$sql, "interrupt_inner", fixed = TRUE)
      expect_identical(child_seen$conditions[[1L]], dplyr::tibble(
        classes = list(class(leaf)), message = leaf$message,
        phase = "invocation", handling = "propagated"
      ))
    }
    if (catch_child) {
      expect_s3_class(result, "tbl_sql")
    } else {
      expect_identical(result, leaf)
    }
    if (parent_audited) {
      rows <- last_sent_queries(details = TRUE)
      expect_identical(rows$purpose, if (catch_child) {
        c("observed_label_collision", "result")
      } else {
        "observed_label_collision"
      })
      expect_identical(rows$status, if (catch_child) {
        c("returned", "prepared")
      } else {
        "interrupted"
      })
      expect_identical(rows$sql[[1L]], before$sql[[1L]])
      expect_no_match(paste(rows$sql, collapse = "\n"), "interrupt_inner",
                      fixed = TRUE)
      expect_identical(rows$conditions[[1L]], if (catch_child) {
        before$conditions[[1L]]
      } else {
        dplyr::tibble(classes = list(class(leaf)), message = leaf$message,
                      phase = "invocation", handling = "propagated")
      })
    } else {
      expect_error(last_sent_queries(), "marginplyr.audit_sql",
                   class = "marginplyr_error")
    }
    options(marginplyr.audit_sql = TRUE)
    for (name in c("independent_one", "independent_two")) {
      fresh <- summarize_with_margins(
        dbplyr::lazy_frame(v = 1, .name = name), fresh = sum(v, na.rm = TRUE)
      )
      expect_identical(last_sent_queries()$sql,
                       as.character(dbplyr::sql_render(fresh)))
    }
  }
  for (parent_audited in c(FALSE, TRUE)) {
    for (child_audited in c(FALSE, TRUE)) {
      for (catch_child in c(FALSE, TRUE)) {
        run(parent_audited, child_audited, catch_child)
      }
    }
  }
})
