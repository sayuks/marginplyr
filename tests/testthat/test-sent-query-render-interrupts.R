# A controlled renderer supplies legal overlapping condition classes; ordinary
# SQLite drivers need not emit them. The public verb and accessor are the seam.
test_that("diagnostic rendering preserves compound cancellation once", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(
    marginplyr.audit_sql = FALSE, warn = 0,
    rlib_warning_verbosity = "verbose", lifecycle_verbosity = "warning"
  )
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "render_interrupt_source"
  )
  render <- dbplyr::sql_render
  cases <- list(
    c("warning", "interrupt", "condition"),
    c("interrupt", "warning", "condition"),
    c("interrupt", "error", "condition"),
    c("error", "interrupt", "condition"),
    c("warning", "error", "interrupt", "condition"),
    c("interrupt", "error", "warning", "condition")
  )
  for (classes in cases) {
    leaf <- structure(
      list(message = "compound renderer cancellation", call = NULL),
      class = classes
    )
    for (warn in c(0, 2)) {
      run <- function(audited) {
        options(marginplyr.audit_sql = audited, warn = warn)
        settings <- options(c(
          "warn", "rlib_warning_verbosity", "lifecycle_verbosity"
        ))
        renders <- 0L
        testthat::local_mocked_bindings(sql_render = function(...) {
          renders <<- renders + 1L
          if (renders == 1L) stop(leaf)
          render(...)
        }, .package = "dbplyr")
        delivered <- list()
        completed <- FALSE
        outcome <- tryCatch(withCallingHandlers({
          summarize_with_margins(
            remote, total = sum(.data$v),
            .grouping = rollup(dplyr::all_of("g")),
            .check_margin_label = TRUE
          )
          completed <- TRUE
        }, condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
        }), interrupt = identity)
        expect_identical(options(names(settings)), settings)
        list(outcome = outcome, delivered = delivered,
             completed = completed, renders = renders)
      }
      off <- run(FALSE)
      on <- run(TRUE)
      expect_identical(on, off)
      expect_identical(on, list(
        outcome = leaf, delivered = list(leaf), completed = FALSE, renders = 1L
      ))
      rows <- last_sent_queries(details = TRUE)
      expect_identical(nrow(rows), 1L)
      expect_false(any(rows$purpose == "result"))
      expect_identical(rows$sql, NA_character_)
      expect_identical(rows$source, "render")
      expect_identical(rows$status, "prepared")
      expect_identical(rows$conditions, list(dplyr::tibble(
        classes = list(classes), message = "compound renderer cancellation",
        phase = "render", handling = "propagated"
      )))
      expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
      expect_identical(last_sent_queries(FALSE, TRUE), rows)
    }
  }
})
