# Run from the checkout root. This script does not replace package functions.
# Rscript <script> [fresh-output.rds]
# Expected on the investigated snapshot: 1 condition off, 5 on, exit status 1.
main <- function(output = NULL) {
  if (!is.null(output) && file.exists(output)) stop("Output already exists")
  pkgload::load_all(".", quiet = TRUE)
  old <- options()
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  methods::setClass(
    "AuditGenericSQLiteConnection", contains = "SQLiteConnection"
  )
  on.exit(methods::removeClass("AuditGenericSQLiteConnection"), add = TRUE)
  dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "generic_source"
  )
  subclass_con <- methods::new("AuditGenericSQLiteConnection", con)
  dialect <- dbplyr::sql_dialect(con)
  variant <- dbplyr::sql_translation(con)
  original_sum <- variant$aggregate$sum
  leaf <- structure(list(message = 'SQLite translated SUM', call = NULL),
                    class = c('AuditFormatterCondition', 'warning', 'condition'))
  registerS3method('conditionMessage', 'AuditFormatterCondition', function(cnd) {
    in_summary <- any(vapply(sys.calls(), function(call) {
      is.call(call) && identical(call[[1L]], as.name('sent_query_condition_message'))
    }, logical(1)))
    if (in_summary) signalCondition(simpleCondition('secondary formatter condition'))
    cnd$message
  }, envir = asNamespace('base'))
  on.exit(rm('conditionMessage.AuditFormatterCondition',
             envir = asNamespace('base')$.__S3MethodsTable__.), add = TRUE)

  # Use dbplyr's normal connection/dialect extension surface. SUM's SQL and
  # the actual DBI operations remain the original SQLite implementations.
  registerS3method(
    "sql_dialect", "AuditGenericSQLiteConnection",
    function(con) {
      structure(dialect, class = c("audit_generic_sqlite", class(dialect)))
    }, envir = asNamespace("dbplyr")
  )
  registerS3method(
    "sql_translation", "audit_generic_sqlite",
    function(con) {
      dbplyr::sql_variant(
        variant$scalar,
        dbplyr::sql_translator(
          .parent = variant$aggregate,
          sum = function(...) {
            warning(leaf)
            original_sum(...)
          }
        ),
        variant$window
      )
    }, envir = asNamespace("dbplyr")
  )
  on.exit(rm(
    list = c("sql_dialect.AuditGenericSQLiteConnection",
             "sql_translation.audit_generic_sqlite"),
    envir = asNamespace("dbplyr")$.__S3MethodsTable__.
  ), add = TRUE)
  source <- dplyr::tbl(subclass_con, "generic_source")

  run <- function(audited) {
    options(marginplyr.audit_sql = audited, warn = 0)
    delivered <- list()
    result <- withCallingHandlers(
      summarize_with_margins(
        source, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
        .check_margin_label = TRUE
      ),
      condition = function(cnd) {
        delivered[[length(delivered) + 1L]] <<- cnd
      }
    )
    rows <- if (audited) last_sent_queries(details = TRUE) else NULL
    # Later collection is outside the recorded construction lifetime.
    values <- suppressWarnings(dplyr::collect(result))
    if (audited) stopifnot(identical(last_sent_queries(details = TRUE), rows))
    list(
      classes = class(result),
      conditions = vapply(delivered, conditionMessage, character(1)),
      condition_classes = lapply(delivered, class),
      primary_unchanged = identical(
        delivered[vapply(delivered, inherits, logical(1), 'warning')], list(leaf)
      ),
      values = values, rows = rows
    )
  }
  off <- run(FALSE)
  on <- run(TRUE)
  stopifnot(
    identical(off$classes, on$classes), identical(off$values, on$values),
    off$primary_unchanged, on$primary_unchanged,
    identical(on$rows$status, c("returned", "prepared")),
    all(vapply(on$rows$conditions, function(summaries) {
      all(summaries$message == leaf$message)
    }, logical(1)))
  )
  observation <- list(
    R = R.version.string,
    versions = vapply(c("dbplyr", "dplyr", "DBI", "RSQLite", "pkgload"),
                      function(pkg) as.character(packageVersion(pkg)),
                      character(1)),
    off = off, on = on
  )
  if (!is.null(output)) saveRDS(observation, output)
  cat("Delivered conditions with auditing off:", length(off$conditions), "\n")
  cat("Delivered conditions with auditing on:", length(on$conditions), "\n")
  cat("Result values, classes, and original condition identity agree.\n")
  # This desired transparency assertion fails on the investigated snapshot.
  stopifnot(identical(off$conditions, on$conditions))
  invisible(observation)
}
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
main(if (length(args)) args[[1L]] else NULL)
