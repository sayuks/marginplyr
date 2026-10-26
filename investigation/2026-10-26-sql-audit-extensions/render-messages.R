# Run from the checkout root. This script does not replace package functions.
# Rscript <script> [ordinary|bare] [fresh-output.rds]
# Expected on the investigated snapshot: 1 message off, 4 on, exit status 1.
main <- function(mode = "ordinary", output = NULL) {
  stopifnot(mode %in% c("ordinary", "bare"))
  if (!is.null(output) && file.exists(output)) stop("Output already exists")
  pkgload::load_all(".", quiet = TRUE)
  old <- options()
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  methods::setClass(
    "AuditMessageSQLiteConnection", contains = "SQLiteConnection"
  )
  on.exit(methods::removeClass("AuditMessageSQLiteConnection"), add = TRUE)
  dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "message_source"
  )
  subclass_con <- methods::new("AuditMessageSQLiteConnection", con)
  dialect <- dbplyr::sql_dialect(con)
  variant <- dbplyr::sql_translation(con)
  original_sum <- variant$aggregate$sum
  leaf <- simpleMessage("SQLite translated SUM")

  # Use dbplyr's normal connection/dialect extension surface. SUM's SQL and
  # the actual DBI operations remain the original SQLite implementations.
  registerS3method(
    "sql_dialect", "AuditMessageSQLiteConnection",
    function(con) {
      structure(dialect, class = c("audit_message_sqlite", class(dialect)))
    }, envir = asNamespace("dbplyr")
  )
  registerS3method(
    "sql_translation", "audit_message_sqlite",
    function(con) {
      dbplyr::sql_variant(
        variant$scalar,
        dbplyr::sql_translator(
          .parent = variant$aggregate,
          sum = function(...) {
            if (mode == "ordinary") message(leaf) else signalCondition(leaf)
            original_sum(...)
          }
        ),
        variant$window
      )
    }, envir = asNamespace("dbplyr")
  )
  on.exit(rm(
    list = c("sql_dialect.AuditMessageSQLiteConnection",
             "sql_translation.audit_message_sqlite"),
    envir = asNamespace("dbplyr")$.__S3MethodsTable__.
  ), add = TRUE)
  source <- dplyr::tbl(subclass_con, "message_source")

  run <- function(audited) {
    options(marginplyr.audit_sql = audited)
    delivered <- list()
    result <- withCallingHandlers(
      summarize_with_margins(
        source, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
        .check_margin_label = TRUE
      ),
      message = function(cnd) {
        delivered[[length(delivered) + 1L]] <<- cnd
        # A bare signal need not supply muffleMessage.
        if (!is.null(findRestart("muffleMessage"))) {
          invokeRestart("muffleMessage")
        }
      }
    )
    rows <- if (audited) last_sent_queries(details = TRUE) else NULL
    # Later collection is outside the recorded construction lifetime.
    values <- suppressMessages(dplyr::collect(result))
    list(
      classes = class(result),
      messages = vapply(delivered, conditionMessage, character(1)),
      message_classes = lapply(delivered, class),
      delivered_unchanged = all(vapply(delivered, identical, logical(1), leaf)),
      values = values, rows = rows
    )
  }
  off <- run(FALSE)
  on <- run(TRUE)
  stopifnot(
    identical(off$classes, on$classes), identical(off$values, on$values),
    off$delivered_unchanged, on$delivered_unchanged,
    identical(on$rows$status, c("returned", "prepared")),
    all(vapply(on$rows$conditions, nrow, integer(1)) == 0L)
  )
  observation <- list(
    mode = mode, R = R.version.string,
    versions = vapply(c("dbplyr", "dplyr", "DBI", "RSQLite", "pkgload"),
                      function(pkg) as.character(packageVersion(pkg)),
                      character(1)),
    off = off, on = on
  )
  if (!is.null(output)) saveRDS(observation, output)
  cat("Mode:", mode, "\n")
  cat("Messages with auditing off:", length(off$messages), "\n")
  cat("Messages with auditing on:", length(on$messages), "\n")
  cat("Result values, classes, and original condition identity agree.\n")
  # This desired transparency assertion fails on the investigated snapshot.
  stopifnot(identical(off$messages, on$messages))
  invisible(observation)
}
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2L)
main(
  if (length(args) >= 1L) args[[1L]] else "ordinary",
  if (length(args) >= 2L) args[[2L]] else NULL
)
