# Run from the package source directory. Both modes run the same controls.
pkgload::load_all(".", quiet = TRUE)

capture <- function(expr) tryCatch(expr, error = identity)

selected <- function(plan, column = "included") {
  if (inherits(plan, "condition")) {
    return(paste(class(plan)[[1L]], conditionMessage(plan), sep = " | "))
  }
  plan[[column]][[1L]]
}

audit <- function() last_sent_queries(details = TRUE)

pivot_read <- function(con) {
  DBI::dbExecute(con, paste(
    "CREATE TABLE source_rows AS SELECT * FROM",
    "(VALUES ('east', 1), ('west', 2), ('west', 3)) AS t(region, value)"
  ))
  DBI::dbExecute(con, "CREATE SEQUENCE inspected_rows START 1")
  reads <- function() capture(DBI::dbGetQuery(
    con, "SELECT currval('inspected_rows') AS n"
  )$n[[1L]])
  untouched <- function() inherits(reads(), "error") &&
    grepl("sequence is not yet defined", conditionMessage(reads()), fixed = TRUE)
  source <- paste(
    "PIVOT (SELECT CASE WHEN nextval('inspected_rows') > 0",
    "THEN region ELSE 'other' END AS k, value FROM source_rows) ON k"
  )
  make <- function(on) dplyr::tbl(
    con, dbplyr::sql(paste(source, on, "USING sum(value)")),
    vars = c("east", "west")
  )
  dynamic <- make("")
  static <- make("IN ('east', 'west')")
  literal <- inspect_grouping(dynamic, .grouping = grouping_set(east),
                              .format = "list")
  stopifnot(identical(selected(literal), "east"), untouched(), nrow(audit()) == 0L)
  static_plan <- inspect_grouping(
    static, .grouping = grouping_set(dplyr::where(is.numeric)), .format = "list"
  )
  stopifnot(identical(selected(static_plan), c("east", "west")), untouched(),
            identical(audit()$purpose, "selection_proxy"))
  actual <- capture(inspect_grouping(
    dynamic, .grouping = grouping_set(dplyr::where(is.numeric)), .format = "list"
  ))
  rows <- reads()
  log <- audit()
  print(list(dynamic_pivot_plan = selected(actual), sequence_after = rows,
             purpose = log$purpose, status = log$status, sql = log$sql))
  list(
    observed = !inherits(actual, "error") && isTRUE(as.numeric(rows) == 3) &&
      identical(log$purpose, "selection_proxy") &&
      grepl("LIMIT 0", log$sql, fixed = TRUE),
    contract = untouched() && (inherits(actual, "marginplyr_error") ||
      identical(selected(actual), c("east", "west")))
  )
}

proxy_names <- function(con) {
  input <- dplyr::tbl(
    con, dbplyr::sql("SELECT 1 AS region, 'west' AS REGION"),
    vars = c("region", "REGION")
  )
  stopifnot(identical(names(dplyr::collect(input)), c("region", "REGION")))
  literal <- inspect_grouping(input, .grouping = grouping_set(REGION),
                              .format = "list")
  stopifnot(identical(selected(literal), "REGION"), nrow(audit()) == 0L)
  reference_overlap <- capture(inspect_grouping(
    input, .by = REGION, .grouping = grouping_set(REGION)
  ))
  stopifnot(inherits(reference_overlap, "marginplyr_error"))
  group <- capture(inspect_grouping(
    input, .grouping = grouping_set(dplyr::where(is.character)), .format = "list"
  ))
  group_query <- audit()$sql
  fixed <- capture(inspect_grouping(
    input, .by = dplyr::where(is.character), .format = "list"
  ))
  overlap <- capture(inspect_grouping(
    input, .by = REGION, .grouping = grouping_set(dplyr::where(is.character)),
    .format = "list"
  ))
  print(list(typed_dimension = selected(group), typed_fixed = selected(fixed, "fixed"),
             mixed_overlap = selected(overlap), sql = group_query))
  list(
    observed = identical(selected(group), "REGION_1") &&
      inherits(fixed, "simpleError") &&
      grepl("Unknown `.by` column `REGION_1`", conditionMessage(fixed),
            fixed = TRUE) && identical(selected(overlap), "REGION_1"),
    contract = (inherits(group, "marginplyr_error") ||
      identical(selected(group), "REGION")) &&
      (inherits(fixed, "marginplyr_error") ||
      identical(selected(fixed, "fixed"), "REGION")) &&
      inherits(overlap, "marginplyr_error")
  )
}

receiver_dispatch <- function(duck, sqlite) {
  input <- tibble::tibble(a = 1:2, b = c("x", "y"))
  backends <- list(
    arrow = arrow::arrow_table(input),
    duckdb = dplyr::copy_to(duck, input, "tibble_receiver"),
    sqlite = dplyr::copy_to(sqlite, input, "tibble_receiver")
  )
  direct <- rlang::quo(dplyr::all_of(names(tidyselect::peek_data())[1L]))
  wrapped <- rlang::quo(dplyr::all_of(identity(names(tidyselect::peek_data())[1L])))
  stopifnot(identical(names(tidyselect::eval_select(direct, input)), "a"))
  method <- function(x) {
    stored <- attr(x, "names")
    active <- capture(tidyselect::peek_data())
    if (identical(stored, c("a", "b")) && identical(active, x)) rev(stored) else stored
  }
  stopifnot(!exists("names.tbl_df", envir = globalenv(), inherits = FALSE))
  assign("names.tbl_df", method, envir = globalenv())
  on.exit(rm("names.tbl_df", envir = globalenv()), add = TRUE)
  stopifnot(identical(names(tidyselect::eval_select(direct, input)), "b"))
  checks <- lapply(names(backends), function(backend) {
    x <- backends[[backend]]
    fixed <- function(expr) {
      capture(inspect_grouping(x, .by = !!expr, .format = "list"))
    }
    variable <- function(expr) {
      capture(inspect_grouping(x, .by = a, .grouping = grouping_set(!!expr),
                               .format = "list"))
    }
    stopifnot(identical(selected(fixed(wrapped), "fixed"), "b"),
              identical(selected(variable(wrapped)), "b"))
    actual_fixed <- fixed(direct)
    actual_dimension <- variable(direct)
    print(list(backend = backend, direct_fixed = selected(actual_fixed, "fixed"),
               direct_dimension = selected(actual_dimension)))
    c(
      observed = identical(selected(actual_fixed, "fixed"), "a") &&
        inherits(actual_dimension, "marginplyr_error") &&
        grepl("both `.by` and `.grouping`", conditionMessage(actual_dimension),
              fixed = TRUE),
      contract = identical(selected(actual_fixed, "fixed"), "b") &&
        identical(selected(actual_dimension), "b")
    )
  })
  list(observed = all(vapply(checks, "[[", logical(1), "observed")),
       contract = all(vapply(checks, "[[", logical(1), "contract")))
}

reader_options <- function() {
  fields <- stats::setNames(list(1:2, 3:4), c("a", "a"))
  batch <- do.call(arrow::record_batch, fields)
  table <- do.call(arrow::arrow_table, fields)
  wrong_format <- function(x) capture(inspect_grouping(x, .format = "bogus"))
  wrong_duplicates <- function(x) capture(inspect_grouping(x, .duplicates = "bogus"))
  check <- function(f, argument) {
    ordinary <- f(table)
    reader <- arrow::RecordBatchReader$create(batch)
    actual <- f(reader)
    remaining <- reader$read_next_batch()
    stopifnot(inherits(ordinary, "marginplyr_error"),
              grepl(argument, conditionMessage(ordinary), fixed = TRUE),
              identical(remaining$num_rows, 2L) ||
                identical(as.numeric(remaining$num_rows), 2))
    print(list(argument = argument, reader = conditionMessage(actual),
               table = conditionMessage(ordinary), unread_rows = remaining$num_rows))
    c(
      observed = !inherits(actual, "marginplyr_error") &&
        grepl("Field names must be unique", conditionMessage(actual), fixed = TRUE),
      contract = inherits(actual, "marginplyr_error") &&
        grepl(argument, conditionMessage(actual), fixed = TRUE)
    )
  }
  format <- check(wrong_format, ".format")
  duplicates <- check(wrong_duplicates, ".duplicates")
  list(observed = format[["observed"]] && duplicates[["observed"]],
       contract = format[["contract"]] && duplicates[["contract"]])
}

main <- function(mode) {
  options(marginplyr.audit_sql = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  outcomes <- list(pivot = pivot_read(duck), proxy_names = proxy_names(duck),
                   receiver = receiver_dispatch(duck, sqlite), reader = reader_options())
  passed <- vapply(outcomes, "[[", logical(1), mode)
  print(passed)
  all(passed)
}

mode <- commandArgs(trailingOnly = TRUE)
stopifnot(length(mode) == 1L, mode %in% c("observed", "contract"))
if (!main(mode)) quit(status = 1L)
