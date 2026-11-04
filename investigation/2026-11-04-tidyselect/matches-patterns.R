args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

inspect_pattern_selectors <- function() {
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  # Selecting column names should work before this derived step is evaluated.
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  snapshot <- dplyr::collect(step)
  regex <- stringr::regex("^a", ignore_case = TRUE)
  fixed <- stringr::fixed("add", ignore_case = TRUE)
  coll <- stringr::coll("a", ignore_case = TRUE, locale = "en")
  pattern_expressions <- rlang::exprs(
    matches(stringr::regex("^a", ignore_case = TRUE)), matches(regex),
    matches(stringr::fixed("add", ignore_case = TRUE)), matches(fixed),
    matches(stringr::coll("a", ignore_case = TRUE, locale = "en")), matches(coll),
    -matches(stringr::regex("^a")), !matches(fixed),
    c(everything(), -matches(regex)),
    c(!matches(fixed), matches("^b$")),
    matches(regex) & matches(coll), matches(fixed) | last_col(1L)
  )
  controls <- rlang::exprs(matches("^a"), -matches("^a"), !matches("add"),
                          c(everything(), -matches("^a")))
  inspect <- list(
    .by = function(expr) inspect_grouping(step, .by = !!expr, .format = "list")$fixed,
    grouping_set = function(expr) inspect_grouping(
      step, .grouping = grouping_set(!!expr), .format = "list"
    )$included,
    rollup = function(expr) inspect_grouping(
      step, .grouping = rollup(!!expr), .format = "list"
    )$included
  )
  failures <- 0L
  for (expr in c(controls, pattern_expressions)) {
    quo <- rlang::new_quosure(expr, rlang::current_env())
    cols <- names(tidyselect::eval_select(quo, snapshot))
    direct_step <- dplyr::tbl_vars(dplyr::select(step, !!quo))
    stopifnot(identical(as.character(direct_step), cols))
    for (slot in names(inspect)) {
      observed <- tryCatch(inspect[[slot]](quo), error = identity)
      expected <- if (slot == "rollup") {
        lapply(length(cols):0L, function(n) cols[seq_len(n)])
      } else {
        list(cols)
      }
      if (inherits(observed, "error")) {
        stopifnot(any(vapply(pattern_expressions, identical, logical(1), y = expr)))
        stopifnot(grepl("Can't safely determine column metadata", conditionMessage(observed),
                       fixed = TRUE))
        failures <- failures + 1L
      } else {
        stopifnot(identical(observed, expected))
      }
    }
  }
  cat("dtplyr typed-metadata refusals:", failures, "of",
      length(pattern_expressions) * length(inspect), "pattern calls. Controls:",
      length(controls) * length(inspect), "\n")
  failures
}

count_pattern_error_queries <- function() {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  connection <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  data <- tibble::tibble(a = "x", b = 1L)
  step <- dplyr::copy_to(connection, data, "pattern_test")
  count <- 0L
  increment_count <- function() count <<- count + 1L
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"), print = FALSE,
    tracer = increment_count
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)
  queries <- c(literal = 0L, stringr = 0L)
  selections <- list(literal = quote(matches("[")),
                     stringr = quote(matches(stringr::regex("["))))
  for (key in names(selections)) {
    expr <- selections[[key]]
    verbs <- list(
      inspect_by = function(expr) inspect_grouping(step, .by = !!expr),
      inspect_grouping = function(expr) inspect_grouping(
        step, .grouping = grouping_set(!!expr)
      ),
      expand_by = function(expr) expand_with_margins(step, .by = !!expr),
      expand_grouping = function(expr) expand_with_margins(
        step, .grouping = grouping_set(!!expr)
      ),
      summarize_by = function(expr) summarize_with_margins(
        step, n = dplyr::n(), .by = !!expr
      ),
      summarize_grouping = function(expr) summarize_with_margins(
        step, n = dplyr::n(), .grouping = grouping_set(!!expr)
      )
    )
    for (placement in names(verbs)) {
      direct <- tryCatch(tidyselect::eval_select(expr, data), error = identity)
      count <- 0L
      error <- tryCatch(suppressWarnings(verbs[[placement]](expr)), error = identity)
      stopifnot(inherits(direct, "error"), inherits(error, "error"),
                identical(class(error), class(direct)))
      audit <- last_sent_queries(include_result = FALSE, details = TRUE)
      stopifnot(nrow(audit) == count, count <= 1L,
                all(audit$purpose == "selection_proxy"),
                all(grepl("LIMIT 0", audit$sql, fixed = TRUE)))
      cat(key, placement, count, paste(audit$sql, collapse = " "), "\n")
      queries[[key]] <- queries[[key]] + count
    }
  }
  print(queries)
  queries
}

pattern_refusals <- inspect_pattern_selectors()
queries <- count_pattern_error_queries()
if ("--assert" %in% args) {
  stopifnot(pattern_refusals == 0L, all(queries == 0L))
} else {
  stopifnot(pattern_refusals == 36L, identical(unname(queries), c(0L, 6L)))
}
