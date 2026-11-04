# Run from the pinned package root.
pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
capture <- function(expr) tryCatch(expr, error = identity)
unsafe <- function(value) {
  inherits(value, "marginplyr_error") &&
    grepl("Can't safely determine column metadata", conditionMessage(value),
          fixed = TRUE)
}
missing_selection <- function(value) {
  inherits(value, "error") &&
    !inherits(value, "marginplyr_error") &&
    grepl("absent", conditionMessage(value), fixed = TRUE)
}
duplicate <- function(value) {
  inherits(value, "marginplyr_error") &&
    grepl("Duplicate grouping sets", conditionMessage(value), fixed = TRUE) &&
    grepl("1, 2", conditionMessage(value), fixed = TRUE)
}

run <- function(lane, assert_contract) {
  data <- tibble::tibble(a = 1:2, b = c("x", "y"))
  derived <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = paste0(b, "x"))
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "grouping_contracts")
  counter <- new.env(parent = emptyenv())
  counter$queries <- 0L
  signature <- c("duckdb_connection", "character")
  invisible(suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE, tracer = function() {
      counter$queries <- counter$queries + 1L
    }
  ))))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE)

  if (lane == "selection") {
    cols <- c("a", "added")
    absent <- "absent"
    proxy <- list(a = integer(), b = integer(), added = integer())
    get_keys <- function(input, selection, fixed) {
      if (fixed) {
        return(inspect_grouping(input, .by = !!selection,
                                .format = "list")$fixed[[1L]])
      }
      inspect_grouping(input, .grouping = grouping_set(!!selection),
                       .format = "list")$included[[1L]]
    }
    env_selection <- rlang::exprs(
      all_of(.env$cols),
      tidyselect::all_of(.env[["cols"]]),
      any_of(.env$cols),
      starts_with(.env[["cols"]]),
      tidyselect::one_of(.env$cols),
      all_of(.env$cols[c(1L, 2L)])
    )
    plain_selection <- rlang::exprs(
      all_of(cols),
      tidyselect::all_of(cols),
      any_of(cols),
      starts_with(cols),
      tidyselect::one_of(cols),
      all_of(cols[c(1L, 2L)])
    )
    for (fixed in c(FALSE, TRUE)) {
      for (i in seq_along(env_selection)) {
        expr <- env_selection[[i]]
        oracle <- names(tidyselect::eval_select(expr, proxy))
        stopifnot(identical(oracle, cols))
        stopifnot(identical(get_keys(derived, plain_selection[[i]], fixed), cols))
        observed <- capture(get_keys(derived, expr, fixed))
        stopifnot(if (assert_contract) identical(observed, oracle) else unsafe(observed))
      }
      for (expr in rlang::exprs(all_of(.env$absent), all_of(.env[["absent"]]))) {
        counter$queries <- 0L
        observed <- capture(get_keys(remote, expr, fixed))
        stopifnot(missing_selection(observed))
        stopifnot(counter$queries == if (assert_contract) 0L else 1L)
        observed <- capture(get_keys(derived, expr, fixed))
        stopifnot(if (assert_contract) missing_selection(observed) else unsafe(observed))
        counter$queries <- 0L
        stopifnot(missing_selection(capture(get_keys(
          remote, quote(all_of(absent)), fixed
        ))), counter$queries == 0L)
      }
    }

    # A deferred binding must not be forced merely to classify selection.
    bindings <- 0L
    makeActiveBinding("active_cols", function() {
      bindings <<- bindings + 1L
      cols
    }, environment())
    for (fixed in c(FALSE, TRUE)) {
      active <- capture(get_keys(derived, quote(all_of(.env$active_cols)), fixed))
      typed <- capture(get_keys(derived, quote(where(is.character)), fixed))
      stopifnot(unsafe(active), unsafe(typed), bindings == 0L)
    }
    return(invisible(NULL))
  }
  calls <- 0L
  predicate <- function(column) {
    calls <<- calls + 1L
    is.character(column)
  }
  forms <- list(
    grouping_spec(grouping_sets(a, a), where(predicate)),
    grouping_spec(grouping_sets(a, a), grouping_set(where(predicate))),
    grouping_spec(grouping_sets(a, a), b, where(predicate))
  )
  controls <- list(
    grouping_spec(grouping_sets(a, a), b),
    grouping_sets(a, a, where(predicate))
  )
  inspect <- function(input, spec, policy = "error") {
    inspect_grouping(input, .grouping = spec, .duplicates = policy,
                     .format = "list")
  }
  summarize <- function(input, spec, policy = "error") {
    summarize_with_margins(input, n = dplyr::n(), .grouping = spec,
                           .duplicates = policy)
  }
  for (verb in list(inspect, summarize)) {
    for (spec in forms) {
      calls <- 0L
      counter$queries <- 0L
      observed <- capture(verb(remote, spec))
      stopifnot(duplicate(observed))
      stopifnot(
        counter$queries == if (assert_contract) 0L else 1L,
        calls == if (assert_contract) 0L else 2L
      )
      calls <- 0L
      observed <- capture(verb(derived, spec))
      stopifnot(if (assert_contract) duplicate(observed) else unsafe(observed))
      stopifnot(calls == 0L)
    }
    for (spec in controls) {
      calls <- 0L
      counter$queries <- 0L
      stopifnot(duplicate(capture(verb(remote, spec))),
                counter$queries == 0L, calls == 0L)
      stopifnot(duplicate(capture(verb(derived, spec))), calls == 0L)
    }
  }
  for (policy in c("keep", "drop")) {
    calls <- 0L
    counter$queries <- 0L
    result <- inspect(remote, forms[[1L]], policy)
    n <- if (policy == "keep") 2L else 1L
    stopifnot(identical(result$set_id, seq_len(n)),
              identical(result$included, rep(list(c("a", "b")), n)),
              counter$queries == 1L, calls == 2L)
  }
  calls <- 0L
  counter$queries <- 0L
  valid <- grouping_spec(grouping_sets(a, grouping_set()), where(predicate))
  result <- inspect(remote, valid)
  stopifnot(identical(result$included, list(c("a", "b"), "b")),
            counter$queries == 1L, calls == 2L)
}

if (!length(args)) {
  run("selection", FALSE)
  run("product", FALSE)
  cat("Lexical selection: 12 failed name-only inspections, 4 hidden dtplyr",
      "selection errors and 4 premature DuckDB DBI method invocations;",
      "all plain-binding and tidyselect controls passed.\n")
  cat("Product duplicates: 6 premature DuckDB DBI method invocations with",
      "12 predicate calls and 6 hidden dtplyr duplicates; all 8",
      "literal/union and 3 valid-plan controls passed.\n")
} else {
  stopifnot(length(args) == 1L, args %in% c("--assert-selection", "--assert-product"))
  run(sub("^--assert-", "", args), TRUE)
}
cat(R.version.string, R.version$platform, "\n")
for (package in c("rlang", "dplyr", "tidyselect", "dtplyr", "dbplyr", "duckdb")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
