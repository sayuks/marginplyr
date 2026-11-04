# Run from the repository root with dtplyr and DuckDB installed.
# Use --assert for the names-only inspection and early-diagnostic contract.
pkgload::load_all(quiet = TRUE)

run <- function() {
  frame <- data.frame(a = "x", b = 1L)
  root <- dtplyr::lazy_dt(frame)
  derived <- dplyr::mutate(root, extra = paste0(a, "!"))
  data <- data.frame(frame, extra = "x!")
  assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)

  consumers <- list(
    by_walk = function(input, ...) {
      inspect_grouping(input, .by = c(...), .format = "list")$fixed
    },
    by_helper = function(input, ...) {
      inspect_grouping(input, .by = tidyselect::all_of(c(...)),
                       .format = "list")$fixed
    },
    spec_walk = function(input, ...) {
      inspect_grouping(input, .grouping = grouping_set(c(...)),
                       .format = "list")$included
    },
    spec_helper = function(input, ...) {
      inspect_grouping(input, .grouping = grouping_set(tidyselect::all_of(c(...))),
                       .format = "list")$included
    }
  )
  expected <- list(empty = list(character()), populated = list(c("b", "a")))
  capture <- function(expr) tryCatch(expr, error = identity)
  is_metadata_refusal <- function(result) {
    inherits(result, "marginplyr_error") &&
      grepl("Can't safely determine column metadata", conditionMessage(result),
            fixed = TRUE)
  }

  inline_walk <- inspect_grouping(derived, .grouping = grouping_set(c("b", "a")),
                                  .format = "list")$included
  inline_helper <- inspect_grouping(
    derived, .by = tidyselect::all_of(c("b", "a")), .format = "list"
  )$fixed
  stopifnot(identical(inline_walk, expected$populated),
            identical(inline_helper, expected$populated))

  matches <- logical()
  for (case in names(consumers)) {
    consumer <- consumers[[case]]
    for (backend in c("local", "root", "derived")) {
      input <- switch(backend, local = data, root = root, derived = derived)
      empty <- capture(consumer(input))
      populated <- capture(consumer(input, "b", "a"))
      for (name in names(expected)) {
        result <- switch(name, empty = empty, populated = populated)
        matches_now <- identical(result, expected[[name]])
        outcome <- if (matches_now) "matches" else if (
          is_metadata_refusal(result)
        ) "typed metadata refusal" else "unexpected result"
        cat(case, backend, name, outcome, "\n")
        if (backend == "derived") {
          matches <- c(matches, matches_now)
        } else {
          stopifnot(matches_now)
        }
      }
    }
  }

  predicate_reads <- 0L
  predicate <- function(x) {
    predicate_reads <<- predicate_reads + 1L
    is.character(x)
  }
  local_typed <- consumers$spec_walk(data, tidyselect::where(predicate))
  stopifnot(identical(local_typed, list(c("a", "extra"))),
            predicate_reads == ncol(data))
  predicate_reads <- 0L
  unsafe_typed <- capture(consumers$spec_walk(derived, tidyselect::where(predicate)))
  stopifnot(is_metadata_refusal(unsafe_typed), predicate_reads == 0L)
  cat("Genuine type predicate refused without execution on the derived step.\n")

  direct <- function(input, ...) {
    tidyselect::eval_select(rlang::quo(c(...)), data = input)
  }
  stopifnot(identical(names(direct(data)), character()),
            identical(names(direct(data, "b", "a")), c("b", "a")))

  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, frame, "forwarded_grouping_names", temporary = TRUE)
  queries <- new.env(parent = emptyenv())
  old <- options(marginplyr.variadic_probe_queries = queries)
  on.exit(options(old), add = TRUE)
  suppressMessages(trace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    tracer = quote({
      recorded <- getOption("marginplyr.variadic_probe_queries")
      recorded$sql <- c(recorded$sql, statement)
    }),
    where = asNamespace("DBI"), print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)

  queries$sql <- character()
  inline_error <- capture(inspect_grouping(
    remote, .grouping = grouping_set(c("absent"))
  ))
  stopifnot(inherits(inline_error, "vctrs_error_subscript_oob"),
            length(queries$sql) == 0L)
  for (case in names(consumers)) {
    queries$sql <- character()
    forwarded_error <- capture(consumers[[case]](remote, "absent"))

    stopifnot(inherits(forwarded_error, "error"),
              grepl("`absent` doesn.t exist", conditionMessage(forwarded_error)),
              all(grepl("LIMIT 0", queries$sql, fixed = TRUE)))
    cat(case, "DuckDB forwarded absent column sends", length(queries$sql), "\n")
    matches <- c(matches, length(queries$sql) == 0L)
  }
  if (assert_contract) stopifnot(all(matches))
}

run()
