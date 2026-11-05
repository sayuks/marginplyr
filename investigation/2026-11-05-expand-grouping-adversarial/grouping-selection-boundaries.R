# Run from the package root. The optional backends are dtplyr and DuckDB.
# Pass --assert for all contracts or --assert=qualified, --assert=embraced,
# or --assert=pronoun for one contract. Default mode checks the controls.
pkgload::load_all(quiet = TRUE)

frame <- data.frame(a = c("x", "x"), b = 1:2)
matches <- logical()

run_callback <- function(expr) {
  calls <- 0L
  next_col <- function() {
    calls <<- calls + 1L
    c("a", "b")[[calls]]
  }
  value <- eval(substitute(expr))
  list(value = value, calls = calls)
}
callback <- run_callback(
  inspect_grouping(frame, .grouping = grouping_set(tidyselect::all_of(next_col())),
                   .format = "list")$included[[1L]]
)
callback_expand <- run_callback(
  expand_with_margins(
    frame, .grouping = rollup(tidyselect::all_of(next_col())), .id = "set"
  )
)
callback_controls <- list(
  run_callback(names(tidyselect::eval_select(
    rlang::quo(tidyselect::all_of(next_col())), frame
  ))),
  run_callback(inspect_grouping(
    frame, .grouping = grouping_set(next_col()), .format = "list"
  )$included[[1L]]),
  run_callback(inspect_grouping(
    frame, .by = tidyselect::all_of(next_col()), .format = "list"
  )$fixed[[1L]]),
  run_callback(inspect_grouping(
    frame, .grouping = grouping_set(
      tidyselect::all_of(next_col()), tidyselect::where(is.factor)
    ),
    .format = "list"
  )$included[[1L]])
)
stopifnot(all(vapply(callback_controls, identical, logical(1),
                     list(value = "a", calls = 1L))))
cat("Helper callback, inspected:", callback$value, "calls:", callback$calls,
    "; expanded margin:", names(callback_expand$value)[1L],
    "calls:", callback_expand$calls, "\n")

run_qualified <- function(expr) {
  delayedAssign("a", if (is.character(tidyselect::peek_data()$a)) "b" else "a")
  eval(substitute(expr))
}
qualified <- c(
  native = run_qualified(names(tidyselect::eval_select(
    rlang::quo(base::`(`(c(a))), frame
  ))),
  by = run_qualified(inspect_grouping(
    frame, .by = base::`(`(c(a)), .format = "list"
  )$fixed[[1L]]),
  spec = run_qualified(inspect_grouping(
    frame, .grouping = grouping_set(base::`(`(c(a))), .format = "list"
  )$included[[1L]]),
  expand = run_qualified(names(expand_with_margins(
    frame, .grouping = rollup(base::`(`(c(a)))
  ))[1L]),
  identity = run_qualified(inspect_grouping(
    frame, .by = base::identity(c(a)), .format = "list"
  )$fixed[[1L]]),
  qualified_c = run_qualified(inspect_grouping(
    frame, .by = base::`(`(base::c(a)), .format = "list"
  )$fixed[[1L]]),
  bare = run_qualified(inspect_grouping(
    frame, .by = (c(a)), .format = "list"
  )$fixed[[1L]])
)
stopifnot(
  all(qualified[c("native", "identity", "qualified_c")] == "b"),
  qualified[["bare"]] == "a"
)
print(qualified)
matches["qualified"] <- all(qualified[c("by", "spec", "expand")] == "b")

capture <- function(expr) tryCatch(expr, error = identity)
is_metadata_refusal <- function(result) {
  inherits(result, "marginplyr_error") &&
    grepl("Can't safely determine column metadata", conditionMessage(result),
          fixed = TRUE)
}

cols <- c("b", "a")
consumers <- list(
  embraced_by = function(input, col) inspect_grouping(
    input, .by = c({{ col }}, b), .format = "list"
  )$fixed,
  embraced_spec = function(input, col) inspect_grouping(
    input, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
  )$included,
  env_by = function(input) inspect_grouping(
    input, .by = .env$cols, .format = "list"
  )$fixed,
  env_spec = function(input) inspect_grouping(
    input, .grouping = grouping_set(.env[["cols"]]), .format = "list"
  )$included
)
typed_controls <- function(derived, frame) {
  stopifnot(
    identical(
      inspect_grouping(derived, .by = c(a, b), .format = "list")$fixed,
      list(c("a", "b"))
    ),
    identical(
      inspect_grouping(
        derived, .grouping = grouping_set(tidyselect::all_of(.env$cols)),
        .format = "list"
      )$included,
      list(cols)
    ),
    identical(names(tidyselect::eval_select(rlang::quo(.env$cols), frame)), cols),
    is_metadata_refusal(capture(inspect_grouping(
      derived, .grouping = grouping_set(tidyselect::where(is.character))
    )))
  )
}
derived <- dplyr::mutate(dtplyr::lazy_dt(frame), added = paste0(a, "!"))
typed_controls(derived, frame)
for (case in names(consumers)) {
  consumer <- consumers[[case]]
  args <- if (startsWith(case, "embraced")) list(quote(a)) else list()
  expected <- if (length(args)) list(c("a", "b")) else list(cols)
  local <- do.call(consumer, c(list(frame), args))
  lazy <- capture(do.call(consumer, c(list(derived), args)))
  stopifnot(identical(local, expected))
  cat(case, "derived:", if (is_metadata_refusal(lazy)) {
    "typed metadata refusal"
  } else {
    paste(unlist(lazy), collapse = ",")
  }, "\n")
  matches[paste0(case, '_dtplyr')] <- identical(lazy, expected)
}

trace_errors <- function() {
  cols <- "absent"
  for (name in c("env_by", "env_spec")) {
    environment(consumers[[name]]) <- environment()
  }
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, frame, "spec_selection_probe", temporary = TRUE)
  queries <- new.env(parent = emptyenv())
  old <- options(marginplyr.grouping_selection_probe = queries)
  on.exit(options(old), add = TRUE)
  suppressMessages(trace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    tracer = quote({
      calls <- getOption("marginplyr.grouping_selection_probe")
      calls$sql <- c(calls$sql, statement)
    }),
    where = asNamespace("DBI"), print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)

  queries$sql <- character()
  explicit <- capture(inspect_grouping(
    remote, .grouping = grouping_set(tidyselect::all_of(.env$cols))
  ))
  stopifnot(
    inherits(explicit, "error"),
    grepl("absent", conditionMessage(explicit), fixed = TRUE),
    length(queries$sql) == 0L
  )
  clean <- logical()
  for (case in names(consumers)) {
    args <- if (startsWith(case, "embraced")) list("absent") else list()
    queries$sql <- character()
    result <- capture(do.call(consumers[[case]], c(list(remote), args)))
    stopifnot(
      inherits(result, "vctrs_error_subscript_oob"),
      grepl("absent", conditionMessage(result), fixed = TRUE),
      all(grepl("LIMIT 0", queries$sql, fixed = TRUE))
    )
    cat(case, "DuckDB absent-column metadata sends:", length(queries$sql), "\n")
    clean[paste0(case, '_duckdb')] <- length(queries$sql) == 0L
  }
  clean
}
matches <- c(matches, trace_errors())
args <- commandArgs(trailingOnly = TRUE)
checks <- list(
  qualified = "qualified",
  embraced = names(matches)[startsWith(names(matches), "embraced_")],
  pronoun = names(matches)[startsWith(names(matches), "env_")]
)
requested <- startsWith(args, "--assert=")
if (any(requested)) {
  selected <- sub("^--assert=", "", args[requested])
  stopifnot(all(selected %in% names(checks)))
  matches <- matches[unlist(checks[selected], use.names = FALSE)]
}
if ("--assert" %in% args || any(requested)) {
  if (!all(matches)) stop("Contract mismatch: ",
                          paste(names(matches)[!matches], collapse = ", "))
}
