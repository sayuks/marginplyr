# Replays the 2026-11-04 names-only inspection observation.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)

base <- tibble::tibble(a = c("x", "y"), b = 1:2)
local <- dplyr::mutate(base, added = paste0(a, "!"))
callback_count <- 0L
derive <- function(x) {
  callback_count <<- callback_count + 1L
  paste0(x, "!")
}
inputs <- list(
  local = local,
  root = dtplyr::lazy_dt(local),
  derived = dplyr::mutate(dtplyr::lazy_dt(base), added = derive(a))
)
selectors <- list(
  peek_vars = quote(tidyselect::peek_vars()),
  names = quote(names(tidyselect::peek_data())),
  indexed = quote(tidyselect::peek_vars()[c(3L, 1L)])
)
roles <- c(".by", "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")

inspect <- function(data, role, selection) {
  if (role == ".by") {
    return(inspect_grouping(data, .by = !!selection, .format = "list"))
  }
  constructor <- getExportedValue("marginplyr", role)
  inspect_grouping(data, .grouping = constructor(!!selection), .format = "list")
}
probe <- function(input, selector, role, wrapped) {
  data <- inputs[[input]]
  expr <- selectors[[selector]]
  expected_names <- names(tidyselect::eval_select(expr, local[0, ]))
  expected <- inspect(
    data, role, rlang::expr(tidyselect::all_of(!!expected_names))
  )
  if (wrapped) expr <- rlang::expr(tidyselect::all_of(!!expr))
  result <- tryCatch(inspect(data, role, expr), error = identity)
  status <- if (identical(result, expected)) {
    "matched"
  } else if (inherits(result, "marginplyr_error") &&
             grepl("Can't safely determine column metadata",
                   conditionMessage(result), fixed = TRUE)) {
    "unsafe metadata refusal"
  } else {
    stop("Unexpected inspection outcome: ",
         paste(capture.output(str(result)), collapse = " "))
  }
  data.frame(input, selector, role, wrapped, status)
}

cases <- expand.grid(
  input = names(inputs),
  selector = names(selectors),
  role = roles,
  wrapped = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)
outcomes <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  do.call(probe, as.list(cases[i, ]))
}))

# The same graph genuinely needs its column types for a typed predicate.
typed <- tryCatch(
  inspect(inputs$derived, "grouping_set", quote(tidyselect::where(is.character))),
  error = identity
)
print(with(outcomes, table(input, wrapped, status)))
cat("Upstream callback calls:", callback_count, "\n")
cat("Typed control:", conditionMessage(typed), "\n")
stopifnot(
  callback_count == 0L,
  inherits(typed, "marginplyr_error"),
  grepl("Can't safely determine column metadata", conditionMessage(typed),
        fixed = TRUE)
)

# Count actual DuckDB query calls for a failure settled by the column names.
# The raw and wrapped selectors here yield the same tidyselect condition.
observe_duckdb <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tbl <- dplyr::copy_to(con, local, "direct_names_probe")
  statements <- character()
  signature <- c("duckdb_connection", "character")
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE, tracer = function() {
      statements <<- c(statements, get("statement", parent.frame()))
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE, after = FALSE)
  counts <- matrix(NA_integer_, nrow = length(roles), ncol = 2L,
                   dimnames = list(roles, c("direct", "wrapped")))
  for (role in roles) {
    raw <- quote(tidyselect::peek_vars()[99L])
    for (wrapped in c(FALSE, TRUE)) {
      expr <- if (wrapped) rlang::expr(tidyselect::all_of(!!raw)) else raw
      expected <- tryCatch(tidyselect::eval_select(expr, local[0, ]),
                           error = identity)
      statements <- character()
      observed <- tryCatch(inspect(tbl, role, expr), error = identity)
      stopifnot(
        inherits(observed, "error"),
        identical(class(observed), class(expected)),
        identical(conditionMessage(observed), conditionMessage(expected)),
        grepl("Selections can't have missing values",
              conditionMessage(observed), fixed = TRUE)
      )
      counts[role, 1L + as.integer(wrapped)] <- length(statements)
      if (length(statements) > 0L) {
        stopifnot(all(grepl("LIMIT 0", statements, fixed = TRUE)))
      }
    }
  }
  print(counts)
  counts
}

duck_counts <- observe_duckdb()
print(sessionInfo())
if (assert_desired) {
  stopifnot(all(outcomes$status == "matched"))
} else {
  refused <- with(outcomes, input == "derived" & !wrapped)
  stopifnot(
    all(outcomes$status[refused] == "unsafe metadata refusal"),
    all(outcomes$status[!refused] == "matched")
  )
}
if (assert_desired) {
  stopifnot(all(duck_counts == 0L))
} else {
  stopifnot(all(duck_counts[, "direct"] == 1L),
            all(duck_counts[, "wrapped"] == 0L))
}
