# Run from the root of the archived package revision.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyselect))
run <- function() {
  options(marginplyr.audit_sql = TRUE, lifecycle_verbosity = "quiet")
  data <- tibble(
    prefix1 = c(1L, 2L), prefix02 = c(1, 2), prefix_end = c("a", "b"),
    middle_end = c("a", "c"), flag = c(TRUE, FALSE),
    day = as.Date(c("2020-01-01", "2020-01-02")), tail = c("b", "c")
  )
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  derive <- function(x) {
    x |> mutate(added = prefix1 + 1L) |> filter(flag) |>
      select(tail, added, everything())
  }
  step <- dtplyr::lazy_dt(data)
  dt_query <- derive(step)
  arrow_table <- arrow::Table$create(data)
  arrow_query <- derive(arrow_table)
  duck_table <- copy_to(duck, data, "selector_data", temporary = FALSE)
  duck_query <- derive(duck_table)
  inputs <- list(
    local = list(data, data),
    dtplyr = list(step, data[0, ]),
    dtplyr_derived = list(dt_query, as_tibble(dt_query)[0, ]),
    arrow = list(arrow_table, as.data.frame(arrow::schema(arrow_table))),
    arrow_query = list(arrow_query, as.data.frame(arrow::schema(arrow_query))),
    duckdb = list(duck_table, collect(head(duck_table, 0))),
    duckdb_query = list(duck_query, collect(head(duck_query, 0))),
    sqlite = list(copy_to(sqlite, data, "selector_data"), data[0, ])
  )
  vars <- c("tail", "prefix1", "day")
  helpers <- c(
    'starts_with("PREFIX")', 'ends_with("_END")', 'contains(c("fix", "tail"))',
    'matches("^(prefix|middle)", perl = TRUE)', 'num_range("prefix", 1:2)',
    'one_of(c("flag", "tail"))', 'everything()', 'last_col(offset = 1)',
    'all_of(vars)', 'any_of(c("tail", "unknown", "prefix1"))',
    'where(is.numeric)'
  )
  forms <- c(
    '%s', '-(%s)', '!(%s)', '!(-(%s))', '-(!(%s))',
    'c(everything(), -(%s), last_col())', 'c(-(%s), !last_col())',
    '(%s) & !last_col()', 'last_col() / (%s)'
  )
  cases <- c(
    unlist(lapply(forms, sprintf, helpers), use.names = FALSE),
    'tidyselect::starts_with("prefix", vars = rev(peek_vars()))',
    'tidyselect::starts_with("prefix", vars = names(tidyselect::peek_data()))',
    'all_of(names(tidyselect::peek_data()))',
    'all_of(names(Filter(is.numeric, tidyselect::peek_data())))',
    'last_col(offset = length(tidyselect::peek_vars()) - 1L)',
    'num_range("prefix", 2L, width = 2L)', 'matches("^prefix", ignore.case = FALSE)',
    'starts_with(c("tail", "prefix"), ignore.case = FALSE)',
    'tidyselect::everything(vars = rev(peek_vars()))',
    'where(~ length(.x) > 0L && is.numeric(.x))',
    'c(where(is.numeric), -where(is.integer))',
    '!(where(is.numeric) | where(is.character))',
    'c(last_col(), -(prefix1:middle_end), -where(is.logical), tail)',
    'all_of("absent")', 'any_of("absent")', '!everything()',
    'last_col(offset = 200L)', 'where(~ c(TRUE, FALSE))',
    'c(renamed = prefix1)', 'c(prefix1, NA_integer_)',
    'c(starts_with("prefix"), -starts_with("prefix"), tail)',
    'c(-all_of(character()), -any_of("absent"))',
    'c(0L, -last_col())', '-c()', '!(c())'
  )
  quiet <- function(expr) suppressWarnings(tryCatch(expr, error = identity))
  render <- function(x) {
    if (inherits(x, "error")) {
      return(paste(class(x)[1L], gsub("[\r\n]+", " ", conditionMessage(x))))
    }
    paste(x, collapse = ",")
  }
  for (input in names(inputs)) {
    fixture <- inputs[[input]]
    value <- fixture[[1L]]
    oracle <- fixture[[2L]]
    allow_predicates <- input != "sqlite"
    for (position in c("fixed", "included")) {
      for (case in cases) {
        if (!allow_predicates && grepl("peek_data", case, fixed = TRUE)) next
        q <- rlang::new_quosure(str2lang(case), environment())
        want <- quiet(names(eval_select(q, oracle, allow_rename = FALSE,
                                        allow_predicates = allow_predicates)))
        got <- quiet(if (position == "fixed") {
          inspect_grouping(value, .by = !!q, .format = "list")
        } else {
          inspect_grouping(value, .grouping = grouping_set(!!q), .format = "list")
        })
        answer <- if (inherits(got, "error")) got else got[[position]][[1L]]
        queries <- if (startsWith(input, "duckdb") || input == "sqlite") {
          last_sent_queries(include_result = FALSE, details = TRUE)
        }
        status <- if (inherits(want, "error")) {
          if (inherits(answer, "error")) "both rejected" else "accepted unexpectedly"
        } else if (identical(want, answer)) "equal" else "different"
        count <- if (is.null(queries)) 0L else nrow(queries)
        cat(input, position, case, status, render(want), render(answer), count,
            sep = "\t")
        cat("\n")
      }
    }
  }
}
run()
