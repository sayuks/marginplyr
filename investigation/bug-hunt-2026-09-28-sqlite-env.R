# Reproduce the SQLite `.env` summary failure from the public API.
# Run from the repository root. The command exits red until all three cases
# return the same values as the local backend.
pkgload::load_all(quiet = TRUE)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

dimension <- data.frame(.env = "east", check.names = FALSE)
ordinary <- data.frame(g = "east")
remote_dimension <- dplyr::copy_to(
  con, dimension, "env_dimension_probe", temporary = TRUE
)
remote_ordinary <- dplyr::copy_to(
  con, ordinary, "env_output_probe", temporary = TRUE
)

cases <- list(
  dimension = list(
    local = function() summarize_with_margins(
      dimension, rows = dplyr::n(),
      .grouping = grouping_set(tidyselect::all_of(".env")),
      .margin_label = NULL
    ),
    remote = function() summarize_with_margins(
      remote_dimension, rows = dplyr::n(),
      .grouping = grouping_set(tidyselect::all_of(".env")),
      .margin_label = NULL
    )
  ),
  summary_output = list(
    local = function() {
      name <- ".env"
      summarize_with_margins(
        ordinary, !!name := dplyr::n(),
        .grouping = rollup(g), .margin_label = NULL
      )
    },
    remote = function() {
      name <- ".env"
      summarize_with_margins(
        remote_ordinary, !!name := dplyr::n(),
        .grouping = rollup(g), .margin_label = NULL
      )
    }
  ),
  identifier = list(
    local = function() summarize_with_margins(
      ordinary, rows = dplyr::n(),
      .grouping = rollup(g), .margin_label = NULL, .id = ".env"
    ),
    remote = function() summarize_with_margins(
      remote_ordinary, rows = dplyr::n(),
      .grouping = rollup(g), .margin_label = NULL, .id = ".env"
    )
  )
)

canonical <- function(result) {
  result <- tibble::as_tibble(result)
  result <- result[sort(names(result))]
  result[vctrs::vec_order(result), , drop = FALSE]
}

failures <- character()
for (name in names(cases)) {
  expected <- canonical(cases[[name]]$local())
  actual <- tryCatch(
    canonical(dplyr::collect(cases[[name]]$remote())),
    error = identity
  )
  if (inherits(actual, "error")) {
    failures <- c(failures, name)
    cat(name, ": ERROR: ", conditionMessage(actual), "\n", sep = "")
  } else if (!identical(actual, expected)) {
    failures <- c(failures, name)
    cat(name, ": WRONG RESULT\n", sep = "")
    print(actual)
    print(expected)
  } else {
    cat(name, ": PASS\n", sep = "")
  }
}

DBI::dbDisconnect(con)
if (length(failures) > 0L) {
  stop("SQLite `.env` cases failed: ", toString(failures), call. = FALSE)
}
