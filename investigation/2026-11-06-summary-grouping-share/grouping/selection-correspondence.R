Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- tibble::tibble(
  alpha = c("x", "y"), beta = 1:2, gamma = c(1, 2),
  delta = factor(c("x", "y")), epsilon = as.Date(c("2026-01-01", "2026-01-02")),
  x01 = c(TRUE, FALSE), x02 = c("u", "v"), `a,b` = 3:4, `..0` = 5:6
)
pick <- c("alpha", "gamma")
selectors <- c(
  "alpha", "alpha:gamma", "gamma:alpha", "-beta", "!beta", "c(alpha, gamma)",
  "alpha | gamma", "(alpha | beta) & (beta | gamma)", "c(alpha:gamma, -beta)",
  "c(1L, 3L)", "1:3", "-2L", "-c(2L, 3L)", "everything()", "last_col()",
  "last_col(offset = 2L)", "starts_with('x')", "ends_with('a')", "contains('a')",
  "matches('^x[0-9]+$')", "num_range('x', 1:2, width = 2L)", "all_of(pick)",
  "any_of(c(pick, 'absent'))", "one_of(pick)", "all_of(.env$pick)",
  ".env$pick", ".env[['pick']]", "c(alpha, NULL)", "c(alpha, all_of(NULL))",
  "NULL", "integer()", "character()", "where(is.character)", "where(is.numeric)",
  "where(is.factor)", "where(is.logical)", "where(function(x) inherits(x, 'Date'))",
  "c(where(is.numeric), starts_with('x'))", "!where(is.character)",
  "c(alpha = alpha)", "all_of(c(alpha = 'alpha'))", "tidyselect::peek_vars()",
  "names(tidyselect::peek_data())", "tidyselect::peek_vars()[c(1L, 3L)]",
  "base::`(`(c('alpha', 'gamma'))", "((alpha:gamma))", "`..0`", "`a,b`"
)
records <- list()
for (spelling in selectors) {
  selection <- rlang::new_quosure(parse(text = spelling)[[1L]], environment())
  expected <- names(tidyselect::eval_select(selection, data[0, ]))
  for (backend in c("tibble", "data.frame", "data.table", "dtplyr")) {
    input <- switch(backend,
      tibble = data, data.frame = as.data.frame(data),
      data.table = data.table::as.data.table(data), dtplyr = dtplyr::lazy_dt(data)
    )
    for (position in c(".by", "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
      args <- list(.data = input, .format = "list")
      literal <- args
      if (position == ".by") {
        args$.by <- selection
        literal$.by <- rlang::call2("all_of", expected)
      } else {
        args$.grouping <- rlang::eval_tidy(rlang::call2(position, selection))
        literal$.grouping <- rlang::eval_tidy(rlang::call2(position, rlang::call2("all_of", expected)))
      }
      actual <- tryCatch(rlang::eval_tidy(rlang::call2("inspect_grouping", !!!args)), error = identity)
      control <- tryCatch(rlang::eval_tidy(rlang::call2("inspect_grouping", !!!literal)), error = identity)
      if (length(expected) == 0L && position %in% c("rollup", "cube")) {
        stopifnot(inherits(actual, "marginplyr_error"), inherits(control, "marginplyr_error"),
                  grepl("requires at least one dimension", conditionMessage(actual), fixed = TRUE))
        result <- "required empty-dimension refusal"
      } else {
        if (!identical(actual, control)) {
          print(list(spelling = spelling, backend = backend, position = position,
                     expected = expected, actual = actual, control = control))
          stop("Selection differs from typed tidyselect's literalized equivalent")
        }
        result <- "exact plan match"
      }
      records[[length(records) + 1L]] <- data.frame(spelling, backend, position, result)
    }
  }
}
results <- do.call(rbind, records)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
write.csv(results, file.path(output, "selection-correspondence-results.csv"), row.names = FALSE)
print(table(results$result))
cat(nrow(results), "correspondence comparisons completed\n")
