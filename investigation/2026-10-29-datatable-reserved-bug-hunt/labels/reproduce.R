# --assert-correct tests the public repair contract and is expected to fail
# on the investigated snapshot. The default asserts the dated observation.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
assert_correct <- "--assert-correct" %in% commandArgs(TRUE)
expected_error <- function(code) tryCatch({force(code); NULL}, error = identity)
verbs <- c("summarize_with_margins", "expand_with_margins", "nest_with_margins", "nest_by_with_margins")
for (collision in c(FALSE, TRUE)) {
  input <- tibble::tibble(.SD = if (collision) "Total" else "a", value = 2L)
  table <- data.table::as.data.table(list(safe = input$.SD, value = input$value))
  data.table::setnames(table, names(input))
  before <- data.table::copy(table)
  source <- dtplyr::lazy_dt(table, immutable = TRUE)
  stopifnot(identical(collect(source), input))
  renamed <- rename(source, safe = all_of(".SD"))
  # Same expression and input values, changing only the generated output name.
  good_count <- collect(summarise(renamed, count = sum(safe == "Total")))
  stopifnot(identical(good_count$count, as.integer(collision)))
  bad_count <- expected_error(collect(summarise(renamed, .SD = sum(safe == "Total"))))
  stopifnot(inherits(bad_count, "error"), grepl("A column may not be called .SD", conditionMessage(bad_count), fixed = TRUE))
  for (verb in verbs) {
    run <- function(data, name = ".SD", check = TRUE) {
      grouping <- rollup(all_of(name))
      fun <- get(verb)
      result <- if (identical(verb, "summarize_with_margins")) {
        fun(data, total = sum(value), .grouping = grouping, .check_margin_label = check)
      } else fun(data, .grouping = grouping, .check_margin_label = check)
      if (inherits(result, "dtplyr_step")) collect(result) else result
    }
    local <- expected_error(run(input))
    safe <- expected_error(run(renamed, "safe"))
    actual <- expected_error(run(source))
    unchecked <- expected_error(run(source, check = FALSE))
    stopifnot(is.null(unchecked), identical(table, before))
    if (collision) {
      stopifnot(inherits(local, "marginplyr_error"), inherits(safe, "marginplyr_error"))
    } else stopifnot(is.null(local), is.null(safe))
    cat(verb, "collision=", collision, "actual=", if (is.null(actual)) "success" else conditionMessage(actual), "\n")
    if (assert_correct) {
      if (collision) stopifnot(inherits(actual, "marginplyr_error")) else stopifnot(is.null(actual))
    } else {
      stopifnot(inherits(actual, "error"), !inherits(actual, "marginplyr_error"),
                grepl("A column may not be called .SD", conditionMessage(actual), fixed = TRUE))
    }
  }
}
cat("Collision-count diagnosis confirmed; source unchanged.\n")
