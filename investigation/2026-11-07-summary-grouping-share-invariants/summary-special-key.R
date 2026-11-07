Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
mode <- commandArgs(trailingOnly = TRUE)
if (length(mode) == 0L) mode <- "observe"
stopifnot(mode %in% c("observe", "contract", "controls"))

fixture <- tibble::tibble(key_value = c(10L, 10L, 20L), x = c(2L, 3L, 5L))
specials <- c(".N", ".I", ".SD", ".GRP", ".NGRP")
names_to_check <- c(specials, ".BY", ".ROW", "key_value")

result_for <- function(data, name, role, read, check_lazy = FALSE) {
  key <- rlang::sym(name)
  target <- switch(read,
    pronoun = rlang::call2("[[", rlang::sym(".data"), name),
    bare = key,
    count = rlang::expr(dplyr::n())
  )
  answer <- if (identical(read, "count")) target else rlang::call2("sum", target)
  grouped <- if (identical(role, "inherited")) {
    dplyr::group_by(data, !!key)
  } else {
    data
  }
  query <- summarize_with_margins(
    grouped, answer = !!answer,
    .by = !!if (identical(role, "fixed")) key else NULL,
    .grouping = !!if (identical(role, "dimension")) rlang::call2("rollup", key) else NULL,
    .margin_label = NULL, .id = "set", .sort = "last"
  )
  if (check_lazy) stopifnot(inherits(query, "dtplyr_step"))
  dplyr::collect(query)
}

if (mode != "controls") {
  input <- tibble::tibble(.N = c(10L, 10L, 20L), x = c(2L, 3L, 5L))
  query <- summarize_with_margins(
    dtplyr::lazy_dt(input), answer = sum(.data[[".N"]]),
    .grouping = rollup(.N), .margin_label = NULL, .id = "set",
    .sort = "last"
  )
  stopifnot(inherits(query, "dtplyr_step"))
  actual <- dplyr::collect(query)
  print(actual)
  stopifnot(identical(actual$.N, c(10L, 20L, NA_integer_)))
  stopifnot(identical(actual$set, c(1L, 1L, 2L)))
  expected <- if (mode == "observe") c(2L, 1L, 3L) else c(20L, 20L, 40L)
  stopifnot(identical(actual$answer, expected))
}

# In ordinary dtplyr the public special name fails with a locked binding.
# Keeping a safe measure separate from the ordinary key reproduces the
# grouping-key copy semantics of the Margin adapter.
ordinary_input <- dtplyr::lazy_dt(tibble::tibble(.N = fixture$key_value, x = fixture$x))
ordinary_refusal <- tryCatch(dplyr::collect(dplyr::summarize(
  ordinary_input, answer = sum(.data[[".N"]])
)), error = identity)
stopifnot(inherits(ordinary_refusal, "error"))
stopifnot(grepl("locked binding", conditionMessage(ordinary_refusal), fixed = TRUE))
safe <- dplyr::mutate(dplyr::rename(ordinary_input, key_value = .N),
                      key_copy = key_value)
safe_detail <- dplyr::collect(dplyr::summarize(
  safe, answer = sum(.data[["key_value"]]), .by = key_copy
))
safe_total <- dplyr::collect(dplyr::summarize(
  safe, answer = sum(.data[["key_value"]])
))
stopifnot(identical(safe_detail$answer, c(20L, 20L)))
stopifnot(identical(safe_total$answer, 40L))

wrong <- 0L
controls <- 0L
for (name in names_to_check) {
  input <- data.table::as.data.table(fixture)
  data.table::setnames(input, "key_value", name)
  before <- data.table::copy(input)
  data <- dtplyr::lazy_dt(input, immutable = TRUE)
  for (role in c("dimension", "fixed", "inherited")) {
    for (read in c("pronoun", "bare", "count")) {
      actual <- result_for(data, name, role, read, check_lazy = TRUE)
      expected <- result_for(tibble::as_tibble(input), name, role, read)
      renamed <- result_for(dtplyr::lazy_dt(fixture), "key_value", role, read)
      names(renamed)[[1L]] <- name
      stopifnot(identical(input, before))
      stopifnot(identical(names(actual), names(expected)))
      stopifnot(identical(actual[[name]], expected[[name]]))
      stopifnot(identical(actual$set, expected$set))
      stopifnot(identical(renamed, expected))
      controls <- controls + 1L
      bad <- name %in% specials && read != "count"
      if (!bad) {
        stopifnot(identical(actual, expected))
        controls <- controls + 1L
      } else if (mode == "observe") {
        stopifnot(!identical(actual$answer, expected$answer))
        wrong <- wrong + 1L
      }
    }
  }
}
cat("Wrong collected summaries:", wrong, "\n")
cat("Passing matrix result comparisons:", controls, "\n")
