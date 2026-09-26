# Intended-result probes at af3ce0743c8b2e974b622126b428fa019fb53779.
# Run from the repository root; an optional argument selects one probe.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

probe_unpack_true <- function() {
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  factory <- function(offset) {
    force(offset)
    function(x) data.frame(value = sum(x) + offset)
  }
  expected <- dplyr::summarise(
    dplyr::group_by(data, g),
    dplyr::across(x, factory(dplyr::n()), .unpack = identity(TRUE)),
    .groups = "drop"
  )
  literal <- summarize_with_margins(
    data, dplyr::across(x, factory(dplyr::n()), .unpack = TRUE),
    .grouping = grouping_set(g)
  )
  stopifnot(identical(expected, literal),
            identical(expected$x_value, c(2, 32)))
  actual <- summarize_with_margins(
    data, dplyr::across(x, factory(dplyr::n()), .unpack = identity(TRUE)),
    .grouping = grouping_set(g)
  )
  print(actual)
  stopifnot(identical(actual, expected))
}

probe_unpack_false <- function() {
  data <- tibble::tibble(x = 1)
  factory <- function(offset) {
    force(offset)
    function(x) sum(x) + offset
  }
  expected <- dplyr::summarise(
    data, dplyr::across(x, factory(dplyr::n()), .unpack = identity(FALSE))
  )
  literal <- summarize_with_margins(
    data, dplyr::across(x, factory(dplyr::n()), .unpack = FALSE)
  )
  stopifnot(identical(expected, literal), identical(expected$x, 1))
  actual <- summarize_with_margins(
    data, dplyr::across(x, factory(dplyr::n()), .unpack = identity(FALSE))
  )
  print(actual)
  stopifnot(identical(actual, expected))
}

probe_first_packed <- function() {
  data <- tibble::tibble(x = 1)
  expected <- dplyr::summarise(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    total = sum(x)
  )
  control <- summarize_with_margins(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    total = sum(x)
  )
  stopifnot(identical(control, expected), identical(control$packed$x, 1))
  actual <- summarize_with_margins(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    total = sum(x), p = share_of_total(total)
  )
  stopifnot(identical(actual$packed, expected$packed), identical(actual$p, 1))
}

probe_pronoun_data <- function() {
  data <- tibble::tibble(.data = "a", value = 1L)
  expected <- dplyr::summarise(data, total = sum(value),
                              .by = dplyr::all_of(".data"))
  missing <- summarize_with_margins(
    data, total = sum(value),
    .grouping = grouping_set(dplyr::all_of(".data")), .margin_label = NULL
  )
  stopifnot(identical(expected, missing), identical(expected[[".data"]], "a"))
  actual <- summarize_with_margins(
    data, total = sum(value),
    .grouping = grouping_set(dplyr::all_of(".data"))
  )
  print(actual)
  stopifnot(identical(actual, expected))
}

probe_pronoun_env <- function() {
  data <- tibble::tibble(.env = "a", value = 1L)
  expected <- dplyr::summarise(data, total = sum(value),
                              .by = dplyr::all_of(".env"))
  missing <- summarize_with_margins(
    data, total = sum(value),
    .grouping = grouping_set(dplyr::all_of(".env")), .margin_label = NULL
  )
  stopifnot(identical(expected, missing), identical(expected[[".env"]], "a"))
  actual <- summarize_with_margins(
    data, total = sum(value),
    .grouping = grouping_set(dplyr::all_of(".env"))
  )
  stopifnot(identical(actual, expected))
}

probe_dtplyr_share <- function() {
  data <- tibble::tibble(.N = "a", value = 2)
  source <- dtplyr::lazy_dt(data)
  ordinary <- dplyr::collect(dplyr::summarise(
    dplyr::group_by(source, .N), total = sum(value)
  ))
  stopifnot(identical(ordinary, tibble::tibble(.N = "a", total = 2)))
  control <- dplyr::collect(summarize_with_margins(
    source, total = sum(value), .grouping = rollup(.N)
  ))
  expected <- summarize_with_margins(
    data, total = sum(value), p = share_of_total(total),
    .grouping = rollup(.N)
  )
  stopifnot(identical(control$total, c(2, 2)), identical(expected$p, c(1, 1)))
  actual <- dplyr::collect(summarize_with_margins(
    source, total = sum(value), p = share_of_total(total),
    .grouping = rollup(.N)
  ))
  stopifnot(identical(actual, expected))
}

probe_dtplyr_projection <- function() {
  data <- tibble::tibble(.N = 1:2, g = c("a", "b"))
  source <- dtplyr::lazy_dt(data)
  expected <- expand_with_margins(
    data, .grouping = rollup(g), .margin_label = NULL
  )
  ordinary <- dplyr::collect(dplyr::union_all(
    source, dplyr::mutate(source, g = NA_character_)
  ))
  ordinary <- ordinary[names(expected)]
  stopifnot(identical(ordinary, expected))
  actual <- dplyr::collect(expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL
  ))
  stopifnot(identical(actual, expected))
}

probes <- list(
  unpack_true = probe_unpack_true,
  unpack_false = probe_unpack_false,
  first_packed = probe_first_packed,
  pronoun_data = probe_pronoun_data,
  pronoun_env = probe_pronoun_env,
  dtplyr_share = probe_dtplyr_share,
  dtplyr_projection = probe_dtplyr_projection
)
selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) > 0L) {
  stopifnot(all(selected %in% names(probes)))
  probes <- probes[selected]
}
passed <- vapply(names(probes), function(name) {
  cat("\n[", name, "]\n", sep = "")
  tryCatch({
    probes[[name]]()
    cat("PASS\n")
    TRUE
  }, error = function(error) {
    cat("FAIL: ", conditionMessage(error), "\n", sep = "")
    FALSE
  })
}, logical(1))
cat("\n", sum(passed), "/", length(passed),
    " intended-result probes passed.\n", sep = "")
quit(status = if (all(passed)) 0L else 1L)
