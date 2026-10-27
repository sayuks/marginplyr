# Run from the repository root. Each mode asserts the intended public behavior
# and exits nonzero while its investigated defect remains reproducible.
args <- commandArgs(trailingOnly = TRUE)
modes <- if (length(args)) args else c("raw", "selectors", "missing")
stopifnot(all(modes %in% c("raw", "selectors", "missing")))
pkgload::load_all(".", quiet = TRUE)

capture_error <- function(expr) tryCatch(expr, error = identity)
checks <- 0L
failures <- 0L
check <- function(ok) {
  checks <<- checks + 1L
  if (!isTRUE(ok)) failures <<- failures + 1L
}

if ("raw" %in% modes) {
  initial <- c(checks, failures)
  fixtures <- list(
    raw = as.raw(1L),
    matrix = matrix(as.raw(c(1L, 2L)), nrow = 1L),
    packed = tibble::tibble(byte = as.raw(1L))
  )
  verbs <- c(
    "nest_with_margins", "nest_by_with_margins",
    "expand_with_margins", "summarize_with_margins"
  )
  for (kind in names(fixtures)) {
    input <- tibble::tibble(g = fixtures[[kind]], value = 1L)
    for (label in list(NULL, NA_character_)) {
      for (collision_check in c(FALSE, TRUE)) {
        for (verb in verbs) {
          result <- capture_error(do.call(verb, list(
            .data = input, .grouping = rollup(g),
            .margin_label = label, .check_margin_label = collision_check,
            .id = "set"
          )))
          # Refusing an unsupported typed-missing request is acceptable; silently
          # substituting an ordinary value is the symptom this loop catches.
          ok <- if (inherits(result, "error")) {
            inherits(result, "marginplyr_error")
          } else {
            total <- vctrs::vec_slice(result$g, which(result$set == 2L))
            length(vctrs::vec_detect_missing(total)) > 0L &&
              all(vctrs::vec_detect_missing(total))
          }
          check(ok)
        }
      }
    }
  }
  # A raw fixed key and an included-only dimension need no missing value.
  input <- tibble::tibble(g = as.raw(c(0L, 1L)), value = 1:2)
  for (spec in list(NULL, grouping_set(g))) {
    result <- if (is.null(spec)) {
      nest_with_margins(input, .by = g, .margin_label = NULL)
    } else {
      nest_with_margins(input, .grouping = spec, .margin_label = NULL)
    }
    check(identical(result$g, input$g))
  }
  cat("raw: checks", checks - initial[[1L]],
      "failures", failures - initial[[2L]], "\n")
}

# A real disposable namespace is needed: attaching an environment cannot
# reproduce the distinction between a qualified export and a local alias.
install_selector_fixture <- function() {
  scratch <- tempfile("marginplyr-selector-probe-")
  dir.create(scratch)
  package <- file.path(scratch, "specselectors")
  library <- file.path(scratch, "library")
  dir.create(file.path(package, "R"), recursive = TRUE)
  dir.create(library)
  writeLines(c(
    "Package: specselectors", "Version: 0.0.1",
    "Title: Disposable Selection Probe",
    "Description: A local namespace fixture for a dated investigation.",
    "Author: Probe", "Maintainer: Probe <probe@example.org>",
    "License: MIT", "Imports: tidyselect"
  ), file.path(package, "DESCRIPTION"))
  helpers <- c(
    "all_of", "any_of", "starts_with", "ends_with", "contains",
    "matches", "num_range", "everything", "last_col", "one_of"
  )
  writeLines(paste0("export(", c("choose_character", helpers), ")"),
             file.path(package, "NAMESPACE"))
  writeLines(c(
    "choose_character <- function() {",
    "  data <- tidyselect::peek_data()",
    "  names(data)[vapply(data, is.character, logical(1))]",
    "}",
    paste(helpers, "<- choose_character")
  ), file.path(package, "R", "selectors.R"))
  log <- file.path(scratch, "install.log")
  status <- system2(file.path(R.home("bin"), "R"), c(
    "CMD", "INSTALL", "--no-byte-compile",
    paste0("--library=", shQuote(library)), shQuote(package)
  ), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log), collapse = "\n"))
  .libPaths(c(library, .libPaths()))
  list(scratch = scratch, helpers = helpers)
}

if ("selectors" %in% modes) {
  initial <- c(checks, failures)
  fixture <- install_selector_fixture()
  input <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  alias <- specselectors::starts_with
  check(identical(alias, specselectors::choose_character))
  check(identical(names(dplyr::select(input, alias())), "a"))
  expected <- inspect_grouping(input, .by = a, .format = "list")
  expected_rollup <- inspect_grouping(
    input, .grouping = rollup(a), .format = "list"
  )
  for (helper in fixture$helpers) {
    selection <- rlang::call2(helper, .ns = "specselectors")
    actual <- capture_error(inspect_grouping(
      input, .by = !!selection, .format = "list"
    ))
    check(identical(actual, expected))
    actual <- capture_error(inspect_grouping(
      input, .grouping = rollup(!!selection), .format = "list"
    ))
    check(identical(actual, expected_rollup))
    for (verb in c("nest_with_margins", "nest_by_with_margins")) {
      actual <- capture_error(rlang::eval_tidy(rlang::call2(
        verb, input, .by = selection
      )))
      check(!inherits(actual, "error") &&
              nrow(actual) == 2L && identical(actual$a, input$a) &&
              identical(vapply(actual$data, nrow, integer(1)), c(1L, 1L)))
    }
  }
  for (selection in list(
    quote(alias()), quote(specselectors::choose_character()),
    quote(tidyselect::starts_with("a"))
  )) {
    check(identical(inspect_grouping(
      input, .by = !!selection, .format = "list"
    ), expected))
    check(identical(inspect_grouping(
      input, .grouping = rollup(!!selection), .format = "list"
    ), expected_rollup))
  }
  unloadNamespace("specselectors")
  unlink(fixture$scratch, recursive = TRUE)
  cat("selectors: checks", checks - initial[[1L]],
      "failures", failures - initial[[2L]], "\n")
}

if ("missing" %in% modes) {
  initial <- c(checks, failures)
  input <- data.frame(a = 1)
  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  verbs <- c(
    "inspect_grouping", "summarize_with_margins", "expand_with_margins",
    "nest_with_margins", "nest_by_with_margins"
  )
  for (constructor in constructors) {
    wrapper <- rlang::new_function(
      rlang::pairlist2(x = ),
      rlang::call2(constructor, quote(({{ x }})), quote(a)),
      globalenv()
    )
    for (verb in verbs) {
      result <- capture_error(rlang::eval_tidy(rlang::call2(
        verb, input, .grouping = quote(wrapper())
      )))
      check(inherits(result, "marginplyr_error") &&
              grepl("Argument 1", conditionMessage(result), fixed = TRUE) &&
              grepl(constructor, conditionMessage(result), fixed = TRUE))
    }
  }
  # Public arguments have an absent/default meaning rather than a constructor
  # position's empty-argument refusal.
  for (verb in verbs) {
    expected <- rlang::eval_tidy(rlang::call2(verb, input))
    for (position in c(".by", ".grouping")) {
      for (parenthesized in c(FALSE, TRUE)) {
        forwarded <- quote({{ x }})
        if (parenthesized) forwarded <- rlang::call2("(", forwarded)
        wrapper <- rlang::new_function(
          rlang::pairlist2(x = ),
          rlang::call2(
            verb, input,
            !!!stats::setNames(list(forwarded), position)
          ),
          globalenv()
        )
        result <- capture_error(wrapper())
        check(identical(result, expected))
      }
    }
  }
  control <- function(x) grouping_sets({{ x }}, a)
  result <- capture_error(inspect_grouping(input, .grouping = control()))
  check(inherits(result, "marginplyr_error"))
  cat("missing: checks", checks - initial[[1L]],
      "failures", failures - initial[[2L]], "\n")
}
cat("TOTAL: checks", checks, "failures", failures, "\n")
if (failures > 0L) quit(status = 1L)
