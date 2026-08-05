# Two admission rules that keep a mistake from becoming a plausible result.
#
# `...` accepts any named expression, so an argument name the verb does not
# have becomes a constant summary column instead of an error; and an input
# without dplyr methods reached `group_vars()` and reported an internal
# generic rather than the argument the caller supplied.

admission_data <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
}

test_that("a removed option is reported instead of summarized", {
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .sort = TRUE
    ),
    "no `.sort` argument",
    class = "marginplyr_error"
  )
})

test_that("a near miss on a removed option names what the caller wrote", {
  # The caller never wrote `.sort`, so an error naming only the option they
  # were reaching for sends them looking for a word that is not in their code.
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .sorts = TRUE
    ),
    "`.sorts` is not an argument.+neither is the `.sort` it resembles",
    class = "marginplyr_error"
  )
})

test_that("every removed option answers its near misses the same way", {
  # `.groups` reached the table by way of a bespoke check that matched the name
  # exactly, so its misspellings used to fall through to the generic "captured
  # as a summary" message. The guidance is a property of the option, not of how
  # the caller spelled it.
  guidance <- "Margin-summary results are always ungrouped\\."

  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .groups = "drop"
    ),
    paste0(
      "`summarize_with_margins\\(\\)` has no `\\.groups` argument; ", guidance
    ),
    class = "marginplyr_error"
  )

  for (misspelling in c(".group", ".groupss")) {
    spliced <- stats::setNames(list("drop"), misspelling)
    expect_error(
      summarize_with_margins(
        admission_data(),
        s = sum(v),
        .grouping = rollup(g),
        !!!spliced
      ),
      paste0(
        "`\\", misspelling, "` is not an argument.+neither is the `\\.groups` ",
        "it resembles; ", guidance
      ),
      class = "marginplyr_error"
    )
  }
})

test_that("the synonym answers removed options identically", {
  # `summarise_with_margins()` is the same object, but the option names are read
  # from formals and the messages name one spelling, so the synonym is where a
  # divergence would show first. Comparing the messages asserts that directly;
  # a pattern per spelling would pass while the two drifted apart.
  removed_option_message <- function(verb, option) {
    spliced <- stats::setNames(list(TRUE), option)
    condition <- rlang::catch_cnd(
      verb(admission_data(), s = sum(v), .grouping = rollup(g), !!!spliced),
      classes = "marginplyr_error"
    )
    conditionMessage(condition)
  }

  for (option in c(".groups", ".group", ".sort", ".sorts")) {
    expect_identical(
      removed_option_message(summarise_with_margins, option),
      removed_option_message(summarize_with_margins, option)
    )
  }
})

test_that("the first option-shaped name written is the one reported", {
  # `.groups` had its own check ahead of this loop, so it won wherever it
  # appeared in the call. It has no such standing now, and both orders are
  # asserted because either one alone would also pass under a rule that ranked
  # removed options above near misses.
  reported <- function(...) {
    spliced <- list(...)
    conditionMessage(rlang::catch_cnd(
      summarize_with_margins(
        admission_data(),
        s = sum(v),
        .grouping = rollup(g),
        !!!spliced
      ),
      classes = "marginplyr_error"
    ))
  }

  expect_match(
    reported(.margin_labels = "ALL", .groups = "drop"),
    "Did you mean `\\.margin_label`"
  )
  expect_match(
    reported(.groups = "drop", .margin_labels = "ALL"),
    "has no `\\.groups` argument"
  )
})

test_that("a misspelled option names the argument it resembles", {
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .margin_labels = "ALL"
    ),
    "Did you mean `.margin_label`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .groupings = rollup(g)
    ),
    "Did you mean `.grouping`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .duplicate = "drop"
    ),
    "Did you mean `.duplicates`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .ids = "set"
    ),
    "Did you mean `.id`"
  )
})

test_that("the check is scoped to names that resemble an option", {
  # A leading dot is ordinary in an output name, and only an exact match or a
  # one-character difference is treated as a mistake.
  expect_named(
    summarize_with_margins(
      admission_data(),
      .n = dplyr::n(),
      .grouping = rollup(g)
    ),
    c("g", ".n")
  )
  expect_named(
    summarize_with_margins(
      admission_data(),
      .total_by_region = sum(v),
      .grouping = rollup(g)
    ),
    c("g", ".total_by_region")
  )
  expect_named(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .id = ".set"
    ),
    c("g", ".set", "s")
  )
  # `.data` and `...` are formals but not options a caller can misspell into
  # `...`, so they are kept out of the comparison. Counting `.data` made
  # `.date` — an ordinary output name — a near miss.
  expect_named(
    summarize_with_margins(
      admission_data(),
      .date = max(v),
      .grouping = rollup(g)
    ),
    c("g", ".date")
  )
})

test_that("the option check survives splicing", {
  spliced <- list(.sort = TRUE)

  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      !!!spliced
    ),
    class = "marginplyr_error"
  )
})

test_that("input that dplyr cannot group is rejected in the caller's terms", {
  for (input in list(as.matrix(admission_data()), as.list(admission_data()))) {
    expect_error(
      summarize_with_margins(input, s = sum(v), .grouping = rollup(g)),
      "must be a data frame or a lazy table",
      class = "marginplyr_error"
    )
  }

  expect_error(
    summarize_with_margins(NULL, s = sum(v), .grouping = rollup(g)),
    "`NULL` was supplied"
  )
})

test_that("every entry point admits input the same way", {
  input <- as.matrix(admission_data())
  # The same message, not merely the same class: the nesting verbs reject a
  # matrix on their own narrower whitelist too, which would satisfy a
  # class-only assertion while answering the caller with the classes that nest.
  admission_message <- "must be a data frame or a lazy table"

  expect_error(
    expand_with_margins(input, .grouping = rollup(g)),
    admission_message,
    class = "marginplyr_error"
  )
  expect_error(
    nest_with_margins(input, .grouping = rollup(g)),
    admission_message,
    class = "marginplyr_error"
  )
  expect_error(
    nest_by_with_margins(input, .grouping = rollup(g)),
    admission_message,
    class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(input, .grouping = rollup(g)),
    admission_message,
    class = "marginplyr_error"
  )
})

test_that("admission does not widen what the nesting verbs accept", {
  skip_if_backend_absent("arrow")

  # Admitted by the shared rule, still refused by nesting's own constraint.
  expect_error(
    nest_with_margins(
      arrow::as_arrow_table(admission_data()),
      .grouping = rollup(g)
    ),
    "which can be nested",
    class = "marginplyr_error"
  )
})

test_that("supported backends are still admitted", {
  expect_no_error(
    summarize_with_margins(
      tibble::as_tibble(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    )
  )
  expect_no_error(
    summarize_with_margins(
      dplyr::group_by(admission_data(), g),
      s = sum(v)
    )
  )

  skip_if_backend_absent("dtplyr")
  expect_no_error(
    dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})

test_that("arrow input is still admitted", {
  skip_if_backend_absent("arrow")

  expect_no_error(
    dplyr::collect(summarize_with_margins(
      arrow::as_arrow_table(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})
