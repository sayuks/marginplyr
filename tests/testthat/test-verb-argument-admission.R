# Three admission rules that keep a mistake from becoming a plausible result.
#
# `...` accepts any named expression, so an argument name the verb does not
# have becomes a constant summary column instead of an error; an input
# without dplyr methods reached `group_vars()` and reported an internal
# generic rather than the argument the caller supplied; and an option argument
# validated with `match.arg()` accepted abbreviations nothing documents.

admission_data <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
}

# Two tests stood here, `a removed option is reported instead of summarized`
# and `a near miss on a removed option names what the caller wrote`. Both were
# written against `.sort`, which ADR 0018 returned as a live argument: a name
# the verb has matches its own formal and never reaches `...`, so neither test
# entered the branch it existed for any more, while both still passed.
#
# They were not repointed at `.groups`, because the test below already makes
# both assertions about the one removed option left, in the same order and more
# tightly. Repointing would have duplicated it rather than covering anything.
test_that("every removed option answers its near misses the same way", {
  # `.groups` reached the table by way of a bespoke check that matched the name
  # exactly, so its misspellings used to fall through to the generic "captured
  # as a summary" message. The guidance is a property of the option, not of how
  # the caller spelled it. Both messages also name what the caller wrote: a
  # caller who wrote `.groupss` never wrote `.groups`, and an error naming only
  # the option they were reaching for sends them looking for a word that is not
  # in their code.
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

  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .groupss = "drop"
    ),
    paste0(
      "`\\.groupss` is not an argument.+neither is the `\\.groups` it ",
      "resembles; ", guidance
    ),
    class = "marginplyr_error"
  )
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

  for (option in c(".groups", ".groupss")) {
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

test_that("a name callers write on purpose is left alone", {
  # `.group` is one character from the removed `.groups`, so the net would read
  # it as a misspelling. It is a column callers produce deliberately, and it is
  # exempt for that reason — not because of anything about the distance, which
  # is the same one-character deletion that makes `.duplicate` worth catching.
  expect_named(
    summarize_with_margins(
      admission_data(),
      .group = max(v),
      .grouping = rollup(g)
    ),
    c("g", ".group")
  )
  # The exemption is the name, not a prefix of it: the option itself and its
  # other near misses are still answered.
  for (misspelling in c(".groups", ".groupss")) {
    spliced <- stats::setNames(list("drop"), misspelling)
    expect_error(
      summarize_with_margins(
        admission_data(),
        s = sum(v),
        .grouping = rollup(g),
        !!!spliced
      ),
      "Margin-summary results are always ungrouped",
      class = "marginplyr_error"
    )
  }
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
  spliced <- list(.groups = "drop")

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

# Every fixed-vocabulary option, paired with the verb that takes it and the
# values that verb accepts. The vocabularies are read from the shared constants
# rather than written out again here, so a vocabulary that grows is covered
# without an edit; test-grouping-interface.R holds each verb's formal to the
# same constants, which is what makes reading them here an assertion about the
# documented signature rather than a tautology.
option_vocabulary_cases <- function() {
  margin_verbs <- c(
    "summarize_with_margins",
    "summarise_with_margins",
    "expand_with_margins",
    "nest_with_margins",
    "nest_by_with_margins"
  )
  nesting_verbs <- c("nest_with_margins", "nest_by_with_margins")

  cases <- list()
  for (verb in margin_verbs) {
    duplicates <- if (verb %in% nesting_verbs) {
      nest_duplicates_choices
    } else {
      margin_duplicates_choices
    }
    cases <- c(cases, list(
      list(verb = verb, option = ".duplicates", values = duplicates),
      list(
        verb = verb,
        option = ".margin_label_position",
        values = margin_label_position_choices
      ),
      list(verb = verb, option = ".sort", values = margin_sort_choices)
    ))
  }
  # `inspect_grouping()` reads a plan rather than executing one, so it takes
  # `.duplicates` and its own `.format` but no Margin presentation options.
  c(cases, list(
    list(
      verb = "inspect_grouping",
      option = ".duplicates",
      values = margin_duplicates_choices
    ),
    list(
      verb = "inspect_grouping",
      option = ".format",
      values = grouping_format_choices
    )
  ))
}

call_with_option <- function(verb, option, value) {
  args <- list(quote(admission_data()), .grouping = quote(rollup(g)))
  if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
    args <- c(args, list(s = quote(sum(v))))
  }
  args[[option]] <- value
  eval(rlang::call2(verb, !!!args))
}

option_case_label <- function(case, value) {
  paste0(case$verb, "(", case$option, " = \"", value, "\")")
}

# The one message a rejected value may produce, built from the vocabulary the
# case says the verb accepts. Asserting the whole sentence rather than a
# pattern is what holds a verb to its own vocabulary: a message enumerating one
# value more or fewer is a different string.
expected_vocabulary_message <- function(case) {
  paste0(
    "`", case$option, "` must be one of ",
    paste0("\"", case$values, "\"", collapse = ", "),
    "."
  )
}

option_rejection_message <- function(verb, option, value) {
  condition <- rlang::catch_cnd(
    call_with_option(verb, option, value),
    classes = "marginplyr_error"
  )
  if (is.null(condition)) {
    return(NA_character_)
  }
  conditionMessage(condition)
}

test_that("every documented option value is accepted", {
  # The other half of the vocabulary contract, and the half that makes the
  # rejection messages below assertions rather than assumptions: no verb may
  # refuse a value its own rejection message offers.
  refused <- character()

  for (case in option_vocabulary_cases()) {
    for (value in case$values) {
      accepted <- tryCatch(
        {
          call_with_option(case$verb, case$option, value)
          TRUE
        },
        marginplyr_error = function(cnd) FALSE
      )
      if (!accepted) {
        refused <- c(refused, option_case_label(case, value))
      }
    }
  }

  expect_identical(refused, character())
})

test_that("an abbreviation of an option value is rejected", {
  # `match.arg()` resolved any unambiguous prefix to the value it abbreviates,
  # so `.sort = "f"` and `.duplicates = "k"` were accepted (#110). Every
  # vocabulary here is distinct in its first character, so the one-character
  # prefix is exactly the abbreviation `match.arg()` would have taken.
  for (case in option_vocabulary_cases()) {
    for (value in case$values) {
      abbreviation <- substr(value, 1L, 1L)
      expect_identical(
        option_rejection_message(case$verb, case$option, abbreviation),
        expected_vocabulary_message(case),
        info = option_case_label(case, abbreviation)
      )
    }
  }
})

test_that("a wholly invalid option value names only what the verb accepts", {
  for (case in option_vocabulary_cases()) {
    expect_identical(
      option_rejection_message(case$verb, case$option, "zzz"),
      expected_vocabulary_message(case),
      info = option_case_label(case, "zzz")
    )
  }
})

test_that("a diagnostic offering a `.duplicates` policy offers a real one", {
  # The rejection message above is not the only place a vocabulary is spoken
  # aloud: refusing a duplicate grouping set names the policies that would have
  # accepted it. That message enumerated `"drop"` and `"keep"` from a constant,
  # so the nesting verbs told a caller to use a value they then refuse — the
  # same defect as #110, one message further in.
  duplicated_sets <- grouping_sets(grouping_set(g), grouping_set(g))

  refusal <- function(verb) {
    fn <- get(verb, envir = asNamespace("marginplyr"))
    args <- list(admission_data(), .grouping = duplicated_sets)
    if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
      args <- c(args, list(s = quote(sum(v))))
    }
    conditionMessage(rlang::catch_cnd(
      eval(rlang::call2(verb, !!!args)),
      classes = "marginplyr_error"
    ))
  }

  for (case in option_vocabulary_cases()) {
    if (!identical(case$option, ".duplicates")) {
      next
    }
    message <- refusal(case$verb)
    expect_match(
      message,
      "^Duplicate grouping sets were produced",
      info = case$verb
    )
    for (value in setdiff(margin_duplicates_choices, case$values)) {
      expect_false(
        grepl(paste0("\"", value, "\""), message, fixed = TRUE),
        info = paste(case$verb, value)
      )
    }
    for (value in setdiff(case$values, "error")) {
      expect_true(
        grepl(paste0("\"", value, "\""), message, fixed = TRUE),
        info = paste(case$verb, value)
      )
    }
  }
})

test_that("the nesting verbs answer `.duplicates = \"keep\"` in their terms", {
  # The narrower formal used to be widened before validation and taken away
  # again by a second guard, so the rejection message named `"keep"` and the
  # verb then refused it (#110). One vocabulary owns the answer now, and the
  # message the caller sees is the one every other invalid value gets.
  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    expect_identical(
      option_rejection_message(verb, ".duplicates", "keep"),
      "`.duplicates` must be one of \"error\", \"drop\".",
      info = verb
    )
  }
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
  skip_if_suggest_absent("arrow")

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

  skip_if_suggest_absent("dtplyr")
  expect_no_error(
    dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})

test_that("arrow input is still admitted", {
  skip_if_suggest_absent("arrow")

  expect_no_error(
    dplyr::collect(summarize_with_margins(
      arrow::as_arrow_table(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})
