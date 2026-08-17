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

# The arguments every case supplies whatever it is testing, so that a verb's
# own requirements are stated once. A summary is added only where the verb
# demands one.
option_call_args <- function(verb, grouping = quote(rollup(g))) {
  args <- list(quote(admission_data()), .grouping = grouping)
  if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
    args <- c(args, list(s = quote(sum(v))))
  }
  args
}

call_with_option <- function(verb, option, value) {
  args <- option_call_args(verb)
  # Single-bracket assignment from a list, because `args[[option]] <- NULL`
  # removes the element instead of writing one: the call would then omit the
  # option and exercise its default, which is the very thing the `NULL` case
  # below asserts is not what happens.
  args[option] <- list(value)
  eval(rlang::call2(verb, !!!args))
}

# The same call with the option never written, which is what a forwarded
# vocabulary has to agree with.
call_without_option <- function(verb) {
  eval(rlang::call2(verb, !!!option_call_args(verb)))
}

option_case_label <- function(case, value) {
  written <- if (length(value) == 1L) {
    paste0("\"", value, "\"")
  } else {
    paste0("c(", paste0("\"", value, "\"", collapse = ", "), ")")
  }
  paste0(case$verb, "(", case$option, " = ", written, ")")
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

test_that("the case table reaches every option on every verb that takes it", {
  # Every test below iterates that table and concludes something from what it
  # found, so a table that arrived empty -- or one an option quietly dropped
  # out of -- is a table that passes all of them. What this catches is an axis
  # being dropped, and not an axis never added: each entry's vocabulary is read
  # from the shared constants, while the verbs and the options are written out
  # in `option_vocabulary_cases()` and written out again here.
  cases <- option_vocabulary_cases()

  expect_gt(length(cases), 0L)
  expect_setequal(
    unique(vapply(cases, function(case) case$option, character(1L))),
    c(".duplicates", ".margin_label_position", ".sort", ".format")
  )
  expect_setequal(
    unique(vapply(cases, function(case) case$verb, character(1L))),
    c(
      "summarize_with_margins",
      "summarise_with_margins",
      "expand_with_margins",
      "nest_with_margins",
      "nest_by_with_margins",
      "inspect_grouping"
    )
  )
  # A one-value vocabulary would make the reordering case below identical to
  # the forwarded one, so it would assert acceptance and refusal at once.
  for (case in cases) {
    expect_true(
      length(case$values) > 1L,
      info = paste(case$verb, case$option)
    )
  }
})

test_that("a forwarded vocabulary resolves to the first value", {
  # The public formals spell their vocabularies out, so an argument nobody
  # wrote arrives as the whole vector and has to stand for its first entry.
  # `match_margin_choice()` cannot tell that from a caller who typed the same
  # vector, and the two are deliberately not told apart (#210): a function of
  # the caller's own that repeats the signature forwards exactly this vector,
  # and refusing it would break that wrapper while breaking nothing else.
  #
  # Asserted where the resolution happens, because a Margin result does not
  # show it. On any one input most values of `.duplicates` and
  # `.margin_label_position` produce the same result as each other, so a verb
  # resolving a forwarded vocabulary to the wrong member would return the right
  # answer anyway; the test below pairs this with what the verbs do.
  for (case in option_vocabulary_cases()) {
    expect_identical(
      match_margin_choice(case$values, case$values, case$option),
      case$values[[1L]],
      info = option_case_label(case, case$values)
    )
  }
})

test_that("every verb accepts the vocabulary its own signature spells out", {
  # The other half: that each verb hands its formal down far enough to be
  # resolved, and answers as though the argument had never been written.
  # Equality with the option left out is all this can assert -- see above --
  # and it is what fails if a verb starts refusing the vector its own default
  # supplies.
  for (case in option_vocabulary_cases()) {
    expect_identical(
      call_with_option(case$verb, case$option, case$values),
      call_without_option(case$verb),
      info = option_case_label(case, case$values)
    )
  }
})

test_that("a reordering of the vocabulary is refused", {
  # The vocabulary is read as the default because it is the spelling the
  # signature gives, not because its members are the permitted values in some
  # order. `rlang::arg_match()` and `rlang::arg_match0()` both accept any
  # permutation and are looser here, which is recorded in
  # investigation/rlang-arg-match-for-option-arguments.md.
  for (case in option_vocabulary_cases()) {
    reordered <- rev(case$values)
    expect_identical(
      option_rejection_message(case$verb, case$option, reordered),
      expected_vocabulary_message(case),
      info = option_case_label(case, reordered)
    )
  }
})

test_that("`NULL` is rejected by every option rather than taken as a default", {
  # `match.arg(NULL, choices)` returns `choices[1]`, so every option argument
  # used to read a `NULL` as a request for its own default. #110 stopped that
  # along with the abbreviations above, and #144 settled it as a decision: the
  # *Option arguments* section on `?summarize_with_margins` says which
  # arguments do give a `NULL` a meaning and why an option vocabulary is not
  # among them. The untouched formal is what selects a default, and it arrives
  # as the whole vocabulary rather than as a `NULL`.
  for (case in option_vocabulary_cases()) {
    expect_identical(
      option_rejection_message(case$verb, case$option, NULL),
      expected_vocabulary_message(case),
      info = paste0(case$verb, "(", case$option, " = NULL)")
    )
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
    args <- option_call_args(verb, grouping = duplicated_sets)
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
