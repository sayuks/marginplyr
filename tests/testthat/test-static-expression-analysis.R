# The analysis that reads summary expressions before execution used to assume
# shapes real expressions do not always have, and raised the base-R condition
# that assumption produced: an `NA` `if()` condition, an out-of-bounds `[[`,
# and a `vapply()` type error (#100). None of the three is catchable by class
# or tells the caller what to change, so each is asserted here by class as
# well as by message.
#
# Which outcome is right differs per site, and that is the point ADR-0015
# draws: a shape the analysis simply does not recognize must fall through and
# evaluate, an error raised by the caller's own code must reach them with its
# own class intact, and only a fault the analysis itself detects becomes a
# Package condition.

test_that("a call whose head is a call is evaluated, not classified", {
  # `call_name()` answers `NULL` here, so the `$`/`[[` test used to compare
  # `NULL` against a character vector, yielding `NA` inside `if()`. Such a call
  # is not a data-mask reference, so the analysis has nothing to say about it
  # and dplyr must see it unchanged.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  fns <- list(total = function(x, na_rm) sum(x, na.rm = na_rm))

  from_list <- summarize_with_margins(
    data,
    value = fns$total(value, TRUE),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  from_lambda <- summarize_with_margins(
    data,
    value = (function(x) sum(x))(value),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expected <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(value = fns$total(value, TRUE), .groups = "drop")

  expect_equal(
    dplyr::arrange(from_list[!is.na(from_list$region), ], region),
    as.data.frame(expected),
    ignore_attr = "row.names"
  )
  expect_equal(from_lambda, from_list)
})

test_that("falling through a call head still sees its arguments", {
  # Falling through must reach the arguments rather than abandon the
  # expression: the dependency check that forbids reading an earlier share
  # from an ordinary summary is the same walk, so a blind fall-through would
  # let this call through and produce a wrong number instead of an error.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  fns <- list(double = function(x) x * 2)

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_total(total),
      derived = fns$double(share),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("every call head that is not a bare symbol is walked", {
  # A bare symbol in the function position is the one head that cannot read a
  # column: R resolves it through function lookup, which skips non-function
  # bindings, so a share named `sum` does not shadow `sum(x)`. Every other head
  # is evaluated in the data mask exactly like an argument, so a read hidden
  # there bypasses the guard against an ordinary summary using an earlier share
  # (#130).
  #
  # The exclusion is written as that one shape rather than as a list of walked
  # shapes. #100 could justify only the double-bracket head at the time and
  # enumerated it, which is why a redundant pair of parentheses -- making the
  # head a call to the paren function -- slipped past that fix. The cases
  # below are the head
  # shapes that were reaching the caller as `attempt to apply non-function`,
  # `$ operator is invalid for atomic vectors`, and `missing value where
  # TRUE/FALSE needed`: untyped conditions naming nothing the caller can act
  # on, which is the class ADR-0015 separates.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # `fns` is read from the summary expressions below, which codetools cannot
  # follow through the data mask.
  fns <- list(
    total = function(x, na_rm) sum(x, na_rm),
    double = function(x) x * 2
  )
  # nolint end

  from_index <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = fns[[if (length(share)) "double" else "double"]](value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_index, "marginplyr_error")

  # The same `[[` head wrapped in redundant parentheses. The head is now a call
  # to `(`, so an enumeration of walked head shapes misses it.
  from_parenthesized <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (fns[[share]])(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_parenthesized, "marginplyr_error")

  # The object of a `$` head. #101 stopped the field name counting as a read,
  # which is what makes walking this head safe; the object was still missed.
  from_dollar_object <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = share$total(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_dollar_object, "marginplyr_error")

  from_condition <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (if (share > 0) sum else prod)(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_condition, "marginplyr_error")

  # A head that is an ordinary call returning a function. Nothing about this
  # shape is special; it is here because an enumeration would have to name it.
  from_computed <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(s) function(v) s * v)(share)(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_computed, "marginplyr_error")

  # `total` in `fns$total` names a field of `fns` rather than reading the
  # share of that name, so this call must execute. Rejecting it is the defect
  # #100 was filed against, reached from the other direction, and walking the
  # head must not bring it back.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      total = share_of_total(units),
      derived = fns$total(value, TRUE),
      .grouping = rollup(region)
    )
  )

  # A bare symbol head stays excluded, which is what the exclusion is for.
  # `doubled` here names both a share and a function, and R's function lookup
  # skips the non-function binding: the call reaches the function even though
  # the mask holds a share of that name, so reporting the head as a read would
  # reject a call that runs correctly.
  # nolint start: object_usage_linter.
  # `doubled` is called from the summary expression below, through the data
  # mask, which codetools cannot follow.
  doubled <- function(x) sum(x) * 2
  # nolint end
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      doubled = share_of_total(units),
      derived = doubled(value),
      .grouping = rollup(region)
    )
  )
})

test_that("a namespaced call reads neither of its operands", {
  # `pkg::fun` is the head shape the mask does not evaluate: `::` takes both
  # operands literally, so neither names a column. Walking every non-symbol
  # head brings it into scope of the walk for the first time, and reporting
  # its parts rejects `dplyr::n()` wherever a summary is named `n` -- which is
  # how this package's own vignettes write it.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(expression_data_symbols(quote(dplyr::n())), character())
  expect_identical(
    expression_data_symbols(quote(stats::median(value))),
    "value"
  )
  # Argument position reaches the same node, so one branch answers both.
  expect_identical(
    expression_data_symbols(quote(list(m = stats::median))),
    character()
  )
  expect_identical(expression_data_symbols(quote(pkg:::fun(x))), "x")

  # A share source depending on an alias named after the function it calls.
  expect_no_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      records = dplyr::n(),
      share = share_of_total(records),
      .grouping = rollup(region)
    )
  )

  # An ordinary summary written after a share of the same name.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      n = share_of_total(units),
      rows = dplyr::n(),
      .grouping = rollup(region)
    )
  )

  # `base::get()` still reaches the `get()` branch: the namespace qualifier
  # names the function, and the literal it is given is still a mask read.
  expect_identical(
    expression_data_symbols(quote(base::get("share"))),
    "share"
  )
})

test_that("a formula is walked as the call to `~` that it is", {
  # `rlang::call_name()` unwraps a one-sided formula to its right-hand side.
  # That errors outright when the right-hand side is a bare symbol, and
  # misreads the call otherwise -- `~ .data$share` answers `$`, so the walk
  # entered a branch written for a different shape and got the right answer by
  # accident. Asking a formula for a call name is the mistake; it is a call to
  # `~` and the general walk already handles it.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(expression_data_symbols(quote(~.x)), ".x")
  expect_identical(expression_data_symbols(quote(~value)), "value")
  expect_identical(
    expression_data_symbols(quote(~ .x + share)),
    c(".x", "share")
  )
  expect_identical(expression_data_symbols(quote(~ .data$share)), "share")
  expect_identical(expression_data_symbols(quote(~ get("share"))), "share")

  # A formal's default is a new entry point into a formula: defaults were not
  # walked at all before, so this shape could not reach the walk until now.
  # It is asserted end to end because this walk is the only analysis that
  # descends into a default.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(f = ~.x) length(f))(),
      .grouping = rollup(region)
    )
  )

  # A formula written at the top of a summary expression -- `length(~value)`
  # -- used to abort from `find_summary_context_helpers()` before this walk was
  # reached: the same `is_call()`-then-`call_name()` pairing appeared at nine
  # further sites. That predated #130 and was fixed in #163, whose tests below
  # cover the sites and the shapes this one does not.

  # A formula in the `.fns` position of `across()` is the one spelling of this
  # that users are documented to write (`R/share.R:307`), and it was the only
  # shape in #130's tables asserted at the walk alone. The guard is where the
  # caller meets it, so it is asserted there too: `.x` is over-reported by
  # design, and the read of `share` beside it must still be refused.
  from_across <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = across(value, ~ .x + share),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_across, "marginplyr_error")
})

test_that("a read inside an injected quosure is still reached", {
  # A quosure is a call to `~`, so it is named as one -- no name -- and every
  # walk descends into it instead. That answer is only safe if descending
  # reaches what the quosure carries, which is what this asserts in the
  # package's own terms rather than in rlang's: the guard against an ordinary
  # summary reading an earlier share is the walk, so a read it stops seeing is
  # a guard that stops firing, and the wrong number is returned in silence
  # (#130).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sum(!!rlang::quo(share)),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("a rewritten expression gives back the quosure it walked into", {
  # Every walk that rewrites a node rebuilds it, and `rlang::call2()` and
  # `as.call()` build a plain call. A quosure is a call to `~` carrying an
  # environment and a class, so a walk that descended into one handed dplyr a
  # one-sided formula in its place and the summary was given a language object
  # instead of the value the caller injected (#165).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # `rewrite_summary_selections()` is the walk that flattened this one. The
  # quosure carries no selection to rewrite, so what it must give back is the
  # node it was handed.
  counted <- summarize_with_margins(
    data,
    units = sum(!!rlang::quo(dplyr::n())),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(counted$units, c(2L, 1L, 3L))

  # `rewrite_grouping_expr()` is the second walk, and it substitutes a constant
  # into the quosure rather than leaving it untouched, so this asserts the
  # rebuild rather than a walk that happened to change nothing. On `main` the
  # flattening reached the caller as `sum(~0L)` (#165), and before #163 as a
  # `grouping_id()` diagnostic naming a fault the caller did not have.
  identified <- summarize_with_margins(
    data,
    units = sum(value),
    level = sum(!!rlang::quo(grouping_id(region))),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(identified$level, c(0L, 0L, 1L))
})

test_that("a rewritten expression gives back the formula object it walked", {
  # A formula object is a call to `~` carrying attributes exactly as a quosure
  # is, so the same rebuild drops its class and its `.Environment`. Nothing
  # written in source is exposed -- a `~` typed inside a summary expression is
  # a bare call with no attributes at rewrite time -- so the shape is an
  # injected formula the caller holds.
  #
  # `rlang::is_formula()` still answers `TRUE` for the flattened node, because
  # it tests the call's shape. The loss shows only where the class is used, and
  # `~` is one such place: R returns a classed call unevaluated and rebuilds an
  # unclassed one against whatever environment it is evaluated in, so a
  # flattened lambda resolves in the data mask rather than where it was
  # written.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  written_in <- rlang::env(offset = 100)
  lambda <- rlang::new_formula(NULL, quote(.x + offset), env = written_in)

  result <- summarize_with_margins(
    data,
    units = sum(value),
    kept = identical(attr(!!lambda, ".Environment"), !!written_in),
    applied = rlang::as_function(!!lambda)(units),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_true(all(result$kept))
  expect_identical(result$applied, result$units + 100)
})

test_that("a selection inside a quosure resolves in the quosure's own env", {
  # The rewrite evaluates a selection in the environment of the dot it is
  # walking. A quosure carries an environment of its own, and that is the point
  # of injecting one, so a selection inside it resolves there instead. Reading
  # the outer environment would look up a name the caller never put in it.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  held <- local({
    selected <- "value"
    rlang::quo(dplyr::pick(dplyr::all_of(selected)))
  })

  result <- summarize_with_margins(
    data,
    rows = nrow(!!held),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(result$rows, c(2L, 1L, 3L))
})

# The four helpers that read a bare name, derived rather than listed: they are
# the registered Contextual helpers marginplyr itself owns, and a fifth owned
# spelling is one someone has to decide this question about rather than one
# that inherits an answer silently. Both halves of the criterion do work. None
# of the dplyr-owned families reads a bare name: `across()` and its siblings
# take a selection, which tidyselect resolves, `where()` takes a predicate
# function, and the refused `cur_*()` spellings take nothing at all. And the
# Grouping specification constructors are marginplyr's too but are not
# Contextual helpers (ADR 0019), so their arguments are evaluated in the
# caller's environment rather than read against the Grouping plan -- which is
# the answer #169 turns on.
marginplyr_owned_spellings <- function() {
  owned <- Filter(
    function(family) {
      identical(static_spelling_namespaces(family), "marginplyr")
    },
    contextual_helper_families()
  )
  sort(unlist(lapply(owned, static_spelling_names), use.names = FALSE))
}

# The probes below read `contextual_probe_data()`, from
# `helper-contextual-probes.R`: the other suite probing these helpers reads the
# same input, and a copy here recorded nothing but which was written first. Its
# two dimensions are what give a `rollup()` a parent level for a Parent share to
# divide by, and `units` is the measure the preceding ordinary summary takes.

# One summary per helper, written as a function from the *argument* to the whole
# call, because the argument is the only thing that varies between the written
# spelling and the injected one. A probe that built its own argument would fix
# one of the two and assert nothing about the other.
#
# `name` is the bare name that helper takes -- a grouping column for one family
# and a preceding ordinary summary for the other -- and it is what every shape
# below is derived from, so no shape is written out twice.
#
# `.probe_data` is the symbol the input is bound to in the evaluation
# environment; the grouping columns and the preceding summary sit inside
# defused expressions, which `codetools` cannot follow into.
# nolint start: object_usage_linter.
injection_probes <- function() {
  summary_probe <- function(helper, name) {
    list(
      name = name,
      call = function(argument) {
        rlang::expr(summarize_with_margins(
          .probe_data,
          t = sum(units),
          k = !!rlang::call2(helper, argument),
          .grouping = rollup(region, grade),
          .sort = "last"
        ))
      }
    )
  }

  list(
    grouping_bit = summary_probe("grouping_bit", quote(region)),
    grouping_id = summary_probe("grouping_id", quote(region)),
    share_of_parent = summary_probe("share_of_parent", quote(t)),
    share_of_total = summary_probe("share_of_total", quote(t))
  )
}
# nolint end

run_injection_probe <- function(probe, argument) {
  rlang::eval_bare(
    probe$call(argument),
    rlang::env(rlang::current_env(), .probe_data = contextual_probe_data())
  )
}

test_that("every helper reading a bare name has an injection probe", {
  # The derivation is what decides coverage, so a spelling registered without a
  # probe has to fail here rather than go unexercised -- the same reason
  # `test-contextual-helpers.R` checks its own probe table against the
  # registry.
  expect_identical(
    sort(names(injection_probes())),
    marginplyr_owned_spellings()
  )
})

test_that("a helper reading a bare name accepts one forwarded by injection", {
  # `rlang::enquo()` is what the tidy-eval idiom hands the author of a wrapper,
  # and a quosure is not a symbol, so all four helpers refused `!!enquo(col)`
  # while telling the caller they had not written a bare name -- which they
  # had, at their own call. The workaround was to reach for `rlang::ensym()`
  # instead, and nothing in the diagnostic said so (#169).
  probes <- injection_probes()
  for (helper in names(probes)) {
    probe <- probes[[helper]]
    written <- run_injection_probe(probe, probe$name)

    # The quosure is built on the empty environment, so the assertion carries
    # the environment answer as well as the acceptance one: nothing there can
    # answer a lookup, and the result is the same because the name is resolved
    # against the Grouping plan rather than anywhere at all (ADR 0019).
    injected <- run_injection_probe(
      probe,
      rlang::new_quosure(probe$name, env = rlang::empty_env())
    )
    expect_identical(injected, written, info = helper)

    # A quosure can carry another, so the unwrapping repeats until it reaches
    # something that is not one; a single step would hand the test back the
    # shape it exists to see through.
    nested <- run_injection_probe(
      probe,
      rlang::new_quosure(
        rlang::new_quosure(probe$name, env = rlang::empty_env()),
        env = rlang::empty_env()
      )
    )
    expect_identical(nested, written, info = helper)
  }
})

test_that("an injected quosure's environment does not decide the name", {
  # The environment is discarded, and that is the decision rather than an
  # oversight (#169): these helpers resolve a name against the Grouping plan by
  # spelling, so there is no lookup for an environment to answer. A binding of
  # the same name in the quosure's own environment is the case that would show
  # one being consulted, and it is the reading #165 gives one layer out, where
  # a selection inside an injected quosure really is evaluated there.
  probes <- injection_probes()
  for (helper in names(probes)) {
    probe <- probes[[helper]]
    shadow <- rlang::env()
    rlang::env_bind(shadow, !!rlang::as_name(probe$name) := "grade")

    expect_identical(
      run_injection_probe(probe, rlang::new_quosure(probe$name, env = shadow)),
      run_injection_probe(probe, probe$name),
      info = helper
    )
  }
})

test_that("an injected quosure carrying no bare name is refused as written", {
  # One answer for all four helpers, and the same answer the un-injected
  # spelling already gives: only a quosure carrying a bare name is one, and a
  # quosure carrying anything else is refused exactly where that expression is
  # refused without the injection (#169). The message is what the equality
  # asserts -- the injected form adds a clause naming the injection and stops
  # there, rather than reporting a different fault.
  probes <- injection_probes()
  for (helper in names(probes)) {
    probe <- probes[[helper]]
    # Derived from the bare name the helper takes, so the three shapes are one
    # rule rather than twelve written-out calls: the pronoun spelling, the
    # string, and a call around the same name.
    shapes <- list(
      rlang::call2("$", quote(.data), probe$name),
      rlang::as_string(probe$name),
      rlang::call2("+", probe$name, 1)
    )

    refusals <- list()
    for (index in seq_along(shapes)) {
      written <- expect_error(run_injection_probe(probe, shapes[[index]]))
      expect_s3_class(written, "marginplyr_error")
      # The written spelling says nothing about an injection, which is what
      # stops the clause leaking into a refusal that has nothing to do with one.
      expect_false(grepl("injected", conditionMessage(written), fixed = TRUE))

      injected <- expect_error(run_injection_probe(
        probe,
        rlang::new_quosure(shapes[[index]], env = rlang::empty_env())
      ))
      expect_s3_class(injected, "marginplyr_error")
      # The clause lands at the end of the refusal rather than at the end of
      # the message, which is a distinction only a re-authored helper draws:
      # `share_of_parent()` and `share_of_total()` carry their remedy in an `i`
      # bullet after it (#223), while the grouping helpers are still one line
      # and the two positions coincide. Written over the lines so that both
      # shapes are the one rule, and byte-exact either way.
      written_lines <- strsplit(
        conditionMessage(written),
        "\n",
        fixed = TRUE
      )[[1L]]
      written_lines[[1L]] <- paste0(
        written_lines[[1L]],
        " The injected quosure carries `",
        deparse1(shapes[[index]]),
        "`, which is not a bare name."
      )
      expect_identical(
        conditionMessage(injected),
        paste(written_lines, collapse = "\n"),
        info = helper
      )
      refusals[[index]] <- injected
    }

    # `rlang::as_label()` reads `.data$region` as `region`, so a clause written
    # with it would quote the refused part as the bare name the message says it
    # is not. Asserted on the shape that shows it rather than left to the
    # equality above, which `deparse1()` on both sides would satisfy either way.
    # Read back from the refusal the loop already raised, since running the call
    # again would assert about a second execution of it.
    expect_match(
      conditionMessage(refusals[[1L]]),
      paste0("carries `.data$", rlang::as_string(probe$name), "`"),
      fixed = TRUE
    )
  }
})

test_that("an injected quosure carrying the empty argument is named as one", {
  # The empty argument deparses to nothing at all, so a clause built from
  # `deparse1()` alone would refuse it with an empty pair of backticks. Only an
  # injected one reaches the label at all: the clause is the sole caller of
  # `call_part_label()` and it labels nothing that is not a quosure, so a
  # written `grouping_id(, )` is refused without one -- which is why this sits
  # here rather than beside the empty-argument cases #181 covers.
  probe <- injection_probes()$grouping_id
  error <- expect_error(run_injection_probe(
    probe,
    rlang::new_quosure(rlang::missing_arg(), env = rlang::empty_env())
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(
    conditionMessage(error),
    "The injected quosure carries `<empty>`",
    fixed = TRUE
  )
})

test_that("a caller's two mistakes at once are reported by one message", {
  # An injected non-name in a call that also has the wrong arity. Which
  # diagnostic wins is each helper's own decision rather than the clause's, and
  # the two decide it differently on purpose, so it is asserted rather than left
  # to the message equality above -- which compares one helper against itself
  # and would not notice either decision changing.
  data <- contextual_probe_data()
  injected <- rlang::new_quosure(quote(1 + 1), env = rlang::empty_env())

  # `grouping_bit()` counts columns in a message of its own, reached before the
  # non-column one deliberately (#181): a caller who passed two of anything
  # needs the count, not a remark about one of them.
  counted <- expect_error(rlang::inject(summarize_with_margins(
    data,
    k = grouping_bit(!!injected, region),
    .grouping = rollup(region, grade)
  )))
  expect_s3_class(counted, "marginplyr_error")
  expect_identical(
    conditionMessage(counted),
    "`grouping_bit()` requires exactly one column."
  )

  # The share helpers' headline covers the arity and the name-ness together, so
  # both halves describe this call, and the clause quotes what was injected --
  # which is what tells the two arguments apart, there being no position in it.
  named <- expect_error(rlang::inject(summarize_with_margins(
    data,
    t = sum(units),
    k = share_of_total(!!injected, t),
    .grouping = rollup(region, grade)
  )))
  expect_s3_class(named, "marginplyr_error")
  expect_match(
    conditionMessage(named),
    "The injected quosure carries `1 + 1`, which is not a bare name.",
    fixed = TRUE
  )
})

test_that("a share helper refuses a named empty argument as a non-name", {
  # Not an injected spelling, and recorded in ADR 0019 as the one un-injected
  # change asking the name question once produced. `share_of_total(, )` is two
  # arguments to the parser and is refused for the count, but
  # `share_of_total(x = )` is one argument and it is empty, and
  # `rlang::is_symbol()` answered `TRUE` for it -- the empty argument is a
  # symbol whose name is `""`. So the call was admitted and refused one layer on
  # for a preceding summary named ``, which is a summary nobody wrote: #181's
  # defect, in the family that ticket did not reach.
  data <- contextual_probe_data()

  error <- expect_error(summarize_with_margins(
    data,
    t = sum(units),
    k = share_of_total(x = ),
    .grouping = rollup(region, grade)
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(
    conditionMessage(error),
    "requires exactly one bare name of a preceding ordinary summary",
    fixed = TRUE
  )
  # The old diagnostic named the summary it invented, which is what makes the
  # regression readable: an assertion on the new wording alone would pass on any
  # refusal at all.
  expect_false(
    grepl("unknown preceding", conditionMessage(error), fixed = TRUE)
  )
})

test_that("injection is transparent to the checks after the name test", {
  # Unwrapping happens before every question `grouping_helper_vars()` asks, not
  # only before the symbol test, so a name that arrives injected is the same
  # name to the duplicate check and to the plan membership check. Reading one
  # through and the others around it is the shape that would let an injected
  # duplicate through.
  data <- contextual_probe_data()
  injected <- rlang::new_quosure(quote(region), env = rlang::empty_env())

  duplicated <- expect_error(rlang::inject(summarize_with_margins(
    data,
    k = grouping_id(region, !!injected),
    .grouping = rollup(region, grade)
  )))
  expect_s3_class(duplicated, "marginplyr_error")
  expect_match(
    conditionMessage(duplicated),
    "does not accept duplicate columns",
    fixed = TRUE
  )

  unknown <- expect_error(rlang::inject(summarize_with_margins(
    data,
    k = grouping_id(!!rlang::new_quosure(quote(nowhere))),
    .grouping = rollup(region, grade)
  )))
  expect_s3_class(unknown, "marginplyr_error")
  expect_match(
    conditionMessage(unknown),
    "Column `nowhere` is not part of `.by` or `.grouping`.",
    fixed = TRUE
  )
})

# Runs `code` with rlang's soft deprecations raised as errors, restoring
# whatever the checking environment had before. Written with `on.exit()` rather
# than withr so the tests add no dependency beyond the ones DESCRIPTION
# declares, as `with_required_suggests()` in `test-optional-backends.R` is, and
# at file level so the next test asserting a signal-free walk reaches it.
with_deprecation_errors <- function(code) {
  previous <- options(lifecycle_verbosity = "error")
  on.exit(options(previous), add = TRUE)
  force(code)
}

test_that("no walk subsets a quosure", {
  # rlang soft-deprecated `[` and `[[` on a quosure, so a walk spelling its
  # reads that way signals a lifecycle condition into whatever handler the
  # caller has installed -- the class of signal `margin_column_pronoun()`
  # exists to avoid producing. Raising the soft deprecation is what makes the
  # assertion hold: the warning itself is shown only once every eight hours,
  # so a test reading for one would pass on a walk that still subsets.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # Each walk in turn: the share analysis that reads a summary for an earlier
  # share, the two rewrites, and the context-helper search.
  with_deprecation_errors(expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sum(!!rlang::quo(share)),
      .grouping = rollup(region)
    ),
    "`share`"
  ))
  with_deprecation_errors(expect_no_error(
    summarize_with_margins(
      data,
      units = sum(!!rlang::quo(dplyr::n())),
      level = sum(!!rlang::quo(grouping_id(region))),
      .grouping = rollup(region)
    )
  ))
  with_deprecation_errors(expect_error(
    summarize_with_margins(
      data,
      units = sum(!!rlang::quo(dplyr::cur_group_id())),
      .grouping = rollup(region)
    ),
    "does not support"
  ))
})

test_that("every analysis that names a call reads a formula as a `~` call", {
  # The nine further analyses #163 lists guarded with `rlang::is_call()` and
  # then asked for a name, so each read a formula as whatever its right-hand
  # side is. Each is asserted at its own function rather than end to end,
  # because only one half of the misread reports itself: a bare symbol on the
  # right raises "`call` must be a defused call, not a symbol", while a call on
  # the right silently sends the formula into a branch written for another
  # shape, where `expr[[2L]]` is the formula's right-hand side and not the
  # operand that branch means to read.
  #
  # `grouping_arg_spec()` is a tenth site, which #163 lists as unaffected on
  # the grounds that it guards each read individually. That is NULL-safety for
  # a non-call and no protection from a formula, which passes `is_call()`
  # there like anywhere else; the test below covers it.
  proxy <- data.frame(value = double(), other = double())
  env <- rlang::current_env()

  # The three reads #163 names -- `"$"`, `"get"`, and `"+"` -- and the
  # two-sided formula, which is the one shape `call_name()` did answer as `~`.
  expect_null(static_call_name(quote(~ .data$share)))
  expect_null(static_call_name(quote(~ get("share"))))
  expect_null(static_call_name(quote(~ .x + share)))
  expect_null(static_call_ns(quote(~ dplyr::n())))
  expect_null(static_call_name(quote(a ~ b)))

  # An injected quosure is a `~` call as well, and gets the same answer for a
  # stronger reason than a formula does: every site reads operands from the
  # node it has just named, and a quosure answers none of those reads as the
  # call it carries, so a name read through to the call inside would not
  # describe the operands beside it. That is this defect rather than a fix for
  # it. What the answer must not cost is the read inside, which the walk
  # reaches as a part -- asserted end to end below.
  expect_null(static_call_name(rlang::quo(dplyr::across(value, mean))))
  expect_null(static_call_ns(rlang::quo(dplyr::n())))
  # A quosure carrying no call is a name this cannot read, not an error: that
  # is the shape `rlang::call_name()` aborts on.
  expect_null(static_call_name(rlang::quo(value)))

  # Anything that is not a call has no name to read either. That is deliberate
  # and load-bearing rather than incidental: it is what lets a site ask without
  # a guard of its own, which `share_helper_call_kind()`, `is_across_call()`,
  # `is_name_only_expr()`, and `grouping_arg_spec()` each rely on.
  expect_null(static_call_name(quote(value)))
  expect_null(static_call_ns(quote(value)))
  expect_null(static_call_name(1L))
  expect_null(static_call_ns(1L))
  expect_false(is_share_helper_call(quote(value)))
  expect_false(is_across_call(quote(value)))
  # A language object that is no call, which is what the guard dropped from
  # `is_name_only_expr()` used to answer. A literal takes the branch above it
  # and is a name-only selection, as it was before.
  expect_false(
    is_name_only_expr(expression(x), env = env, data_vars = "region")
  )
  expect_true(is_name_only_expr(1L, env = env, data_vars = "region"))

  # A walk still reaches what the formula holds: the helper inside is found
  # once, from the parts, rather than twice -- once from the formula misread
  # as the call it wraps, and once from that call itself.
  expect_identical(
    find_summary_context_helpers(quote(length(~ dplyr::cur_group()))),
    "cur_group"
  )
  expect_identical(
    find_summary_context_helpers(quote(length(~value))),
    character()
  )

  # The rewrite belongs to the `across()` inside the formula, which the walk
  # over the arguments already reaches. Rewriting the formula as that
  # `across()` too turned a lambda into the two-sided formula
  # `dplyr::all_of("value") ~ mean`.
  expect_identical(
    rewrite_summary_selections(
      quote(~ dplyr::across(c(value), mean)),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote(~ dplyr::across(dplyr::all_of("value"), mean))
  )
  expect_identical(
    rewrite_summary_selections(
      quote(~.x),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote(~.x)
  )

  # A formula names no output columns, is no share helper call, no `across()`
  # call, no name-only selection, and no grouping helper -- whatever sits on
  # its right-hand side. Each of these answered for that right-hand side.
  expect_identical(
    known_data_frame_output_names(quote(~ dplyr::pick(value)), env, proxy),
    character()
  )
  expect_null(share_helper_call_kind(quote(~ share_of_total(units))))
  expect_false(is_across_call(quote(~ dplyr::across(value, mean))))
  expect_false(
    is_name_only_expr(quote(~c(region)), env = env, data_vars = "region")
  )
  expect_null(grouping_helper_name(quote(~ grouping_id(region))))

  # The predicate search still finds `where()`, from the parts rather than
  # from a misread name, and answers a bare-symbol right-hand side instead of
  # aborting on it.
  expect_true(contains_selection_predicate(quote(~ where(is.numeric))))
  expect_false(contains_selection_predicate(quote(~.x)))
})

test_that("a formula in a summary expression evaluates instead of aborting", {
  # `derived = purrr::map_dbl(v, ~.x)` is the realistic spelling of this. purrr
  # is neither an Import nor a Suggest of this package, so it is written here
  # against one that is: `rlang::as_function()` is what `map_dbl()` applies the
  # formula through, and it is the same shape -- a lambda whose right-hand side
  # is a bare symbol, in argument position. `length(~value)` is #163's
  # contrived one, and it reaches further: the formula sits at the top of the
  # summary expression rather than inside a call, so every analysis that reads
  # a top-level expression sees it.
  #
  # Both are ordinary R that marginplyr has nothing to say about, so ADR-0015
  # asks them to fall through and evaluate rather than to raise a condition of
  # any class. A formula is a value, not a deferred read of the mask: the
  # length of `~value` is 2 whatever the data holds.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = length(~value),
    lambda = rlang::as_function(~.x)(units),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expect_identical(result$derived, rep(2L, nrow(result)))
  expect_identical(result$lambda, result$units)
  expect_identical(result$share, result$units / sum(data$value))
})

test_that("a formula in a grouping specification reaches tidyselect", {
  # `grouping_arg_spec()` read a name to decide whether an argument is a
  # Grouping constructor call or a selection to evaluate later. It guarded each
  # read with `rlang::is_call()` separately, which #163 reads as protection --
  # but that is NULL-safety for an argument that is no call at all, and a
  # formula is a call. `rollup(~region)` passed both guards and aborted with
  # the untyped condition, from a site the ticket lists as unaffected.
  #
  # A formula is no Grouping constructor, so the argument is a selection, and
  # tidyselect is what has something to say about a formula in one. Its message
  # names the rewrite; the internal abort named an rlang argument the caller
  # never wrote.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      .grouping = rollup(~region)
    ),
    "Formula shorthand must be wrapped in `where\\(\\)`"
  )
  # The selection is the caller's own, so what tidyselect raises reaches them
  # as tidyselect raised it -- neither re-typed as a Package condition nor
  # replaced by one of this package's internal invariants (ADR-0015).
  expect_s3_class(error, "rlang_error")
  expect_false(inherits(error, "marginplyr_error"))

  # A constructor argument is still recognized as one, and a bare column is
  # still a name-only selection.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      .grouping = rollup(region)
    )
  )
})

test_that("a formula wrapping a share helper is refused, not computed", {
  # The one shape whose misread raised nothing. `rlang::call_args()` unwraps a
  # formula exactly as `call_name()` does, so `~ share_of_total(units)` was
  # read as the direct share `share_of_total(units)` -- name, argument, and
  # all. The formula the caller wrote was dropped and a share appeared under
  # the name they gave it, which is the silently wrong result the raised
  # conditions elsewhere in #163 were not.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      derived = ~ share_of_total(units),
      .grouping = rollup(region)
    ),
    "must be the complete right-hand side"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("the head is walked before the arguments, and the guard says so", {
  # The walk returns symbols in source order, and the head is syntactically
  # first. That is not an internal detail: the guard names
  # `share_dependency[[1L]]` (`R/share.R:786`), so this order decides which
  # share an expression reading two of them is reported against. Asserting it
  # through the message is what makes it a property of the diagnostic the
  # caller reads rather than of the vector the walk happens to build.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_head <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      a = share_of_total(units),
      b = share_of_total(units),
      derived = (if (a > 0) sum else prod)(b),
      .grouping = rollup(region)
    ),
    "`a`"
  )
  expect_s3_class(from_head, "marginplyr_error")

  # The same two shares with neither in a head: the first one written is the
  # one named, so the case above is reporting the head rather than reporting
  # whichever share happens to sort first.
  from_argument <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      a = share_of_total(units),
      b = share_of_total(units),
      derived = b + a,
      .grouping = rollup(region)
    ),
    "`b`"
  )
  expect_s3_class(from_argument, "marginplyr_error")

  expect_identical(
    expression_data_symbols(quote((if (a > 0) sum else prod)(b))),
    c("a", "sum", "prod", "b")
  )
})

test_that("a function definition binds its formals", {
  # The walk used to collect every symbol in a function body, so a lambda
  # binding a name equal to a preceding share was rejected while a lambda
  # genuinely reading that share was accepted only when it sat in argument
  # position. Both halves are asserted here, because a fix that suppresses
  # formal names by filtering the walk's output passes the first and breaks
  # the second (#130).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # A read the guard owes the caller. In head position this returned a silent
  # `NA` column rather than any condition, which is what made #130 a blocker.
  from_lambda_head <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(y) share * 100)(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_lambda_head, "marginplyr_error")

  from_backslash_head <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (\(y) share * 100)(value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_backslash_head, "marginplyr_error")

  # The same read in argument position was already rejected, and must stay so.
  from_argument <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sum(vapply(value, function(z) z + share, numeric(1))),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_argument, "marginplyr_error")

  # A formal shadowing the share. The lambda never reads the column, so
  # rejecting this is the #101 defect reached through a different construct.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sum(vapply(value, function(share) share + 1, numeric(1))),
      .grouping = rollup(region)
    )
  )

  # Formals reach the body and the defaults, and nothing else. The argument
  # here is evaluated in the mask, so it reads the share even though the
  # function it is passed to binds that name.
  from_sibling_argument <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(share) share)(share),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_sibling_argument, "marginplyr_error")

  # Nesting stacks: the inner formal shadows only inside the inner body.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sum(
        vapply(value, function(z) (function(share) share)(z), numeric(1))
      ),
      .grouping = rollup(region)
    )
  )
})

test_that("a formal's default value is a data-mask read", {
  # A default is evaluated in the function's own frame, whose enclosure is the
  # data mask, so it reads the mask like any other expression. The walk
  # collected nothing from the formals pairlist, so this was silent in every
  # position -- the same failure as a lambda head, reached without one (#130).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_default <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(y = share) y)(),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_default, "marginplyr_error")

  # A default is scoped against every formal, not only the ones written before
  # it: R evaluates defaults lazily in a frame that already holds them all, so
  # `(function(a = b, b = 1) a)()` is 1. `k` here therefore names the formal
  # `share` rather than the column, and the call must run.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (function(share, k = share) sum(k))(value),
      .grouping = rollup(region)
    )
  )
})

test_that("a bound name shadows a symbol and a `get()`, but not a pronoun", {
  # The bound set has to be threaded through the recursion as a scope rather
  # than applied to the walk's output, and these are the three cases that tell
  # the two implementations apart. `.data$share` reads the column whatever is
  # bound locally, because the pronoun is dplyr's contract; `get("share")`
  # goes through ordinary name resolution and finds the binding (#130).
  expect_identical(
    expression_data_symbols(quote(share), bound = "share"),
    character()
  )
  expect_identical(expression_data_symbols(quote(share)), "share")
  expect_identical(
    expression_data_symbols(quote(.data$share), bound = "share"),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(.data[["share"]]), bound = "share"),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(get("share")), bound = "share"),
    character()
  )
})

test_that("`<-`, `for`, and `local()` bind names rather than reading them", {
  # `<-` inside a summary expression assigns into the bottom of the data mask,
  # `for` binds its index the same way, and `local()` evaluates in a child
  # environment of it. The walk collected every symbol it passed, so each bound
  # name was reported as a column read, and where one collided with a preceding
  # share the guard rejected legal code naming a share the expression never
  # reads (#162).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_assignment <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = {
      share <- 2
      share
    },
    .grouping = rollup(region)
  )
  expect_identical(unique(from_assignment$derived), 2)

  from_loop <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = {
      out <- 0
      for (share in c(1, 2)) {
        out <- out + share
      }
      out
    },
    .grouping = rollup(region)
  )
  expect_identical(unique(from_loop$derived), 3)

  from_local <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = local({
      share <- 4
      share
    }),
    .grouping = rollup(region)
  )
  expect_identical(unique(from_local$derived), 4)

  # A binding statement reached outside a block, which is the other branch the
  # walk answers: nothing follows it for the binding to reach, so only its
  # reads count and it has none. A bare `for` cannot be asserted here because
  # it evaluates to `NULL` and no summary can be one, so the walk-level table
  # below is the layer that covers that spelling.
  from_bare_assignment <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = (share <- 2),
    .grouping = rollup(region)
  )
  expect_identical(unique(from_bare_assignment$derived), 2)
})

test_that("a bound name does not hide a genuine read beside it", {
  # The other half of #162: dropping a name because something binds it must
  # not drop the reads that reach the mask in the same expression. Each of
  # these really does read the share, and the guard still owes the caller a
  # `marginplyr_error`.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_value <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = {
        tmp <- share
        tmp
      },
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_value, "marginplyr_error")

  from_loop_body <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = {
        out <- 0
        for (i in c(1, 2)) {
          out <- out + share
        }
        out
      },
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_loop_body, "marginplyr_error")

  from_local_body <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = local({
        x <- 1
        x + share
      }),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_local_body, "marginplyr_error")

  from_bare_assignment <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = (tmp <- share),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_bare_assignment, "marginplyr_error")
})

test_that("`rm()` puts a name back within reach of the column", {
  # The bound set may only shrink where the walk can see the removal, and
  # `rm()` is the one statement that shrinks it: after `rm(share)` the next
  # read reaches the column again. Losing that read would be the silent class
  # #130 removed rather than the loud class this issue owns, so it is the one
  # direction growing a bound set is not allowed to be wrong in (#162).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_removal <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = {
        share <- 1
        rm(share)
        share
      },
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_removal, "marginplyr_error")

  # A removal the walk cannot read has to reach the guard too, since emptying
  # the set is only the safe answer if the reads it restores are reported.
  from_dynamic_removal <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = {
        share <- 1
        rm(list = paste0("sh", "are"))
        share
      },
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_dynamic_removal, "marginplyr_error")

  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      rm(tmp)
      tmp
    })),
    "tmp"
  )
  # `remove()` is the same function under its other name, and a string names
  # the binding as directly as a symbol does.
  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      remove("tmp")
      tmp
    })),
    "tmp"
  )
  # A removal the walk cannot read empties the set rather than being ignored:
  # `rm(list = v)` removes whatever that vector holds, and `rm(x, envir = e)`
  # may remove nothing at all. The same holds under the other spelling.
  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      rm(list = v)
      tmp + share
    })),
    c("v", "tmp", "share")
  )
  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      remove(list = v)
      tmp + share
    })),
    c("v", "tmp", "share")
  )
  # A name it does not remove stays bound, so the removal costs nothing beside
  # itself.
  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      rm(other)
      tmp
    })),
    "other"
  )
})

test_that("a removal takes back only the names it names", {
  # The transition `rm()` introduces runs the opposite way to every other one
  # here -- bound to unbound -- so each place the bound set travels needs
  # asserting in that direction too. Every expectation below was confirmed
  # against R by evaluating the block in a child of an environment holding the
  # removed names, and reading which binding the final read reached (#162).

  # A removal beside an untouched binding: `a` reaches the enclosing value
  # again, `b` is still the local one.
  expect_identical(
    expression_data_symbols(quote({
      a <- 1
      b <- 2
      rm(a)
      a + b
    })),
    "a"
  )
  # Several names removed at once, under both spellings of the call.
  expect_identical(
    expression_data_symbols(quote({
      a <- 1
      b <- 2
      rm(a, b)
      a + b
    })),
    c("a", "b")
  )
  expect_identical(
    expression_data_symbols(quote({
      a <- 1
      b <- 2
      remove(a, b)
      a + b
    })),
    c("a", "b")
  )
  # Rebinding after a removal binds again: the set grows and shrinks along the
  # block rather than being decided once for it.
  expect_identical(
    expression_data_symbols(quote({
      tmp <- 1
      rm(tmp)
      tmp <- 2
      tmp
    })),
    character()
  )
  # A nested block and a redundant parenthesis carry a removal out of
  # themselves exactly as they carry a binding: neither opens a scope, so
  # `rm()` inside one removes the binding the statements after it would see.
  # Both blocks are parsed from text because writing the inner braces out is
  # the one shape `brace_linter` refuses.
  expect_identical(
    expression_data_symbols(str2lang("{ tmp <- 1; { rm(tmp) }; tmp }")),
    "tmp"
  )
  expect_identical(
    expression_data_symbols(str2lang("{ tmp <- 1; (rm(tmp)); tmp }")),
    "tmp"
  )
})

test_that("a binding is visible to the statements after it and no others", {
  # A `{` opens no scope, so the bound set has to grow along the block rather
  # than be collected from it and applied to the whole. The two spellings below
  # differ only in the order of their statements, and a walk that filtered the
  # block with its bindings would answer both `"share"` -- reporting no read of
  # the column `tmp` that the second one really does make (#162).
  expect_identical(
    expression_data_symbols(quote({
      tmp <- share
      tmp
    })),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote({
      tmp + share
      tmp <- 1
    })),
    c("tmp", "share")
  )
  # An assignment that is not a statement of the block may not run, so the name
  # it would bind stays a read. That over-reports, which is the side of this
  # walk whose errors are diagnostics rather than silence.
  expect_identical(
    expression_data_symbols(quote({
      if (p) tmp <- 1
      tmp
    })),
    c("p", "tmp")
  )
  # A nested block and a redundant parenthesis are the other side of that: they
  # open no scope either, and unlike an `if` they always run, so the binding
  # inside one reaches the statements after it. Reading them as opaque would
  # reject `{ { tmp <- 1 }; tmp }` naming a share it never reads, which is the
  # false positive this issue removes rather than one to leave standing. The
  # nested block is parsed from text because writing it out is the one brace
  # shape `brace_linter` refuses, and the shape is the whole point of the case.
  expect_identical(
    expression_data_symbols(str2lang("{ { tmp <- share }; tmp }")),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote({
      (tmp <- share)
      tmp
    })),
    "share"
  )
  # `<<-` assigns past the environment it runs in, so what it binds is not
  # decidable here and its target is left reported for the same reason.
  expect_identical(
    expression_data_symbols(quote({
      tmp <<- 1
      tmp
    })),
    "tmp"
  )
  # `=` is the same node under another spelling.
  expect_identical(
    expression_data_symbols(quote({
      tmp = share
      tmp
    })),
    "share"
  )
})

test_that("a loop reads its sequence and binds its index past the loop", {
  expect_identical(
    expression_data_symbols(quote(for (i in v) i + share)),
    c("v", "share")
  )
  # R binds the index in the enclosing environment, and binds it even when the
  # sequence is empty, so nothing after the loop reads a column of that name.
  expect_identical(
    expression_data_symbols(quote({
      for (i in v) NULL
      i
    })),
    "v"
  )
})

test_that("an assignment still reads its value and a replacement target", {
  # The target of `x <- ...` is not a read, but everything else about the
  # statement is: the value always, and a replacement form's target, which is
  # read before it is rebuilt.
  expect_identical(expression_data_symbols(quote(x <- x + 1)), "x")
  expect_identical(
    expression_data_symbols(quote(names(x) <- share)),
    c("x", "share")
  )
  # The bound set reaches a symbol and a `get()` and stops at a pronoun, the
  # same three-way split a formal makes (#130).
  expect_identical(
    expression_data_symbols(quote({
      share <- 1
      get("share")
    })),
    character()
  )
  expect_identical(
    expression_data_symbols(quote({
      share <- 1
      .data$share
    })),
    "share"
  )
})

test_that("an ordinary `$` field name is not a data-mask read", {
  # `cfg$share` reads a field of a list. The name after `$` is fixed text
  # rather than a lookup, so collecting it made the guard against reading an
  # earlier share believe an ordinary summary read the share of that name, and
  # legal code was rejected -- while `cfg[["share"]]`, the same access spelled
  # with a string, ran and returned the right answer (#101).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # `cfg` is read from the summary expressions below, which codetools cannot
  # follow through the data mask.
  cfg <- list(share = "annual", total = 99)
  # nolint end

  from_dollar <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    note = cfg$share,
    .grouping = rollup(region)
  )
  from_bracket <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    note = cfg[["share"]],
    .grouping = rollup(region)
  )

  expect_equal(from_dollar, from_bracket)
  expect_identical(unique(from_dollar$note), "annual")

  # The same disagreement reached from the other side: the field name made a
  # share source look as though it depended on an earlier summary alias.
  source_dollar <- summarize_with_margins(
    data,
    total = sum(value),
    scaled = sum(value) * cfg$total,
    sh = share_of_parent(scaled),
    .grouping = rollup(region)
  )
  source_bracket <- summarize_with_margins(
    data,
    total = sum(value),
    scaled = sum(value) * cfg[["total"]],
    sh = share_of_parent(scaled),
    .grouping = rollup(region)
  )

  expect_equal(source_dollar, source_bracket)
})

test_that("the object of a `$` and the index of a `[[` are still read", {
  # Only the field name stops counting. Dropping `$` calls from the walk
  # altogether would let a read of the object through, and `[[` is not the
  # same shape at all: its index is evaluated, so `cfg[[share]]` reads the
  # share the guard exists to catch.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_object <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = share$field,
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_object, "marginplyr_error")

  from_index <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = list(a = 1)[[share]],
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_index, "marginplyr_error")

  expect_identical(expression_data_symbols(quote(cfg$share)), "cfg")
  expect_identical(
    expression_data_symbols(quote(cfg[[share]])),
    c("cfg", "share")
  )
  # `@` names a slot the same literal way, and the walk reaches it through the
  # same fall-through, so it is fixed in the same pass (#101). It is asserted
  # on the walk rather than through a summary because an S4 object in a
  # summary expression would add a dependency the package does not otherwise
  # need.
  expect_identical(expression_data_symbols(quote(cfg@share)), "cfg")
})

test_that("a genuine read of an earlier share is still rejected", {
  # The two true positives the false positives above sit next to: an ordinary
  # summary that really does name the share, and a share source that really
  # does depend on an earlier alias. Both messages are the ones #101 must
  # leave untouched.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  ordinary <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      note = share,
      .grouping = rollup(region)
    ),
    "Ordinary summaries cannot use an earlier Parent share (`share`)",
    fixed = TRUE
  )
  expect_s3_class(ordinary, "marginplyr_error")

  source <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      scaled = total * 2,
      sh = share_of_parent(scaled),
      .grouping = rollup(region)
    ),
    paste0(
      "Parent share `sh` cannot use source summary `scaled` because it ",
      "depends on earlier summary alias `total`"
    ),
    fixed = TRUE
  )
  expect_s3_class(source, "marginplyr_error")
})

test_that("`.data$x` still reads a column and `.env$x` still reads none", {
  # The pronoun branches sit above the `$` handling and are unchanged: the
  # fix must not reach them.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # Read through `.env` from the summary expression below.
  share <- "from the environment"
  # nolint end

  pronoun <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = .data$share * 2,
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(pronoun, "marginplyr_error")

  from_env <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    note = .env$share,
    .grouping = rollup(region)
  )
  expect_identical(unique(from_env$note), "from the environment")
})

test_that("a `get()` call with no name argument raises the caller's error", {
  # The analysis reads the looked-up name out of `get()`, and a call that
  # supplies neither `x` nor a positional argument has none to read. Nothing
  # is wrong with the analysis, so the call evaluates and base R's own
  # condition reaches the caller -- as it does from plain `summarise()`.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  baseline <- expect_error(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(z = get(mode = "numeric"), .groups = "drop")
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      z = get(mode = "numeric"),
      .grouping = rollup(region)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_s3_class(error$parent, "missingArgError")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("an `across()` `.names` template must name one output per column", {
  # This one the analysis does detect: it expands the template itself, so it
  # knows before any backend read that the template names two outputs for one
  # column. ADR-0005 puts the rejection here rather than leaving it to the
  # query dplyr would otherwise build.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(c(units), sum, .names = "{c('x','y')}"),
      .grouping = rollup(region)
    ),
    "must produce one name per column"
  )

  expect_s3_class(error, "marginplyr_error")
  # The template is what the caller has to rewrite, so the message quotes it.
  expect_match(conditionMessage(error), "{c('x','y')}", fixed = TRUE)
  expect_match(conditionMessage(error), "`units`", fixed = TRUE)
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("an unnamed `across()` argument is numbered by its own position", {
  # The placeholder names the analysis gives the unnamed additional arguments
  # of an `across()` call are only ever read back in a diagnostic, so a wrong
  # one is invisible until something quotes it. Building them over *every*
  # additional argument while assigning to the unnamed ones is visible sooner
  # than that: the two sides differ in length whenever any additional argument
  # is named, so base R recycles and warns from a call that otherwise
  # succeeds, once per grouping-set branch (#104).
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )

  # Only untyped warnings are collected. Passing an argument through
  # `across()`'s `...` at all is deprecated by dplyr, which warns for its own
  # reasons from the same call; that warning is dplyr's to raise, carries its
  # own class, and is untouched by this fix, whereas a bare `simpleWarning`
  # here can only have come from the analysis.
  untyped_warnings <- character()
  withCallingHandlers(
    summarize_with_margins(
      data,
      dplyr::across(c(units, revenue), sum, na.rm = TRUE, TRUE),
      .grouping = rollup(region)
    ),
    warning = function(cnd) {
      if (inherits(cnd, "simpleWarning")) {
        untyped_warnings <<- c(untyped_warnings, conditionMessage(cnd))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(untyped_warnings, character())

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(revenue),
      dplyr::across(
        c(total),
        share_of_total,
        .names = "{.col}_s",
        na.rm = TRUE,
        TRUE
      ),
      .grouping = rollup(region)
    ),
    "does not accept additional function arguments"
  )
  # dplyr names an argument passed on to the function by its position among
  # those arguments, so the unnamed one here -- second of the two -- is `..2`.
  # The `and` is cli's serial join, which ADR 0023 adopts unchanged.
  expect_match(conditionMessage(error), "`na.rm` and `..2`", fixed = TRUE)
})

test_that("`across()` argument numbering holds without a mix of names", {
  # The two ends of the same rule: with no named argument the ordinals are
  # already what a sequence over everything would produce, and with no unnamed
  # one there is nothing to number. Neither reaches the subassignment that
  # #104 was about, so both must keep behaving as they did.
  data <- data.frame(
    region = c("East", "East", "West"),
    revenue = c(2, 4, 8)
  )
  base_call <- function(...) {
    summarize_with_margins(
      data,
      total = sum(revenue),
      ...,
      .grouping = rollup(region)
    )
  }

  all_unnamed <- expect_error(
    base_call(dplyr::across(
      c(total),
      share_of_total,
      .names = "{.col}_s",
      TRUE,
      TRUE
    )),
    "does not accept additional function arguments"
  )
  expect_match(conditionMessage(all_unnamed), "`..1` and `..2`", fixed = TRUE)

  all_named <- expect_error(
    base_call(dplyr::across(
      c(total),
      share_of_total,
      .names = "{.col}_s",
      na.rm = TRUE
    )),
    "does not accept additional function arguments"
  )
  expect_match(conditionMessage(all_named), "`na.rm`.", fixed = TRUE)
})

test_that("an empty argument in a summary expression evaluates, not aborts", {
  # Every empty-index spelling carries R's missing marker as an argument, and
  # `x[, "col"]` is everyday R rather than a contrived shape, so these are
  # written as the four spellings a caller writes rather than as a synthetic
  # `rlang::missing_arg()` (#168). An empty argument holds no share helper and
  # nothing else the analysis recognizes, so ADR-0015 asks each to fall through
  # and evaluate exactly as `dplyr::summarise()` evaluates it.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    subset = sum(value[]),
    picked = sum(dplyr::pick(value)[, "value"]),
    row = sum(matrix(value, nrow = 1)[1, ]),
    trailing = mean(value, ),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expected <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      subset = sum(value[]),
      picked = sum(dplyr::pick(value)[, "value"]),
      row = sum(matrix(value, nrow = 1)[1, ]),
      trailing = mean(value, ),
      .groups = "drop"
    )

  expect_equal(
    dplyr::arrange(result[!is.na(result$region), ], region),
    as.data.frame(expected),
    ignore_attr = "row.names"
  )
})

test_that("a function rejecting an empty argument raises the caller's error", {
  # `sum(value, )` is the fourth spelling from #168, and it is the one whose
  # empty argument the called function itself refuses -- `mean()` accepts a
  # trailing empty argument and `sum()` does not. Falling through is still the
  # right answer: nothing is wrong with the analysis, so the call evaluates and
  # the condition `dplyr::summarise()` produces for the same expression is the
  # one that reaches the caller.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  baseline <- expect_error(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(trailing = sum(value, ), .groups = "drop")
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      trailing = sum(value, ),
      .grouping = rollup(region)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_match(
    conditionMessage(error$parent),
    "argument 2 is empty",
    fixed = TRUE
  )
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("an empty argument does not hide a share helper beside it", {
  # Answering `NULL` for the empty argument is a fall-through, not a stop: the
  # walk has to carry on to the arguments after it, or a share helper written
  # beside an empty index would reach the backend as an ordinary summary
  # instead of the diagnostic that names it.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      derived = sum(c(total[], share_of_total(total))),
      .grouping = rollup(region)
    ),
    "`share_of_total()`",
    fixed = TRUE
  )
  expect_s3_class(error, "marginplyr_error")
})

# The tests below cover the second place an empty argument reaches, and the
# reason it needed one of its own (#174). #168's walk only reads a summary
# expression; an `across()` call is taken apart and rebuilt, so an argument the
# caller omitted has to come back out in the position they left it, still
# omitted. R decides what "omitted" means and dplyr follows it: `f(a = )` takes
# the default exactly as `f()` does, so `dplyr::summarise()` is the oracle for
# every shape here rather than a rule restated in this package.
#
# One difference from dplyr is deliberate and is not a shape omitted below.
# dplyr deprecated an omitted `.cols` and warns about it, while a summary
# staged here reaches dplyr with its selection already resolved to an
# `all_of()` literal -- the invariant `native_summary_output_names()` depends
# on -- so the call dplyr finally sees omits nothing and its lifecycle warning
# does not fire. That is what the omitted spelling `across(.fns = sum)` has
# always done here, and the empty spelling now matches it. The columns and the
# values are dplyr's either way, which is what these assert.

test_that("an omitted `across()` selection keeps the columns dplyr selects", {
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )

  result <- summarize_with_margins(
    data,
    dplyr::across(, sum),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  # Named and positional spellings of the same omission. R matches `.cols` by
  # name here and finds it empty, which is the same missing argument the
  # positional spelling leaves in the first position.
  named <- summarize_with_margins(
    data,
    dplyr::across(.cols = , .fns = sum),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expected <- suppressWarnings(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(dplyr::across(, sum), .groups = "drop")
  )

  expect_equal(
    dplyr::arrange(result[!is.na(result$region), ], region),
    as.data.frame(expected),
    ignore_attr = "row.names"
  )
  expect_equal(named, result)

  # The one difference from dplyr, asserted rather than left to the comment
  # above. Raising the deprecation is what makes it visible: dplyr refuses
  # `across(, sum)` outright there, while the summary staged here reaches
  # dplyr with `.cols` resolved to an `all_of()` literal, so nothing about the
  # call it finally sees is deprecated. A change that let the signal through
  # fails here and sends its author to that comment rather than passing
  # silently in either direction.
  with_deprecation_errors({
    expect_error(
      data |>
        dplyr::group_by(region) |>
        dplyr::summarise(dplyr::across(, sum), .groups = "drop"),
      "across()",
      fixed = TRUE
    )
    expect_equal(
      summarize_with_margins(
        data,
        dplyr::across(, sum),
        .grouping = rollup(region),
        .margin_label = NULL
      ),
      result
    )
  })
})

test_that("an omitted `across()` function keeps the columns dplyr returns", {
  # An omitted `.fns` is the identity, so every group must hold one row for the
  # result to be a summary at all -- which is dplyr's rule, not this package's,
  # and the reason the plan below carries a single grouping set.
  data <- data.frame(
    region = c("East", "West"),
    units = c(1, 3),
    revenue = c(2, 4)
  )
  plan <- grouping_sets(grouping_set(region))

  result <- summarize_with_margins(
    data,
    dplyr::across(units, ),
    .grouping = plan
  )
  # `{.fn}` expands to `"1"` for an omitted `.fns`, exactly as it does for a
  # `.fns` that is a single unnamed function.
  templated <- summarize_with_margins(
    data,
    dplyr::across(c(units, revenue), , .names = "{.col}_{.fn}"),
    .grouping = plan
  )

  expected <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(dplyr::across(units, ), .groups = "drop")
  expected_templated <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      dplyr::across(c(units, revenue), , .names = "{.col}_{.fn}"),
      .groups = "drop"
    )

  expect_equal(result, as.data.frame(expected), ignore_attr = "row.names")
  expect_equal(
    templated,
    as.data.frame(expected_templated),
    ignore_attr = "row.names"
  )
})

test_that("an omitted `across()` argument leaves the positions around it", {
  # The surrounding arguments are what a rebuild that drops or appends gets
  # wrong: an argument removed from the middle moves every positional argument
  # after it into a formal that is not its own.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )
  summarize <- function(...) {
    summarize_with_margins(
      data,
      ...,
      .grouping = rollup(region),
      .margin_label = NULL
    )
  }
  expected <- function(...) {
    as.data.frame(suppressWarnings(
      data |>
        dplyr::group_by(region) |>
        dplyr::summarise(..., .groups = "drop")
    ))
  }
  without_margins <- function(result) {
    dplyr::arrange(result[!is.na(result$region), ], region)
  }

  # An omitted `.cols` before a `.names` template that names what follows it.
  expect_equal(
    without_margins(summarize(dplyr::across(, sum, .names = "{.col}_total"))),
    expected(dplyr::across(, sum, .names = "{.col}_total")),
    ignore_attr = "row.names"
  )
  # An omitted `.names` after a selection and a function, both positional.
  expect_equal(
    without_margins(summarize(dplyr::across(c(units), sum, .names = ))),
    expected(dplyr::across(c(units), sum, .names = )),
    ignore_attr = "row.names"
  )
  # An omitted `.unpack`, whose default this package also reads.
  expect_equal(
    without_margins(summarize(dplyr::across(units, sum, .unpack = ))),
    expected(dplyr::across(units, sum, .unpack = )),
    ignore_attr = "row.names"
  )
  # Partially named: an omitted `.cols`, a positional `.fns`, and a named
  # argument that `across()` forwards to it. The forwarded argument has to
  # stay forwarded, which it does not if the omission is closed by inserting
  # an argument ahead of it. dplyr deprecated forwarding through `...`, and
  # that its warning still reaches the caller is the evidence the argument is
  # in that position after the rebuild rather than in a formal of its own.
  expect_equal(
    suppressWarnings(
      without_margins(summarize(dplyr::across(, mean, na.rm = TRUE)))
    ),
    expected(dplyr::across(, mean, na.rm = TRUE)),
    ignore_attr = "row.names"
  )
  # Collected rather than expected one at a time, since a plan of this shape
  # runs one branch per grouping set and each raises the warning of its own.
  signalled <- character()
  withCallingHandlers(
    summarize(dplyr::across(, mean, na.rm = TRUE)),
    warning = function(condition) {
      signalled <<- c(signalled, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl(
    "argument of `across()` is deprecated",
    signalled,
    fixed = TRUE
  )))
  # And with the function list named, where `{.fn}` names each output.
  expect_equal(
    without_margins(summarize(dplyr::across(, list(total = sum, m = mean)))),
    expected(dplyr::across(, list(total = sum, m = mean))),
    ignore_attr = "row.names"
  )
})

test_that("an invalid omitted `across()` argument raises dplyr's own error", {
  # An omitted `.fns` over a group of more than one row is a size error dplyr
  # itself raises, and the analysis has no fault to report: the caller must see
  # the condition their own expression produces, not a missing-argument lookup
  # from inside the rebuild (ADR-0015).
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )

  baseline <- expect_error(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(dplyr::across(units, ), .groups = "drop")
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(units, ),
      .grouping = rollup(region)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_false(inherits(error, "missingArgError"))
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "must be size 1", fixed = TRUE)
})

test_that("an omitted selection in a share `across()` selects every source", {
  # The share planner reads the selection from the same parse, so an omitted
  # one has to reach it as the `everything()` dplyr would have applied --
  # over the eligible preceding summaries, which is what an `across()` share
  # selects from.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )
  summarize <- function(selection) {
    rlang::inject(summarize_with_margins(
      data,
      units = sum(units),
      revenue = sum(revenue),
      dplyr::across(!!!selection, share_of_total, .names = "{.col}_share"),
      .grouping = rollup(region),
      .margin_label = NULL
    ))
  }

  expect_equal(
    summarize(list(rlang::missing_arg())),
    summarize(list(quote(dplyr::everything())))
  )
})

test_that("an omitted share `.fns` is refused by name, not by lookup", {
  # A share helper written past an omitted `.fns` is not a `.fns` at all, and
  # the diagnostic that says so is the one the caller can act on.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      units = sum(units),
      dplyr::across(units, , share_of_total, .names = "{.col}_share"),
      .grouping = rollup(region)
    ),
    "`.fns` must be",
    fixed = TRUE
  )
  expect_s3_class(error, "marginplyr_error")
  expect_false(inherits(error, "missingArgError"))
})

test_that("an empty argument answers as omitted wherever the walk reads one", {
  # `across()` was the reconstruction path #174 was filed for, and the audit
  # that fixed it found the same read in four more places, each reached by an
  # expression R and dplyr both accept: a name position (`rm(x, )`), an
  # injected output name, the `.data` pronoun's index, and the target of a
  # binding written as a call. All five bound a part of the caller's call to a
  # name, which is `missingArgError` on the first read of it (#168), and the
  # empty argument passes `rlang::is_symbol()` besides, so the name `""` was
  # what the two survivors reported.
  #
  # Each expression is read into a variable before it is asserted on, because
  # `expect_identical()` quotes its own first argument and rlang's quotation
  # walks what it finds there -- an expression carrying an empty argument
  # aborts in the expectation rather than in what it is testing.
  read <- function(expr) expression_data_symbols(expr)

  subset <- read(quote(sum(value[])))
  pronoun <- read(quote(sum(.data[[, 1]])))
  # `` `for`(, seq, body) `` and `` `<-`(, value) `` are calls R evaluates
  # without complaint, so the walk has to answer them: neither binds a name,
  # and the parts around the empty target are read as they always were.
  looped <- read(quote({
    `for`(, share, NULL)
    1
  }))
  assigned <- read(quote({
    `<-`(, share)
    2
  }))
  # An `rm()` whose argument is empty removes nothing readable, so the bound
  # set empties and the read after it is reported -- the over-reporting side
  # this walk is required to be wrong on (#162).
  removed <- read(quote({
    x <- share
    rm(x, )
    x
  }))
  injected <- known_injected_argument_name(quote(`:=`(, 1)))
  # The contrast, in both spellings of a name this can read: the empty
  # argument has to fall through the same branch a readable name is taken by,
  # rather than the branch taking it under the name `""`.
  injected_symbol <- known_injected_argument_name(quote(x := 1))
  injected_string <- known_injected_argument_name(quote("x" := 1))

  expect_identical(subset, "value")
  expect_identical(pronoun, character())
  expect_identical(looped, "share")
  expect_identical(assigned, "share")
  expect_identical(removed, c("share", "x"))
  expect_identical(injected, "")
  expect_identical(injected_symbol, "x")
  expect_identical(injected_string, "x")
})

test_that("an empty argument outside `across()` evaluates as dplyr evaluates", {
  # The end-to-end half of the same audit, with dplyr as the oracle: whatever
  # each expression does in `dplyr::summarise()` is what it must do here,
  # whether that is a result or the error the caller's own code produces.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # A name a `tibble()` builds from an empty argument is a name dplyr accepts,
  # so a result is what both sides must return.
  injected <- summarize_with_margins(
    data,
    tibble::tibble(`:=`(, 1)),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expected <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(tibble::tibble(`:=`(, 1)), .groups = "drop")
  expect_equal(
    dplyr::arrange(injected[!is.na(injected$region), ], region),
    as.data.frame(expected),
    ignore_attr = "row.names"
  )

  # `rm(x, )` is refused by `rm()` itself, so the caller must see that refusal
  # rather than a missing-argument lookup from inside the walk.
  baseline <- expect_error(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(
        derived = {
          x <- 2
          rm(x, )
          5
        },
        .groups = "drop"
      )
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      derived = {
        x <- 2
        rm(x, )
        5
      },
      .grouping = rollup(region)
    )
  )
  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_false(inherits(error, "missingArgError"))
  expect_match(
    conditionMessage(error$parent),
    "zero-length variable name",
    fixed = TRUE
  )
})

test_that("`parse_across_arguments()` answers an empty argument as omitted", {
  # The seam every `across()` path reads, and the one place that has to know
  # about the empty argument: each field below answers what it answers for an
  # argument that is absent, so no caller binds R's missing marker to a name
  # -- the read that raises `missingArgError` whether it is spelled `for` or
  # `<-` (#168).
  empty <- parse_across_arguments(quote(across(, , .names = , .unpack = )))
  absent <- parse_across_arguments(quote(across()))

  expect_identical(empty$cols, absent$cols)
  expect_identical(empty$fns, absent$fns)
  expect_identical(empty$names, absent$names)
  expect_identical(empty$unpack, absent$unpack)
  expect_identical(empty$additional, absent$additional)

  # The positions are the half that is not the same. An empty argument
  # occupies the formal it was written in, so the indices name it and a
  # rewrite puts its replacement back where the caller left it.
  expect_identical(empty$cols_index, 1L)
  expect_identical(empty$fns_index, 2L)
  expect_identical(empty$names_index, 3L)
  expect_identical(empty$unpack_index, 4L)
  expect_true(rlang::is_missing(empty$call_args[[1L]]))
  expect_true(rlang::is_missing(empty$call_args[[3L]]))

  # A supplied argument is unchanged by the same reading.
  supplied <- parse_across_arguments(
    quote(across(units, sum, .names = "{.col}", .unpack = FALSE))
  )
  expect_identical(supplied$cols, quote(units))
  expect_identical(supplied$fns, quote(sum))
  expect_identical(supplied$names, "{.col}")
  expect_identical(supplied$unpack, FALSE)
})

test_that("a grouping helper reads an empty argument as a non-column", {
  # The third place an empty argument reaches, found by the same audit and
  # reached by neither path above: a `grouping_id()` or `grouping_bit()` call is
  # read by `grouping_helper_vars()` rather than by the `across()` rebuild #174
  # fixed. That read tested each argument with `is.symbol()`, which the empty
  # argument passes because it is a symbol whose name is `""`, so
  # `as.character()` wrote that name into the column vector. A trailing empty
  # argument was then refused for a column missing from the plan, and two empty
  # arguments for a duplicate that exists only because both read as the same
  # name. Both conditions were already Package conditions -- what the
  # caller could not act on is a diagnostic naming a column they never wrote
  # (#181).
  data <- data.frame(
    region = c("East", "East", "West"),
    store = c("a", "b", "c"),
    value = c(1, 3, 6)
  )
  refuse <- function(error) {
    expect_s3_class(error, "marginplyr_error")
    expect_match(
      conditionMessage(error),
      "only accepts bare grouping columns",
      fixed = TRUE
    )
    error
  }

  # The answer this has to reach, asserted first because it is the baseline and
  # not a message of its own: what the same function already gives a non-column
  # argument. Nothing asserted it before, which is how the empty argument came
  # to be read as a column at all -- the branch that refuses a literal is one
  # the suite never executed.
  literal_id <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_id(1),
      .grouping = rollup(region, store)
    )
  ))
  literal_bit <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_bit("region"),
      .grouping = rollup(region, store)
    )
  ))

  trailing <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_id(region, ),
      .grouping = rollup(region, store)
    )
  ))
  leading <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_id(, region),
      .grouping = rollup(region, store)
    )
  ))
  both <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_id(, ),
      .grouping = rollup(region, store)
    )
  ))
  # `grouping_bit()` is two arguments to the parser here, so its arity check is
  # what caught this call before -- an answer that depended on which check ran
  # first, and one describing a column count rather than either of the two
  # things the caller wrote.
  bit <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_bit(, ),
      .grouping = rollup(region, store)
    )
  ))
  # Its one-argument form was refused for the same wrong reason, and reaches
  # the same answer now that the empty argument is read before the count is.
  bit_trailing <- refuse(expect_error(
    summarize_with_margins(
      data,
      b = grouping_bit(region, ),
      .grouping = rollup(region, store)
    )
  ))

  # Each empty spelling reaches its own helper's baseline exactly, which is the
  # whole of what was asked for: not a new diagnostic, but the one a caller
  # writing anything else that is not a column already gets.
  for (error in list(trailing, leading, both)) {
    expect_identical(conditionMessage(error), conditionMessage(literal_id))
  }
  for (error in list(bit, bit_trailing)) {
    expect_identical(conditionMessage(error), conditionMessage(literal_bit))
  }

  # The column vector is what these diagnostics are built from, so a message
  # naming the empty name is the direct witness that an empty argument reached
  # it.
  for (error in list(trailing, leading, both, bit, bit_trailing)) {
    expect_no_match(conditionMessage(error), "``", fixed = TRUE)
  }

  # Only the empty argument moves ahead of the arity checks. A non-column that
  # is not empty is still counted first, which is what a caller passing two of
  # anything to `grouping_bit()` needs told.
  arity <- expect_error(
    summarize_with_margins(
      data,
      b = grouping_bit(1, 2),
      .grouping = rollup(region, store)
    ),
    "requires exactly one column",
    fixed = TRUE
  )
  expect_s3_class(arity, "marginplyr_error")
})

test_that("no analysed shape reaches the caller as an untyped condition", {
  # The classes below are what each site raised before #100, and before #168 in
  # `missingArgError`'s case. Asserting their absence together keeps a future
  # rewrite that reintroduces one of them from passing on the message alone.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )
  fns <- list(total = function(x, na_rm) sum(x, na.rm = na_rm))

  errors <- list(
    call_head = tryCatch(
      summarize_with_margins(
        data,
        units = fns$total(units, TRUE),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    ),
    missing_get_name = tryCatch(
      summarize_with_margins(
        data,
        z = get(mode = "numeric"),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    ),
    across_names = tryCatch(
      summarize_with_margins(
        data,
        dplyr::across(c(units), sum, .names = "{c('x','y')}"),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    ),
    empty_argument = tryCatch(
      summarize_with_margins(
        data,
        total = sum(units[]),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    )
  )

  expect_s3_class(errors$call_head, "data.frame")
  expect_s3_class(errors$empty_argument, "data.frame")
  for (error in errors[c("missing_get_name", "across_names")]) {
    expect_s3_class(error, "condition")
    expect_false(inherits(error, "simpleError"))
    expect_false(inherits(error, "subscriptOutOfBoundsError"))
    expect_false(inherits(error, "missingArgError"))
  }
})

# The tests below all build the rejected call by injection rather than by
# writing each shape out, because the shapes are what varies and the call
# around them is not. Injecting a plain language object splices it into the
# quosure exactly as writing it there would, so the walk sees the expression
# under test and nothing else (#165 covers the quosure case).
reflective_summary <- function(data, expr) {
  # nolint start: object_usage_linter.
  # `value` and `region` are columns of the data mask the verb builds, which
  # `codetools` cannot follow into.
  rlang::inject(summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    derived = !!expr,
    .grouping = rollup(region)
  ))
  # nolint end
}

expect_share_dependency_error <- function(data, expr) {
  error <- expect_error(
    reflective_summary(data, expr),
    "Ordinary summaries cannot use an earlier Total share (`share`)",
    fixed = TRUE
  )
  expect_s3_class(error, "marginplyr_error")
}

test_that("a reflective lookup reads the name it is given", {
  # `get("share")` and `share` reach the same binding: the primitives below
  # resolve a name through ordinary lexical scope, which under a data mask
  # starts at the mask. A name handed to one as a string is therefore a mask
  # read exactly as the symbol is, and the guard against an ordinary summary
  # using an earlier share owes the caller the same diagnostic for both. Only
  # `get()` was read that way, so the other three reached the placeholder --
  # silently for two of them, and as an untyped base condition for `mget()`
  # (#173).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(expression_data_symbols(quote(get0("share"))), "share")
  expect_identical(expression_data_symbols(quote(exists("share"))), "share")
  # `mget()` takes a vector, so its names are recovered from one rather than
  # from a lone literal: a `c()` of literals is as statically known as a
  # literal is.
  expect_identical(
    expression_data_symbols(quote(mget(c("share", "units")))),
    c("share", "units")
  )
  # A bound name shadows all four for the reason it shadows `get()`: each
  # performs ordinary name resolution, which finds the binding first.
  expect_identical(
    expression_data_symbols(quote(get0("share")), bound = "share"),
    character()
  )

  expect_share_dependency_error(data, quote(get0("share") * 100))
  expect_share_dependency_error(data, quote(exists("share")))
  expect_share_dependency_error(data, quote(mget("share")[[1L]] * 100))
})

test_that("an evaluated language object is read where it is built", {
  # A name reaches the mask as a string here rather than as a symbol, so the
  # walk sees no symbol to report: `as.name("share")` builds the read that
  # `eval()` then performs. Recovering the language object is what puts these
  # under the same rule as `share`, and parsing a literal is the whole of the
  # recovery -- nothing evaluates the caller's code to find out what it reads.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(
    expression_data_symbols(quote(eval(as.name("share")))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(eval(as.symbol("share")))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(eval(str2lang("share * 2")))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(eval(parse(text = "share * 2")))),
    "share"
  )
  # `quote()` needs no recovery of its own -- the general walk already reports
  # the symbols under it -- but it reaches this branch too, so it is asserted
  # beside the others rather than left to a walk the branch now shadows.
  expect_identical(expression_data_symbols(quote(eval(quote(share)))), "share")
  # A bound name shadows the recovered read as it shadows a symbol: what
  # `eval()` resolves is the binding, not the column.
  expect_identical(
    expression_data_symbols(quote(eval(as.name("share"))), bound = "share"),
    character()
  )

  expect_share_dependency_error(data, quote(eval(as.name("share")) * 100))
  expect_share_dependency_error(data, quote(eval(as.symbol("share")) * 100))
  expect_share_dependency_error(data, quote(eval(str2lang("share * 2"))))
  expect_share_dependency_error(data, quote(eval(parse(text = "share * 2"))))
})

test_that("a lookup this walk cannot resolve reads every alias", {
  # `get(name)` names whatever the string holds at run time, which is not
  # decidable here and must not be found out by evaluating it. #130's contract
  # resolves an undecidable shape toward over-reporting, so the walk reports a
  # marker and the guard reads it as a read of every alias in scope: the call
  # is refused wherever one exists, rather than silently computing against a
  # staging placeholder.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # Read from the summary expressions below, which the linter cannot follow
  # into a data mask.
  name <- "share"
  existing <- "units"
  # nolint end

  # The argument is walked as well as recovered, because a reflective call
  # evaluates its arguments in the mask like any other call does.
  expect_identical(
    expression_data_symbols(quote(get(name))),
    c("name", unresolved_lookup_name())
  )
  expect_identical(
    expression_data_symbols(quote(eval(built))),
    c("built", unresolved_lookup_name())
  )

  expect_share_dependency_error(data, quote(get(name) * 100))
  expect_share_dependency_error(data, quote(eval(as.name(name)) * 100))
  expect_share_dependency_error(data, quote(eval(str2lang(name)) * 100))

  # Over-reporting is a claim about the aliases in scope, so a call holding
  # none is untouched: a lookup this walk cannot resolve is not a fault of its
  # own, and refusing it outright would reject legal code.
  resolved <- summarize_with_margins(
    data,
    units = sum(value),
    derived = get(existing),
    .grouping = rollup(region)
  )
  expect_identical(resolved$derived, resolved$units)
})

test_that("a lookup constrained to an external environment reads no column", {
  # The environment argument terminates the search: these four require an
  # environment there and never fall back to the mask, so a call supplying one
  # reads no column and must keep working beside a share of that name. The
  # environment expression itself is still a mask read -- it is evaluated
  # there like any argument is.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # Read from the summary expressions below, which the linter cannot follow
  # into a data mask.
  outside <- rlang::env(share = 2)
  # nolint end

  expect_identical(
    expression_data_symbols(quote(get0("share", envir = outside))),
    "outside"
  )
  expect_identical(
    expression_data_symbols(quote(exists("share", where = outside))),
    "outside"
  )
  # A second unnamed argument is the environment for each of them, whatever
  # that argument is called in the primitive's own signature.
  expect_identical(
    expression_data_symbols(quote(mget("share", outside))),
    "outside"
  )

  from_environment <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    got = get("share", envir = outside),
    got0 = get0("share", envir = outside),
    gotm = mget("share", envir = outside)[[1L]],
    there = exists("share", where = outside),
    .grouping = rollup(region)
  )
  expect_identical(unique(from_environment$got), 2)
  expect_identical(unique(from_environment$got0), 2)
  expect_identical(unique(from_environment$gotm), 2)
  expect_identical(unique(from_environment$there), TRUE)
})

test_that("a parenthesized head is still the primitive it names", {
  # `(get)("share")` calls the same primitive, and #130 recorded the shape it
  # defeats: the head is a call to `(`, so a branch matching on the call's
  # name does not see it. The parentheses are a mask read of their own --
  # `(get)` is evaluated as a value rather than through function lookup -- so
  # both answers are reported.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(
    expression_data_symbols(quote((get)("share"))),
    c("get", "share")
  )
  expect_identical(
    expression_data_symbols(quote(((get))("share"))),
    c("get", "share")
  )

  expect_share_dependency_error(data, quote((get)("share") * 100))
})

test_that("a primitive named through another primitive is still itself", {
  # `match.fun("get")`, `getFunction("get")` and `get("get")` each evaluate to
  # the same primitive, so a head spelled any of those ways performs the same
  # lookup as `get("share")` and owes the caller the same diagnostic. Each was
  # silently `NA` while the branch matched on the call's own name, which a
  # call whose head is a call does not have.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # The head is a read of its own wherever it is not a bare symbol, so the
  # name it looks the function up under is reported beside the column.
  expect_identical(
    expression_data_symbols(quote(match.fun("get")("share"))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(get("get")("share"))),
    c("get", "share")
  )

  expect_share_dependency_error(data, quote(match.fun("get")("share") * 100))
  expect_share_dependency_error(data, quote(getFunction("get")("share") * 100))
  expect_share_dependency_error(data, quote(get("get")("share") * 100))
  expect_share_dependency_error(data, quote(get0("get0")("share") * 100))
})

test_that("`do.call()` of a reflective primitive is an unresolved lookup", {
  # `do.call()` runs the call it builds in the caller's environment, which
  # under a data mask is the mask, so a primitive it names looks a column up
  # there. What that primitive is handed is a list built at run time, so the
  # name is not recoverable and the marker is the answer -- as it is for any
  # lookup this walk cannot resolve.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(
    expression_data_symbols(quote(do.call("get", list("share")))),
    unresolved_lookup_name()
  )
  expect_identical(
    expression_data_symbols(quote(do.call(get, list("share")))),
    c("get", unresolved_lookup_name())
  )
  # Qualified, which is the one position left where a `::` names the primitive
  # to the walk rather than to the call's own name read: a `base::get()` *call*
  # is named `get` by the shared reader, through the parentheses #178 made
  # transparent as well as without them, so it never reaches the callee
  # reading. Handed to `do.call()` it is a value, and reading it is what makes
  # this an unresolved lookup rather than a `do.call()` of something ordinary.
  expect_identical(
    expression_data_symbols(quote(do.call(base::get, list("share")))),
    unresolved_lookup_name()
  )
  # A `do.call()` of anything else is left alone: its arguments are values by
  # the time it runs, and the walk has reported the expressions that built
  # them.
  expect_identical(
    expression_data_symbols(quote(do.call("sum", list(value)))),
    "value"
  )

  expect_share_dependency_error(
    data,
    quote(do.call("get", list("share")) * 100)
  )
  expect_share_dependency_error(data, quote(do.call(get, list("share")) * 100))

  legal <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    doubled = do.call("sum", list(value)) * 2,
    .grouping = rollup(region)
  )
  expect_identical(legal$doubled, c(8, 12, 20))
})

test_that("a primitive reached as a value is out of the walk's reach", {
  # `sapply(c("share"), get)` hands the primitive on as a value, and the
  # environment it then searches from is the frame inside `sapply()` rather
  # than the mask -- so it raises instead of reading the placeholder, and the
  # walk owes it nothing. This is asserted rather than assumed because the
  # comment at `is_reflective_lookup()` rests on it: were it to reach the
  # mask, the shape would be a silent miss of exactly the kind #173 removes.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = sapply(c("share"), get)[[1L]],
      .grouping = rollup(region)
    )
  )
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "'share' not found")
})

test_that("a reflective alias of an earlier summary is not a share source", {
  # The other direction of the same rule. A share source must be an ordinary
  # summary that depends on nothing earlier, and an alias built reflectively
  # depends on exactly what the spelled-out alias does: the dependency is the
  # read, however the name reached the mask.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # Read from the summary expression below, which the linter cannot follow
  # into a data mask.
  name <- "units"
  # nolint end
  aliased <- function(expr) {
    rlang::inject(summarize_with_margins(
      data,
      units = sum(value),
      alias = !!expr,
      share = share_of_total(alias),
      .grouping = rollup(region)
    ))
  }

  for (expr in list(
    quote(get0("units")),
    quote(mget("units")[[1L]]),
    quote(eval(as.name("units"))),
    quote(eval(parse(text = "units")))
  )) {
    error <- expect_error(
      aliased(expr),
      paste0(
        "Total share `share` cannot use source summary `alias` because it ",
        "depends on earlier summary alias `units`"
      ),
      fixed = TRUE
    )
    expect_s3_class(error, "marginplyr_error")
  }

  # An unresolvable lookup reaches the same refusal through the over-report:
  # every alias in scope is a dependency, so the alias is no source.
  unresolved <- expect_error(
    aliased(quote(get(name))),
    "cannot use source summary `alias`",
    fixed = TRUE
  )
  expect_s3_class(unresolved, "marginplyr_error")
})

test_that("a lazy input is refused at planning like a local one", {
  # The decision is made while the call is planned, before any backend sees a
  # query, so the two paths owe the caller the same condition. A lazy frame
  # that reached execution would raise whatever its backend makes of a
  # placeholder instead, which is the difference this asserts away.
  #
  # No dialect is simulated and none is needed: nothing here renders SQL, so
  # the test needs no optional driver package and runs in every configuration
  # rather than skipping wherever one is absent.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  lazy <- dbplyr::tbl_lazy(data)

  expect_share_dependency_error(lazy, quote(get0("share") * 100))

  source <- expect_error(
    rlang::inject(summarize_with_margins(
      lazy,
      units = sum(value),
      alias = eval(as.name("units")),
      share = share_of_total(alias),
      .grouping = rollup(region)
    )),
    "cannot use source summary `alias`",
    fixed = TRUE
  )
  expect_s3_class(source, "marginplyr_error")
})

test_that("a name the recovery cannot read fails closed, not through", {
  # Each shape here is one the recovery does not read: a literal that is not a
  # name, a vector holding something other than literals, text that does not
  # parse, a constructor this walk does not recognize, a head with no name of
  # its own. All of them resolve to the marker rather than to silence, which
  # is the direction #130 fixed and the one the guard depends on.
  expect_identical(
    expression_data_symbols(quote(get(NA_character_))),
    unresolved_lookup_name()
  )
  expect_identical(
    expression_data_symbols(quote(mget(c("share", name)))),
    c("name", unresolved_lookup_name())
  )
  expect_identical(
    expression_data_symbols(quote(eval(str2lang("share +")))),
    unresolved_lookup_name()
  )
  expect_identical(
    expression_data_symbols(quote(eval(as.name()))),
    unresolved_lookup_name()
  )
  expect_identical(
    expression_data_symbols(quote(eval(str2lang()))),
    unresolved_lookup_name()
  )
  # `bquote()` substitutes an expression through `.()`, which this walk cannot
  # see, so it is unrecognized rather than recovered -- and the symbols under
  # it are still reported by the walk of the parts.
  expect_identical(
    expression_data_symbols(quote(eval(bquote(share)))),
    c("share", unresolved_lookup_name())
  )
  expect_identical(
    expression_data_symbols(quote(eval(fns$build()))),
    c("fns", unresolved_lookup_name())
  )
})

test_that("a name the recovery can read is read, however it is spelled", {
  # The other half of the same table: shapes that are statically knowable, and
  # the two that are knowable to read nothing at all.
  expect_identical(
    expression_data_symbols(quote(eval(expression(share)))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(eval(str2expression("share * 2")))),
    "share"
  )
  # A namespace qualifier under the parentheses names the same primitive, as
  # it does without them.
  expect_identical(
    expression_data_symbols(quote((base::get)("share"))),
    "share"
  )
  # The name itself may be written inside parentheses, which is the value half
  # of the reading #178 gives a head: `("share")` is the string it wraps, so
  # the lookup resolves rather than being reported as a read of every alias in
  # scope, which is what an unreadable name answers.
  expect_identical(expression_data_symbols(quote(get(("share")))), "share")
  expect_identical(expression_data_symbols(quote(get((("share"))))), "share")
  expect_identical(
    expression_data_symbols(quote(get((c("sha", "re"))))),
    c("sha", "re")
  )
  # A string is not a language object: `eval()` answers the string itself and
  # looks nothing up.
  expect_identical(expression_data_symbols(quote(eval("share"))), character())
  # `c()` of nothing names nothing, so there is nothing to report and nothing
  # unresolved either.
  expect_identical(expression_data_symbols(quote(get(c()))), character())
})

test_that("a head that names no function leaves the call unrecognized", {
  # A shape the analysis does not recognize falls through and evaluates, which
  # is ADR-0015's answer wherever no fault of the analysis is involved: the
  # walk reports the parts and claims nothing about what the call resolves.
  expect_identical(
    expression_data_symbols(quote(match.fun()("share"))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(match.fun(name)("share"))),
    "name"
  )
  expect_identical(
    expression_data_symbols(quote(do.call(1L, list("share")))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(do.call(args = list("share")))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(eval(envir = outside))),
    "outside"
  )
})

test_that("an unnamed argument list still answers one name per argument", {
  # `rlang::call_args()` names every argument, empty where the caller passed
  # one positionally, so the walk never reaches the fallback. It is what keeps
  # the reading correct for a list that carries no names at all: `match("",
  # NULL)` and `sum(NULL == "")` both answer 0 rather than failing, so a
  # missing name vector would read as a call with no positional arguments and
  # an environment argument beside a name would go unseen.
  expect_identical(argument_names(list(1, 2)), c("", ""))
  expect_identical(argument_names(list(a = 1, 2)), c("a", ""))
})

# The tests below cover the boundary R draws between an expression the data
# mask evaluates and one a call captures as language data (#179). Every
# analysis here walked into `quote()` as if its argument were code: the
# dependency walk reported a quoted name as a read of that column, the share,
# selection, and context-helper searches found helpers no caller had asked
# for, and the two rewrites replaced the quoted object with what the helper
# compiles to -- so `deparse1(quote(grouping_bit(region)))` answered `"0L"`
# and a summary holding `quote(share_of_total(units))` was refused as a share
# helper written in the wrong position.
#
# The rule the analyses follow is R's own. A captured argument is data until
# something evaluates it; an operand the capturing call does evaluate --
# `substitute()`'s `env` -- is code like any other; and a captured expression
# handed to `eval()` is the read it performs, which is what keeps #173's
# dependency rule from being escaped by spelling a share `eval(quote(share))`.
#
# Only a capture written plainly is recognized, `quote()` or `base::quote()`.
# Every other spelling falls through to the walk, which reports the symbols
# under it. That direction is deliberate: reading a capture where there is
# none costs a diagnostic, while missing one is the silent miss #130 fixed the
# walk to prevent, and no static reading of `pkg::quote()` or of a computed
# head can tell which of the two it is.

test_that("a quoted expression is data at every analysis that walks one", {
  proxy <- data.frame(value = double(), other = double())
  env <- rlang::current_env()

  # The dependency walk. A quoted name is not a read of the column, and the
  # arguments beside the quoted one are still walked.
  expect_identical(expression_data_symbols(quote(quote(share))), character())
  expect_identical(
    expression_data_symbols(quote(deparse1(quote(share * units)))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(list(quote(share), value))),
    "value"
  )
  # A quoted name reaches no branch of the walk that resolves one, either: the
  # pronoun, the reflective primitives, and the binding constructs are all
  # code the mask never runs here.
  expect_identical(
    expression_data_symbols(quote(quote(.data$share))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(quote(get("share")))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(quote({
      tmp <- share
      tmp
    }))),
    character()
  )

  # The share search, which decides both whether a summary requests a share
  # and whether it is refused for writing a helper in a position that is not
  # the complete right-hand side.
  expect_null(share_expression_kind(quote(deparse1(quote(share_of_total(x))))))
  expect_false(contains_share_helper(quote(quote(share_of_parent(x)))))

  # The context-helper search, whose finding is a refusal of the whole call.
  expect_identical(
    find_summary_context_helpers(quote(deparse1(quote(dplyr::cur_group())))),
    character()
  )
  # The predicate search, which refuses a share `across()` naming `where()`.
  expect_false(contains_selection_predicate(quote(quote(where(is.numeric)))))

  # The selection rewrite gives back the quoted object the caller wrote, down
  # to the argument names inside it: the walk descends past a capture into the
  # parts beside it, so the rebuilt call must carry the tags of the ones it
  # did not rewrite.
  quoted_selection <- quote(paste(
    deparse1(quote(dplyr::across(value, mean, .names = "{.col}"))),
    collapse = ""
  ))
  expect_identical(
    rewrite_summary_selections(
      quoted_selection,
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quoted_selection
  )
  # A node the caller injected keeps its class and its environment across the
  # rebuild, as #165 requires of every walk -- and here without the walk ever
  # reading it, since a captured part is given back as the object it is.
  injected <- rlang::quo(dplyr::across(value, mean))
  captured <- rlang::call2("quote", injected)
  expect_identical(
    rewrite_summary_selections(
      captured,
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    captured
  )

  # The selection beside the quoted one is still resolved.
  expect_identical(
    rewrite_summary_selections(
      quote(list(
        quote(dplyr::across(value, mean)),
        dplyr::across(value, mean)
      )),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote(list(
      quote(dplyr::across(value, mean)),
      dplyr::across(dplyr::all_of("value"), mean)
    ))
  )
})

test_that("`substitute()` protects what it captures and reads what it runs", {
  # `substitute()` captures its first argument and evaluates its second, so
  # the two halves of one call get opposite answers. Under a data mask the
  # capture reads nothing: dplyr binds a column and an earlier summary alike
  # so that `substitute(share)` answers the symbol rather than the value --
  # measured, not assumed, which is why the bare form is protected at all.
  expect_identical(
    expression_data_symbols(quote(substitute(share))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(substitute(expr = share))),
    character()
  )
  # The `env` argument is evaluated in the mask like any other operand.
  expect_identical(
    expression_data_symbols(quote(substitute(share, list(x = other)))),
    "other"
  )
  expect_identical(
    expression_data_symbols(quote(substitute(share, env = mask))),
    "mask"
  )
})

test_that("a capture the walk cannot name plainly is analyzed, not assumed", {
  # A namespace qualifier naming base is the same primitive; one naming
  # anything else is a function this walk knows nothing about, and a computed
  # head is the shape it cannot name at all. Both fall through and report the
  # symbols under them, which over-reports rather than protecting an
  # expression that may well be evaluated.
  expect_identical(
    expression_data_symbols(quote(base::quote(share))),
    character()
  )
  expect_identical(expression_data_symbols(quote(pkg::quote(share))), "share")
  # A parenthesized head is evaluated in the mask like any other operand, so
  # the name of the primitive is itself a read there -- the answer the walk has
  # given every head that is not a bare symbol since #130.
  expect_identical(
    expression_data_symbols(quote((quote)(share))),
    c("quote", "share")
  )
  # It stays that answer under the parenthesis reading #178 added, and that is
  # the one place two static rules read the same pair of parentheses
  # differently. A Contextual helper is recognized through them because its
  # meaning never comes from the calling environment; a capture is refused
  # through them because its meaning does -- `(quote)` is evaluated as a value,
  # so R's function lookup never runs and any binding wins, which is the head
  # this walk may not claim.
  expect_identical(
    static_spelling_name(quote((pick)(units)), "selection"),
    "pick"
  )
  expect_identical(
    expression_data_symbols(quote(((base::quote))(share))),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote((base::quote)(share))),
    "share"
  )
  # And the pair around the *call* is not a head at all, so the capture inside
  # it is still a capture.
  expect_identical(
    expression_data_symbols(quote((quote(share)))),
    character()
  )
  # A call carrying more arguments than the primitive takes is not the shape
  # this recognizes either: R refuses `quote(a, b)` when it runs, and the walk
  # reports the argument beyond the captured one rather than claiming it is
  # data.
  expect_identical(expression_data_symbols(quote(quote(share, units))), "units")

  # A name the expression itself binds may be the function the call reaches,
  # and a binding is the one shadowing this walk can see. R's function lookup
  # would skip a non-function binding and find `base::quote()` anyway, which
  # makes the shape undecidable, so it is answered in the direction that
  # reports the read: a summary that binds `quote` and then calls it is walked
  # as the call to its own function it may well be.
  expect_identical(
    expression_data_symbols(quote({
      quote <- function(e) e
      quote(share)
    })),
    "share"
  )
  expect_identical(
    expression_data_symbols(quote(quote(share)), bound = "quote"),
    "share"
  )
  # A qualified head is out of reach of any binding, so it keeps its capture.
  expect_identical(
    expression_data_symbols(quote({
      quote <- function(e) e
      base::quote(share)
    })),
    character()
  )

  # `evalq()` captures its first argument and then evaluates it, so it is no
  # boundary: the walk reports the symbols under it, as it always has.
  expect_identical(expression_data_symbols(quote(evalq(share))), "share")
  # `bquote()` captures too, but `.()` substitutes an expression evaluated in
  # the enclosing frame -- the mask -- so `bquote(f(.(share)))` really does
  # read the share. The whole call stays analyzed rather than being read for
  # the parts of it that are data.
  expect_identical(expression_data_symbols(quote(bquote(share))), "share")
  expect_identical(
    expression_data_symbols(quote(bquote(f(.(share))))),
    "share"
  )
})

test_that("evaluating a captured expression is the read it performs", {
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  # What the capture branch stops reporting, the evaluation branch recovers:
  # `eval()` runs the language it is handed in the mask, so the names under a
  # recovered `quote()` are read exactly as a written symbol is.
  expect_identical(expression_data_symbols(quote(eval(quote(share)))), "share")
  expect_identical(
    expression_data_symbols(quote(eval(substitute(share)))),
    "share"
  )
  # A bound name shadows the recovered read as it shadows a symbol.
  expect_identical(
    expression_data_symbols(quote(eval(quote(share))), bound = "share"),
    character()
  )
  # A `substitute()` given an environment replaces names from it, which this
  # walk cannot read, so the language handed to `eval()` is unknown and the
  # marker says so -- beside the read of the environment argument itself.
  expect_identical(
    expression_data_symbols(quote(eval(substitute(share, mask)))),
    c("mask", unresolved_lookup_name())
  )

  expect_share_dependency_error(data, quote(eval(quote(share)) * 100))
  expect_share_dependency_error(data, quote(eval(substitute(share)) * 100))
})

test_that("a quoted helper reaches the summary as the language it is", {
  # End to end, on the local planning path. Each of these was analyzed as a
  # request the caller never made: the grouping helper compiled to its
  # branch-local constant, the context helper refused the call, the selection
  # was resolved to `all_of()`, and the share helper was refused for its
  # position (#179).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    total = sum(value),
    grouping = deparse1(quote(grouping_bit(region))),
    context = deparse1(quote(dplyr::cur_group())),
    selection = deparse1(quote(dplyr::across(value, mean))),
    helper = deparse1(quote(share_of_total(total))),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expect_identical(
    names(result),
    c("region", "total", "grouping", "context", "selection", "helper")
  )
  expect_identical(unique(result$grouping), "grouping_bit(region)")
  expect_identical(unique(result$context), "dplyr::cur_group()")
  expect_identical(unique(result$selection), "dplyr::across(value, mean)")
  expect_identical(unique(result$helper), "share_of_total(total)")

  # A language object the caller keeps is the same object on the way out,
  # rather than one carrying whatever the analysis rewrote inside it.
  kept <- summarize_with_margins(
    data,
    total = sum(value),
    call = list(quote(share_of_total(total))),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(kept$call[[1L]], quote(share_of_total(total)))
})

test_that("a quoted alias of an earlier share is not a dependency on it", {
  # The guard #130 wrote and #173 extended reports a read of an earlier share,
  # and a quoted name is not one. Refusing this call named a share the summary
  # never reads and left the caller no rewrite that keeps the quoted object.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    label = deparse1(quote(share)),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expect_identical(names(result), c("region", "units", "share", "label"))
  expect_identical(unique(result$label), "share")
})

test_that("a share source quoting an earlier alias is still self-contained", {
  # The other direction of the same rule, which #173 asserts for the reads a
  # source really makes: a source summary must depend on nothing written
  # earlier, and quoting an earlier name is not a dependency on it. Refusing
  # this named the source as depending on a summary it never reads.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    alias = sum(value),
    units = length(quote(alias)) * sum(value),
    share = share_of_total(units),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expect_identical(names(result), c("region", "alias", "units", "share"))
  expect_identical(result$units, result$alias)
  expect_identical(result$share, result$units / sum(data$value))
})

test_that("a lazy plan reads a quoted expression as data too", {
  # The same planning decisions on the lazy path, which makes them before any
  # SQL is rendered. The quoted grouping helper was compiled to the branch
  # constant of whichever grouping set was being staged -- the rendered query
  # said `deparse1(quote(GROUPING("region")))` -- while the quoted context and
  # share helpers were refused outright, so neither call reached a dialect at
  # all.
  #
  # What the dialect then does with a `quote()` call is the dialect's own
  # translation and not this package's: dbplyr has no capture rule, so it
  # builds SQL out of whatever the call holds. The helper names surviving into
  # the query are what says the analysis left the expression alone.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  query <- summarize_with_margins(
    postgres,
    total = sum(value),
    grouping = deparse1(quote(grouping_bit(region))),
    context = deparse1(quote(cur_group())),
    helper = deparse1(quote(share_of_total(value))),
    .grouping = rollup(region)
  )
  sql <- as.character(dbplyr::sql_render(query))
  expect_match(sql, "quote(grouping_bit(", fixed = TRUE)
  expect_match(sql, "quote(cur_group())", fixed = TRUE)
  expect_match(sql, "quote(share_of_total(", fixed = TRUE)

  # A share beside a quoted expression plans and renders, which is the
  # positive half: the planner stages both, and the query carries the
  # expression the caller wrote. The quoted name has to be a column here,
  # which is the same fact as the paragraph above -- dbplyr resolves what a
  # `quote()` holds rather than carrying it, so a name no column has fails at
  # build time. Round-tripping language is a local guarantee; what holds on
  # every backend is that this package's analysis leaves the expression alone.
  # A simulated connection answers no query, so it cannot say what its dialect
  # does with an ineligible share source and the default refuses the share.
  # What this asserts is the planning either side of that, so it asks for the
  # share the connection cannot vouch for.
  shared <- summarize_with_margins(
    postgres,
    units = sum(value),
    share = share_of_total(units),
    label = deparse1(quote(value)),
    .grouping = rollup(region),
    .check_share_source = FALSE
  )
  expect_s3_class(shared, "tbl_lazy")
  expect_match(
    as.character(dbplyr::sql_render(shared)),
    "quote(\"value\")",
    fixed = TRUE
  )

  # A quoted alias of an earlier share is no longer refused here either. It
  # does not execute: dbplyr's own rule about a name created in the same
  # `summarise()` reads the quoted symbol as a use of it and says so, in its
  # own class. That is the External condition ADR-0015 keeps intact, and the
  # assertion is written so that it holds whichever way dbplyr answers -- what
  # this ticket changed is that the refusal is not marginplyr's.
  planned <- tryCatch(
    summarize_with_margins(
      postgres,
      units = sum(value),
      share = share_of_total(units),
      label = deparse1(quote(share)),
      .grouping = rollup(region),
      .check_share_source = FALSE
    ),
    error = function(cnd) cnd
  )
  expect_false(inherits(planned, "marginplyr_error"))
})

# The boundary has two sides, and the first pass drew only one of them. A
# capture stopped being analyzed everywhere, including where an `eval()`
# evaluates it, so `eval(quote(cur_group_id()))` ran and answered a
# branch-local identifier -- the value that guard exists to refuse, now
# returned silently -- while `eval(quote(share_of_total(total)))` reached the
# helper itself, which reports a Grouping plan the caller already has, and
# `eval(quote(grouping_bit(region)))` stopped compiling to its branch constant
# and reported that it works only inside the verb it was inside.
#
# The dependency walk never had that hole: #173 built `static_language_values()`
# to recover the language `eval()` runs. The searches and the rewrites read the
# same recovery now, so what a capture withholds from the analysis, evaluating
# it gives back.

test_that("a capture an `eval()` runs is analyzed as the code it becomes", {
  proxy <- data.frame(value = double(), other = double())
  env <- rlang::current_env()

  # The three searches, over the language the call evaluates.
  expect_identical(
    find_summary_context_helpers(quote(eval(quote(dplyr::cur_group_id())))),
    "cur_group_id"
  )
  expect_identical(
    share_expression_kind(quote(eval(quote(share_of_total(total))))),
    "total"
  )
  expect_true(contains_selection_predicate(quote(eval(quote(where(
    is.numeric
  ))))))

  # Whatever the recovery can read reaches them, not only a capture: a name
  # parsed from a string is language this call evaluates too, and a head that
  # names `eval()` without being a symbol names it here as it does in the
  # dependency walk.
  expect_identical(
    find_summary_context_helpers(quote(eval(str2lang("cur_group()")))),
    "cur_group"
  )
  expect_identical(
    find_summary_context_helpers(quote(eval(expression(cur_group())))),
    "cur_group"
  )
  expect_identical(
    find_summary_context_helpers(quote((eval)(quote(cur_group())))),
    "cur_group"
  )

  # The rewrite reaches inside the capture the `eval()` runs, and only there:
  # the selection is resolved as it would be without the `quote()`.
  expect_identical(
    rewrite_summary_selections(
      quote(eval(quote(dplyr::across(value, mean)))),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote(eval(quote(dplyr::across(dplyr::all_of("value"), mean))))
  )
})

test_that("evaluating a captured helper reaches the rule it always did", {
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  context <- expect_error(
    summarize_with_margins(
      data,
      gid = eval(quote(dplyr::cur_group_id())),
      .grouping = rollup(region),
      .margin_label = NULL
    ),
    "does not support `cur_group_id()`",
    fixed = TRUE
  )
  expect_s3_class(context, "marginplyr_error")

  position <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      s = eval(quote(share_of_total(total))),
      .grouping = rollup(region),
      .margin_label = NULL
    ),
    "must be the complete right-hand side of a named summary",
    fixed = TRUE
  )
  expect_s3_class(position, "marginplyr_error")

  # The grouping helpers are the case that must keep working rather than keep
  # failing: the rewrite compiles the helper inside the capture, so the
  # summary answers the branch constant it answered before this boundary
  # existed.
  compiled <- summarize_with_margins(
    data,
    bit = eval(quote(grouping_bit(region))),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(compiled$bit, c(0L, 0L, 1L))

  # And on the lazy path the dialect gets its own `GROUPING()` rather than a
  # call to a helper no backend can run.
  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  query <- summarize_with_margins(
    postgres,
    total = sum(value),
    bit = eval(quote(grouping_bit(region))),
    .grouping = rollup(region)
  )
  sql <- as.character(dbplyr::sql_render(query))
  expect_match(sql, "eval(quote(GROUPING(", fixed = TRUE)
})

test_that("`expression()` is one capture on both sides of the boundary", {
  # `static_language_values()` has recovered `expression()` for `eval()` since
  # #173, so reading it as ordinary code everywhere else made the two halves
  # of one boundary disagree about the same call: it was language where an
  # `eval()` ran it and a share helper written in the wrong position where
  # nothing did.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  expect_identical(
    expression_data_symbols(quote(expression(share))),
    character()
  )
  expect_identical(
    expression_data_symbols(quote(expression(share, units))),
    character()
  )
  expect_null(share_expression_kind(quote(expression(share_of_total(x)))))
  # The recovery is unchanged, and is now the only thing that reports it.
  expect_identical(
    expression_data_symbols(quote(eval(expression(share)))),
    "share"
  )

  carried <- summarize_with_margins(
    data,
    total = sum(value),
    e = deparse1(expression(share_of_total(total))),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_identical(unique(carried$e), "expression(share_of_total(total))")
})

test_that("only the walk that tracks bindings reads a shadowed capture", {
  # One reading of the capture, told what each caller knows.
  # `captured_call_parts()` is where a bound name stops being read as the
  # primitive it spells, and the share dependency walk is the only analysis
  # with a bound set to hand it -- the only one that tracks bindings at all,
  # and the only one whose wrong answer is silence about a wrong number.
  proxy <- data.frame(value = double())
  env <- rlang::current_env()

  expect_identical(
    captured_call_parts(quote(quote(share)), bound = "quote"),
    FALSE
  )
  expect_identical(captured_call_parts(quote(quote(share))), TRUE)

  # The searches and the rewrites pass no bound set, which is the reading they
  # already give every name they match: a locally bound `across` is resolved
  # as a selection, and a locally bound `cur_group` is refused as the
  # branch-local helper. A shadowed capture is read the same scope-blind way,
  # so the answer is a diagnostic or an uncompiled helper rather than a silent
  # value.
  expect_identical(
    rewrite_summary_selections(
      quote({
        across <- function(...) 1
        dplyr::across(value, mean)
      }),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote({
      across <- function(...) 1
      dplyr::across(dplyr::all_of("value"), mean)
    })
  )
  expect_identical(
    find_summary_context_helpers(quote({
      cur_group <- function() 1
      cur_group()
    })),
    "cur_group"
  )
  expect_identical(
    find_summary_context_helpers(quote({
      quote <- function(e) e
      quote(cur_group())
    })),
    character()
  )
})

test_that("a rewrite opens only the language a search can read", {
  # The rewrite and the searches read one index. `substitute()` given an
  # environment substitutes from it, which this analysis cannot read, so what
  # reaches the mask is unknown: the searches contribute nothing about it and
  # the rewrite leaves it alone rather than compiling a helper nothing else
  # can see. Without the environment the language is the expression as
  # written, and both halves read it.
  proxy <- data.frame(value = double())
  env <- rlang::current_env()

  unreadable <- quote(eval(substitute(dplyr::across(value, mean), env)))
  expect_identical(
    rewrite_summary_selections(
      unreadable,
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    unreadable
  )
  expect_identical(
    find_summary_context_helpers(
      quote(eval(substitute(dplyr::cur_group(), env)))
    ),
    character()
  )

  expect_identical(
    rewrite_summary_selections(
      quote(eval(substitute(dplyr::across(value, mean)))),
      env = env,
      data_proxy = proxy,
      normalize_across_names = FALSE
    ),
    quote(eval(substitute(dplyr::across(dplyr::all_of("value"), mean))))
  )
  expect_identical(
    find_summary_context_helpers(quote(eval(substitute(dplyr::cur_group())))),
    "cur_group"
  )
})

test_that("a formula a capture carries keeps its class and environment", {
  # #165's rule reaches the captured part too, and by a stronger route: the
  # rewrite gives back the object rather than rebuilding it, so a formula the
  # caller injected inside a `quote()` keeps the `.Environment` a lambda
  # resolves against. Asserted end to end, because a flattened one still
  # answers `TRUE` to `rlang::is_formula()` and shows only where the class is
  # used.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  written_in <- rlang::env(offset = 100)
  lambda <- rlang::new_formula(NULL, quote(.x + offset), env = written_in)

  result <- summarize_with_margins(
    data,
    units = sum(value),
    kept = identical(attr(quote(!!lambda), ".Environment"), !!written_in),
    applied = rlang::as_function(quote(!!lambda))(units),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expect_true(all(result$kept))
  expect_identical(result$applied, result$units + 100)
})
