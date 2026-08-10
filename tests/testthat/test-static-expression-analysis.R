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

  # The three reads #163 names, and the two-sided formula that is the one
  # shape `call_name()` did answer as `~`.
  expect_null(static_call_name(quote(~ .data$share)))
  expect_null(static_call_name(quote(~ get("share"))))
  expect_null(static_call_ns(quote(~ dplyr::n())))
  expect_null(static_call_name(quote(a ~ b)))

  # An injected quosure is a `~` call as well, and gets the same answer for a
  # stronger reason than a formula does: every site reads operands from the
  # node it has just named, and a quosure answers none of those reads as the
  # call it carries. Its length is 2 whatever it holds, so a name read through
  # to the call inside would not describe the operands beside it -- which is
  # this defect rather than a fix for it.
  expect_null(static_call_name(rlang::quo(dplyr::across(value, mean))))
  expect_null(static_call_ns(rlang::quo(dplyr::n())))
  expect_identical(length(rlang::quo(dplyr::across(value, mean))), 2L)
  # A quosure carrying no call is a name this cannot read, not an error: that
  # is the shape `rlang::call_name()` aborts on.
  expect_null(static_call_name(rlang::quo(value)))
  # Anything that is not a call has no name to read either, so a site needs no
  # `rlang::is_call()` guard of its own before asking.
  expect_null(static_call_name(quote(value)))
  expect_null(static_call_ns(1L))

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
  # `derived = purrr::map_dbl(v, ~.x)` is the realistic spelling of this;
  # written against an Import it is `rlang::as_function()`, which is what
  # `map_dbl()` applies the formula through. `length(~value)` is #163's
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
  expect_match(conditionMessage(error), "`na.rm`, `..2`", fixed = TRUE)
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
  expect_match(conditionMessage(all_unnamed), "`..1`, `..2`", fixed = TRUE)

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

test_that("no analysed shape reaches the caller as an untyped condition", {
  # The classes below are what each site raised before #100. Asserting their
  # absence together keeps a future rewrite that reintroduces one of them from
  # passing on the message alone.
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
    )
  )

  expect_s3_class(errors$call_head, "data.frame")
  for (error in errors[c("missing_get_name", "across_names")]) {
    expect_s3_class(error, "condition")
    expect_false(inherits(error, "simpleError"))
    expect_false(inherits(error, "subscriptOutOfBoundsError"))
  }
})
