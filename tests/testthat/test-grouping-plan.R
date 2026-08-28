# The refusal a colliding `s` receives, asserted in full wherever it is
# asserted: the compiler tests below and the Margin verb one, which is the same
# diagnostic reaching a caller by the route they meet it on.
ambiguous_s_message <- paste0(
  "`s` is both a column of the input and a name bound to a grouping ",
  "specification, so a nested position cannot tell which one you mean.\n",
  "i For the column, write `all_of(\"s\")`.\n",
  "i For the specification, write `!!s`."
)

# One compilation, so that a test varying the specification varies nothing
# else. `.duplicates = "keep"` is what keeps a union of agreeing readings from
# being rejected for the agreement.
compile_against <- function(spec, data_vars) {
  compile_grouping_spec(
    spec,
    data_vars,
    .duplicates = "keep",
    duplicates_choices = margin_duplicates_choices
  )
}

# The nested kinds each parent kind admits, written out rather than derived, so
# that the derivation has something to be wrong against. Two tests read it for
# different purposes -- one against the derivation itself, one against the
# behaviour that follows from it -- and a wrong table fails the first, which is
# what makes sharing it safe.
admitted_nested_kinds_table <- function() {
  kinds <- names(grouping_kind_rules())
  list(
    set = character(),
    sets = kinds,
    rollup = "set",
    cube = "set",
    product = kinds
  )
}

test_that("rollup and cube compile to concrete grouping sets", {
  rollup_plan <- compile_grouping_spec(
    rollup(a, b, c),
    data_vars = c("a", "b", "c"),
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(
    rollup_plan$sets,
    list(c("a", "b", "c"), c("a", "b"), "a", character())
  )
  expect_identical(rollup_plan$set_ids, 1:4)
  expect_equal(
    unname(rollup_plan$grouping_masks),
    matrix(c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L),
      ncol = 3L,
      byrow = TRUE
    )
  )

  cube_plan <- compile_grouping_spec(
    cube(grouping_set(country, state), year),
    data_vars = c("country", "state", "year"),
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(
    cube_plan$sets,
    list(
      c("country", "state", "year"),
      c("country", "state"),
      "year",
      character()
    )
  )
})

test_that("grouping families support union, nesting, and Cartesian product", {
  vars <- c("a", "b", "c", "d")

  product <- compile_grouping_spec(
    grouping_spec(rollup(a, b), cube(c)),
    data_vars = vars,
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(
    product$sets,
    list(
      c("a", "b", "c"), c("a", "b"),
      c("a", "c"), "a", "c", character()
    )
  )

  nested <- compile_grouping_spec(
    grouping_sets(rollup(a, b), cube(c, d)),
    data_vars = vars,
    .duplicates = "drop",
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(
    nested$sets,
    list(c("a", "b"), "a", character(), c("c", "d"), "c", "d")
  )
})

test_that("grouping specification kinds enforce the nesting grammar", {
  nested_calls <- list(
    set = quote(grouping_set(a)),
    sets = quote(grouping_sets(grouping_set(a))),
    rollup = quote(rollup(a)),
    cube = quote(cube(a)),
    product = quote(grouping_spec(grouping_set(a)))
  )
  allowed_nested <- list(
    set = character(),
    sets = names(nested_calls),
    rollup = "set",
    cube = "set",
    product = names(nested_calls)
  )
  nesting_errors <- c(
    set = paste0(
      "A `grouping_set()` can contain columns, not another ",
      "grouping family."
    ),
    rollup = paste0(
      "`rollup()` only accepts columns or `grouping_set()` ",
      "composite dimensions."
    ),
    cube = paste0(
      "`cube()` only accepts columns or `grouping_set()` ",
      "composite dimensions."
    )
  )
  constructors <- c(
    set = "grouping_set",
    sets = "grouping_sets",
    rollup = "rollup",
    cube = "cube",
    product = "grouping_spec"
  )

  compile_nested <- function(parent, child) {
    spec <- eval(
      rlang::call2(constructors[[parent]], nested_calls[[child]])
    )
    compile_grouping_spec(
      spec,
      "a",
      .duplicates = "keep",
      duplicates_choices = margin_duplicates_choices
    )
  }

  for (parent in names(constructors)) {
    for (child in names(nested_calls)) {
      if (child %in% allowed_nested[[parent]]) {
        expect_no_error(compile_nested(parent, child))
        next
      }

      error <- expect_error(compile_nested(parent, child))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(conditionMessage(error), nesting_errors[[parent]])
    }
  }

  for (constructor in constructors) {
    spec <- eval(rlang::call2(constructor, rlang::sym("a")))
    expect_no_error(
      compile_grouping_spec(
        spec,
        "a",
        .duplicates = "keep",
        duplicates_choices = margin_duplicates_choices
      )
    )
  }
})

test_that("a nested specification position recognizes a spelling or a name", {
  data_vars <- c("region", "grade", "value")
  spec_from_caller <- function(...) rollup(...)
  compile <- function(spec) {
    compile_grouping_spec(
      spec,
      data_vars,
      .duplicates = "keep",
      duplicates_choices = margin_duplicates_choices
    )
  }

  # The two recognized forms resolve to the same family, so the name is a
  # complete substitute for the spelling and not a narrower one.
  from_spelling <- compile(grouping_sets(rollup(region), grade))
  bound <- rollup(region)
  expect_equal(compile(grouping_sets(bound, grade))$sets, from_spelling$sets)

  # A caller's own function is neither, so the value it returns arrives where a
  # column selection is expected. The position says so itself rather than
  # leaving tidyselect to report a specification as an unusable selection.
  refused <- expect_error(
    compile(grouping_sets(spec_from_caller(region), grade))
  )
  expect_s3_class(refused, "marginplyr_error")
  expect_identical(
    conditionMessage(refused),
    paste0(
      "`spec_from_caller(region)` is a grouping specification, but a nested ",
      "position recognizes one only when it is a call to `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`, or a ",
      "name bound to a specification.\n",
      "i Anything else is read as a column selection.\n",
      "i Assign the specification to a name first, then use that name here."
    )
  )

  # The workaround the diagnostic names is the one that works.
  from_caller <- spec_from_caller(region)
  expect_equal(
    compile(grouping_sets(from_caller, grade))$sets,
    from_spelling$sets
  )

  # Every constructor position reports the same rule, `grouping_set()`
  # included, where the binding the diagnostic asks for then reaches the
  # grammar error that position really has.
  constructors <- c(
    "grouping_set",
    "grouping_sets",
    "rollup",
    "cube",
    "grouping_spec"
  )
  for (constructor in constructors) {
    error <- expect_error(compile(eval(rlang::call2(
      constructor,
      quote(spec_from_caller(region))
    ))))
    expect_s3_class(error, "marginplyr_error")
    expect_match(
      conditionMessage(error),
      "is a grouping specification, but a nested position",
      fixed = TRUE
    )
  }
  bound_in_set <- expect_error(compile(grouping_set(from_caller)))
  expect_identical(
    conditionMessage(bound_in_set),
    paste0(
      "A `grouping_set()` can contain columns, not another ",
      "grouping family."
    )
  )
})

test_that("a nested column selection is unaffected by the specification rule", {
  data_vars <- c("region", "region_code", "grade", "value")
  compile <- function(spec) {
    compile_grouping_spec(
      spec,
      data_vars,
      .duplicates = "keep",
      duplicates_choices = margin_duplicates_choices
    )
  }
  selected <- c("region", "grade")

  expect_equal(
    compile(grouping_sets(tidyselect::starts_with("region"), grade))$sets,
    list(c("region", "region_code"), "grade")
  )
  expect_equal(
    compile(grouping_sets(tidyselect::all_of(selected)))$sets,
    list(c("region", "grade"))
  )
  expect_equal(
    compile(grouping_sets(-c(region_code, value), grade))$sets,
    list(c("region", "grade"), "grade")
  )
  expect_equal(
    compile(grouping_spec(c(region, grade), tidyselect::last_col()))$sets,
    list(c("region", "grade", "value"))
  )

  # A selection that fails is still tidyselect's own report, unchanged.
  unknown <- expect_error(compile(grouping_sets(unknown, grade)))
  expect_false(inherits(unknown, "marginplyr_error"))
  expect_match(conditionMessage(unknown), "Column `unknown` doesn't exist")

  # A specification inside a selection is not a nested specification, and the
  # position does not speak for a part of an argument it did not refuse: a
  # specification really is the wrong kind of object where `c()` puts it, and a
  # caller who bound it to a name has already done what this rule would ask.
  # That is what a selection makes of a name the data does not hold; the other
  # half is below. tidyselect deprecates the bare external vector these three
  # write, and it is not what they are here to show, so its warning is
  # suppressed rather than asserted.
  bound <- rollup(region)
  embedded <- expect_error(
    suppressWarnings(compile(grouping_sets(c(bound, grade), region)))
  )
  expect_false(inherits(embedded, "marginplyr_error"))
  expect_match(conditionMessage(embedded), "Can't select columns with `bound`")
  for (selection in list(quote(-bound), quote(tidyselect::all_of(bound)))) {
    spec <- eval(rlang::call2("grouping_sets", selection, rlang::sym("grade")))
    embedded_error <- expect_error(suppressWarnings(compile(spec)))
    expect_false(inherits(embedded_error, "marginplyr_error"))
  }

  # Where a column shares the name, tidyselect refuses nothing at all and the
  # column is what a selection means by it. The ambiguity refusal does not
  # reach inside a selection either (ADR 0026): the caller wrote a selection,
  # so the specification reading was never available to that argument.
  colliding <- rlang::env(rlang::current_env(), region = rollup(value))
  inside <- eval(quote(grouping_sets(c(region, grade))), envir = colliding)
  expect_equal(compile(inside)$sets, list(c("region", "grade")))
})

test_that("a nested position admits nested kinds by its own parent rule", {
  # The half of "both readings are available" that the input may not answer,
  # and the one thing no behavioural test reaches: an empty answer reads as a
  # position that admits nothing rather than as a derivation that stopped
  # working, and the two are the same silence.
  kinds <- names(grouping_kind_rules())
  expect_identical(kinds, c("set", "sets", "rollup", "cube", "product"))

  # `rollup` and `cube` are what pin the stand-in's arity. Both read a
  # composite's arity as well as its kind, so a stand-in carrying no argument
  # answers `character()` for them and turns the refusal off in two of the
  # five positions.
  admitted <- admitted_nested_kinds_table()
  for (kind in kinds) {
    expect_identical(
      admitted_nested_kinds(
        new_grouping_spec(kind, list(rlang::quo(NULL))),
        find_grouping_kind_rule(kind)
      ),
      admitted[[kind]]
    )
  }

  # The answer is kept rather than recomputed, because asking costs a raised
  # Package condition for every kind the parent refuses. A memo that stopped
  # working reports nothing, so the derivation is counted directly, through a
  # rule that admits everything and a parent kind no session has asked about.
  asked <- 0L
  probe_rule <- list(
    validate_nested = function(parent, nested) {
      asked <<- asked + 1L
      invisible(NULL)
    }
  )
  probe_parent <- list(type = basename(tempfile("probe-kind-")))
  expect_identical(admitted_nested_kinds(probe_parent, probe_rule), kinds)
  expect_identical(asked, length(kinds))
  expect_identical(admitted_nested_kinds(probe_parent, probe_rule), kinds)
  expect_identical(asked, length(kinds))
})

test_that("a nested name the input and a binding both claim is refused", {
  data_vars <- c("s", "grade", "value")
  # Derived from the kind table, so a sixth kind arrives here as a cell rather
  # than as a case nothing covers.
  bindings <- list(
    set = grouping_set(value),
    sets = grouping_sets(value),
    rollup = rollup(value),
    cube = cube(value),
    product = grouping_spec(value)
  )
  expect_identical(names(bindings), names(grouping_kind_rules()))
  admitted <- admitted_nested_kinds_table()
  compile_bound <- function(parent, binding) {
    env <- rlang::env(rlang::caller_env(), s = binding)
    spec <- eval(
      rlang::call2(
        grouping_kind_rules()[[parent]]$constructor,
        rlang::sym("s")
      ),
      envir = env
    )
    compile_against(spec, data_vars)
  }

  for (parent in names(bindings)) {
    for (child in names(bindings)) {
      if (child %in% admitted[[parent]]) {
        error <- expect_error(compile_bound(parent, bindings[[child]]))
        expect_s3_class(error, "marginplyr_error")
        expect_identical(conditionMessage(error), ambiguous_s_message)
        next
      }

      # The narrowing, which is the half a later reader is likeliest to
      # "fix": where the position takes no nested specification of that kind,
      # there is nothing to be ambiguous about and the column reading stands.
      expect_identical(compile_bound(parent, bindings[[child]])$sets[[1L]], "s")
    }
  }

  # The line is the kind and nothing further. A specification of an admitted
  # kind that is invalid on its own terms is still what the caller wrote, so
  # the name is ambiguous just the same and `!!` is what reports what is wrong
  # with it. Reading arity or validity here would put them on the same footing
  # as the input, and only one of those is the caller's spelling.
  invalid <- list(grouping_sets(), rollup(), cube())
  for (parent in c("sets", "product")) {
    for (binding in invalid) {
      error <- expect_error(compile_bound(parent, binding))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(conditionMessage(error), ambiguous_s_message)
    }
  }
  for (parent in c("rollup", "cube")) {
    empty_composite <- expect_error(compile_bound(parent, grouping_set()))
    expect_s3_class(empty_composite, "marginplyr_error")
    expect_identical(conditionMessage(empty_composite), ambiguous_s_message)
  }

  # Availability is derived from the parent's own rule and from nothing
  # recursive. Preflighting the bound specification instead would let an
  # ambiguity inside it swallow the refusal outside it -- a binding that
  # raises is a binding that is not a specification -- and #255 would be
  # reachable again one level further in.
  nested <- rlang::env(rlang::current_env(), inner = rollup(value))
  nested$outer <- eval(quote(grouping_sets(inner)), envir = nested)
  swallowed <- expect_error(compile_against(
    eval(quote(grouping_sets(outer)), envir = nested),
    c("inner", "outer", "value")
  ))
  expect_s3_class(swallowed, "marginplyr_error")
  expect_match(
    conditionMessage(swallowed),
    "`outer` is both a column of the input",
    fixed = TRUE
  )
})

# The other reader of a kind nothing has validated, and the one where the
# guards' answer is not the answer: this site declines rather than refuses, so
# what a method raising took from it was a compiled call rather than a
# diagnostic (#280).
#
# Three methods, because this site asks one the guards do not. `is.na()` and
# `length()` are the guards' pair, and a kind neither can classify is not a
# kind this position admits, so the column reading stands -- the answer a
# binding that raises when it is read already gets. `as.character()` is the
# third: `%in%` dispatches through it, so a kind that classified was matched
# with the class it carries still on it, and now is not. Every shape holds
# `set` underneath, which this position admits, so the third is refused as
# ambiguous and the first two would be if they could be classified.
test_that("a colliding binding whose kind may raise loses its reading", {
  compile_colliding <- function(kind) {
    env <- rlang::env(rlang::caller_env(), s = new_grouping_spec(kind, list()))
    compile_against(
      eval(quote(grouping_sets(s)), envir = env),
      c("s", "value")
    )
  }
  raising <- function(generic) {
    kind_answering(
      stats::setNames(list(raising_kind_method), generic),
      "nested_raising"
    )
  }

  for (generic in c("is.na", "length")) {
    expect_identical(
      compile_colliding(raising(generic))$sets,
      list("s"),
      info = generic
    )
  }

  refused <- expect_error(compile_colliding(raising("as.character")))
  expect_s3_class(refused, "marginplyr_error")
  expect_identical(conditionMessage(refused), ambiguous_s_message)
})

# The other way a method takes the classification away from R: by answering,
# and answering wrongly. A catch answers a method that fails to answer, and a
# `length()` reporting `1` over two strings passes one -- so a classification
# that caught what its questions raised and then returned the value it had
# asked about returned two strings under a contract promising one, and `%in%`
# on the line after raised the untyped error the catch was put there to remove.
#
# The answer is classified a second time for that, with no class left on it to
# dispatch. The colliding reading is what holds that: delete the second
# classification and it raises `'length = 2' in coercion to 'logical(1)'`
# instead of declining.
#
# The guard is asserted too, and it is characterization rather than the same
# evidence -- it refuses either way. What refuses it without the second
# classification is `[[`, which answers nothing for an index of two elements
# and nothing for one of none, so the sentence the caller reads is the registry
# lookup's accident rather than the guard's reading. Both lengths are written
# because they are two accidents and not one. Making it the guard's own is what
# the second classification does here, and an accident that stopped holding
# would be read as this test's subject failing.
test_that("a kind whose classification lies is no more a name for it", {
  lying <- function(kind, length_answer) {
    kind_answering(
      list(
        length = function(x) length_answer,
        is.na = function(x, ...) FALSE
      ),
      paste0("nested_lying_", length_answer),
      kind = kind
    )
  }
  two_strings <- lying(c("set", "sets"), 1L)
  no_strings <- lying(character(), 1L)

  env <- rlang::env(
    rlang::current_env(),
    s = new_grouping_spec(two_strings, list())
  )
  expect_identical(
    compile_against(
      eval(quote(grouping_sets(s)), envir = env),
      c("s", "value")
    )$sets,
    list("s")
  )

  for (kind in list(two_strings, no_strings)) {
    error <- expect_error(compile_against(new_grouping_spec(kind, list()), "a"))
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      conditionMessage(error),
      "Invalid grouping specification."
    )
  }
})

test_that("the ambiguity refusal names a working spelling for each reading", {
  # Both spellings are executed rather than described, and they are read back
  # out of the diagnostic that printed them, so an advice line that stopped
  # parsing fails here. The non-syntactic names are what execute the quoting:
  # `rlang::expr_deparse()` alone writes `` `a`b` `` for the last of them,
  # which does not parse.
  spelled <- function(line) {
    gsub("^`+ ?| ?`+$", "", sub("\\.$", "", sub("^i [^,]+, write ", "", line)))
  }
  for (name in c("s", "a b", "a`b")) {
    data_vars <- c(name, "value")
    compile <- function(spec) compile_against(spec, data_vars)
    env <- rlang::env(rlang::current_env())
    rlang::env_bind(env, !!name := rollup(value))
    nested_call <- function(spelling) {
      eval(
        rlang::call2("grouping_sets", rlang::parse_expr(spelling)),
        envir = env
      )
    }

    refused <- expect_error(compile(nested_call(quoted_name_spelling(name))))
    expect_s3_class(refused, "marginplyr_error")
    lines <- strsplit(conditionMessage(refused), "\n", fixed = TRUE)[[1L]]
    expect_length(lines, 3L)

    column <- compile(nested_call(spelled(lines[[2L]])))
    expect_identical(column$sets, list(name))
    specification <- compile(nested_call(spelled(lines[[3L]])))
    expect_identical(specification$sets, list("value", character()))
  }
})

test_that("a nested name only one reading claims keeps that reading", {
  data_vars <- c("s", "grade", "value")
  compile <- function(spec) compile_against(spec, data_vars)

  # The two readings the position has always had, neither of them touched: a
  # binding the input has no column for, and a column nothing binds.
  bound_elsewhere <- rollup(value)
  expect_identical(
    compile(grouping_sets(bound_elsewhere))$sets,
    list("value", character())
  )
  expect_identical(compile(grouping_sets(s))$sets, list("s"))

  # A constructor call is read by its spelling and never by what a name is
  # bound to, so a column of that name changes nothing.
  colliding <- rlang::env(rlang::current_env(), s = rollup(value))
  expect_identical(
    compile(eval(quote(grouping_sets(rollup(value))), envir = colliding))$sets,
    list("value", character())
  )

  # A caller's own function is neither of the two recognized spellings, and a
  # call is never a bare name, so a column sharing the function's name reaches
  # nothing: #190's refusal is still what the position reports.
  spec_from_caller <- rlang::env(rlang::current_env())
  spec_from_caller$grade <- function(...) rollup(...)
  from_caller <- expect_error(
    compile(eval(quote(grouping_sets(grade(value))), envir = spec_from_caller))
  )
  expect_s3_class(from_caller, "marginplyr_error")
  expect_match(
    conditionMessage(from_caller),
    "is a grouping specification, but a nested position",
    fixed = TRUE
  )

  # A colliding name whose binding is not a specification, one whose binding
  # raises when it is read, and one carrying the class with no kind to read:
  # none of the three has a second reading available, so the column reading
  # stands. All three catches are narrow in what they decide and not in what
  # they swallow.
  not_a_spec <- rlang::env(rlang::current_env(), s = 1)
  expect_identical(
    compile(eval(quote(grouping_sets(s)), envir = not_a_spec))$sets,
    list("s")
  )
  raising <- rlang::env(rlang::current_env())
  delayedAssign(
    "s",
    stop("this binding is not a specification"),
    assign.env = raising
  )
  from_raising <- eval(quote(grouping_sets(s)), envir = raising)
  expect_identical(suppressWarnings(compile(from_raising))$sets, list("s"))
  kindless <- rlang::env(
    rlang::current_env(),
    s = structure(list(args = list()), class = "margin_grouping_spec")
  )
  expect_identical(
    compile(eval(quote(grouping_sets(s)), envir = kindless))$sets,
    list("s")
  )
  # The other shape a kind cannot be read from, and the one that raises rather
  # than answering `NULL`: `$` is invalid for an atomic vector, so reading the
  # kind is guarded as reading the binding is. Without that guard this call
  # raises `$ operator is invalid for atomic vectors`, which is #262 arriving
  # one position lower.
  atomic <- rlang::env(
    rlang::current_env(),
    s = structure(1L, class = "margin_grouping_spec")
  )
  expect_identical(
    compile(eval(quote(grouping_sets(s)), envir = atomic))$sets,
    list("s")
  )

  # Top-level `.grouping` is evaluated in the caller's environment with no
  # data mask, so it has no column-selection reading to be ambiguous with and
  # a colliding column changes nothing there.
  data <- data.frame(s = c("x", "y"), value = c(1, 2))
  top_level <- eval(
    quote(inspect_grouping(data, .grouping = s)),
    envir = colliding
  )
  expect_identical(top_level$included, c("(value)", "()"))
})

test_that("a colliding nested name is read once, in the preflight", {
  data <- data.frame(
    s = c("x", "y"),
    grade = c("a", "b"),
    value = c(1, 2)
  )
  reads <- 0L
  count_reads <- function(name, value, expr) {
    reads <<- 0L
    env <- rlang::env(rlang::caller_env())
    makeActiveBinding(
      name,
      function() {
        reads <<- reads + 1L
        value
      },
      env
    )
    result <- tryCatch(eval(expr, envir = env), error = function(cnd) cnd)
    list(reads = reads, result = result)
  }

  # The counter first: an active binding reports every read of the name, so a
  # zero below is a read that did not happen rather than a mechanism that
  # stopped counting.
  probe <- count_reads("s", 1, quote(s))
  expect_identical(probe$result, 1)
  expect_identical(probe$reads, 1L)

  # Reading the binding is what deciding costs, and there is no cheaper
  # question: which kind a name is bound to cannot be known without reading
  # it.
  refused <- count_reads(
    "s",
    rollup(value),
    quote(inspect_grouping(data, .grouping = grouping_sets(s)))
  )
  expect_s3_class(refused$result, "marginplyr_error")
  expect_identical(refused$reads, 1L)

  # The kinds a position admits are asked before the binding is read, so
  # `grouping_set()`, which admits none, reads nothing.
  in_set <- count_reads(
    "s",
    rollup(value),
    quote(inspect_grouping(data, .grouping = grouping_set(s)))
  )
  expect_identical(in_set$reads, 0L)
  expect_identical(in_set$result$included, "(s)")

  # The check sits in the preflight, which runs once for an operation and is
  # handed to the compilation passes, where the gate runs again on each.
  # Moving it into the gate would read this binding once per pass instead of
  # once in all -- three times here, by the count the last assertion in this
  # test pins -- and make that many of whatever forcing it does visible to the
  # caller.
  not_a_spec <- count_reads(
    "s",
    1,
    quote(inspect_grouping(data, .grouping = grouping_sets(s)))
  )
  expect_identical(not_a_spec$reads, 1L)
  expect_identical(not_a_spec$result$included, "(s)")

  # Once for each argument the name is written as.
  twice <- count_reads(
    "s",
    1,
    quote(inspect_grouping(
      data,
      .grouping = grouping_sets(s, s),
      .duplicates = "keep"
    ))
  )
  expect_identical(twice$reads, 2L)

  # Outside a collision nothing moves: a bound name the input holds no column
  # for is read in the preflight and once per compilation pass, as it was
  # before this refusal existed. Both counts are here because the number of
  # passes is what differs -- a plan settled by name alone is compiled against
  # the names first, so that a plan error need not wait for a backend read.
  name_only <- count_reads(
    "t",
    rollup(value),
    quote(inspect_grouping(data, .grouping = grouping_sets(t)))
  )
  expect_identical(name_only$reads, 3L)
  expect_identical(name_only$result$included, c("(value)", "()"))
  with_predicate <- count_reads(
    "t",
    rollup(value),
    quote(inspect_grouping(
      data,
      .grouping = grouping_sets(t, tidyselect::where(is.character)),
      .duplicates = "keep"
    ))
  )
  expect_identical(with_predicate$reads, 2L)
  expect_identical(
    with_predicate$result$included,
    c("(value)", "()", "(s, grade)")
  )
})

test_that("a Margin verb refuses an ambiguous nested name", {
  # What a caller sees is a result with the wrong grouping columns rather than
  # a plan object, so the defect as it was reported gets one verb case.
  data <- data.frame(s = c("x", "y"), value = c(1, 2))
  colliding <- rlang::env(rlang::current_env(), s = rollup(value))

  error <- expect_error(eval(
    quote(summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets(s)
    )),
    envir = colliding
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), ambiguous_s_message)

  bound <- eval(
    quote(summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets(!!s)
    )),
    envir = colliding
  )
  expect_identical(names(bound), c("value", "n"))
  expect_identical(nrow(bound), 3L)

  # Namespace-qualified here only because a test file is linted as package
  # code; the bare spelling the diagnostic prints is the one executed above,
  # where it is read back out of the message.
  column <- eval(
    quote(summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets(tidyselect::all_of("s"))
    )),
    envir = colliding
  )
  expect_identical(names(column), c("s", "n"))
  expect_identical(nrow(column), 2L)
})

test_that("recognizing a nested specification adds no caller evaluation", {
  data <- data.frame(
    region = c("E", "E", "W"),
    grade = c("a", "b", "a"),
    units = c(1, 3, 6)
  )

  # ADR 0008 fixes how often a caller's quosure is evaluated. The refused
  # specification is read from the failed selection rather than evaluated a
  # second time to identify it, so the caller's function still runs once.
  refused_calls <- 0L
  spec_from_caller <- function(...) {
    refused_calls <<- refused_calls + 1L
    rollup(...)
  }
  expect_error(summarize_with_margins(
    data,
    t = sum(units),
    .grouping = grouping_sets(spec_from_caller(region), grade)
  ))
  expect_identical(refused_calls, 1L)

  # A name-only selection is resolved once per compilation pass, and both
  # passes run when the whole specification is settled by name alone.
  selection_calls <- 0L
  counted <- function(x) {
    selection_calls <<- selection_calls + 1L
    x
  }
  summarize_with_margins(
    data,
    t = sum(units),
    .grouping = grouping_sets(tidyselect::all_of(counted("region")), grade)
  )
  expect_identical(selection_calls, 2L)
})

test_that("empty grouping rules preserve their phase and error precedence", {
  expect_equal(
    compile_grouping_spec(
      grouping_set(a),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list("a")
  )
  expect_equal(
    compile_grouping_spec(
      grouping_set(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list(character())
  )
  expect_equal(
    compile_grouping_spec(
      grouping_spec(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list(character())
  )

  sets_error <- expect_error(
    compile_grouping_spec(
      grouping_sets(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(sets_error, "marginplyr_error")
  expect_identical(
    conditionMessage(sets_error),
    paste0(
      "`grouping_sets()` requires at least one set.\n",
      "i Use `grouping_set()` for the empty grouping set."
    )
  )

  for (constructor in c("rollup", "cube")) {
    spec <- eval(rlang::call2(constructor))
    error <- expect_error(compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = margin_duplicates_choices
    ))
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      conditionMessage(error),
      sprintf("`%s()` requires at least one dimension.", constructor)
    )
  }

  for (constructor in c("rollup", "cube")) {
    resolved_spec <- eval(rlang::call2(
      constructor,
      quote(tidyselect::any_of("missing"))
    ))
    resolved_empty <- expect_error(
      compile_grouping_spec(
        resolved_spec,
        "a",
        duplicates_choices = margin_duplicates_choices
      )
    )
    expect_identical(
      conditionMessage(resolved_empty),
      sprintf("`%s()` requires at least one dimension.", constructor)
    )

    composite_spec <- eval(rlang::call2(
      constructor,
      quote(grouping_set(tidyselect::any_of("missing")))
    ))
    empty_composite <- expect_error(
      compile_grouping_spec(
        composite_spec,
        "a",
        duplicates_choices = margin_duplicates_choices
      )
    )
    expect_identical(
      conditionMessage(empty_composite),
      "An empty `grouping_set()` cannot be a composite dimension."
    )
  }

  child_error <- expect_error(
    compile_grouping_spec(
      rollup(grouping_sets()),
      "a",
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_identical(conditionMessage(child_error), conditionMessage(sets_error))
})

test_that("invalid grouping input lists every supported constructor", {
  error <- expect_error(compile_grouping_spec(
    1,
    "a",
    duplicates_choices = margin_duplicates_choices
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`."
    )
  )
})

# The one refusal in `R/grouping-plan.R` that nothing executed. #223's phase 3
# is what made it visible rather than what left it unrun: re-authoring gave the
# body its own line, where the flat form spread one dead call over three.
#
# It is a second line of defence behind the test above, which refuses an object
# that is not a specification at all. This one refuses an object that says it
# is: `new_grouping_spec()` is the only constructor, so a specification whose
# `type` is not one name, or is a name no rule answers, reaches the compiler
# only from a hand-built object. That is why the guard is here, and why nothing
# had run it. It is not promoted to a bare `stop()` -- that would move a site
# across ADR 0015's boundary, which #223's phase 3 may not do -- so what is
# left is to run it.
#
# Both guards are reached. They read different things, the shape of the
# specification's own fields and whether its kind names a rule, and answer with
# the same sentence, so either one running alone would leave the other unrun
# and report nothing about it.
#
# Every object is written in both positions a specification is reachable in,
# because the guard is reached by a different route from each -- the top-level
# object the caller passed as `.grouping`, and an argument of a well-formed
# parent, which `grouping_arg_spec()` evaluates and the preflight recurses into.
# #262 reproduced at both, and a fix holding at one and not the other would
# read as a fix.
test_that("a malformed grouping specification is refused by both guards", {
  malformed <- list(
    # A `type` that is not one name, and `args` that are not a list: the two
    # halves of the first guard's condition, which no single object fails both
    # of while still reaching the second.
    structure(
      list(type = character(), args = list()),
      class = "margin_grouping_spec"
    ),
    structure(
      list(type = "set", args = "not a list"),
      class = "margin_grouping_spec"
    ),
    # One name, and every field the right shape, but no rule answers it.
    structure(
      list(type = "pivot", args = list()),
      class = "margin_grouping_spec"
    ),
    # The class over something with no field to read at all, which a guard
    # reading `type` before establishing that much answered with base R's own
    # untyped error from that line instead of the refusal below it (#262).
    # Both, because what raises differs -- `$` is invalid for an atomic vector
    # and a closure is not subsettable, one a `simpleError` and the other a
    # `notSubsettableError` -- and the guard's answer does not.
    structure(1:3, class = "margin_grouping_spec"),
    structure(function() 1, class = "margin_grouping_spec"),
    # An object that answers for one field and raises on the other, which is
    # why the two are read through separate catches rather than one decision
    # about whether the object can be read at all. An active binding is what
    # writes it without a `$` method: nothing here is a shape a caller is
    # expected to build, and what it pins is that the guard's answer comes from
    # what the object could say, field by field.
    local({
      spec <- rlang::new_environment(list(type = "set"))
      raise <- function() {
        rlang::abort("reading args raises", class = "grouping_spec_field_error")
      }
      makeActiveBinding("args", raise, spec)
      structure(spec, class = "margin_grouping_spec")
    })
  )

  # `identity` is the top-level position and `grouping_sets` the nested one.
  positions <- list(identity, grouping_sets)
  for (spec in malformed) {
    for (position in positions) {
      error <- expect_error(compile_against(position(spec), "a"))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(
        conditionMessage(error),
        "Invalid grouping specification."
      )
    }
  }
})

# The second hazard the read left, and #262's answer for the read applied to it
# (#280). A kind that was read still has to be classified, and deciding whether
# it is one name asks the value's own `is.na()` and `length()` methods; either
# can raise instead of answering, and what came out of the guard was then
# whatever the object raised, in place of the refusal below it.
#
# `set` underneath every shape, so nothing here is refused for the name it
# holds: a classification that answered would compile. Both positions, for the
# reason the test above writes every object in both, and the public routes as
# well, because that is where the defect was reported and a guard is reached
# from them through the whole lifecycle rather than the compiler alone.
test_that("a grouping specification kind that raises is refused by the guard", {
  kinds <- lapply(c("is.na", "length"), function(generic) {
    kind_answering(
      stats::setNames(list(raising_kind_method), generic),
      "guard_raising"
    )
  })
  data <- data.frame(a = 1)
  calls <- list(
    top = function(spec) compile_against(spec, "a"),
    nested = function(spec) compile_against(grouping_sets(spec), "a"),
    inspect_grouping = function(spec) inspect_grouping(data, .grouping = spec),
    summarize_with_margins = function(spec) {
      summarize_with_margins(data, n = dplyr::n(), .grouping = spec)
    }
  )

  for (kind in kinds) {
    spec <- new_grouping_spec(kind, list())
    for (route in names(calls)) {
      error <- expect_error(calls[[route]](spec))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(
        conditionMessage(error),
        "Invalid grouping specification.",
        info = route
      )
    }

    # Blamed on the verb the caller wrote, which is the acceptance's other half
    # and what a condition raised out of a method would not have been: the
    # method's own call is what `stop()` in one blames. Only the public routes
    # are asked, an internal caller having no call a caller wrote.
    for (route in c("inspect_grouping", "summarize_with_margins")) {
      expect_identical(
        rlang::call_name(conditionCall(expect_error(calls[[route]](spec)))),
        route,
        info = route
      )
    }
  }
})

# The other half of #262, and the half nothing above would report. Establishing
# that a field can be read is not the same question as requiring a list, and
# the narrower reading passes every test above: `new_grouping_spec()` builds a
# list, so every specification a caller can construct is one, and the object
# the reading would newly refuse is one no test had reason to write. It is
# still a specification that says what it is when it is asked -- the guard
# reads fields, and this object answers for both of them -- so refusing it
# would be the guard deciding by storage what it is written to decide by
# content, and a caller whose specification arrived from somewhere else would
# be refused for how it got here.
#
# One storage form carries it, because `is.list()` is what a rewrite of that
# reading most easily becomes and an environment is what separates it from `$`.
# A pairlist would not: `is.list()` is already true of one.
test_that("a specification is read for what it says, not how it is stored", {
  fields <- list(type = "set", args = list(rlang::quo(a)))
  as_spec <- function(x) structure(x, class = "margin_grouping_spec")

  expect_identical(
    compile_against(as_spec(rlang::new_environment(fields)), "a"),
    compile_against(as_spec(fields), "a")
  )
})

test_that("selectors and fixed .by columns are resolved once", {
  selected <- c("a", "b")
  plan <- compile_grouping_spec(
    rollup(tidyselect::all_of(selected)),
    data_vars = c("year", "a", "b", "value"),
    .by = "year",
    duplicates_choices = margin_duplicates_choices
  )

  expect_equal(plan$by, "year")
  expect_equal(plan$dimensions, c("a", "b"))
  expect_equal(
    plan$sets,
    list(c("year", "a", "b"), c("year", "a"), "year")
  )
})

test_that("a renaming grouping selection is refused by every constructor", {
  data_vars <- c("region", "year", "value")
  renamed_message <- paste0(
    "Can't rename grouping dimension:\n",
    "i `area = region`.\n",
    "i Grouping dimensions must name existing columns."
  )
  renaming_calls <- list(
    quote(tidyselect::all_of(c(area = "region"))),
    quote(c(area = region))
  )
  constructors <- c(
    "grouping_set",
    "grouping_sets",
    "rollup",
    "cube",
    "grouping_spec"
  )

  for (constructor in constructors) {
    for (selection in renaming_calls) {
      spec <- eval(rlang::call2(constructor, selection))
      error <- expect_error(compile_grouping_spec(
        spec,
        data_vars,
        duplicates_choices = margin_duplicates_choices
      ))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(conditionMessage(error), renamed_message)
    }
  }

  nested <- expect_error(
    compile_grouping_spec(
      rollup(grouping_set(tidyselect::all_of(c(area = "region")))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(nested, "marginplyr_error")
  expect_identical(conditionMessage(nested), renamed_message)

  several <- expect_error(
    compile_grouping_spec(
      rollup(tidyselect::all_of(c(area = "region", when = "year"))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(several, "marginplyr_error")
  expect_identical(
    conditionMessage(several),
    paste0(
      "Can't rename grouping dimensions:\n",
      "i `area = region` and `when = year`.\n",
      "i Grouping dimensions must name existing columns."
    )
  )
})

test_that("non-renaming grouping selections keep resolving", {
  data_vars <- c("region", "region_code", "year")
  selected <- c("region", "year")

  expect_equal(
    compile_grouping_spec(
      rollup(region, year),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "year")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::all_of(selected)),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "year")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::starts_with("region")),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "region_code")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(-year),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "region_code")
  )
  # A name that repeats the column it selects renames nothing, so the plan it
  # builds still names a column of the input.
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::all_of(c(region = "region"))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    "region"
  )
})

by_rename_vars <- function() {
  c("region", "area", "region_code", "value")
}

by_rename_message <- function() {
  paste0(
    "Can't rename `.by` column:\n",
    "i `area = region`.\n",
    "i Fixed `.by` keys must name existing columns."
  )
}

test_that("a renaming .by selection is refused", {
  data_vars <- by_rename_vars()

  # The renamed-to name is another column of the input, so a resolution that
  # reports the name the caller wrote fixes the plan on `area` and never groups
  # by the column the selection named (#134).
  clashing <- expect_error(
    resolve_fixed_keys(rlang::quo(c(area = region)), character(), data_vars)
  )
  expect_s3_class(clashing, "marginplyr_error")
  expect_identical(conditionMessage(clashing), by_rename_message())

  # The renamed-to name is no column at all, which used to be rejected as an
  # unknown `.by` column the caller never wrote.
  absent <- expect_error(
    resolve_fixed_keys(
      rlang::quo(c(area = region)),
      character(),
      c("region", "value")
    )
  )
  expect_s3_class(absent, "marginplyr_error")
  expect_identical(conditionMessage(absent), by_rename_message())

  several <- expect_error(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(c(area = "region", size = "value"))),
      character(),
      data_vars
    )
  )
  expect_s3_class(several, "marginplyr_error")
  expect_identical(
    conditionMessage(several),
    paste0(
      "Can't rename `.by` columns:\n",
      "i `area = region` and `size = value`.\n",
      "i Fixed `.by` keys must name existing columns."
    )
  )
})

test_that("fixed keys settled by name alone need no typed metadata", {
  data_vars <- by_rename_vars()
  selected <- c("region", "value")

  expect_identical(
    resolve_fixed_keys(rlang::quo(region), character(), data_vars),
    "region"
  )
  expect_identical(
    resolve_fixed_keys(rlang::quo(c(region, area)), character(), data_vars),
    c("region", "area")
  )
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(selected)),
      character(),
      data_vars
    ),
    c("region", "value")
  )
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::starts_with("region")),
      character(),
      data_vars
    ),
    c("region", "region_code")
  )
  expect_identical(
    resolve_fixed_keys(rlang::quo(NULL), character(), data_vars),
    character()
  )
  # A name that repeats the column it selects renames nothing.
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(c(region = "region"))),
      character(),
      data_vars
    ),
    "region"
  )
  # Grouping columns are names dplyr resolved, so a grouped input's keys are
  # taken as they stand rather than selected again.
  expect_identical(
    resolve_fixed_keys(rlang::quo(NULL), "region", data_vars),
    "region"
  )
  # A predicate is the one selection column names cannot answer, so it is left
  # for the typed snapshot.
  expect_null(
    resolve_fixed_keys(rlang::quo(where(is.numeric)), character(), data_vars)
  )
})

test_that("duplicate grouping sets have explicit policies", {
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  expect_error(
    compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "Duplicate grouping sets"
  )
  dropped <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "drop",
    duplicates_choices = margin_duplicates_choices
  )
  kept <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "keep",
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(dropped$sets, list("a"))
  expect_identical(dropped$set_ids, 1L)
  expect_equal(kept$sets, list("a", "a"))
  expect_identical(kept$set_ids, 1:2)
})

# This diagnostic does not pluralize by suffixing a noun the way the eight
# builders enumerated in #206 do: it switches a whole phrase on the number of
# *groups* of duplicated positions, so the arms share no wording to pin once.
# The singular arm is pinned by "compile_grouping_spec() reads a narrowed
# duplicates vocabulary" below, which asserts it for the vocabulary's sake
# rather than the phrase's; the plural arm has this test, and nothing else in
# the suite tells the arms apart — every other expectation reaching this
# diagnostic matches the prefix alone, which both arms satisfy (#225).
test_that("duplicated grouping sets in more than one group name their groups", {
  # Two *distinct* duplicated sets rather than one set duplicated twice, which
  # is what makes `split()` return more than one group and selects the plural
  # arm.
  spec <- grouping_sets(
    grouping_set(a),
    grouping_set(b),
    grouping_set(a),
    grouping_set(b)
  )

  duplicated <- expect_error(
    compile_grouping_spec(
      spec,
      c("a", "b"),
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(duplicated, "marginplyr_error")
  # The groups arrive in `split()`'s order, which is by key and not by first
  # position, so `2, 4` precedes `1, 3`. This records what the code emits;
  # whether that ordering is what a reader wants belongs to #223.
  expect_identical(
    conditionMessage(duplicated),
    paste0(
      "Duplicate grouping sets were produced at position groups:\n",
      "i 2, 4; 1, 3.\n",
      "i Use `.duplicates = \"drop\"` or `\"keep\"`."
    )
  )
})

test_that("invalid or ambiguous specifications fail early", {
  expect_error(
    compile_grouping_spec(
      rollup(a),
      "a",
      .by = "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "both `.by` and `.grouping`"
  )
  expect_error(
    compile_grouping_spec(
      rollup(cube(a)),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "only accepts columns"
  )
  expect_error(
    compile_grouping_spec(
      grouping_sets(),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "requires at least one set"
  )
  expect_error(
    compile_grouping_spec(
      rollup(floor(a)),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "object 'a' not found"
  )
})

# The compiler's guard on this is an invariant rather than a Package condition
# (#159; the reasoning is at its site). That demotion is only correct while the
# selection is what a caller actually meets, so both halves are pinned here.
# The public half would hold with the guard promoted back, since the public
# path never reaches it, and the internal half is what records the demotion --
# neither says enough alone. The upstream message stays loosely matched: ADR
# 0015 propagates an external condition unchanged, so its wording is
# tidyselect's to revise.
test_that("an unknown `.by` column fails outside the public contract", {
  data <- data.frame(region = "x", value = 1)

  public <- expect_error(
    inspect_grouping(data, .by = nope, .grouping = rollup(region))
  )
  expect_false(inherits(public, "marginplyr_error"))
  expect_match(conditionMessage(public), "Column `nope` doesn't exist")

  internal <- expect_error(
    compile_grouping_spec(
      rollup(region),
      "region",
      .by = "nope",
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_false(inherits(internal, "marginplyr_error"))
  expect_identical(conditionMessage(internal), "Unknown `.by` column `nope`.")
})

# The plan compiler was reachable without going through
# `compile_grouping_spec()`, so the preflight and the `.duplicates` matching
# the wrapper performs were a second source of truth that production supplied
# for itself (#119). A test that compiled through the wrapper therefore proved
# nothing about the sequence production ran. This holds the two together: the
# wrapper is the entry point, and a new call site that skips it fails here
# rather than in whichever plan silently stopped being preflighted.
test_that("compile_grouping_spec() is the only caller of the plan compiler", {
  # Assembled rather than written out, so the source scan below does not
  # report this file for holding the name it searches for.
  impl <- paste0("compile_grouping_spec", "_impl")
  ns <- asNamespace("marginplyr")
  # Reads the loaded namespace rather than `R/`, which is not installed beside
  # the tests. `namespace_functions()` is the enumeration every structural gate
  # shares; this one decides by deparsing rather than by walking, so it is the
  # one that takes nothing else from `helper-namespace-walk.R`.
  objects <- namespace_functions(ns)
  calls_impl <- vapply(
    objects,
    function(name) {
      any(grepl(impl, deparse(body(get(name, envir = ns))), fixed = TRUE))
    },
    logical(1)
  )
  expect_identical(objects[calls_impl], "compile_grouping_spec")

  # The namespace scan cannot see a test, and a test reaching the
  # implementation with `:::` is the half of #119 that was actually there. The
  # sources sit beside this file whenever the suite runs, so scanning them
  # needs no installed copy; the assertion below refuses a scan that found no
  # files rather than passing on one.
  sources <- list.files(".", pattern = "^test-.*\\.R$", full.names = TRUE)
  expect_gt(length(sources), 1L)
  reached_by <- Filter(
    function(path) {
      any(grepl(paste0(impl, "("), readLines(path), fixed = TRUE))
    },
    sources
  )
  expect_identical(reached_by, character())
})

# The Margin vocabulary was hard-coded here, so the nesting verbs' narrower one
# reached the compiler through `prepare_grouping_plan()` and through no test.
test_that("compile_grouping_spec() reads a narrowed duplicates vocabulary", {
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  refused <- expect_error(
    compile_grouping_spec(
      spec,
      "a",
      .duplicates = "keep",
      duplicates_choices = nest_duplicates_choices
    )
  )
  expect_s3_class(refused, "marginplyr_error")
  expect_identical(
    conditionMessage(refused),
    "`.duplicates` must be one of \"error\" or \"drop\"."
  )

  # An untouched `.duplicates` stands for the first entry of the list the
  # caller stated, not of the Margin one.
  duplicated <- expect_error(
    compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = nest_duplicates_choices
    )
  )
  # The diagnostic offers the policies this caller could have asked for
  # instead, so a narrowed vocabulary must not offer `"keep"` (#110).
  expect_identical(
    conditionMessage(duplicated),
    paste0(
      "Duplicate grouping sets were produced at positions:\n",
      "i 1, 2.\n",
      "i Use `.duplicates = \"drop\"`."
    )
  )

  dropped <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "drop",
    duplicates_choices = nest_duplicates_choices
  )
  expect_equal(dropped$sets, list("a"))
})
