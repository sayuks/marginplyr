test_that("typed derived plans refuse caller brackets without running them", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  fixture <- dtplyr_caller_bracket_fixture()
  source <- fixture$source
  before <- fixture$before
  step <- fixture$step
  explicit <- dplyr::collect(step)
  expect_identical(explicit, tibble::tibble(
    g = c("a", "b"), value = 1:2, derived = 2:3
  ))
  expect_identical(fixture$calls, 2L)
  fixture$calls <- integer()
  expect_identical(
    inspect_grouping(step, .grouping = rollup(g)),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(fixture$calls, integer())
  expect_error(
    inspect_grouping(step, .grouping = rollup(where(is.character))),
    "Can't safely determine column metadata", fixed = TRUE,
    class = "marginplyr_error"
  )
  expect_identical(fixture$calls, integer())
  expect_identical(source, before)
})

test_that("fixed keys and both Nest verbs refuse a caller bracket before use", {
  skip_if_suggest_absent("dtplyr")
  fixture <- dtplyr_caller_bracket_fixture()
  source <- fixture$source
  before <- fixture$before
  step <- fixture$step
  expect_error(inspect_grouping(step, .by = where(is.character)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(step, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(step, .grouping = rollup(where(is.character))),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(fixture$calls, integer())
  expect_identical(source, before)
})

test_that("typed derived metadata does not look up active or lazy brackets", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  for (binding in c("[", "[.data.table")) {
    for (inherited in c(FALSE, TRUE)) {
      for (lazy in c(FALSE, TRUE)) {
        lookups <- 0L
        binding_env <- new.env(parent = environment())
        if (lazy) {
          delayedAssign(binding, {
            lookups <<- lookups + 1L
            base::`[`
          }, assign.env = binding_env)
        } else {
          makeActiveBinding(binding, function() {
            lookups <<- lookups + 1L
            base::`[`
          }, binding_env)
        }
        caller <- if (inherited) new.env(parent = binding_env) else binding_env
        if (inherited) {
          caller[[binding]] <- NULL
        }
        step <- eval(quote(dplyr::mutate(
          dtplyr::lazy_dt(source), derived = value + 1L
        )), caller)
        expect_identical(inspect_grouping(step, .grouping = rollup(g))$included,
                         c("(g)", "()"))
        expect_error(inspect_grouping(step, .by = where(is.character)),
                     class = "marginplyr_error")
        expect_error(inspect_grouping(
          step, .grouping = rollup(where(is.character))
        ), class = "marginplyr_error")
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          expect_error(verb(step, .by = where(is.character)),
                       class = "marginplyr_error")
        }
        expect_identical(lookups, 0L)
        expect_identical(source, before)
      }
    }
  }
})

test_that("subset metadata respects bracket dispatch even without evaluation", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "[.data.table")) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- data.table::copy(source)
    calls <- integer()
    ordinary <- if (binding == "[") base::`[` else get(
      "[.data.table", envir = asNamespace("data.table")
    )
    root <- local({
      bracket <- function(x, ...) {
        calls <<- c(calls, nrow(x))
        result <- data.table::copy(ordinary(x, ...))
        data.table::set(result, j = "g", value = seq_len(nrow(result)))
        result
      }
      assign(binding, bracket)
      dtplyr::lazy_dt(source)
    })
    for (step in list(dplyr::filter(root, value > 0L),
                      dplyr::select(root, g))) {
      explicit <- dplyr::collect(step)
      expect_identical(explicit$g, 1:2)
      expect_identical(calls, 2L)
      calls <- integer()
      expect_identical(inspect_grouping(step, .grouping = rollup(g))$included,
                       c("(g)", "()"))
      expect_error(inspect_grouping(step, .by = where(is.character)),
                   "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
      expect_identical(calls, integer())
      expect_identical(source, before)
    }
  }
})

test_that("join metadata checks the effective outer bracket environment", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "[.data.table")) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- data.table::copy(source)
    calls <- integer()
    binding_env <- new.env(parent = environment())
    makeActiveBinding(binding, function() {
      calls <<- c(calls, 1L)
      base::`[`
    }, binding_env)
    caller <- new.env(parent = binding_env)
    masked <- eval(quote(dtplyr::lazy_dt(source)), caller)
    ordinary <- dtplyr::lazy_dt(source)
    left <- dplyr::mutate(masked, derived = value + 1L)
    safe <- dplyr::left_join(left, ordinary, by = c("g", "value"))
    explicit <- dplyr::collect(safe)
    expect_identical(explicit$g, c("a", "b"))
    expect_identical(calls, integer())
    expect_identical(
      inspect_grouping(safe, .grouping = rollup(where(is.character))),
      inspect_grouping(explicit, .grouping = rollup(g))
    )
    unsafe <- dplyr::left_join(ordinary, masked, by = c("g", "value"))
    expect_error(inspect_grouping(unsafe, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_identical(calls, integer())
    expect_identical(source, before)
  }
})

test_that("ordinary bracket dispatch preserves typed plans and nesting cells", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "[.data.table")) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- data.table::copy(source)
    root <- local({
      assign(binding, if (binding == "[") base::`[` else get(
        "[.data.table", envir = asNamespace("data.table")
      ))
      dtplyr::lazy_dt(source)
    })
    for (step in list(root, dplyr::filter(root, value > 0L))) {
      explicit <- tibble::tibble(g = c("a", "b"), value = 1:2)
      expect_identical(inspect_grouping(step, .by = where(is.character)),
                       inspect_grouping(explicit, .by = g))
      expect_identical(
        inspect_grouping(step, .grouping = grouping_set(where(is.character))),
        inspect_grouping(explicit, .grouping = grouping_set(g))
      )
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        expect_identical(
          dtplyr_metadata_nest_result(verb(step, .by = where(is.character))),
          dtplyr_metadata_nest_result(verb(explicit, .by = g))
        )
      }
    }
    step <- dplyr::mutate(root, derived = value + 1L)
    explicit <- dplyr::collect(step)
    expect_identical(explicit$derived, 2:3)
    expect_identical(
      inspect_grouping(step, .grouping = rollup(where(is.character))),
      inspect_grouping(explicit, .grouping = rollup(g))
    )
    expect_identical(
      inspect_grouping(step, .by = where(is.character)),
      inspect_grouping(explicit, .by = g)
    )
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_identical(
        dtplyr_metadata_nest_result(verb(
          step, .grouping = rollup(where(is.character))
        )),
        dtplyr_metadata_nest_result(verb(explicit, .grouping = rollup(g)))
      )
      expect_identical(
        dtplyr_metadata_nest_result(verb(step, .by = where(is.character))),
        dtplyr_metadata_nest_result(verb(explicit, .by = g))
      )
    }
    expect_identical(source, before)
  }
})

test_that("root metadata needs no caller bracket binding", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "[.data.table")) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- data.table::copy(source)
    lookups <- 0L
    caller <- new.env(parent = environment())
    makeActiveBinding(binding, function() {
      lookups <<- lookups + 1L
      stop("The caller bracket must not be looked up")
    }, caller)
    root <- eval(quote(dtplyr::lazy_dt(source)), caller)
    explicit <- dplyr::collect(root)
    expect_identical(explicit, tibble::tibble(g = c("a", "b"), value = 1:2))
    expect_identical(
      inspect_grouping(root, .grouping = rollup(where(is.character))),
      inspect_grouping(explicit, .grouping = rollup(g))
    )
    expect_identical(inspect_grouping(root, .by = where(is.character)),
                     inspect_grouping(explicit, .by = g))
    expect_identical(lookups, 0L)
    expect_identical(source, before)
  }
})

test_that("non-functions retain base-dispatch typed plans and Nest cells", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "+", "[.data.table")) {
    for (value in list(NULL, 42L)) {
      for (inherited in c(FALSE, TRUE)) {
        source <- data.table::data.table(g = c("a", "b"), value = 1:2)
        before <- data.table::copy(source)
        binding_env <- new.env(parent = environment())
        assign(binding, value, envir = binding_env)
        caller <- binding_env
        if (inherited) {
          caller <- new.env(parent = binding_env)
          assign(binding, numeric(), envir = caller)
        }
        step <- eval(quote(dplyr::mutate(
          dtplyr::lazy_dt(source), derived = value + 1L
        )), caller)
        explicit <- dplyr::collect(step)
        expect_identical(explicit, tibble::tibble(
          g = c("a", "b"), value = 1:2, derived = 2:3
        ))
        expect_identical(
          inspect_grouping(step, .grouping = rollup(g)),
          inspect_grouping(explicit, .grouping = rollup(g))
        )
        expect_identical(
          inspect_grouping(step, .grouping = rollup(where(is.character))),
          inspect_grouping(explicit, .grouping = rollup(g))
        )
        expect_identical(
          inspect_grouping(step, .by = where(is.character)),
          inspect_grouping(explicit, .by = g)
        )
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          expect_identical(
            dtplyr_metadata_nest_result(verb(
              step, .grouping = rollup(where(is.character))
            )),
            dtplyr_metadata_nest_result(verb(explicit, .grouping = rollup(g)))
          )
          expect_identical(
            dtplyr_metadata_nest_result(verb(step, .by = where(is.character))),
            dtplyr_metadata_nest_result(verb(explicit, .by = g))
          )
        }
        expect_identical(source, before)
      }
    }
  }
})

test_that("typed derived metadata refuses missing caller brackets safely", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  make_step <- function(`[`) {
    dplyr::mutate(dtplyr::lazy_dt(source), derived = value + 1L)
  }
  step <- make_step()
  expect_identical(inspect_grouping(step, .grouping = rollup(g))$included,
                   c("(g)", "()"))
  expect_error(inspect_grouping(step, .by = where(is.character)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(step, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(source, before)
})

test_that("typed plans refuse caller bracket methods before dispatch", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  fixture <- dtplyr_caller_bracket_fixture("[.data.table")
  before <- serialize(fixture$source, NULL)
  registry <- get(".__S3MethodsTable__.", envir = baseenv())
  registered <- get("[.data.table", envir = registry, inherits = FALSE)
  explicit <- dplyr::collect(fixture$step)
  expect_identical(explicit, tibble::tibble(
    g = c("a", "b"), value = 1:2, derived = 2:3
  ))
  expect_identical(fixture$calls, 2L)
  fixture$calls <- integer()
  expect_identical(
    inspect_grouping(fixture$step, .grouping = grouping_set(g)),
    inspect_grouping(explicit, .grouping = grouping_set(g))
  )
  for (verb in list(inspect_grouping, nest_with_margins,
                    nest_by_with_margins)) {
    expect_error(verb(fixture$step, .by = where(is.character)),
                 "Can't safely determine column metadata", fixed = TRUE,
                 class = "marginplyr_error")
    expect_error(verb(
      fixture$step, .grouping = grouping_set(where(is.character))
    ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(fixture$calls, integer())
  expect_identical(serialize(fixture$source, NULL), before)
  expect_identical(get("[", envir = fixture$step$env), base::`[`)
  expect_identical(get("[.data.table", envir = registry, inherits = FALSE),
                   registered)
})

test_that("typed plans leave unsafe bracket registrations unread", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("absent", "replacement", "active", "lazy")) local({
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- serialize(source, NULL)
    root <- dtplyr::lazy_dt(source)
    step <- dplyr::mutate(root, derived = value + 1L)
    registry <- get(".__S3MethodsTable__.", envir = baseenv())
    ordinary <- get("[.data.table", envir = registry, inherits = FALSE)
    on.exit({
      if (exists("[.data.table", envir = registry, inherits = FALSE)) {
        rm("[.data.table", envir = registry)
      }
      assign("[.data.table", ordinary, envir = registry)
    }, add = TRUE)
    lookups <- 0L
    source_sums <- numeric()
    calls <- integer()
    read_method <- function() {
      lookups <<- lookups + 1L
      source_sums <<- c(source_sums, sum(source$value))
      ordinary
    }
    rm("[.data.table", envir = registry)
    if (binding == "replacement") {
      replacement <- function(x, ...) {
        calls <<- c(calls, nrow(x))
        ordinary(x, ...)
      }
      assign("[.data.table", replacement, envir = registry)
    } else if (binding == "active") {
      makeActiveBinding("[.data.table", read_method, registry)
    } else if (binding == "lazy") {
      delayedAssign("[.data.table", read_method(), assign.env = registry)
    }
    expect_identical(
      inspect_grouping(step, .grouping = grouping_set(g))$included, "(g)"
    )
    for (verb in list(inspect_grouping, nest_with_margins,
                      nest_by_with_margins)) {
      expect_error(verb(step, .by = where(is.character)),
                   "Can't safely determine column metadata", fixed = TRUE,
                   class = "marginplyr_error")
      expect_error(verb(
        step, .grouping = grouping_set(where(is.character))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    }
    expect_identical(lookups, 0L)
    expect_identical(source_sums, numeric())
    expect_identical(calls, integer())
    expect_identical(serialize(source, NULL), before)

    caller <- new.env(parent = environment())
    caller$`[.data.table` <- ordinary
    lexical <- eval(quote(dplyr::mutate(
      dtplyr::lazy_dt(source), derived = value + 1L
    )), caller)
    for (verb in list(inspect_grouping, nest_with_margins,
                      nest_by_with_margins)) {
      expect_error(verb(lexical, .by = where(is.character)),
                   class = "marginplyr_error")
      expect_error(verb(
        lexical, .grouping = grouping_set(where(is.character))
      ), class = "marginplyr_error")
    }
    expect_identical(lookups, 0L)
    expect_identical(source_sums, numeric())
    expect_identical(calls, integer())
    expect_identical(serialize(source, NULL), before)
  })
})
