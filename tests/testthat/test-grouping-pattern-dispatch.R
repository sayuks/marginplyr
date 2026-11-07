test_that("pattern proxy methods receive typed local selection data", {
  skip_if_suggest_absent("stringr")

  data <- data.frame(a = "x", b = 1L)
  pattern <- stringr::fixed("a")
  reads <- character()
  method <- function(x, ...) {
    type <- typeof(tidyselect::peek_data()$a)
    reads <<- c(reads, type)
    if (type != "character") {
      stop("pattern proxy requires character column metadata")
    }
    unclass(x)
  }
  rlang::local_bindings(
    vec_proxy.stringr_fixed = method,
    .env = asNamespace("vctrs")$.__S3MethodsTable__.
  )
  selection <- rlang::quo(matches(pattern))
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  reads <- character()
  expect_identical(
    inspect_grouping(data, .grouping = grouping_set(!!selection),
                     .format = "list")$included,
    list("a")
  )
  expect_identical(reads, "character")
})

test_that("pattern consumption methods receive typed local selection data", {
  skip_if_suggest_absent("stringr")

  data <- data.frame(a = "x", b = 1L)
  pattern <- stringr::fixed("a")
  reads <- logical()
  method <- function(x) {
    typed <- is.character(tidyselect::peek_data()$a)
    reads <<- c(reads, typed)
    if (typed) 1L else 2L
  }
  rlang::local_bindings(
    length.stringr_fixed = method,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  selection <- rlang::quo(matches(pattern))
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  reads <- logical()
  expect_identical(
    inspect_grouping(data, .by = !!selection, .format = "list")$fixed,
    list("a")
  )
  expect_true(length(reads) > 0L)
  expect_true(all(reads))
})

# Direct tidyselect and an opaque wrapper provide typed public controls.
check_pattern_consumers <- function(data, local, generic = "length") {
  run <- function(modifier, level) {
    counter <- rlang::env(reads = logical())
    method <- function(x) {
      typed <- is.character(tidyselect::peek_data()$a)
      counter$reads <- c(counter$reads, typed)
      if (generic == "vec_proxy") {
        if (!typed) {
          stop("pattern proxy requires character column metadata")
        }
        return(unclass(x))
      }
      if (typed) 1L else 2L
    }
    method_class <- if (level == "exact") {
      paste0("stringr_", modifier)
    } else {
      "stringr_pattern"
    }
    rlang::local_bindings(
      !!!setNames(list(method), paste0(generic, ".", method_class)),
      .env = asNamespace(
        if (generic == "length") "base" else "vctrs"
      )$.__S3MethodsTable__.
    )
    pattern <- getExportedValue("stringr", modifier)("a")
    selections <- list(
      rlang::quo(tidyselect::matches(pattern)),
      rlang::new_quosure(
        rlang::call2("matches", rlang::call2(modifier, "a", .ns = "stringr")),
        environment()
      )
    )
    for (selection in selections) {
      counter$reads <- logical()
      expect_identical(names(tidyselect::eval_select(selection, local)), "a")
      budget <- length(counter$reads)
      counter$reads <- logical()
      expected <- inspect_grouping(
        data, .by = tidyselect::matches(identity(pattern)), .format = "list"
      )
      counter$reads <- logical()
      expect_identical(
        inspect_grouping(data, .by = !!selection, .format = "list"), expected
      )
      expect_length(counter$reads, budget)
      expect_true(all(counter$reads))
      for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                            "grouping_spec")) {
        spec <- rlang::eval_tidy(rlang::call2(constructor, selection))
        original <- spec
        counter$reads <- logical()
        plan <- inspect_grouping(data, .grouping = spec, .format = "list")
        expect_identical(plan$included[[1L]], "a")
        expect_identical(spec, original)
        expect_length(counter$reads, budget)
        expect_true(all(counter$reads))
      }
      for (verb in c("summarize_with_margins", "expand_with_margins",
                     "nest_with_margins", "nest_by_with_margins")) {
        args <- list(data, .by = selection)
        if (verb == "summarize_with_margins") {
          args$n <- rlang::quo(dplyr::n())
        }
        counter$reads <- logical()
        actual <- dplyr::ungroup(dplyr::collect(
          rlang::eval_tidy(rlang::call2(verb, !!!args))
        ))
        expect_identical(actual$a, "x")
        expect_length(counter$reads, budget)
        expect_true(all(counter$reads))
      }
      expect_true(length(counter$reads) > 0L)
      expect_true(all(counter$reads))
    }
  }
  for (modifier in c("fixed", "regex", "coll")) {
    for (level in c("exact", "inherited")) {
      run(modifier, level)
    }
  }
}

test_that("all local pattern consumers preserve typed pattern dispatch", {
  skip_if_suggest_absent("stringr")
  data <- data.frame(a = "x", b = 1L)
  check_pattern_consumers(data, data)
})

test_that("safe dtplyr pattern consumers preserve typed pattern dispatch", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- data.frame(a = "x", b = 1L)
  step <- dtplyr::lazy_dt(data)
  check_pattern_consumers(step, data)
  expect_identical(data, data.frame(a = "x", b = 1L))
})

test_that("local pattern proxies agree with typed tidyselect", {
  skip_if_suggest_absent("stringr")
  data <- data.frame(a = "x", b = 1L)
  check_pattern_consumers(data, data, "vec_proxy")
})

test_that("dtplyr pattern proxies agree with typed tidyselect", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- data.frame(a = "x", b = 1L)
  step <- dtplyr::lazy_dt(data)
  check_pattern_consumers(step, data, "vec_proxy")
  expect_identical(data, data.frame(a = "x", b = 1L))
})

# A typed pattern must stop at unsafe metadata before any caller method runs.
check_pattern_metadata_refusal <- function(generic) {
  counter <- rlang::env(calls = 0L, method_calls = 0L)
  method <- function(x) {
    counter$method_calls <- counter$method_calls + 1L
    1L
  }
  rlang::local_bindings(
    !!!setNames(list(method), paste0(generic, ".stringr_pattern")),
    .env = asNamespace(
      if (generic == "length") "base" else "vctrs"
    )$.__S3MethodsTable__.
  )
  source <- data.frame(a = "x", b = 1L)
  transform <- function(x) {
    counter$calls <- counter$calls + 1L
    paste0(x, "!")
  }
  step <- dplyr::mutate(dtplyr::lazy_dt(source), added = transform(.data$a))
  pattern <- stringr::regex("a")
  selections <- rlang::quos(
    tidyselect::matches(pattern), tidyselect::matches(stringr::regex("a"))
  )
  for (selection in selections) {
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selection)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
  expect_identical(counter$method_calls, 0L)
  expect_identical(counter$calls, 0L)
  expect_identical(source, data.frame(a = "x", b = 1L))
}

test_that("pattern methods defer before unsafe dtplyr metadata without reads", {
  skip_if_suggest_absent("dtplyr", "stringr")
  check_pattern_metadata_refusal("length")
  check_pattern_metadata_refusal("vec_proxy")
})

# Classifying method presence must not force active or promised bindings.
check_deferred_pattern_methods <- function(generic) {
  counter <- rlang::env(calls = 0L)
  env <- rlang::env(pattern = stringr::fixed("a"))
  makeActiveBinding(paste0(generic, ".stringr_fixed"), function() {
    counter$calls <- counter$calls + 1L
    stop("method binding forced")
  }, env)
  delayedAssign(
    paste0(generic, ".stringr_pattern"), stop("promise forced"),
    assign.env = env
  )
  source <- data.frame(a = "x")
  step <- dplyr::mutate(dtplyr::lazy_dt(source), b = paste0(.data$a, "!"))
  selection <- rlang::new_quosure(quote(matches(pattern)), env)
  expect_error(
    inspect_grouping(step, .by = !!selection),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
  expect_identical(counter$calls, 0L)
  expect_true(rlang::env_binding_are_lazy(
    env, paste0(generic, ".stringr_pattern")
  )[[1L]])
}

test_that("pattern classification leaves deferred method bindings unforced", {
  skip_if_suggest_absent("dtplyr", "stringr")
  check_deferred_pattern_methods("length")
  check_deferred_pattern_methods("vec_proxy")
})

# S3 dispatch still consults global methods when a selection's scope does not.
check_global_pattern_method <- function(generic) {
  counter <- rlang::env(reads = logical())
  method <- function(x) {
    typed <- is.character(tidyselect::peek_data()$a)
    counter$reads <- c(counter$reads, typed)
    if (generic == "vec_proxy") {
      if (!typed) {
        stop("pattern proxy requires character column metadata")
      }
      return(unclass(x))
    }
    if (typed) 1L else 2L
  }
  rlang::local_bindings(
    !!!setNames(list(method), paste0(generic, ".stringr_regex")),
    .env = globalenv()
  )
  env <- rlang::env(baseenv(), pattern = stringr::regex("a"))
  selection <- rlang::new_quosure(quote(tidyselect::matches(pattern)), env)
  data <- data.frame(a = "x", b = 1L)
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  counter$reads <- logical()
  expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(a)")
  expect_true(length(counter$reads) > 0L)
  expect_true(all(counter$reads))
}

test_that("global pattern methods remain visible to isolated caller scopes", {
  skip_if_suggest_absent("stringr")
  check_global_pattern_method("length")
  check_global_pattern_method("vec_proxy")
})

test_that("unrelated pattern methods preserve ordinary names-only matching", {
  skip_if_suggest_absent("dtplyr", "stringr")
  calls <- 0L
  method <- function(x) {
    calls <<- calls + 1L
    stop("unrelated method ran")
  }
  rlang::local_bindings(length.unrelated_pattern = method, .env = globalenv())
  rlang::local_bindings(
    vec_proxy.unrelated_pattern = method, .env = globalenv()
  )
  source <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(source), added = paste0(a, "!"))
  for (modifier in c("regex", "fixed", "coll")) {
    pattern <- getExportedValue("stringr", modifier)("A", ignore_case = TRUE)
    expect_identical(
      inspect_grouping(step, .by = matches(pattern), .format = "list")$fixed,
      list(c("a", "added"))
    )
    expect_identical(
      inspect_grouping(step, .by = !matches(pattern), .format = "list")$fixed,
      list("b")
    )
  }
  expect_identical(calls, 0L)
})

# The method is constructed without evaluating its metadata-dependent body.
rebound_pattern_method <- function(modifier = "regex") {
  state <- rlang::env(seen = character(), operand_calls = 0L)
  method <- "length.stringr_pattern"
  present <- exists(method, globalenv(), inherits = FALSE)
  previous <- if (present) get(method, globalenv()) else NULL
  source <- basename(tempfile("marginplyr_pattern_method_"))
  makeActiveBinding(source, function() {
    type <- typeof(tidyselect::peek_data()$a)
    state$seen <- c(state$seen, type)
    if (type == "character") 1L else 2L
  }, globalenv())
  state$reset <- function() {
    if (exists(method, globalenv(), inherits = FALSE)) {
      rm(list = method, envir = globalenv())
    }
    state$seen <- character()
    state$operand_calls <- 0L
  }
  state$cleanup <- function() {
    state$reset()
    if (present) assign(method, previous, envir = globalenv())
    if (exists(source, globalenv(), inherits = FALSE)) {
      rm(list = source, envir = globalenv())
    }
  }
  state$reset()
  pattern <- rlang::call2(modifier, "a", .ns = "stringr")
  counter <- rlang::call2(
    "<<-", quote(operand_calls), quote(operand_calls + 1L)
  )
  install <- rlang::call2("<<-", method, rlang::expr(as.vector(as.vector(
    list(x = NULL, as.vector(!!source, "symbol")), "pairlist"
  ), "function")))
  state$selection <- rlang::new_quosure(rlang::expr(tidyselect::matches(
    !!pattern, vars = {
      !!counter
      !!install
      tidyselect::peek_vars()
    }
  )), state)
  state
}

test_that("helper-installed pattern methods receive typed selection metadata", {
  skip_if_suggest_absent("stringr")
  data <- data.frame(a = "east", b = 1L)
  fixture <- rebound_pattern_method()
  on.exit(fixture$cleanup(), add = TRUE)
  selection <- fixture$selection
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  direct_budget <- length(fixture$seen)
  expect_identical(fixture$operand_calls, 1L)
  fixture$reset()
  actual <- inspect_grouping(data, .grouping = grouping_set(!!selection),
                             .format = "list")
  expect_identical(actual$included, list("a"))
  expect_identical(fixture$seen, rep("character", direct_budget))
  expect_identical(fixture$operand_calls, 1L)
  expect_identical(data, data.frame(a = "east", b = 1L))
})

# Compare each public consumer with a literal plan and direct typed selection.
expect_rebound_pattern_methods <- function(data, local) {
  a <- rlang::sym("a")
  b <- rlang::sym("b")
  original <- serialize(data, NULL)
  for (modifier in c("fixed", "regex", "coll")) {
    fixture <- rebound_pattern_method(modifier)
    on.exit(fixture$cleanup(), add = TRUE)
    selection <- fixture$selection
    positions <- c("fixed", "grouping_set", "grouping_sets", "rollup", "cube",
                   "grouping_spec")
    for (position in positions) {
      for (consumer in c("inspect", "summary")) {
        fixture$reset()
        expect_identical(names(tidyselect::eval_select(selection, local)), "a")
        budget <- length(fixture$seen)
        fixture$reset()
        run <- function(selection) {
          construct <- getExportedValue(
            "marginplyr", if (position == "fixed") "grouping_set" else position
          )
          spec <- if (position == "fixed") {
            construct(!!b)
          } else {
            construct(!!selection)
          }
          by <- if (position == "fixed") selection else rlang::quo(!!b)
          original_spec <- spec
          result <- if (consumer == "inspect") {
            inspect_grouping(
              data, .by = !!by, .grouping = spec, .format = "list"
            )
          } else {
            tibble::as_tibble(summarize_with_margins(
              data, n = dplyr::n(), .by = !!by, .grouping = spec,
              .margin_label = NULL, .id = "set"
            ))
          }
          expect_identical(spec, original_spec)
          result
        }
        expected <- run(rlang::quo(!!a))
        actual <- run(selection)
        expect_identical(actual, expected, info = paste(
          modifier, position, consumer
        ))
        expect_identical(fixture$seen, rep("character", budget))
        expect_identical(fixture$operand_calls, 1L)

        fixture$reset()
        wrapped <- rlang::quo_get_expr(selection)
        wrapped$vars <- rlang::expr(base::identity(!!wrapped$vars))
        wrapped <- rlang::new_quosure(wrapped, rlang::quo_get_env(selection))
        expect_identical(run(wrapped), expected)
        expect_identical(fixture$seen, rep("character", budget))
        expect_identical(fixture$operand_calls, 1L)

        fixture$seen <- character()
        fixture$operand_calls <- 0L
        expect_identical(run(selection), expected)
        expect_identical(fixture$seen, rep("character", budget))
        expect_identical(fixture$operand_calls, 1L)
      }
    }
    fixture$cleanup()
  }
  expect_identical(serialize(data, NULL), original)
}

test_that("helper-installed pattern methods agree across eager consumers", {
  skip_if_suggest_absent("stringr")
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_pattern_methods(data, data)
  expect_rebound_pattern_methods(as.data.frame(data), data)
})

test_that("helper-installed pattern methods preserve data.table consumers", {
  skip_if_suggest_absent("data.table", "stringr")
  data <- data.table::data.table(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_pattern_methods(data, as.data.frame(data))
})

test_that("helper-installed pattern methods preserve immutable dtplyr plans", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_pattern_methods(dtplyr::lazy_dt(data, immutable = TRUE), data)
  expect_identical(data, tibble::tibble(a = c("east", "west"), b = c(1L, 1L)))
})

test_that("unproven pattern methods refuse before evaluating derived rows", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- data.frame(a = "east", b = 1L)
  rows <- 0L
  touch <- function(x) {
    rows <<- rows + length(x)
    x
  }
  step <- dplyr::mutate(dtplyr::lazy_dt(data), a = touch(a))
  fixture <- rebound_pattern_method()
  on.exit(fixture$cleanup(), add = TRUE)
  selection <- fixture$selection
  for (consumer in c("inspect", "summary")) {
    for (fixed in c(TRUE, FALSE)) {
      run <- function() {
        by <- if (fixed) selection else rlang::quo(b)
        spec <- if (fixed) grouping_set(b) else grouping_set(!!selection)
        if (consumer == "inspect") {
          inspect_grouping(step, .by = !!by, .grouping = spec)
        } else {
          summarize_with_margins(step, n = dplyr::n(), .by = !!by,
                                 .grouping = spec)
        }
      }
      expect_error(run(), "Can't safely determine column metadata",
                   class = "marginplyr_error")
      expect_identical(fixture$seen, character())
      expect_identical(fixture$operand_calls, 0L)
      expect_false(exists(
        "length.stringr_pattern", globalenv(), inherits = FALSE
      ))
    }
  }
  expect_identical(rows, 0L)
  expect_identical(data, data.frame(a = "east", b = 1L))
})

test_that("stable operands retain names-only pattern evaluation budgets", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- data.frame(a = "east", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))
  for (modifier in c("fixed", "regex", "coll")) {
    fixture <- rebound_pattern_method(modifier)
    on.exit(fixture$cleanup(), add = TRUE)
    selection <- rlang::quo_get_expr(fixture$selection)
    selection$vars <- quote({
      operand_calls <<- operand_calls + 1L
      tidyselect::peek_vars()
    })
    selection <- rlang::new_quosure(selection, fixture)
    actual <- inspect_grouping(step, .by = !!selection, .format = "list")
    expect_identical(actual$fixed, list(c("a", "added")))
    expect_identical(fixture$operand_calls, 1L)
    fixture$reset()
    actual <- inspect_grouping(
      step, .by = b, .grouping = grouping_set(!!selection), .format = "list"
    )
    expect_identical(actual$included, list(c("a", "added")))
    expect_identical(fixture$operand_calls, 2L)
    expect_identical(fixture$seen, character())
    fixture$cleanup()
  }
  expect_identical(data, data.frame(a = "east", b = 1L))
})

test_that("nested matches operands also preserve typed pattern dispatch", {
  skip_if_suggest_absent("stringr")
  data <- data.frame(a = "east", b = 1L)
  fixture <- rebound_pattern_method()
  on.exit(fixture$cleanup(), add = TRUE)
  selection <- rlang::quo(tidyselect::all_of(!!fixture$selection))
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  budget <- length(fixture$seen)
  fixture$reset()
  actual <- inspect_grouping(data, .by = !!selection, .format = "list")
  expect_identical(actual$fixed, list("a"))
  expect_identical(fixture$seen, rep("character", budget))
  expect_identical(fixture$operand_calls, 1L)
})
