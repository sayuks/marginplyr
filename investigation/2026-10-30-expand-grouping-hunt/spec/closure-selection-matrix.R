args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
options(lifecycle_verbosity = "quiet")
set.seed(30102026)
data <- tibble::tibble(
  alpha = 1:2,
  beta = c("x", "y"),
  gamma = as.Date("2026-10-30") + 1:2,
  delta = factor(c("b", "a")),
  epsilon = list(1:2, NULL)
)
vars <- names(data)
observe <- function(f) tryCatch(suppressWarnings(f()), error = identity)
wrap <- function(x, depth) {
  for (i in seq_len(depth)) x <- rlang::call2("(", x)
  x
}
base_env <- rlang::env(globalenv(), selected = c("beta", "delta"))
base_env$typed <- function() {
  current <- tidyselect::peek_data()
  names(current)[vapply(current, is.character, logical(1))]
}
bodies <- list(
  quote(c("alpha", "gamma")),
  quote(selected),
  quote(names(tidyselect::peek_data())[c(1L, 3L)]),
  quote(tidyselect::peek_vars()[c(1L, 3L)]),
  quote(typed()),
  quote(names(Filter(is.numeric, tidyselect::peek_data()))),
  quote({
    local <- selected
    local
  }),
  quote({
    local <- typed()
    local
  }),
  quote(paste0(c("al", "be"), c("pha", "ta"))),
  quote(unname(c(left = "alpha", right = "gamma"))),
  quote(c("alpha", "missing")),
  quote(character()),
  quote(c(1L, 3L)),
  quote(c("beta", "beta")),
  quote(c(new = "alpha"))
)
expressions <- list()
for (body in bodies) {
  for (form in c("direct", "default", "nested", "dots")) {
    env <- rlang::env(base_env)
    if (form == "direct") {
      env$choose <- rlang::new_function(pairlist(), body, env)
    } else if (form == "default") {
      env$choose <- rlang::new_function(
        as.pairlist(list(x = body)), quote(x), env
      )
    } else if (form == "nested") {
      env$inner <- rlang::new_function(pairlist(), body, env)
      env$choose <- rlang::new_function(pairlist(), quote(inner()), env)
    } else {
      env$inner <- rlang::new_function(alist(... = ), quote(..1), env)
      env$choose <- rlang::new_function(
        pairlist(), rlang::call2("inner", body), env
      )
    }
    for (depth in 0:2) {
      call <- rlang::call2(wrap(quote(choose), depth))
      expressions[[length(expressions) + 1L]] <- list(
        quo = rlang::new_quosure(rlang::call2("all_of", call), env),
        label = paste(form, depth, rlang::expr_deparse(body))
      )
    }
  }
}
counts <- c(success = 0L, external_error = 0L, empty = 0L, rename = 0L)
failures <- list()
for (entry in expressions) {
  selection <- entry$quo
  expected <- observe(function() tidyselect::eval_select(selection, data))
  for (backend in c("local", "dtplyr")) {
    input <- if (backend == "local") data else dtplyr::lazy_dt(data)
    for (constructor in c("fixed", "grouping_set", "grouping_sets",
                          "grouping_spec", "rollup", "cube")) {
      actual <- observe(function() {
        if (constructor == "fixed") {
          return(inspect_grouping(input, .by = !!selection, .format = "list"))
        }
        specification <- eval(rlang::call2(constructor, selection))
        inspect_grouping(input, .grouping = specification, .format = "list")
      })
      if (inherits(expected, "error")) {
        ok <- inherits(actual, "error") &&
          identical(class(actual), class(expected))
        category <- "external_error"
      } else if (any(names(expected) != vars[unname(expected)])) {
        ok <- inherits(actual, "marginplyr_error") &&
          grepl("Can't rename", conditionMessage(actual), fixed = TRUE)
        category <- "rename"
      } else if (!length(expected) && constructor %in% c("rollup", "cube")) {
        ok <- inherits(actual, "marginplyr_error") &&
          grepl(
            "requires at least one dimension", conditionMessage(actual),
            fixed = TRUE
          )
        category <- "empty"
      } else {
        single_set <- c(
          "fixed", "grouping_set", "grouping_sets", "grouping_spec"
        )
        expected_sets <- if (constructor %in% single_set) {
          list(names(expected))
        } else if (constructor == "rollup") {
          lapply(rev(seq.int(0L, length(expected))), function(n) {
            names(expected)[seq_len(n)]
          })
        } else {
          indices <- unlist(
            lapply(rev(seq.int(0L, length(expected))), function(n) {
              utils::combn(seq_along(expected), n, simplify = FALSE)
            }),
            recursive = FALSE
          )
          lapply(indices, function(i) names(expected)[i])
        }
        answer <- if (inherits(actual, "error")) {
          actual
        } else if (constructor == "fixed") {
          actual$fixed
        } else {
          actual$included
        }
        ok <- identical(answer, expected_sets)
        category <- "success"
      }
      if (ok) {
        counts[[category]] <- counts[[category]] + 1L
      } else {
        failures[[length(failures) + 1L]] <- list(
          label = entry$label, backend = backend, constructor = constructor,
          expected = expected, actual = actual, category = category
        )
      }
    }
  }
}
cat(
  "Expressions:", length(expressions),
  "comparisons:", sum(counts) + length(failures), "\n"
)
print(counts)
cat("Mismatches:", length(failures), "\n")
if (length(failures)) {
  print(utils::head(failures, 8L))
  saveRDS(
    failures,
    file.path(tempdir(), "marginplyr-closure-selection-mismatches.rds")
  )
}
stopifnot(length(failures) == 0L)
