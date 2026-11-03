test_that("ordinary across names use the complete naming context", {
  data <- data.frame(x = 1:3, y = 5:7)
  templates <- c(
    "{.col}_{seq_along(.col)}", "{.col}_{.fn}_{length(.col)}",
    "{c('first', 'second')}", "{.col}_{.fn}_{seq_along(.fn)}"
  )
  for (template in templates) {
    for (fns in rlang::exprs(sum, list(total = sum, average = mean))) {
      if (template == "{c('first', 'second')}" && length(fns) > 1L) next
      expression <- rlang::expr(dplyr::across(
        c(x, y), !!fns, .names = !!template
      ))
      expected <- dplyr::summarise(data, !!expression)
      actual <- summarize_with_margins(data, !!expression)
      expect_identical(actual, expected)
    }
  }
})

test_that("dtplyr across names use the complete column and function context", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(x = 1:3, y = 5:7))
  for (template in c("{.col}_{seq_along(.col)}", "{c('first', 'second')}")) {
    expected <- dplyr::collect(dplyr::summarise(
      data, dplyr::across(c(x, y), sum, .names = template)
    ))
    actual <- dplyr::collect(summarize_with_margins(
      data, dplyr::across(c(x, y), sum, .names = template)
    ))
    expect_identical(actual, expected)
  }
})

test_that("planned across names are shared with source and output naming", {
  data <- data.frame(x = 1:3, y = 5:7)
  run <- function(input, shares = FALSE) {
    calls <- 0L
    mark <- function(value) {
      calls <<- calls + 1L
      value
    }
    outputs <- if (shares) rlang::exprs(
      p = share_of_total(y_2),
      dplyr::across(c(x_1, y_2), share_of_total,
                    .names = "{.col}_p_{mark(seq_along(.col))}")
    ) else list()
    result <- summarize_with_margins(
      input, dplyr::across(
        c(x, y), sum, .names = "{.col}_{mark(seq_along(.col))}"
      ),
      !!!outputs
    )
    list(result = result, calls = calls)
  }
  ordinary_calls <- 0L
  ordinary_mark <- function(value) {
    ordinary_calls <<- ordinary_calls + 1L
    value
  }
  expected <- dplyr::summarise(data, dplyr::across(
    c(x, y), sum, .names = "{.col}_{ordinary_mark(seq_along(.col))}"
  ))
  expect_identical(ordinary_calls, 1L)
  actual <- run(data, shares = TRUE)
  expect_identical(actual$calls, 2L)
  expect_identical(actual$result[names(expected)], expected)
  expect_identical(actual$result$p, 1)
  expect_identical(actual$result$x_1_p_1, 1)
  expect_identical(actual$result$y_2_p_2, 1)
})

test_that("lazy planning evaluates naming braces once and keeps share names", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(x = 1:3, y = 5:7))
  for (shares in c(FALSE, TRUE)) {
    calls <- 0L
    mark <- function(value) {
      calls <<- calls + 1L
      value
    }
    outputs <- if (shares) rlang::exprs(
      p = share_of_total(y_2),
      dplyr::across(c(x_1, y_2), share_of_total,
                    .names = "{c('first', 'second')}")
    ) else list()
    result <- summarize_with_margins(
      data, dplyr::across(c(x, y), sum,
                          .names = "{col}_{mark(seq_along(fn))}"),
      !!!outputs
    )
    expect_identical(calls, 1L)
    actual <- dplyr::collect(result)
    expect_identical(actual$x_1, 6L)
    expect_identical(actual$y_2, 18L)
    if (shares) {
      expect_identical(actual$p, 1)
      expect_identical(actual$first, 1)
      expect_identical(actual$second, 1)
    }
  }
})

test_that("vector templates keep lazy SQL output order", {
  data <- dbplyr::lazy_frame(x = 1:3, y = 5:7)
  for (template in c("{.col}_{seq_along(.col)}",
                     "{.col}_{.fn}_{length(.col)}")) {
    calls <- 0L
    names_spec <- function() {
      calls <<- calls + 1L
      template
    }
    expected <- dplyr::summarise(data, dplyr::across(
      c(x, y), list(total = sum, average = mean), .names = names_spec()
    ))
    expect_identical(calls, 1L)
    calls <- 0L
    actual <- summarize_with_margins(data, dplyr::across(
      c(x, y), list(total = sum, average = mean), .names = names_spec()
    ))
    expect_identical(calls, 1L)
    expect_identical(colnames(actual), colnames(expected))
  }
})

test_that("vector share names retain size, uniqueness and collision checks", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  templates <- c("{c('only')}", "{c('a', 'b', 'c')}",
                 "{c('same', 'same')}", "{c('', 'valid')}",
                 "{c('g', 'valid')}", "{c('set', 'valid')}")
  for (template in templates) {
    expect_error(summarize_with_margins(
      data, dplyr::across(c(x, y), sum),
      dplyr::across(c(x, y), share_of_total, .names = template),
      .grouping = rollup(g), .id = "set"
    ), class = "marginplyr_error", info = template)
  }
})

test_that("an empty lazy selection does not require a naming output", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(x = 1:3))
  expected <- dplyr::collect(dplyr::summarise(
    data, dplyr::across(dplyr::any_of("absent"), sum, .names = "total")
  ))
  actual <- dplyr::collect(summarize_with_margins(
    data, dplyr::across(dplyr::any_of("absent"), sum, .names = "total")
  ))
  expect_identical(actual, expected)
})

# Compare the ordinary backend's complete output with the public Margin result.
expect_vector_across_backend <- function(data, shares = TRUE) {
  for (template in c("{.col}_{seq_along(.col)}",
                     "{.col}_{.fn}_{length(.col)}")) {
    expression <- rlang::expr(dplyr::across(
      c("x", "y"), list(total = sum, average = mean), .names = !!template
    ))
    expected <- dplyr::collect(dplyr::summarise(data, !!expression))
    outputs <- if (shares) rlang::exprs(
      dplyr::across(dplyr::everything(), share_of_total,
                    .names = "p_{seq_along(.col)}")
    ) else list()
    actual <- dplyr::collect(summarize_with_margins(
      data, !!expression, !!!outputs
    ))
    expect_equal(actual[names(expected)], expected)
    if (shares) {
      expect_identical(names(actual), c(names(expected), paste0("p_", 1:4)))
      expect_equal(unname(unlist(actual[paste0("p_", 1:4)])), rep(1, 4))
    }
  }
}

test_that("dtplyr shares retain vectorized multi-function names", {
  skip_if_suggest_absent("dtplyr")
  expect_vector_across_backend(dtplyr::lazy_dt(data.frame(x = 1:3, y = 5:7)))
})

test_that("DuckDB shares retain vectorized multi-function names", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- dplyr::copy_to(con, data.frame(x = 1:3, y = 5:7), "vector_names")
  expect_vector_across_backend(data)
})

test_that("Arrow retains ordinary vectorized multi-function names", {
  skip_if_suggest_absent("arrow")
  expect_vector_across_backend(
    arrow::arrow_table(data.frame(x = 1:3, y = 5:7)), shares = FALSE
  )
})

test_that("preceded local across exposes context-only vector names to shares", {
  data <- data.frame(x = 1:3, y = 5:7)
  for (fns in rlang::exprs(sum, list(total = sum, average = mean))) {
    templates <- c(
      "{.col}_{seq_along(.col)}", "{.col}_{.fn}_{length(.col)}",
      "{.col}_{rev(.col)}_{.fn}", "{.col}_{match(.col, .col)}_{.fn}"
    )
    templates <- c(templates, if (rlang::is_symbol(fns)) {
      "{c('first', 'second')}"
    } else {
      "{c('first', 'second', 'third', 'fourth')}"
    })
    for (template in templates) {
      expression <- rlang::expr(dplyr::across(
        c(x, y), !!fns, .names = !!template
      ))
      ordinary <- dplyr::summarise(data, prior = sum(x), !!expression)
      source <- tail(names(ordinary), 1L)
      actual <- summarize_with_margins(
        data, prior = sum(x), !!expression,
        p = share_of_total(!!rlang::sym(source))
      )
      expect_identical(actual[names(ordinary)], ordinary)
      expect_identical(actual$p, 1)
    }
  }
})

test_that("preceded name prediction does not invoke active function bindings", {
  data <- data.frame(x = 1:3, y = 5:7)
  observations <- list()
  for (mode in c("ordinary", "margin")) {
    reads <- 0L
    env <- rlang::env(environment())
    makeActiveBinding("seq_along", function() {
      reads <<- reads + 1L
      base::seq_along
    }, env)
    expression <- rlang::new_quosure(quote(dplyr::across(
      c(x, y), sum, .names = "{.col}_{seq_along(.col)}"
    )), env)
    result <- if (mode == "ordinary") {
      dplyr::summarise(data, prior = sum(x), !!expression)
    } else {
      summarize_with_margins(
        data, prior = sum(x), !!expression, p = share_of_total(prior)
      )
    }
    observations[[mode]] <- list(result = result[c("prior", "x_1", "y_2")],
                                 reads = reads)
  }
  expect_identical(observations$margin, observations$ordinary)
  expect_identical(observations$ordinary$reads, 1L)
})

test_that("preceded naming callbacks stay in the ordinary local context", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  observations <- list()
  for (mode in c("ordinary", "margin")) {
    sizes <- integer()
    mark <- function(value) {
      sizes <<- c(sizes, dplyr::n())
      value
    }
    dots <- rlang::exprs(
      prior = sum(x),
      dplyr::across(c(x, y), sum, .names = "{.col}_{mark(seq_along(.col))}")
    )
    result <- if (mode == "ordinary") {
      dplyr::summarise(data, !!!dots, .by = g)
    } else {
      summarize_with_margins(data, !!!dots, p = share_of_total(prior), .by = g)
    }
    observations[[mode]] <- list(result = result[setdiff(names(result), "p")],
                                 sizes = sizes)
  }
  expect_identical(observations$margin, observations$ordinary)
  expect_length(observations$ordinary$sizes, 1L)
})

test_that("preceded name-only selections expose vector names to shares", {
  data <- data.frame(x = 1:3, y = 5:7)
  template <- "{.col}_{seq_along(.col)}"
  columns <- c("x", "y")
  for (cols in rlang::exprs(x:y, dplyr::everything(),
                            dplyr::all_of(c("x", "y")),
                            dplyr::all_of(columns))) {
    ordinary <- dplyr::summarise(
      data, prior = sum(x), dplyr::across(!!cols, sum, .names = template)
    )
    actual <- summarize_with_margins(
      data, prior = sum(x),
      dplyr::across(!!cols, sum, .names = template),
      p = share_of_total(y_2)
    )
    expect_identical(actual[names(ordinary)], ordinary)
    expect_identical(actual$p, 1)
  }
})

test_that("preceded helper arguments retain their local data-mask context", {
  data <- data.frame(x = 1:3, y = 5:7)
  observations <- list()
  for (mode in c("ordinary", "margin")) {
    sizes <- integer()
    combine <- function(...) {
      sizes <<- base::c(sizes, dplyr::n())
      base::c(...)
    }
    env <- rlang::env(environment(), c = combine)
    expression <- rlang::new_quosure(quote(dplyr::across(
      dplyr::all_of(c("x", "y")), sum,
      .names = "{.col}_{seq_along(.col)}"
    )), env)
    result <- if (mode == "ordinary") {
      dplyr::summarise(data, prior = sum(x), !!expression)
    } else {
      summarize_with_margins(
        data, prior = sum(x), !!expression, p = share_of_total(prior)
      )
    }
    observations[[mode]] <- list(result = result[c("prior", "x_1")],
                                 sizes = sizes)
  }
  expect_identical(observations$margin, observations$ordinary)
  expect_identical(observations$ordinary$sizes, 0L)
})

test_that("bare selection helpers retain tidyselect's own bindings", {
  data <- data.frame(x = 1:3)
  all_of <- function(x) stop(paste("caller all_of", x))
  ordinary <- dplyr::summarise(
    data, prior = sum(x),
    dplyr::across(all_of("x"), sum, .names = "{.col}_{seq_along(.col)}")
  )
  actual <- summarize_with_margins(
    data, prior = sum(x),
    dplyr::across(all_of("x"), sum, .names = "{.col}_{seq_along(.col)}"),
    p = share_of_total(x_1)
  )
  expect_identical(ordinary$x_1, 6L)
  expect_identical(actual[names(ordinary)], ordinary)
  expect_identical(actual$p, 1)
})

test_that("preceding template rebinding stays with ordinary evaluation", {
  data <- data.frame(x = 1:3, y = 5:7)
  observations <- list()
  for (mode in c("ordinary", "margin")) {
    template <- "{.col}_old"
    dots <- rlang::exprs(
      prior = {
        template <<- "{.col}_new"
        sum(x)
      },
      dplyr::across(c(x, y), sum, .names = template)
    )
    result <- if (mode == "ordinary") {
      dplyr::summarise(data, !!!dots)
    } else {
      summarize_with_margins(data, !!!dots, p = share_of_total(prior))
    }
    observations[[mode]] <- result[setdiff(names(result), "p")]
  }
  expect_identical(names(observations$ordinary), c("prior", "x_new", "y_new"))
  expect_identical(observations$margin, observations$ordinary)
})

test_that("preceding callbacks retain runtime naming bindings", {
  data <- data.frame(x = 1:3, y = 5:7)
  for (binding in c("template", "columns", "seq_along")) {
    observations <- list()
    for (mode in c("ordinary", "margin")) {
      template <- "{.col}_{seq_along(.col)}"
      columns <- c("x", "y")
      seq_along <- base::seq_along
      calls <- 0L
      change <- function(x) {
        calls <<- calls + 1L
        if (binding == "template") template <<- "{.col}_new"
        if (binding == "columns") columns <<- "y"
        if (binding == "seq_along") {
          seq_along <<- function(x) rev(base::seq_along(x))
        }
        sum(x)
      }
      dots <- rlang::exprs(
        prior = change(x),
        dplyr::across(dplyr::all_of(columns), sum, .names = template)
      )
      result <- if (mode == "ordinary") {
        dplyr::summarise(data, !!!dots)
      } else {
        summarize_with_margins(data, !!!dots, p = share_of_total(prior))
      }
      observations[[mode]] <- list(
        result = result[setdiff(names(result), "p")], calls = calls
      )
    }
    expect_identical(observations$margin, observations$ordinary, info = binding)
    expect_identical(observations$ordinary$calls, 1L)
  }
})

test_that("caller data getters remain opaque to name caching", {
  for (getter in c("$", "[[")) {
    observations <- list()
    for (mode in c("ordinary", "margin")) {
      env <- rlang::env(environment(), data = data.frame(x = 1:3, y = 5:7),
                        template = "{.col}_old", calls = 0L)
      env[[getter]] <- evalq(function(value, name) {
        calls <<- calls + 1L
        template <<- "{.col}_new"
        base::`[[`(value, "x")
      }, env)
      prior <- if (getter == "$") {
        quote(sum(.data$x))
      } else {
        quote(sum(.data[["x"]]))
      }
      dot <- rlang::new_quosure(prior, env)
      across <- rlang::new_quosure(quote(dplyr::across(
        c(x, y), sum, .names = template
      )), env)
      result <- if (mode == "ordinary") {
        dplyr::summarise(env$data, prior = !!dot, !!across)
      } else {
        summarize_with_margins(
          env$data, prior = !!dot, !!across, p = share_of_total(prior)
        )
      }
      observations[[mode]] <- list(
        result = result[setdiff(names(result), "p")], calls = env$calls
      )
    }
    expect_identical(observations$margin, observations$ordinary, info = getter)
    expect_identical(observations$ordinary$calls, 1L)
  }
})

test_that("all evaluated prefix arguments retain ordinary naming effects", {
  for (kind in c("additional", "unpack", "head", "literal")) {
    observations <- list()
    for (mode in c("ordinary", "margin")) {
      env <- rlang::env(environment(), template = "{.col}_old", calls = 0L)
      env$change <- evalq(function() {
        calls <<- calls + 1L
        template <<- "{.col}_new"
        TRUE
      }, env)
      prefix <- switch(kind,
        additional = quote(dplyr::across(
          x, sum, na.rm = change(), .names = "{.col}_first"
        )),
        unpack = quote(dplyr::across(
          x, sum, .unpack = {
            change()
            FALSE
          }, .names = "{.col}_first"
        )),
        literal = quote(dplyr::across(
          x, sum, na.rm = TRUE, .unpack = FALSE, .names = "{.col}_first"
        )),
        head = quote((sum)(x))
      )
      expressions <- if (kind == "head") {
        env[["("]] <- evalq(function(x) {
          change()
          x
        }, env)
        list(prior = prefix)
      } else {
        c(rlang::exprs(prior = sum(x)), list(prefix))
      }
      expressions <- c(expressions, rlang::exprs(dplyr::across(
        y, sum, .names = template
      )))
      dots <- lapply(expressions, rlang::new_quosure, env = env)
      data <- data.frame(x = 1:3, y = 5:7)
      shares <- if (kind == "literal") {
        rlang::exprs(p = share_of_total(y_old))
      } else {
        rlang::exprs(p = share_of_total(prior))
      }
      # Legacy across dots warn in dplyr while retaining their evaluation.
      result <- suppressWarnings(if (mode == "ordinary") {
        dplyr::summarise(data, !!!dots)
      } else {
        summarize_with_margins(data, !!!dots, !!!shares)
      })
      observations[[mode]] <- list(
        result = result[setdiff(names(result), "p")], calls = env$calls
      )
    }
    expect_identical(observations$margin, observations$ordinary, info = kind)
    expect_identical(observations$ordinary$calls, as.integer(kind != "literal"))
    expected <- if (kind == "literal") "y_old" else "y_new"
    expect_true(expected %in% names(observations$margin$result))
  }
})

test_that("plain summary prefixes retain complete-name source proofs", {
  data <- data.frame(x = 1:3, y = 5:7, unused = factor("a"))
  for (prior in rlang::exprs(sum(x), dplyr::n(), sum(.data$x),
                             sum(.data[["x"]]))) {
    actual <- summarize_with_margins(
      data, prior = !!prior,
      dplyr::across(x, sum, .names = "{.col}_first"),
      dplyr::across(y, sum, .names = "{.col}_{seq_along(.col)}"),
      p = share_of_total(y_1)
    )
    ordinary <- dplyr::summarise(
      data, prior = !!prior,
      dplyr::across(x, sum, .names = "{.col}_first"),
      dplyr::across(y, sum, .names = "{.col}_{seq_along(.col)}")
    )
    expect_identical(actual[names(ordinary)], ordinary)
    expect_identical(actual$p, 1)
  }
})
