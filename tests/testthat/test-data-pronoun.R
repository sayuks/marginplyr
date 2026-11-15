# Generated expressions reference source columns through `.data[["name"]]`
# built directly, not through unquoting inside the pronoun. rlang
# soft-deprecated the latter and signals a lifecycle condition for it, which
# would otherwise reach every caller handling conditions around a Margin verb
# — including one catching `marginplyr_error`.

lifecycle_signals <- function(expr) {
  count <- 0L
  withCallingHandlers(
    force(expr),
    condition = function(cnd) {
      if (inherits(cnd, "lifecycle_stage")) {
        count <<- count + 1L
      }
    }
  )
  count
}

pronoun_data <- function() {
  data.frame(
    g = c("a", "a", "b"),
    h = c("x", "y", "x"),
    v = c(1, 2, 3)
  )
}

# A factor dimension routes through the label and factor-reconstruction paths,
# which a character dimension never reaches. Before the change these signalled
# 5, 2, and 3 conditions respectively, against 0 for the character fixture —
# so the character calls below assert nothing on their own.
pronoun_factor_data <- function() {
  data.frame(
    g = factor(c("a", "a", "b")),
    h = c("x", "y", "x"),
    v = c(1, 2, 3)
  )
}

test_that("margin_column_pronoun() builds the pronoun call it replaces", {
  expect_equal(
    margin_column_pronoun("region"),
    quote(.data[["region"]])
  )
  expect_equal(lifecycle_signals(margin_column_pronoun("region")), 0L)
})

test_that("Margin verbs signal no lifecycle condition", {
  expect_equal(
    lifecycle_signals(summarize_with_margins(
      pronoun_data(),
      s = sum(v),
      .grouping = rollup(g, h)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(summarize_with_margins(
      pronoun_data(),
      s = sum(v),
      p = share_of_parent(s),
      .grouping = rollup(g, h)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(expand_with_margins(
      pronoun_data(),
      .grouping = rollup(g)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(nest_with_margins(
      pronoun_data(),
      .grouping = rollup(g)
    )),
    0L
  )
})

test_that("the label, factor, and keep paths signal no lifecycle condition", {
  expect_equal(
    lifecycle_signals(summarize_with_margins(
      pronoun_factor_data(),
      s = sum(v),
      .grouping = rollup(g, h)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(expand_with_margins(
      pronoun_factor_data(),
      .grouping = rollup(g)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(nest_with_margins(
      pronoun_factor_data(),
      .grouping = rollup(g),
      .keep = TRUE
    )),
    0L
  )
})

test_that("margin_column_pronoun() rejects a name that is not a literal", {
  expect_error(margin_column_pronoun(quote(region)))
  expect_error(margin_column_pronoun(c("a", "b")))
})

test_that("results are unchanged by the construction", {
  result <- summarize_with_margins(
    pronoun_data(),
    s = sum(v),
    p = share_of_parent(s),
    .grouping = rollup(g, h)
  )

  expect_equal(result$s, c(1, 2, 3, 3, 3, 6))
  expect_equal(result$p, c(1 / 3, 2 / 3, 1, 0.5, 0.5, 1))
})

test_that("a source column named `.data` does not reach the pronoun", {
  # The pronoun always refers to the mask, so a column of that name is data
  # like any other. It is exercised here because the replacement builds the
  # pronoun symbol itself. `all_of()` is required to select it, exactly as in
  # plain dplyr: a bare `.data` is the pronoun in any tidyselect context.
  data <- data.frame(g = c("a", "b"), .data = c(1, 2), v = c(1, 2))

  result <- summarize_with_margins(
    data,
    s = sum(v),
    .by = dplyr::all_of(".data"),
    .grouping = rollup(g)
  )

  expect_equal(nrow(result), 4L)
  expect_equal(sum(result$s), 6)
})

# The per-verb assertions above are kept because they record the measured
# counts this change was made to remove, but they cannot be the whole guard:
# they only fail when a new site sits on a path one of those six calls
# executes. That is not a theoretical gap. This branch was written against
# fifteen sites; while it waited for review, a sixteenth arrived in
# `R/factor.R` on an unrelated commit, and no call asserted above reaches it.
# The scan below is coverage-independent — it reads the namespace's own
# expressions, so a new site fails it wherever it is written, including in
# code no test executes.
unquoted_pronoun_sites <- function(ns) {
  hits <- character()

  is_unquote <- function(x) {
    is.call(x) && identical(x[[1]], quote(`!`)) && length(x) == 2L &&
      is.call(x[[2]]) && identical(x[[2]][[1]], quote(`!`))
  }

  for (nm in namespace_functions(ns)) {
    visit_calls(body(get(nm, envir = ns)), function(node) {
      is_pronoun_index <- identical(node[[1]], quote(`[[`)) &&
        length(node) >= 3L &&
        identical(node[[2]], quote(.data))
      if (is_pronoun_index && is_unquote(node[[3]])) {
        hits <<- c(hits, nm)
      }
    })
  }
  sort(unique(hits))
}

test_that("no function in the package unquotes inside the `.data` pronoun", {
  # Reads the loaded namespace rather than the sources under `R/`, which are
  # not installed alongside the tests.
  expect_equal(unquoted_pronoun_sites(asNamespace("marginplyr")), character())
})

test_that("the pronoun scan detects the shape it exists to reject", {
  # Without this, the scan above would keep passing if the matcher broke.
  ns <- new.env(parent = emptyenv())
  ns$offender <- function(col) rlang::expr(.data[[!!col]])
  ns$innocent <- function(col) margin_column_pronoun(col)

  expect_equal(unquoted_pronoun_sites(ns), "offender")
})

# An injected `.data[[key]]` reaches the Margin verb still holding the symbol,
# and dplyr evaluates `key` in the summary's quosure environment (#1665). The
# global `key` names another column, so a lookup that escaped that environment
# would read it silently rather than fail.
test_that("an injected .data subscript reads the quosure environment's key", {
  data <- data.frame(g = c("a", "b"), v = c(1, 3), w = c(100, 300))
  summarize_through <- function(data, grouping, with_share) {
    key <- "v"
    read <- quote(.data[[key]])
    if (with_share) {
      summarize_with_margins(
        data, t = sum(!!read), s = share_of_total(t), .grouping = !!grouping
      )
    } else {
      summarize_with_margins(data, t = sum(!!read), .grouping = !!grouping)
    }
  }
  summarize_oracle <- function(data) {
    key <- "v"
    read <- quote(.data[[key]])
    dplyr::summarize(data, t = sum(!!read), .by = g)$t
  }
  expect_reads_v <- function() {
    expect_identical(summarize_oracle(data), c(1, 3))
    groupings <- list(
      quote(rollup(g)),
      quote(grouping_sets(grouping_set(g), grouping_set()))
    )
    for (grouping in groupings) {
      for (with_share in c(FALSE, TRUE)) {
        result <- summarize_through(data, grouping, with_share)
        expect_identical(result$t, c(1, 3, 4))
      }
    }
    expect_identical(
      summarize_through(data, quote(grouping_set(g)), FALSE)$t, c(1, 3)
    )
  }

  expect_reads_v()
  skip_if(exists("key", envir = globalenv(), inherits = FALSE))
  assign("key", "w", envir = globalenv())
  withr::defer(rm("key", envir = globalenv()))
  expect_reads_v()
})
