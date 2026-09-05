margin_check_capture <- new.env(parent = emptyenv())

margin_check_collect <- function(con, sql, ...) {
  margin_check_capture$sql <- as.character(sql)
  check_names <- attr(con, "check_names", exact = TRUE)
  result <- as.data.frame(rep(list(FALSE), length(check_names)))
  names(result) <- check_names
  result
}

bad_query_sql_build <- function(op, con, ...) {
  dbplyr::sql("SELECT 1")
}

backend_dialect_error <- function(con) {
  rlang::abort(
    "Backend dialect classification failed.",
    class = "marginplyr_test_backend_error",
    provenance = "backend dialect"
  )
}

test_that("backend classification preserves backend conditions", {
  registerS3method(
    "sql_dialect",
    "marginplyr_backend_error_connection",
    backend_dialect_error,
    envir = asNamespace("dbplyr")
  )
  source <- dbplyr::tbl_lazy(
    data.frame(group = "x"),
    con = dbplyr::simulate_sqlite()
  )
  class(source$con) <- c(
    "marginplyr_backend_error_connection",
    class(source$con)
  )

  error <- expect_error(
    inspect_grouping(source, .grouping = rollup(group)),
    "Backend dialect classification failed"
  )

  expect_s3_class(error, "marginplyr_test_backend_error")
  expect_identical(error$provenance, "backend dialect")
  expect_false(inherits(error, "marginplyr_error"))
})

selection_proxy_capture <- new.env(parent = emptyenv())

proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_selection_proxy_counter", class(result)))
  result
}

proxy_counter_collect <- function(x, ...) {
  selection_proxy_capture$n <- selection_proxy_capture$n + 1L
  NextMethod()
}

normalized_contract_data <- function() {
  data.frame(
    a = c("x", "x", "y"),
    b = c("u", "v", "u"),
    value = 1:3
  )
}

test_that("dtplyr uses the normalized grouping contract", {
  skip_if_suggest_absent("dtplyr")
  data <- normalized_contract_data()

  expect_no_message(
    dt_result <- summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    ) |>
      dplyr::collect()
  )
  expect_setequal(unique(dt_result$gid), c(0L, 1L, 3L))
  expect_true(any(dt_result$a == "Total"))

  dt_rowwise <- nest_by_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(a, b)
  )
  expect_s3_class(dt_rowwise, "rowwise_df")
  expect_equal(names(dt_rowwise), c("a", "b", "data"))
})

test_that("Arrow uses the normalized grouping contract", {
  skip_if_suggest_absent("arrow")
  data <- normalized_contract_data()

  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    n = dplyr::n(),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b)
  ) |>
    dplyr::collect()
  expect_setequal(unique(arrow_result$gid), c(0L, 1L, 3L))
  expect_true(any(arrow_result$a == "Total"))

  factor_result <- summarize_with_margins(
    arrow::Table$create(data.frame(a = factor(c("x", "y")))),
    n = dplyr::n(),
    .grouping = rollup(a)
  ) |>
    dplyr::collect()
  expect_type(factor_result$a, "character")
  expect_true("Total" %in% factor_result$a)
})

# CONTEXT.md's *Absorbing backend*, asserted at the seam ADR 0025 draws it at.
# An Arrow `Table`, `RecordBatch`, or query over either answers an expression
# its own engine cannot evaluate by reading the whole input into R; a `Dataset`
# and a query over one refuse instead, and that refusal is Arrow's to raise.
# Both halves are asserted, because what the decision turns on is that the two
# are told apart (#254).
#
# The three input builders come from `helper-arrow-shapes.R`.
#
# The expressions are chosen for how long they will keep being absorbed rather
# than for how the defect was found. A group collapsed to one string, a subset
# inside an aggregate, and a statistic over two columns are the shapes least
# likely to gain an Arrow kernel, and are the three the reference names;
# `first()` and `last()` are absorbed today and deliberately unused here and
# there, being the likeliest of the absorbed set to stop being so. The block
# below asserts all three are still absorbed, so an Arrow release that
# translates one fails there, naming the drift, rather than here.
test_that("Arrow refuses a summary it would otherwise absorb", {
  skip_if_suggest_absent("arrow")
  data <- arrow_input_data()

  inputs <- absorbing_arrow_inputs(data)
  for (shape in names(inputs)) {
    raised <- expect_error(summarize_with_margins(
      inputs[[shape]],
      joined = paste(s, collapse = ","),
      .grouping = rollup(k)
    ), label = shape)

    # Every expectation carries the shape, so a failure on one input class
    # does not read as a failure on the three beside it.
    expect_true(inherits(raised, "marginplyr_error"), label = shape)
    # The condition this ticket exists to remove, named so that a regression
    # to it fails as itself rather than as a missing class.
    expect_false(inherits(raised, "notSubsettableError"), label = shape)
    text <- conditionMessage(raised)
    # The argument as the caller spelled it, not the expression Arrow blamed
    # inside it and not the internal key columns the branch grouped by.
    expect_match(
      text,
      "joined = paste(s, collapse = \",\")",
      fixed = TRUE,
      info = shape
    )
    # The singular arm of this diagnostic, both inflections of it; the plural
    # one is the guard block below, which names every summary argument because
    # it has no warning to place one from.
    expect_match(
      text, "Arrow cannot evaluate this summary",
      fixed = TRUE, info = shape
    )
    expect_match(text, "to compute it:", fixed = TRUE, info = shape)
    # Both rewrites, each matched on a span the other does not carry. The
    # second bullet holds the words `collect` and `column` between them, so a
    # pair of assertions on those alone is satisfied by that bullet by itself
    # and passes with the first rewrite deleted -- which is the half a caller
    # who cannot narrow their input still needs.
    expect_match(
      text, "Collect the Arrow input first",
      fixed = TRUE, info = shape
    )
    # The second is the whole reason refusing beats absorbing: Arrow reads
    # every column, and a caller who is told can read fewer.
    expect_match(
      text, "Select the columns you need before collecting",
      fixed = TRUE, info = shape
    )
  }
})

# Which summaries the refusal names is a function of how the installed Arrow
# phrases its warning, and both phrasings are inside the range `DESCRIPTION`
# admits, so it is asserted at the reading rather than through a verb: a
# verb-level assertion would either hold on one half of that range only, or --
# by asserting the blamed summary appears, which it does on both halves --
# pass whatever the reading answered.
#
# Both phrasings, over synthesised warnings, because no session holds both
# Arrows at once and `verify-backend.R` fails a job for a skip naming no
# withheld backend -- so a version-gated assertion is not available and would
# leave whichever half CI does not install unasserted.
#
# From Arrow 17.0.0 the warning opens `In <expr>: `, which places the blame on
# one argument. Through 16.0.0 it names the expression inside a sentence, which
# places it on none, and the refusal then names every summary argument rather
# than guessing. Only the second is a claim about a version this package cannot
# install here; both are claims about this reading.
test_that("an absorbed summary is placed from the warning that carries it", {
  dots <- rlang::quos(total = sum(v), joined = paste(s, collapse = ","))
  labels <- c("total = sum(v)", "joined = paste(s, collapse = \",\")")
  placed <- function(message) {
    absorbed_summary_labels(
      rlang::warning_cnd(message = message),
      dots = dots,
      caller_labels = labels
    )
  }

  expect_identical(
    placed("In paste(s, collapse = \",\"): \nPulling data into R"),
    labels[[2L]]
  )
  expect_identical(
    placed("object of type 'closure'; pulling data into R"),
    labels
  )
  # An expression that matches no argument places nothing either, which is the
  # degradation ADR 0022 accepts for a span it cannot recognise.
  expect_identical(placed("In nothing_written_here(): \nx"), labels)
  # And a message holding no lines at all takes that route rather than a guard
  # of its own. The handler cannot deliver one, an empty message matching no
  # marker, so the reading answers for it by indexing rather than by branching.
  expect_identical(placed(""), labels)
})

# Arrow's convention for the label it blames, which the refusal reproduces
# rather than calling the internal that writes it (ADR 0022). What makes the
# reproduction worth asserting on its own is that it agrees with both obvious
# alternatives everywhere except one place: an expression that deparses to a
# single line is spelled identically by `deparse()`, `rlang::as_label()`, and
# Arrow. Every other expression in this file is one of those, so without the
# long case here the convention could be swapped for `as_label()` and the whole
# suite would stay green -- and the refusal would then place nothing on any
# expression long enough to be worth reading, silently naming every summary
# argument instead of the blamed one.
#
# Asserted as properties rather than against `arrow:::format_expr()`: an
# assertion on another package's internal is the coupling ADR 0022 declines,
# and it would make this reachable only where Arrow is installed, where its
# siblings above run everywhere.
test_that("an absorbed expression is labelled by Arrow's convention", {
  short <- quote(paste(s, collapse = ","))
  expect_length(deparse(short), 1L)
  expect_identical(
    absorbed_expression_label(short),
    "paste(s, collapse = \",\")"
  )

  # Past the deparse width the three spellings part company: Arrow keeps the
  # first line and marks the loss, while `as_label()` abbreviates the arguments
  # away to `stats::weighted.mean(...)`, which matches no line Arrow writes.
  long <- quote(stats::weighted.mean(
    value_column_with_a_long_name,
    weight_column_with_a_long_name,
    na.rm = TRUE
  ))
  expect_gt(length(deparse(long)), 1L)

  labelled <- absorbed_expression_label(long)
  expect_length(labelled, 1L)
  expect_true(startsWith(labelled, deparse(long)[[1L]]))
  expect_true(endsWith(labelled, "..."))
  expect_false(identical(labelled, rlang::as_label(long)))
})

# The other half of the handler's reading, asserted the same way and for the
# same reason. Which text marks an absorption is a function of the installed
# Arrow, both phrasings are inside the range `DESCRIPTION` admits, and no
# session holds both -- so the live gate below sees whichever one CI installed
# and cannot answer for the other. Left to that gate alone, capitalising the
# marker and dropping `ignore.case` stays green on every Arrow this package can
# be tested against while switching the refusal off for four versions it
# claims, which is not hypothetical: that is the state this branch found the
# handler in (#254).
#
# The inputs carry a class attribute rather than being Arrow objects, which is
# the whole of what `inherits()` reads, so this runs wherever the suite does --
# a claim about a reading, like its sibling above, and not about a backend.
test_that("an absorption is recognised on every phrasing and only on Arrow", {
  # Built with `structure()` for the reason `test-execution-conditions.R` gives
  # about its own foreign condition: `warning_cnd()` cannot express the case.
  # It collapses a vector `message` to one string, which is exactly the shape
  # the reading guards against, so the guard could not be reached through it.
  warned <- function(message) {
    structure(
      list(message = message, call = NULL),
      class = c("warning", "condition")
    )
  }
  # Every class the handler scopes itself by, not one of them. `Dataset` is in
  # the vector and does not absorb; what the loop asserts is the scoping
  # contract -- that the reading answers for an Arrow input and no other -- and
  # deriving it is what makes the R-side claim that the two readings share one
  # class list an assertion rather than a sentence.
  for (cls in arrow_input_classes()) {
    input <- structure(list(), class = cls)
    marked <- function(message) {
      is_absorbing_backend_warning(warned(message), input)
    }

    # From Arrow 17.0.0, and through 16.0.0, which differ in the case of the
    # phrase and in nothing else the reading depends on. `info` rather than
    # `label`, which would replace the expression and leave a reader knowing
    # which class failed but not which of the four readings did.
    expect_true(
      marked("In paste(s, collapse = \",\"): \nPulling data into R"),
      info = cls
    )
    expect_true(
      marked("object of type 'closure'; pulling data into R"),
      info = cls
    )
    expect_false(marked("Expression not supported in Arrow"), info = cls)

    # A condition class of another package's may carry a message method
    # answering with a vector, and `grepl()` over one answers for whichever
    # element matched. Hence the shape check ahead of the match.
    expect_false(marked(c("Pulling data into R", "x")), info = cls)
  }

  # `.data` decides as well as the text. A caller's own summary expression may
  # spell anything, so the same text over an input that is not Arrow's is some
  # other backend's warning, and a refusal naming Arrow would be wrong twice
  # over.
  not_arrow <- function(.data) {
    is_absorbing_backend_warning(warned("Pulling data into R"), .data)
  }
  expect_false(not_arrow(data.frame(k = 1)))
  expect_false(not_arrow(structure(list(), class = "dtplyr_step")))
})

test_that("Arrow refuses a subset inside an aggregate", {
  skip_if_suggest_absent("arrow")

  raised <- expect_error(summarize_with_margins(
    arrow::Table$create(arrow_input_data()),
    kept = sum(v[v > 1]),
    .grouping = rollup(k)
  ))

  expect_s3_class(raised, "marginplyr_error")
  expect_match(
    conditionMessage(raised),
    "kept = sum(v[v > 1])",
    fixed = TRUE
  )
})

test_that("an Arrow Dataset keeps Arrow's own refusal", {
  skip_if_suggest_absent("arrow")
  data <- arrow_input_data()

  inputs <- refusing_arrow_inputs(data)
  for (shape in names(inputs)) {
    raised <- expect_error(summarize_with_margins(
      inputs[[shape]],
      joined = paste(s, collapse = ","),
      .grouping = rollup(k)
    ), label = shape)

    # An External condition: Arrow's answer to the question put to it, and
    # marginplyr's only part in it is the context it carries. The diagnostic
    # is asserted as well as the class, since propagating one without the
    # other is what ADR 0015 forbids.
    expect_true(inherits(raised, "arrow_not_supported"), label = shape)
    expect_false(inherits(raised, "marginplyr_error"), label = shape)
    expect_match(
      conditionMessage(raised),
      "Call collect() first",
      fixed = TRUE,
      info = shape
    )
  }
})

test_that("an Arrow summary Arrow can evaluate is unchanged and stays lazy", {
  skip_if_suggest_absent("arrow")

  result <- summarize_with_margins(
    arrow::Table$create(arrow_input_data()),
    total = sum(v),
    .grouping = rollup(k)
  )

  expect_s3_class(result, "arrow_dplyr_query")
  expect_setequal(dplyr::collect(result)$total, c(3, 3, 6))
})

# Part one of the two-part regression. The refusal above asserts what
# marginplyr does with an absorbed expression; this asserts that Arrow still
# absorbs the two expressions it is asserted over, and that Arrow still marks
# the absorption with the text the handler keys on.
#
# The text dependency is what makes this block load-bearing rather than
# redundant. Arrow's fallback warning carries no class, no `$parent`, and no
# `$call`, so its message is the only thing to recognise it by; that is
# undocumented behaviour, and this is what keeps it honest, the way
# `document.yaml` keeps the roxygen table-row exception honest.
#
# The absorption is recognised by calling the handler's own reading rather than
# by matching the marker again here. A second match would answer for its own
# spelling and not the handler's: it would report Arrow as still marking the
# absorption while the handler had stopped recognising it, which is exactly the
# drift this block exists to catch.
test_that("Arrow still absorbs the expressions the refusal is asserted over", {
  skip_if_suggest_absent("arrow")
  table <- arrow::Table$create(arrow_input_data())
  raised <- NULL
  # One per shape the shipped pages describe, so a page that stops being true
  # fails here rather than being re-read. `first()` and `last()` are absorbed
  # too and are in none of them: they are the likeliest of the absorbed set to
  # gain an Arrow kernel, so a page naming them and a test asserting them would
  # both be claims with a short life.
  absorbed <- list(
    collapsed = quote(paste(s, collapse = ",")),
    subset = quote(sum(v[v > 1])),
    two_column = quote(stats::weighted.mean(v, v))
  )

  for (shape in names(absorbed)) {
    marked <- FALSE
    result <- withCallingHandlers(
      rlang::inject(
        dplyr::summarize(table, out = !!absorbed[[shape]], .by = k)
      ),
      warning = function(cnd) {
        if (is_absorbing_backend_warning(cnd, table)) {
          marked <<- TRUE
          raised <<- cnd
        }
        invokeRestart("muffleWarning")
      }
    )

    # Labelled, because this is the assertion an Arrow release moving the
    # boundary is meant to fail: it has to name which shape moved.
    expect_true(marked, label = paste("Arrow marks the absorbed", shape))
    # Absorbed rather than translated: a local frame is what Arrow answers
    # with once it has pulled the input into R. `expect_true()` rather than
    # `expect_s3_class()`, which takes no label, and a label is what tells a
    # reader which shape moved.
    expect_true(
      inherits(result, "data.frame"),
      label = paste("Arrow answers the absorbed", shape, "with a local frame")
    )
  }

  # And the ordinary numeric summaries the same pages call unaffected.
  translated <- suppressWarnings(
    dplyr::summarize(table, out = sum(v) / dplyr::n(), .by = k)
  )
  expect_s3_class(translated, "arrow_dplyr_query")

  # The second undocumented Arrow text the refusal reads: the `In <expr>: `
  # header the blamed summary is placed from. Asserted against the warning the
  # installed Arrow actually raised, because a rewording of it would silently
  # degrade every refusal to naming all of a call's summaries, and the suite
  # would stay green -- every other assertion here passes a single summary,
  # where naming the blamed one and naming them all are the same answer.
  #
  # Two answers are correct and a third is not. From Arrow 17.0.0 the header is
  # present and carries Arrow's own deparse of the expression; through 16.0.0
  # there is no header and `NA` is right, that being the degradation the
  # refusal is designed around. A header this reading half-recognises would
  # return some other string, and that is what fails here.
  placed <- absorbing_warning_label(raised)
  expect_true(
    is.na(placed) || identical(placed, "stats::weighted.mean(v, v)"),
    label = paste0("the label placed from Arrow's warning (", placed, ")")
  )
})

# The backstop, reached by taking the marker away -- which is what an Arrow
# release that rewords the warning would do. The handler then does not fire,
# Arrow absorbs, and the branch result is a local frame the input was not: the
# guard raises the same refusal, so the caller sees what they should have seen
# and only the CI gate below records the drift.
#
# Two summaries rather than one, because the guard names every summary
# argument where the handler names the one Arrow blamed. That is also the
# plural arm of this diagnostic; the singular arm is the refusal above.
test_that("the branch guard refuses an absorbed summary the handler missed", {
  skip_if_suggest_absent("arrow")
  testthat::local_mocked_bindings(
    absorbing_warning_marker = function() {
      "no Arrow warning is written with this text"
    }
  )

  raised <- expect_error(suppressWarnings(summarize_with_margins(
    arrow::Table$create(arrow_input_data()),
    joined = paste(s, collapse = ","),
    kept = sum(v[v > 1]),
    .grouping = rollup(k)
  )))

  expect_s3_class(raised, "marginplyr_error")
  message <- conditionMessage(raised)
  expect_match(message, "joined = paste(s, collapse = \",\")", fixed = TRUE)
  expect_match(message, "kept = sum(v[v > 1])", fixed = TRUE)
  # The plural arm, both inflections of it.
  expect_match(message, "Arrow cannot evaluate these summaries", fixed = TRUE)
  expect_match(message, "to compute them:", fixed = TRUE)
})

test_that("Arrow schema metadata supports predicates and computed queries", {
  skip_if_suggest_absent("arrow")

  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  table <- arrow::Table$create(data)
  sources <- list(
    table,
    arrow::InMemoryDataset$create(table),
    dplyr::mutate(table, doubled = value * 2)
  )

  for (source in sources) {
    result <- summarize_with_margins(
      source,
      total = sum(value),
      .grouping = rollup(where(is.character)),
      .margin_label = NULL
    ) |>
      dplyr::collect()

    expect_equal(names(result), c("group", "total"))
    expect_setequal(result$total, c(3L, 3L, 6L))
    expect_true(any(is.na(result$group)))
  }

  factor_result <- summarize_with_margins(
    arrow::Table$create(
      data.frame(group = factor(c("x", "y")), value = 1:2)
    ),
    total = sum(value),
    .grouping = rollup(group),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  expect_setequal(as.character(factor_result$group), c("x", "y", NA))

  numeric_result <- summarize_with_margins(
    table,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  expect_identical(names(numeric_result), c("value", "n"))
  expect_true(anyNA(numeric_result$value))
})

test_that("Arrow metadata preserves ordered dictionaries without collecting", {
  skip_if_suggest_absent("arrow")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  ordered_group <- factor(
    c("b", "a", "b"),
    levels = c("a", "b"),
    ordered = TRUE
  )
  source <- arrow::Table$create(
    data.frame(group = ordered_group, value = 1:3)
  ) |>
    dplyr::mutate(doubled = value * 2)
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  query <- summarize_with_margins(
    source,
    total = sum(value),
    .grouping = rollup(where(is.factor)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 0L)
  result <- dplyr::collect(query)
  expect_s3_class(result$group, "ordered")
  expect_identical(levels(result$group), c("a", "b"))
  expect_true(anyNA(result$group))
  expect_setequal(result$total, c(2L, 4L, 6L))
})

test_that("dtplyr constructs one typed selection proxy for predicates", {
  skip_if_suggest_absent("dtplyr")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  numeric_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(numeric_query)),
    c("code", "value", "n")
  )

  selection_proxy_capture$n <- 0L
  character_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )
  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(character_query)),
    c("group", "n")
  )
})

test_that("DuckDB constructs one typed selection proxy for predicates", {
  skip_if_suggest_absent("duckdb", "DBI")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(
      group = c("x", "y"),
      code = c(1L, 2L),
      value = c(10, 20)
    ),
    "selection_proxy_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  numeric_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(numeric_query)),
    c("code", "value", "n")
  )

  selection_proxy_capture$n <- 0L
  character_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )
  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(character_query)),
    c("group", "n")
  )
})

test_that("public Arrow table classes are supported", {
  skip_if_suggest_absent("arrow")

  data <- data.frame(group = c("x", "y"), value = 1:2)
  table <- arrow::Table$create(data)
  sources <- list(
    table,
    arrow::RecordBatch$create(data),
    arrow::InMemoryDataset$create(table)
  )

  for (source in sources) {
    result <- summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(group)
    ) |>
      dplyr::collect()
    expect_setequal(result$n, c(1L, 1L, 2L))
  }

  reader <- arrow::RecordBatchReader$create(
    arrow::RecordBatch$create(data)
  )
  expect_error(
    summarize_with_margins(
      reader,
      n = dplyr::n(),
      .grouping = rollup(group)
    ),
    "RecordBatchReader"
  )
})

# Deciding whether a selection renames means comparing what it selected against
# the columns it selected from, and a lazy selection proxy is the table object
# itself: `names()` on it returns `con`, `src`, and `lazy_query`, so reading it
# would report a rename for a selection that renames nothing and would name the
# proxy's own fields when one does rename. Both halves are asserted here because
# the first fails only on a backend that never collects its proxy.
test_that("a lazy selection proxy resolves renames against its columns", {
  source <- dbplyr::tbl_lazy(
    data.frame(region = c("x", "y"), value = 1:2),
    con = dbplyr::simulate_sqlite()
  )

  plan <- inspect_grouping(source, .grouping = rollup(region))
  expect_identical(plan$included, c("(region)", "()"))

  error <- expect_error(
    inspect_grouping(
      source,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "Can't rename grouping dimension:\n",
      "i `area = region`.\n",
      "i Grouping dimensions must name existing columns."
    )
  )
})

backend_by_rename_message <- function() {
  paste0(
    "Can't rename `.by` column:\n",
    "i `area = region`.\n",
    "i Fixed `.by` keys must name existing columns."
  )
}

# A `.by` selection naming columns is settled from column names alone, so a
# lazy input is never touched to resolve one. This source has no connection to
# touch: a resolution that reached past the names would fail rather than report
# what the caller has to fix.
test_that("a lazy .by selection is settled before the table is read", {
  source <- dbplyr::tbl_lazy(
    data.frame(region = c("x", "y"), area = c("p", "q"), value = 1:2),
    con = dbplyr::simulate_sqlite()
  )

  plan <- inspect_grouping(source, .by = region, .grouping = rollup(value))
  expect_identical(plan$fixed, c("(region)", "(region)"))

  error <- expect_error(
    inspect_grouping(
      source,
      .by = tidyselect::all_of(c(area = "region")),
      .grouping = rollup(value)
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), backend_by_rename_message())
})

# A `.by` predicate is the one fixed-key selection column names cannot answer,
# so it reads the typed snapshot — for Arrow, the schema the operation already
# acquired. The schema reads are counted because a second one is what a
# resolution of its own would look like, and it would still return the right
# columns (ADR-0002).
test_that("an Arrow .by predicate resolves against the typed snapshot", {
  skip_if_suggest_absent("arrow")

  source <- arrow::Table$create(
    data.frame(region = c("x", "y"), area = c("p", "q"), value = 1:2)
  )
  schema_reads <- 0L
  infer_schema <- getFromNamespace("infer_schema", "arrow")
  testthat::local_mocked_bindings(
    infer_schema = function(x) {
      schema_reads <<- schema_reads + 1L
      infer_schema(x)
    },
    .package = "arrow"
  )

  plan <- inspect_grouping(
    source,
    .by = where(is.numeric),
    .grouping = rollup(region)
  )
  expect_identical(plan$fixed, c("(value)", "(value)"))
  expect_identical(schema_reads, 1L)

  error <- expect_error(
    inspect_grouping(
      source,
      .by = c(area = region, where(is.numeric)),
      .grouping = grouping_set()
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), backend_by_rename_message())
})

margin_label_check_data <- function() {
  data.frame(
    first = c("Total", "x"),
    second = c("y", "Total"),
    value = 1:2
  )
}

# Every dimension has to reach the diagnostic, not just the first one that
# collides. The three tests below assert the same message against the same
# fixture, once per lazy backend that can execute the check. The call is written
# out in each rather than wrapped in a shared expectation, because a closure
# would put `first` and `second` somewhere `codetools` cannot follow them.
test_that("dtplyr checks margin labels across all dimensions", {
  skip_if_suggest_absent("dtplyr")
  expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(margin_label_check_data()),
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns:\ni `first` and `second`"
  )
})

test_that("Arrow checks margin labels across all dimensions", {
  skip_if_suggest_absent("arrow")
  expect_error(
    summarize_with_margins(
      arrow::Table$create(margin_label_check_data()),
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns:\ni `first` and `second`"
  )
})

test_that("DuckDB checks margin labels across all dimensions", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    margin_label_check_data(),
    "margin_label_checks",
    overwrite = TRUE,
    temporary = TRUE
  )
  expect_error(
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns:\ni `first` and `second`"
  )
})

test_that("lazy margin label checks aggregate portable numeric values", {
  registerS3method(
    "db_collect",
    "margin_check_connection",
    margin_check_collect,
    envir = asNamespace("dbplyr")
  )
  con <- dbplyr::simulate_oracle()
  class(con) <- c(
    "Oracle",
    "TestConnection",
    "margin_check_connection",
    "DBIConnection"
  )
  attr(con, "check_names") <- c("first", "second")
  remote <- dbplyr::tbl_lazy(
    data.frame(first = "x", second = "y", value = 1),
    con = con
  )
  class(remote) <- c("tbl_Oracle", "tbl_sql", "tbl_lazy", "tbl")

  expect_no_error(
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    )
  )

  expect_match(margin_check_capture$sql, "CASE WHEN", fixed = TRUE)
  expect_match(margin_check_capture$sql, "THEN 1", fixed = TRUE)
  expect_false(grepl(
    "MAX(\"first\" = 'Total')",
    margin_check_capture$sql,
    fixed = TRUE
  ))
})

test_that("documented SQL dialects use portable margin label checks", {
  registerS3method(
    "db_collect",
    "margin_check_connection",
    margin_check_collect,
    envir = asNamespace("dbplyr")
  )
  simulators <- available_simulators(c(
    "simulate_access",
    "simulate_dbi",
    "simulate_hana",
    "simulate_hive",
    "simulate_impala",
    "simulate_mariadb",
    "simulate_mssql",
    "simulate_mysql",
    "simulate_odbc",
    "simulate_oracle",
    "simulate_postgres",
    "simulate_redshift",
    "simulate_snowflake",
    "simulate_spark_sql",
    "simulate_sqlite",
    "simulate_teradata"
  ))

  for (simulator in simulators) {
    con <- getExportedValue("dbplyr", simulator)()
    con_classes <- class(con)
    class(con) <- append(
      con_classes,
      "margin_check_connection",
      after = 1L
    )
    attr(con, "check_names") <- "group"
    remote <- dbplyr::tbl_lazy(
      data.frame(group = "x", value = 1),
      con = con
    )
    remote_classes <- class(remote)
    if (!"tbl_sql" %in% remote_classes) {
      class(remote) <- append(remote_classes, "tbl_sql", after = 1L)
    }

    margin_check_capture$sql <- NULL
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(group),
      .check_margin_label = TRUE
    )

    expect_match(
      margin_check_capture$sql,
      "SUM(CASE WHEN",
      fixed = TRUE,
      info = simulator
    )
    expect_match(
      margin_check_capture$sql,
      "THEN 1 ELSE 0 END)",
      fixed = TRUE,
      info = simulator
    )
  }
})

reserved_column_data <- function() {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_key_1"]] <- 10:12
  data
}

# The literal every adapter owes, which the local test below pins first. A
# backend test asserts against this rather than against a freshly computed local
# result so that the two cannot drift together.
reserved_column_expected <- function() {
  data.frame(
    group = c("Total", "x", "y"),
    total = c(6L, 3L, 3L)
  )
}

test_that("local summaries reserve user columns that look internal", {
  result <- summarize_with_margins(
    reserved_column_data(),
    total = sum(value),
    .grouping = rollup(group)
  )
  expect_equal(dplyr::arrange(result, group), reserved_column_expected())
})

test_that("the dtplyr union adapter reserves user columns that look internal", {
  skip_if_suggest_absent("dtplyr")
  result <- summarize_with_margins(
    dtplyr::lazy_dt(reserved_column_data()),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(result), reserved_column_expected())
})

test_that("the Arrow union adapter reserves user columns that look internal", {
  skip_if_suggest_absent("arrow")
  result <- summarize_with_margins(
    arrow::Table$create(reserved_column_data()),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(result), reserved_column_expected())
})

generated_name_data <- function() {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_key_1"]] <- 10:12
  data[["..marginplyr_key_1_"]] <- 20:22
  data
}

generated_name_expected <- function() {
  expected <- data.frame(
    group = c("Total", "x", "y"),
    check.names = FALSE
  )
  expected[["..marginplyr_key_1__"]] <- c(6L, 3L, 3L)
  expected
}

test_that("local summaries reserve generated summary names", {
  result <- summarize_with_margins(
    generated_name_data(),
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  )
  expect_equal(dplyr::arrange(result, group), generated_name_expected())
})

test_that("the dtplyr union adapter reserves generated summary names", {
  skip_if_suggest_absent("dtplyr")
  result <- summarize_with_margins(
    dtplyr::lazy_dt(generated_name_data()),
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(result), generated_name_expected())
})

test_that("the Arrow union adapter reserves generated summary names", {
  skip_if_suggest_absent("arrow")
  result <- summarize_with_margins(
    arrow::Table$create(generated_name_data()),
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(result), generated_name_expected())
})

test_that("union adapters reserve dynamically injected summary names", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  summary_name <- "..marginplyr_key_1"

  result <- summarize_with_margins(
    data,
    tibble::tibble(!!summary_name := sum(value)),
    .grouping = rollup(group)
  )

  expect_equal(
    dplyr::arrange(result, group),
    data.frame(
      group = c("Total", "x", "y"),
      check.names = FALSE,
      "..marginplyr_key_1" = c(6L, 3L, 3L)
    )
  )
})

test_that("union adapters diagnose opaque summary name collisions", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  opaque_summary <- function(x) {
    stats::setNames(
      data.frame(sum(x)),
      "..marginplyr_key_1"
    )
  }

  error <- expect_error(
    summarize_with_margins(
      data,
      opaque_summary(value),
      .grouping = rollup(group)
    ),
    "summary output names conflict with internal grouping columns"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("native adapters reserve generated summary names", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_grouping_1_"]] <- 10:12

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  query <- summarize_with_margins(
    postgres,
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_grouping_1"
    ),
    .grouping = rollup(group)
  )
  sql <- dbplyr::sql_render(query)
  expect_match(sql, "\"..marginplyr_grouping_1\"", fixed = TRUE)
  expect_match(sql, "\"..marginplyr_grouping_1__\"", fixed = TRUE)

  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "internal_summary_names",
    overwrite = TRUE,
    temporary = TRUE
  )
  result <- summarize_with_margins(
    remote,
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_grouping_1"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(
    result[["..marginplyr_grouping_1"]],
    c(6, 3, 3)
  )
})

shadowed_summary_data <- function() {
  data.frame(
    group = c("x", "x", "y"),
    value = c(1, 2, 3)
  )
}

test_that("native adapters reject a summary output shadowing a dimension", {
  data <- shadowed_summary_data()
  dots <- unpredictable_summary_dots("group")

  # Local takes the union path, whose result-name check already rejects this.
  # Its condition is the contract the native path has to match, so it is read
  # here rather than restated.
  local_error <- expect_error(
    rlang::inject(summarize_with_margins(
      data,
      !!!dots,
      .grouping = rollup(group)
    )),
    class = "marginplyr_error"
  )
  expect_match(
    conditionMessage(local_error),
    "cannot overwrite grouping column.*`group`"
  )

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  native_error <- expect_error(
    rlang::inject(summarize_with_margins(
      postgres,
      !!!dots,
      .grouping = rollup(group)
    )),
    class = "marginplyr_error"
  )
  expect_identical(
    conditionMessage(native_error),
    conditionMessage(local_error)
  )
  expect_identical(class(native_error), class(local_error))
})

test_that("DuckDB rejects a summary output shadowing a dimension", {
  skip_if_suggest_absent("duckdb", "DBI")

  # The reported failure, kept in the shape it was reported in: two dimensions
  # where the second is named for the first, so `across(all_of("x"), fns)` with
  # `fns <- list(region = sum)` produces `x_region` under `across()`'s own
  # naming.
  data <- data.frame(
    region = c("E", "E", "W"),
    x_region = c("p", "q", "p"),
    x = c(1, 2, 3)
  )
  dots <- unpredictable_summary_dots("region", cols = "x", .names = NULL)

  local_error <- expect_error(
    rlang::inject(summarize_with_margins(
      data,
      !!!dots,
      .grouping = rollup(region, x_region)
    )),
    class = "marginplyr_error"
  )

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "shadowed_dimension",
    overwrite = TRUE,
    temporary = TRUE
  )

  duckdb_error <- expect_error(
    rlang::inject(summarize_with_margins(
      remote,
      !!!dots,
      .grouping = rollup(region, x_region)
    )),
    class = "marginplyr_error"
  )
  expect_identical(
    conditionMessage(duckdb_error),
    conditionMessage(local_error)
  )
})

test_that("native adapters reject summary outputs on their own columns", {
  data <- shadowed_summary_data()
  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  # The native path names its Grouping bit flags `..marginplyr_grouping_*` and
  # takes the Grouping set identifier from `.id`; both are written beside the
  # summary outputs in one `summarize()`, so a collision would silently drop
  # the summary rather than the internal column.
  flag_dots <- unpredictable_summary_dots("..marginplyr_grouping_1")
  flag_error <- expect_error(
    rlang::inject(summarize_with_margins(
      postgres,
      !!!flag_dots,
      .grouping = rollup(group)
    )),
    "summary output names conflict with internal grouping columns"
  )
  expect_s3_class(flag_error, "marginplyr_error")

  id_dots <- unpredictable_summary_dots("sid")
  id_error <- expect_error(
    rlang::inject(summarize_with_margins(
      postgres,
      !!!id_dots,
      .grouping = rollup(group),
      .id = "sid"
    )),
    "`.id` \\(`sid`\\) conflicts with a summary output"
  )
  expect_s3_class(id_error, "marginplyr_error")
})

test_that("Arrow's unpacked named across is caught after the branch runs", {
  skip_if_suggest_absent("arrow")

  # `known_summary_output_names()` skips a named dot because dplyr packs a
  # data-frame result under the name (#431). Arrow does not: it returns a
  # top-level `group` here and discards `s`, so the pre-execution check sees
  # only `s` and the adapter's result-name check is the whole of what refuses
  # this. The local answer is the contract, so it is read here rather than
  # restated.
  data <- shadowed_summary_data()
  local_error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(value, mean, .names = "group"),
      .grouping = rollup(group)
    ),
    class = "marginplyr_error"
  )
  arrow_error <- expect_error(
    summarize_with_margins(
      arrow::as_arrow_table(data),
      s = dplyr::across(value, mean, .names = "group"),
      .grouping = rollup(group)
    ),
    class = "marginplyr_error"
  )
  expect_identical(
    conditionMessage(arrow_error),
    conditionMessage(local_error)
  )
})

test_that("native adapters keep unpredictable names that collide with none", {
  data <- shadowed_summary_data()
  dots <- unpredictable_summary_dots("total", .names = NULL)
  expected <- rlang::inject(summarize_with_margins(
    data,
    !!!dots,
    .grouping = rollup(group)
  ))

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  query <- rlang::inject(summarize_with_margins(
    postgres,
    !!!dots,
    .grouping = rollup(group)
  ))
  expect_identical(
    get_col_names(query, dplyr::everything()),
    names(expected)
  )
  expect_match(
    dbplyr::sql_render(query),
    "GROUPING SETS",
    fixed = TRUE
  )

  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "unshadowed_summary",
    overwrite = TRUE,
    temporary = TRUE
  )
  result <- rlang::inject(summarize_with_margins(
    remote,
    !!!dots,
    .grouping = rollup(group)
  )) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(
    as.data.frame(result),
    dplyr::arrange(expected, group),
    ignore_attr = TRUE
  )
})

test_that("column-wise summaries share one lazy-backend selection", {
  data <- data.frame(
    group = c("b", "a", "b"),
    value = c(1, 2, 3)
  )

  if (suggest_available("dtplyr")) {
    dt_result <- summarize_with_margins(
      dtplyr::lazy_dt(data),
      dplyr::across(
        dplyr::everything(),
        dplyr::n_distinct,
        .names = "n_{.col}"
      ),
      .grouping = rollup(group)
    ) |>
      dplyr::collect()
    expect_equal(names(dt_result), c("group", "n_value"))
    expect_setequal(dt_result$n_value, c(2L, 1L, 3L))
  }

  if (suggest_available("arrow")) {
    arrow_result <- summarize_with_margins(
      arrow::Table$create(data),
      dplyr::across(
        dplyr::everything(),
        dplyr::n_distinct,
        .names = "n_{.col}"
      ),
      .grouping = rollup(group)
    ) |>
      dplyr::collect()
    expect_equal(names(arrow_result), c("group", "n_value"))
    expect_setequal(arrow_result$n_value, c(2L, 1L, 3L))
  }

  if (sqlite_simulation_available()) {
    sqlite <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_sqlite())
    sqlite_query <- summarize_with_margins(
      sqlite,
      dplyr::across(value, mean, .names = "mean_{.col}"),
      .grouping = rollup(group)
    )
    expect_match(
      dbplyr::sql_render(sqlite_query),
      "UNION ALL",
      fixed = TRUE
    )
    expect_false(grepl(
      "mean_group",
      dbplyr::sql_render(sqlite_query),
      fixed = TRUE
    ))
  }

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  expect_error(
    summarize_with_margins(
      postgres,
      dplyr::across(value, mean, .names = "group"),
      .grouping = rollup(group)
    ),
    "cannot overwrite grouping column.*`group`"
  )
})

unordered_margin_data <- function() {
  data.frame(
    group = c("b", "a", "b"),
    value = 1:3
  )
}

# Set equality rather than a sequence, because the point is that no adapter
# drops or invents a margin value while none of them promises an order here.
unordered_margin_expected <- function() {
  c("Total", "a", "b")
}

test_that("local summaries preserve margin values without implicit ordering", {
  local <- summarize_with_margins(
    unordered_margin_data(),
    total = sum(value),
    .grouping = rollup(group)
  )
  expect_setequal(local$group, unordered_margin_expected())
})

test_that("dtplyr preserves margin values without implicit ordering", {
  skip_if_suggest_absent("dtplyr")
  result <- summarize_with_margins(
    dtplyr::lazy_dt(unordered_margin_data()),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect()
  expect_setequal(result$group, unordered_margin_expected())
})

test_that("Arrow preserves margin values without implicit ordering", {
  skip_if_suggest_absent("arrow")
  result <- summarize_with_margins(
    arrow::Table$create(unordered_margin_data()),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect()
  expect_setequal(result$group, unordered_margin_expected())
})

test_that("dtplyr nesting retains original keys and empty rowwise behavior", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    group = c("a", "a", "b"),
    item = c("x", "y", "z"),
    value = 1:3
  )

  kept_lazy <- nest_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(group),
    .keep = TRUE
  )
  expect_s3_class(kept_lazy, "dtplyr_step")
  kept_nested <- dplyr::collect(kept_lazy)
  total_nested <- kept_nested[kept_nested$group == "Total", ]
  expect_equal(total_nested$data[[1]]$group, c("a", "a", "b"))

  kept <- nest_by_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(group),
    .keep = TRUE
  )
  total <- kept[kept$group == "Total", ]
  expect_equal(total$data[[1]]$group, c("a", "a", "b"))

  empty <- nest_by_with_margins(dtplyr::lazy_dt(data[0, ]))
  expect_s3_class(empty, "rowwise_df")
  expect_equal(nrow(empty), 1L)
  expect_equal(names(empty$data[[1]]), names(data))
})

grouped_lazy_data <- function() {
  data.frame(
    year = c(2025L, 2025L, 2026L, 2026L),
    region = c("East", "West", "East", "West"),
    value = c(1, 10, 100, 1000)
  )
}

# One value per (year, region) cell plus the two subtotals a rollup over the
# region adds, which is what "the group became a fixed key" looks like in the
# rows. Fixed here rather than recomputed locally, because a local data frame
# carries no groups for the adapter to promote.
grouped_lazy_values <- function() {
  c(1, 10, 11, 100, 1000, 1100)
}

test_that("grouped dtplyr inputs use their groups as fixed keys", {
  skip_if_suggest_absent("dtplyr")
  grouped_dt <- dtplyr::lazy_dt(grouped_lazy_data()) |>
    dplyr::group_by(year)

  dt_summary <- summarize_with_margins(
    grouped_dt,
    value = sum(value),
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_summary), character())
  expect_setequal(dplyr::collect(dt_summary)$value, grouped_lazy_values())

  dt_union <- expand_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_union), character())

  dt_nest <- nest_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_nest), character())
  expect_equal(names(dplyr::collect(dt_nest)), c("year", "region", "data"))

  dt_nest_by <- nest_by_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_s3_class(dt_nest_by, "rowwise_df")
  expect_equal(dplyr::group_vars(dt_nest_by), c("year", "region"))
})

test_that("grouped Arrow inputs use their groups as fixed keys", {
  skip_if_suggest_absent("arrow")
  grouped_arrow <- arrow::Table$create(grouped_lazy_data()) |>
    dplyr::group_by(year)

  arrow_summary <- summarize_with_margins(
    grouped_arrow,
    value = sum(value),
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(arrow_summary), character())
  expect_setequal(dplyr::collect(arrow_summary)$value, grouped_lazy_values())
})

test_that("grouped SQL inputs use their groups as fixed keys", {
  skip_if_not_installed("dbplyr")
  grouped_sql <- dbplyr::tbl_lazy(
    grouped_lazy_data(),
    con = dbplyr::simulate_postgres()
  ) |>
    dplyr::group_by(year)

  sql_summary <- summarize_with_margins(
    grouped_sql,
    value = sum(value),
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(sql_summary), character())
  expect_match(
    dbplyr::sql_render(sql_summary),
    'GROUPING SETS (("year", "region"), ("year"))',
    fixed = TRUE
  )
  expect_error(
    summarize_with_margins(
      grouped_sql,
      value = sum(value),
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )
})

test_that("DuckDB executes grouped lazy input as fixed keys", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    year = c(2025L, 2025L, 2026L, 2026L),
    region = c("East", "West", "East", "West"),
    value = c(1, 10, 100, 1000)
  )
  dplyr::copy_to(
    con,
    data,
    "grouped_lazy_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  grouped <- dplyr::tbl(con, "grouped_lazy_data") |>
    dplyr::group_by(year)

  query <- summarize_with_margins(
    grouped,
    value = sum(value),
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(query), character())

  result <- dplyr::collect(query)
  result <- dplyr::arrange(result, year, region)
  expect_equal(result$year, c(2025L, 2025L, 2025L, 2026L, 2026L, 2026L))
  expect_equal(
    result$region,
    c("East", "Total", "West", "East", "Total", "West")
  )
  expect_equal(result$value, c(1, 11, 10, 100, 1100, 1000))
})

test_that("PostgreSQL renders native SQL under strict translation", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  old <- options(dplyr.strict_sql = TRUE)
  on.exit(options(old), add = TRUE)
  expect_no_message(
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      ga = marginplyr::grouping_bit(a),
      gid = grouping_id(a, b),
      .grouping = grouping_sets(
        grouping_set(a, b),
        grouping_set()
      )
    )
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_match(sql, "GROUPING(\"a\")", fixed = TRUE)
  expect_match(sql, "GROUPING(\"b\")", fixed = TRUE)
  expect_false(grepl("UNION ALL", sql, fixed = TRUE))
})

test_that("native SQL omits display flags when labels are disabled", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    bit = grouping_bit(a),
    id = grouping_id(a),
    .grouping = rollup(a),
    .margin_label = NULL
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUPING(\"a\")", fixed = TRUE)
  expect_false(grepl("..marginplyr_grouping_", sql, fixed = TRUE))
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("a", "n", "bit", "id")
  )
  expect_identical(dplyr::group_vars(query), character())
})

# The bare `grouping_id()` of #366 resolves before either adapter runs, so the
# native path is asserted against the retyped spelling rather than against a
# transcription of the SQL: what the default has to produce is the same query.
test_that("a bare grouping_id() renders the native SQL of the retyped call", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", b = "u", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- function(gid) {
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      gid = !!gid,
      .grouping = rollup(a, b),
      .margin_label = NULL
    )
  }

  bare <- dbplyr::sql_render(query(quote(grouping_id())))
  written <- dbplyr::sql_render(query(quote(grouping_id(a, b))))

  expect_identical(bare, written)
  expect_match(bare, "GROUPING(\"a\")", fixed = TRUE)
  expect_match(bare, "GROUPING(\"b\")", fixed = TRUE)
})

# A plan with no dimensions still reaches the native adapter -- `GROUP BY
# GROUPING SETS` over the `.by` columns alone -- where the mask has no
# `GROUPING()` term to build from and has to be written as the literal the
# local path computes.
test_that("a bare grouping_id() renders a literal for a dimensionless plan", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    gid = grouping_id(),
    .by = a
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_false(grepl("GROUPING(\"", sql, fixed = TRUE))
  expect_match(sql, "0 AS \"gid\"", fixed = TRUE)
})

test_that("DuckDB collects the same bare grouping_id() the local path gives", {
  skip_if_suggest_absent("duckdb", "DBI")

  data <- data.frame(
    a = c("x", "x", "y"),
    b = c("u", "v", "u"),
    value = 1:3
  )
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "bare_grouping_id",
    overwrite = TRUE,
    temporary = TRUE
  )

  collected <- dplyr::collect(summarize_with_margins(
    remote,
    total = sum(value),
    gid = grouping_id(),
    .grouping = rollup(a, b)
  ))
  local <- summarize_with_margins(
    data,
    total = sum(value),
    gid = grouping_id(),
    .grouping = rollup(a, b)
  )

  expect_setequal(as.integer(collected$gid), local$gid)
  expect_identical(sort(unique(as.integer(collected$gid))), c(0L, 1L, 3L))
})

test_that("native SQL reports incompatible dbplyr query representations", {
  registerS3method(
    "sql_build",
    "lazy_marginplyr_bad_query",
    bad_query_sql_build,
    envir = asNamespace("dbplyr")
  )
  bad_query <- structure(
    list(),
    class = c("lazy_marginplyr_bad_query", "lazy_query")
  )
  grouping_query <- dbplyr::lazy_query(
    "grouping_sets",
    x = bad_query,
    grouping_sets = list(character()),
    group_vars = character()
  )

  error <- expect_error(
    dbplyr::sql_build(
      grouping_query,
      con = dbplyr::simulate_postgres()
    ),
    "dbplyr query representation has changed"
  )

  # Not a Package condition: no call rewrite avoids it. See ADR 0015.
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("native grouping sets remain a subquery after downstream verbs", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", b = "u", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b),
    .margin_label = NULL
  )
  downstream <- list(
    select = dplyr::select(query, a, n, gid),
    mutate = dplyr::mutate(query, n_plus_one = n + 1),
    filter = dplyr::filter(query, n > 0),
    arrange = dplyr::arrange(query, a),
    summarize = dplyr::summarize(query, total = sum(n, na.rm = TRUE))
  )

  for (verb in names(downstream)) {
    sql <- dbplyr::sql_render(downstream[[verb]])
    expect_match(sql, "FROM (", fixed = TRUE, info = verb)
    expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE, info = verb)
  }
})

test_that("unconfirmed SQL dialects use UNION ALL", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)
  connections <- list(dbplyr::simulate_mysql())
  if (sqlite_simulation_available()) {
    connections <- c(connections, list(dbplyr::simulate_sqlite()))
  }

  for (con in connections) {
    remote <- dbplyr::tbl_lazy(data, con = con)
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    )
    sql <- dbplyr::sql_render(query)
    expect_match(sql, "UNION ALL", fixed = TRUE)
    expect_false(grepl("GROUPING SETS", sql, fixed = TRUE))
  }
})

test_that("documented fallback dialects render portable UNION ALL SQL", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)
  simulators <- available_simulators(c(
    "simulate_access",
    "simulate_dbi",
    "simulate_hana",
    "simulate_hive",
    "simulate_impala",
    "simulate_mariadb",
    "simulate_mssql",
    "simulate_mysql",
    "simulate_odbc",
    "simulate_oracle",
    "simulate_redshift",
    "simulate_snowflake",
    "simulate_spark_sql",
    "simulate_sqlite",
    "simulate_teradata"
  ))

  for (simulator in simulators) {
    con <- getExportedValue("dbplyr", simulator)()
    remote <- dbplyr::tbl_lazy(data, con = con)
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    )
    sql <- dbplyr::sql_render(query)
    expect_match(sql, "UNION ALL", fixed = TRUE, info = simulator)
    expect_false(
      grepl("GROUPING SETS", sql, fixed = TRUE),
      info = simulator
    )
  }
})

test_that("PostgreSQL duplicate keep falls back conservatively", {
  skip_if_not_installed("dbplyr")
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x"),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = grouping_sets(grouping_set(a), grouping_set(a)),
    .duplicates = "keep"
  )
  expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE)
})

test_that("DuckDB executes native grouping sets with unambiguous bits", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    a = c("x", NA_character_, "Total"),
    b = c("u", "u", "v"),
    value = 1:3
  )
  dplyr::copy_to(con, data, "grouping_data", overwrite = TRUE, temporary = TRUE)
  remote <- dplyr::tbl(con, "grouping_data")

  result <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    ga = grouping_bit(a),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b),
    .check_margin_label = FALSE
  ) |>
    dplyr::collect()

  expect_true(any(is.na(result$a) & result$gid == 0))
  expect_true(any(is.na(result$a) & result$gid == 1))
  expect_true(any(result$a == "Total" & result$ga == 0))
  expect_true(any(result$a == "Total" & result$ga == 1))
  expect_setequal(unique(result$gid), c(0, 1, 3))
})

test_that("DuckDB keeps input types available to summary expressions", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = c(1L, 1L, 2L), value = 1:3)
  dplyr::copy_to(
    con,
    data,
    "numeric_groups",
    overwrite = TRUE,
    temporary = TRUE
  )

  result <- summarize_with_margins(
    dplyr::tbl(con, "numeric_groups"),
    sum_a = sum(a),
    .grouping = rollup(a)
  ) |>
    dplyr::collect()

  expect_equal(result$sum_a[result$a == "Total"], 4)
})

test_that("DuckDB native and UNION adapters agree", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = c("x", "x", "y"), b = c("u", "v", "u"), value = 1:3)
  dplyr::copy_to(con, data, "adapter_data", overwrite = TRUE, temporary = TRUE)
  remote <- dplyr::tbl(con, "adapter_data")
  spec <- grouping_spec(rollup(a), cube(b))

  native <- summarize_with_margins(
    remote,
    total = sum(value),
    gid = grouping_id(a, b),
    .grouping = spec
  ) |>
    dplyr::collect() |>
    dplyr::arrange(a, b, gid)

  plan <- compile_grouping_spec(
    spec,
    names(data),
    duplicates_choices = margin_duplicates_choices
  )
  dots <- rlang::quos(total = sum(value), gid = grouping_id(a, b))
  fallback <- summarize_margin_union(
    remote,
    summaries = new_summary_arguments(dots),
    plan = plan,
    margin_labels = resolve_margin_labels(
      "Total",
      dimensions = plan$dimensions
    ),
    column_info = margin_column_info(
      grouping_selection_proxy(remote),
      plan$dimensions,
      backend = grouping_backend(remote)
    ),
    reserved_names = unique(c(names(data), names(dots)))
  ) |>
    dplyr::collect() |>
    dplyr::arrange(a, b, gid)

  expect_equal(native, fallback)
})

test_that("DuckDB duplicate keep and downstream verbs remain lazy", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dplyr::copy_to(
    con,
    data.frame(a = c("x", "y"), value = 1:2),
    "duplicate_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  remote <- dplyr::tbl(con, "duplicate_data")

  duplicated <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = grouping_sets(grouping_set(a), grouping_set(a)),
    .duplicates = "keep"
  )
  expect_match(dbplyr::sql_render(duplicated), "GROUPING SETS", fixed = TRUE)
  expect_equal(nrow(dplyr::collect(duplicated)), 4L)

  downstream <- duplicated |>
    dplyr::select(a, n) |>
    dplyr::rename(group = a) |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(n_plus_one = n + 1) |>
    dplyr::arrange(group)
  expect_s3_class(downstream, "tbl_lazy")
  expect_equal(nrow(dplyr::collect(downstream)), 4L)

  resummarized <- duplicated |>
    dplyr::summarise(total = sum(n)) |>
    dplyr::collect()
  expect_equal(resummarized$total, 4)
})

test_that("DuckDB safely quotes factor identifiers and labels", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(check.names = FALSE, "odd name" = factor(c("A", "B")))
  dplyr::copy_to(con, data, "factor_data", overwrite = TRUE, temporary = TRUE)

  result <- summarize_with_margins(
    dplyr::tbl(con, "factor_data"),
    n = dplyr::n(),
    .grouping = rollup(`odd name`),
    .margin_label = "O'Total"
  ) |>
    dplyr::collect()

  expect_true(is.factor(result[["odd name"]]))
  expect_true("O'Total" %in% levels(result[["odd name"]]))
})
