# The fold these tests replaced is what every assertion here is written
# against: `combine_margin_branches()` is a performance change, so the value it
# has to keep is that its answer is the answer `Reduce(dplyr::union_all, ...)`
# gave, column classes and attributes included.
fold_branches_pairwise <- function(branches) {
  Reduce(dplyr::union_all, branches)
}

# A cube over nine dimensions, which is 512 branches -- the count at which a
# left fold of dtplyr steps exhausted the C stack. Both scale tests below want
# the same frame, and the count is the premise of each.
nine_dimension_frame <- function() {
  data <- as.data.frame(lapply(
    seq_len(9),
    function(i) rep(c("x", "y"), length.out = 4L)
  ))
  names(data) <- paste0("d", seq_len(9))
  data$value <- seq_len(4)
  data
}

# `UNION ALL` occurrences that no parenthesis encloses, which is the number of
# branches a rendered query unions at its top level, less one. dbplyr renders a
# subquery inside `FROM (...)`, so paren depth is what tells a widened union
# from a nested one; identifiers are backquoted and string literals here hold no
# parentheses, so scanning the text is exact for these queries.
count_top_level_unions <- function(sql) {
  characters <- strsplit(sql, "", fixed = TRUE)[[1L]]
  depth <- cumsum((characters == "(") - (characters == ")"))
  starts <- gregexpr("UNION ALL", sql, fixed = TRUE)[[1L]]
  starts <- starts[starts > 0L]
  sum(depth[starts] == 0L)
}

test_that("combining branches reproduces a pairwise `union_all()` fold", {
  cases <- list(
    "plain data frame" = data.frame(a = 1:3, b = letters[1:3]),
    "factor and ordered columns" = data.frame(
      f = factor(c("a", "b", "c"), levels = c("c", "b", "a")),
      o = ordered(c("l", "m", "h"), levels = c("l", "m", "h"))
    ),
    "classed columns" = data.frame(
      d = as.Date("2024-01-01") + 0:2,
      p = as.POSIXct("2024-01-01", tz = "UTC") + 0:2
    ),
    "list column" = data.frame(a = 1:2, l = I(list(1, "x")))
  )

  for (label in names(cases)) {
    branches <- rep(list(cases[[label]]), 4L)
    expect_identical(
      combine_margin_branches(branches),
      fold_branches_pairwise(branches),
      info = label
    )
  }
})

test_that("combining branches matches the fold on a reordered branch", {
  first <- data.frame(a = 1:2, b = c("x", "y"))
  branches <- list(first, first[, c("b", "a")])

  expect_identical(
    combine_margin_branches(branches),
    fold_branches_pairwise(branches)
  )
})

test_that("a lone branch is returned untouched", {
  branch <- data.frame(a = 1:2, b = c("x", "y"))

  expect_identical(combine_margin_branches(list(branch)), branch)
  expect_error(combine_margin_branches(list()))
})

# `dplyr::bind_rows()` fills a column a branch does not have with `NA`, which
# would turn a defect in the branch builders into a wider result nobody asked
# for. The fold rejected that pair, and so does this.
test_that("a branch of an unexpected shape is rejected, not filled", {
  branches <- list(
    data.frame(a = 1:2, b = c("x", "y")),
    data.frame(a = 3:4)
  )

  expect_error(combine_margin_branches(branches), "branch 2")
  expect_error(combine_margin_branches(branches), "Only in branch 1: b")
  expect_error(combine_margin_branches(rev(branches)), "Only in branch 2: b")
  # What the guard exists to prevent, stated as the thing that would otherwise
  # happen.
  expect_identical(nrow(dplyr::bind_rows(branches)), 4L)
})

# The eager path has to combine the whole list in one pass, which is the
# property that makes it linear. Withdrawing `union_all()` is how that is
# asserted without timing anything: a fold of any shape fails, a single
# `bind_rows()` does not.
test_that("eager branches are combined without folding pairwise", {
  branches <- lapply(1:4, function(i) data.frame(a = 1:2, b = i))

  testthat::local_mocked_bindings(
    union_all = function(...) stop("pairwise fold on the eager path"),
    .package = "dplyr"
  )

  expect_identical(
    combine_margin_branches(branches),
    data.frame(a = rep(1:2, 4L), b = rep(1:4, each = 2L))
  )
})

test_that("an eager expansion over 512 branches keeps one branch per set", {
  data <- nine_dimension_frame()
  spec <- grouping_spec(cube(dplyr::starts_with("d")))

  dimensions <- names(data)[names(data) != "value"]
  expanded <- expand_with_margins(data, .grouping = spec)

  plan <- compile_grouping_spec(
    spec,
    names(data),
    duplicates_choices = margin_duplicates_choices
  )
  expect_identical(length(plan$sets), 512L)
  expect_identical(nrow(expanded), 512L * nrow(data))
  # One branch labels every dimension, and no other branch can.
  all_total <- rowSums(expanded[dimensions] == "Total") == length(dimensions)
  expect_identical(sum(all_total), nrow(data))
  expect_identical(expanded$value[all_total], data$value)
  expect_identical(class(expanded), class(data))
})

test_that("lazy branches nest logarithmically rather than linearly", {
  skip_if_suggest_absent("dtplyr")

  # Folding 512 branches from the left builds 512 nested dtplyr steps, and
  # collecting that exhausts the C stack; pairing and halving bounds the depth
  # at nine.
  data <- nine_dimension_frame()
  spec <- grouping_spec(cube(dplyr::starts_with("d")))
  expanded <- expand_with_margins(dtplyr::lazy_dt(data), .grouping = spec)

  expect_identical(nrow(dplyr::collect(expanded)), 2048L)
})

test_that("a union-path query is one flat `UNION ALL` over every branch", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  data <- data.frame(
    a = c("x", "x", "y"),
    b = c("u", "v", "u"),
    c = c("p", "q", "p"),
    value = 1:3
  )
  dplyr::copy_to(con, data, "branch_union", temporary = TRUE)
  remote <- dplyr::tbl(con, "branch_union")
  spec <- grouping_spec(cube(a, b, c))

  query <- summarize_with_margins(
    remote,
    total = sum(value, na.rm = TRUE),
    .grouping = spec
  )
  sql <- as.character(dbplyr::sql_render(query))

  # Every branch joins the same top-level `UNION ALL`, with none bracketed into
  # a subquery of its own. Counting alone would not say that -- a nested union
  # renders the same number of `UNION ALL`s, just inside a `FROM (...)` -- so
  # what is counted is the ones outside every parenthesis.
  #
  # dbplyr flattens a union of unions, so this holds whichever way the branches
  # are bracketed on the way in, and it is the property rather than the
  # bracketing that is pinned here.
  plan <- compile_grouping_spec(
    spec,
    names(data),
    duplicates_choices = margin_duplicates_choices
  )
  set_count <- length(plan$sets)
  expect_identical(set_count, 8L)
  expect_identical(count_top_level_unions(sql), set_count - 1L)

  remote_result <- query |>
    dplyr::collect() |>
    dplyr::arrange(a, b, c)
  local_result <- summarize_with_margins(
    data,
    total = sum(value, na.rm = TRUE),
    .grouping = spec
  ) |>
    dplyr::as_tibble() |>
    dplyr::arrange(a, b, c)

  expect_equal(remote_result, local_result)
})

# Attaching the Grouping set identifier is the one place a Margin verb adds a
# column to the caller's own rows, and on `dtplyr` that used to invent a row
# when the caller's table had no columns at all (#184): `data.table` reads a
# table's row count from its first column, so a column-less one is always
# empty, and giving it a column materialises exactly one row.
#
# Every verb is asserted against the local answer rather than against a row
# count written here, because the defect was a disagreement between backends
# and the local backend is the one that holds rows without columns natively.
# Comparing to local also keeps this to one optional backend, as AGENTS.md
# requires.
test_that("a zero-column dtplyr input gains no row with the Grouping set id", {
  skip_if_suggest_absent("dtplyr")

  empty <- data.frame()
  expect_identical(dim(empty), c(0L, 0L))

  # The identifier is what the defect rode in on, so the verb that adds one and
  # nothing else is where it is visible undisguised.
  lazy_expanded <- expand_with_margins(dtplyr::lazy_dt(empty), .id = "s")
  # The fix attaches the identifier to an unexecuted step, and collecting is
  # still the caller's to ask for.
  expect_s3_class(lazy_expanded, "dtplyr_step")
  local_expanded <- expand_with_margins(empty, .id = "s")
  expect_identical(nrow(local_expanded), 0L)
  expect_identical(
    dplyr::collect(lazy_expanded),
    dplyr::as_tibble(local_expanded)
  )

  # Without `.id` no identifier column is added, so this half always agreed and
  # is here to say the fix left it that way.
  expect_identical(
    nrow(dplyr::collect(expand_with_margins(dtplyr::lazy_dt(empty)))),
    nrow(expand_with_margins(empty))
  )

  # Nesting reaches the same attachment through an internal identifier, so it
  # diverged with no `.id` in sight. The whole collected result is compared, as
  # above: the row count is what the defect changed, but the promise
  # `nest_with_margins()` documents for a column-less input is the result.
  expect_identical(
    dplyr::collect(nest_with_margins(dtplyr::lazy_dt(empty))),
    dplyr::as_tibble(nest_with_margins(empty))
  )
  expect_identical(
    dplyr::collect(nest_with_margins(dtplyr::lazy_dt(empty), .id = "s")),
    dplyr::as_tibble(nest_with_margins(empty, .id = "s"))
  )

  # `nest_by_with_margins()` keeps `dplyr::nest_by()`'s one row for an input
  # with no keys, so what the fabricated row showed there was the cell: a
  # `1 x 0` payload standing for a source row that never existed.
  lazy_by <- nest_by_with_margins(dtplyr::lazy_dt(empty))
  local_by <- nest_by_with_margins(empty)
  expect_identical(nrow(lazy_by), nrow(local_by))
  expect_identical(dim(lazy_by$data[[1L]]), dim(local_by$data[[1L]]))
  expect_identical(dim(local_by$data[[1L]]), c(0L, 0L))

  # Summarizing agreed by coincidence -- a Grand total set of an empty input is
  # legitimately one row -- and the count it reports is what says the row is
  # the grouping set's rather than a fabricated one.
  lazy_summary <- dplyr::collect(
    summarize_with_margins(dtplyr::lazy_dt(empty), n = dplyr::n(), .id = "s")
  )
  expect_identical(
    lazy_summary,
    dplyr::as_tibble(summarize_with_margins(empty, n = dplyr::n(), .id = "s"))
  )
  expect_identical(lazy_summary$n, 0L)

  # A summary branch with no summaries either is the one column-less branch
  # whose row is not fabricated: with no keys and nothing to calculate it is
  # the Grand total group, which local dplyr also answers with one row. Fixing
  # the expansion by counting rows would have taken this row away.
  expect_identical(
    dplyr::collect(summarize_with_margins(dtplyr::lazy_dt(empty), .id = "s")),
    dplyr::as_tibble(summarize_with_margins(empty, .id = "s"))
  )
})

# The neighbour of the case above, and the boundary it runs into rather than a
# second instance of the defect. Asked for no summaries and given no key,
# `dtplyr` has no column to carry the Grand total row on, and a one-row,
# zero-column `data.table` does not exist -- `dim()` reads the row count from
# the first column. Nothing in the union adapter decides it: it is what
# `dplyr::summarize()` already answers for the same lazy input, which is what
# this compares against, so a dtplyr that gained the shape reports here.
#
# What the result holds decides this and not what the input held, which is why
# a keyed frame is here beside the column-less one -- and why the limit is
# documented on `summarize_with_margins()` rather than beside #184's
# column-less input, whose other verbs now agree.
test_that("a column-less dtplyr summary keeps dtplyr's own empty answer", {
  skip_if_suggest_absent("dtplyr")

  for (input in list(data.frame(), data.frame(a = 1:3, g = c("x", "x", "y")))) {
    upstream <- dplyr::collect(dplyr::summarize(dtplyr::lazy_dt(input)))
    expect_identical(dim(upstream), c(0L, 0L))
    expect_identical(
      dim(dplyr::collect(summarize_with_margins(dtplyr::lazy_dt(input)))),
      dim(upstream)
    )
    # The local answer is `dplyr::summarize()`'s there too, and it is one row.
    expect_identical(
      dim(summarize_with_margins(input)),
      dim(dplyr::summarize(input))
    )
    expect_identical(nrow(dplyr::summarize(input)), 1L)
  }

  # Anything that puts a column in the result ends the disagreement, which is
  # the escape the documentation offers and the reason this stays a boundary
  # rather than a defect.
  keyed <- data.frame(a = 1:3, g = c("x", "x", "y"))
  expect_identical(
    dplyr::collect(summarize_with_margins(dtplyr::lazy_dt(keyed), .by = g)),
    dplyr::as_tibble(summarize_with_margins(keyed, .by = g))
  )
  expect_identical(
    dplyr::collect(summarize_with_margins(dtplyr::lazy_dt(keyed), .id = "s")),
    dplyr::as_tibble(summarize_with_margins(keyed, .id = "s"))
  )
})

# The count-preserving attachment is asked for only where a backend needs it,
# because `n()` in a `mutate()` is a window function on SQL and an unsupported
# expression on arrow, where it warns and pulls the data into R. Arrow is the
# other lazy backend that can be handed a zero-column table at all, and it
# already answered the local count.
test_that("a zero-column arrow input keeps the local row count", {
  skip_if_suggest_absent("arrow")

  empty <- data.frame()
  query <- expand_with_margins(
    arrow::as_arrow_table(empty),
    .id = "s"
  )

  result <- expect_no_warning(dplyr::collect(query))
  expect_identical(
    result,
    dplyr::as_tibble(expand_with_margins(empty, .id = "s"))
  )
})

# The third fold site: `build_lazy_parent_mapping()` unions one denominator
# mapping per grouping set that has a coarser one. A Parent share needs a pure
# `rollup()`, so that site never sees the branch counts the other two do -- it
# is converted for one mechanism rather than for its own cost.
test_that("Parent shares combine their denominator mappings the same way", {
  data <- data.frame(
    region = rep(c("north", "south"), each = 4L),
    channel = rep(c("web", "store"), times = 4L),
    sku = rep(c("a", "b"), each = 2L),
    value = seq_len(8)
  )

  result <- summarize_with_margins(
    data,
    total = sum(value),
    parent = share_of_parent(total),
    .grouping = rollup(region, channel, sku)
  )

  grand_total <- sum(data$value)
  is_grand_total <- result$region == "Total"
  expect_identical(result$total[is_grand_total], grand_total)
  expect_identical(result$parent[is_grand_total], 1)

  # The set one dimension finer than the Grand total divides by it, and the
  # one below that divides by its own region.
  by_region <- result[
    result$channel == "Total" & result$sku == "Total" & !is_grand_total,
  ]
  expect_equal(by_region$parent, by_region$total / grand_total)

  by_channel <- result[result$sku == "Total" & result$channel != "Total", ]
  region_totals <- stats::setNames(by_region$total, by_region$region)
  expect_equal(
    by_channel$parent,
    by_channel$total / unname(region_totals[by_channel$region])
  )
})

# The second arm of the branch guard ADR 0025 adds. Its first arm refuses an
# Arrow input that absorbed a summary, which is a Package condition because the
# caller can rewrite the call; this one covers every other lazy backend, where
# a local branch from a lazy input is a defect no rewrite of theirs avoids and
# is therefore an internal invariant (ADR 0015).
#
# It has no path through a public verb by construction -- that is what makes it
# an invariant, and why the two invariants in `test-diagnostic-pluralization.R`
# are called directly too. The backend below is fabricated for the same reason:
# no backend marginplyr supports answers a lazy input with a local frame, and
# one that started to is exactly what this reports. It is also what
# `combine_margin_branches()` does not catch on its own -- a branch list that is
# part lazy and part local is accepted in the lazy-first order and collects to
# the combined rows.
local_answering_summarise <- function(.data, ..., .by = NULL) {
  data.frame(k = "E", out = 1)
}

test_that("a lazy input answering with a local branch is an invariant", {
  registerS3method(
    "summarise",
    "marginplyr_test_local_answering",
    local_answering_summarise,
    envir = asNamespace("dplyr")
  )
  lazy <- structure(list(), class = "marginplyr_test_local_answering")

  raised <- expect_error(
    summarize_margin_branch(
      lazy,
      out = 1,
      .by = character(),
      caller_labels = "out = 1"
    ),
    "A lazy input produced a local summary branch"
  )

  # Not a Package condition: the caller cannot rewrite their way out of it, and
  # it must not be reported as the Arrow refusal, which names a remedy that
  # would not apply.
  expect_false(inherits(raised, "marginplyr_error"))
  expect_no_match(conditionMessage(raised), "Arrow", fixed = TRUE)
  expect_match(conditionMessage(raised), "data.frame", fixed = TRUE)
})
