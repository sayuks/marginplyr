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

  expect_identical(length(compile_grouping_spec(spec, names(data))$sets), 512L)
  expect_identical(nrow(expanded), 512L * nrow(data))
  # One branch labels every dimension, and no other branch can.
  all_total <- rowSums(expanded[dimensions] == "Total") == length(dimensions)
  expect_identical(sum(all_total), nrow(data))
  expect_identical(expanded$value[all_total], data$value)
  expect_identical(class(expanded), class(data))
})

test_that("lazy branches nest logarithmically rather than linearly", {
  skip_if_backend_absent("dtplyr")

  # Folding 512 branches from the left builds 512 nested dtplyr steps, and
  # collecting that exhausts the C stack; pairing and halving bounds the depth
  # at nine.
  data <- nine_dimension_frame()
  spec <- grouping_spec(cube(dplyr::starts_with("d")))
  expanded <- expand_with_margins(dtplyr::lazy_dt(data), .grouping = spec)

  expect_identical(nrow(dplyr::collect(expanded)), 2048L)
})

test_that("a union-path query is one flat `UNION ALL` over every branch", {
  skip_if_backend_absent("RSQLite", "DBI")

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
  set_count <- length(compile_grouping_spec(spec, names(data))$sets)
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
