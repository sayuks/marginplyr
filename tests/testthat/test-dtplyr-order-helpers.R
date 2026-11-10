test_that("Margin order ignores late caller helper bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; only one optional backend is required.
  source <- data.table::data.table(g = c(NA_character_, "b", "a"), row_id = 1:3)
  before <- serialize(source, NULL)

  for (helper in c("is.na", "%in%")) {
    root <- new.env(parent = globalenv())
    root$source <- source
    input <- evalq(dtplyr::lazy_dt(source), root)
    spec <- if (helper == "is.na") grouping_set(g) else rollup(g)
    label <- if (helper == "is.na") NA_character_ else "Total"
    query <- nest_with_margins(
      input, .grouping = spec, .margin_label = label, .sort = "last"
    )
    expect_s3_class(query, "dtplyr_step")
    reads <- 0L
    root[[helper]] <- if (helper == "is.na") {
      function(x) {
        reads <<- reads + 1L
        !base::is.na(x)
      }
    } else {
      function(x, table) {
        reads <<- reads + 1L
        !base::`%in%`(x, table)
      }
    }
    expected <- c("a", "b", NA_character_, "Total"[helper == "%in%"])
    actual <- dplyr::collect(query)
    expect_identical(actual$g, expected, info = helper)
    expect_identical(names(actual), c("g", "data"))
    expect_identical(
      lapply(actual$data, function(cell) cell$row_id),
      c(list(3L, 2L, 1L), list(1:3)[helper == "%in%"])
    )
    expect_identical(reads, 0L, info = helper)

    root[[helper]] <- getExportedValue("base", helper)
    expect_identical(dplyr::collect(query), actual)
    expect_identical(serialize(source, NULL), before)
  }
})

# Compare the public keys and cell contents without backend-specific classes.
order_helper_values <- function(result, sort) {
  result <- dplyr::ungroup(result)
  if (sort == "none") {
    keys <- intersect(c("set", "fixed", ".BY"), names(result))
    result <- dplyr::arrange(result, !!!rlang::syms(keys))
  }
  list(
    keys = as.list(result[setdiff(names(result), "data")]),
    cells = lapply(result$data, as.list)
  )
}

test_that("nesting order and cells ignore preexisting caller helpers", {
  skip_if_suggest_absent("dtplyr")
  options <- expand.grid(
    size = c(0L, 4L), fixed = c(FALSE, TRUE), rollup = c(FALSE, TRUE),
    keep = c(FALSE, TRUE), sort = c("none", "first", "last"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(options))) {
    option <- options[i, ]
    local <- tibble::tibble(
      fixed = c(NA_character_, "x", "x", "x")[seq_len(option$size)],
      .BY = c("b", "b", "a", "b")[seq_len(option$size)],
      row_id = seq_len(option$size)
    )
    root <- new.env(parent = globalenv())
    root$source <- data.table::as.data.table(local)
    before <- serialize(root$source, NULL)
    root$is.na <- root$`%in%` <- function(...) stop("caller helper ran")
    input <- evalq(dtplyr::lazy_dt(source), root)
    by <- if (option$fixed) "fixed" else character()
    spec <- if (option$rollup) rollup(.BY) else grouping_set(.BY)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      run <- function(data) {
        verb(data, .by = dplyr::all_of(by), .grouping = spec,
             .keep = option$keep, .sort = option$sort, .id = "set")
      }
      expected <- run(local)
      query <- run(input)
      if (identical(verb, nest_with_margins)) {
        expect_s3_class(query, "dtplyr_step")
      } else {
        expect_s3_class(query, "rowwise_df")
        expect_identical(dplyr::group_vars(query), c(by, ".BY", "set"))
      }
      actual <- dplyr::collect(query)
      expect_identical(names(actual), c(by, ".BY", "set", "data"))
      expect_identical(order_helper_values(actual, option$sort),
                       order_helper_values(expected, option$sort),
                       info = as.character(i))
      expect_identical(serialize(root$source, NULL), before)
    }
  }
})

test_that("shared Margin order preserves lazy and eager boundaries", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c(NA_character_, "b", "a"), row_id = 1:3)
  reads <- 0L
  root$observe <- function(x) {
    if (length(x) > 0L) reads <<- reads + 1L
    rep(TRUE, length(x))
  }
  root$is.na <- root$`%in%` <- function(...) stop("caller helper ran")
  input <- evalq(dplyr::filter(dtplyr::lazy_dt(source), observe(row_id)), root)

  for (sort in c("first", "last")) {
    for (verb in list(
      function(data, ...) nest_with_margins(data, ...),
      function(data, ...) expand_with_margins(data, ...),
      function(data, ...) summarize_with_margins(data, n = dplyr::n(), ...)
    )) {
      reads <- 0L
      query <- verb(input, .grouping = rollup(g), .sort = sort)
      expect_s3_class(query, "dtplyr_step")
      expect_identical(reads, 0L)
      actual <- dplyr::collect(query)
      expect_gt(reads, 0L)
      expected <- dplyr::collect(verb(root$source, .grouping = rollup(g),
                                      .sort = sort))
      expect_identical(as.list(actual[setdiff(names(actual), "data")]),
                       as.list(expected[setdiff(names(expected), "data")]))
    }
    reads <- 0L
    rowwise <- nest_by_with_margins(input, .grouping = rollup(g), .sort = sort)
    expect_s3_class(rowwise, "rowwise_df")
    expect_gt(reads, 0L)
    expect_identical(rowwise$g, if (sort == "first") {
      c("Total", "a", "b", NA_character_)
    } else {
      c("a", "b", NA_character_, "Total")
    })
  }
})

test_that("caller-written ordering helpers keep ordinary lexical lookup", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c("b", "a", NA_character_), row_id = 1:3)
  root$input <- evalq(dtplyr::lazy_dt(source), root)
  reads <- 0L
  root$is.na <- function(x) {
    reads <<- reads + 1L
    !base::is.na(x)
  }
  root$`%in%` <- function(x, table) {
    reads <<- reads + 1L
    !base::`%in%`(x, table)
  }
  query <- evalq(summarize_with_margins(
    input, missing = sum(is.na(row_id)), member = sum(row_id %in% 1L),
    .grouping = rollup(g), .sort = "last"
  ), root)
  expect_identical(reads, 0L)
  actual <- dplyr::collect(query)
  expect_identical(actual$g, c("a", "b", NA_character_, "Total"))
  expect_identical(actual$missing, c(1L, 1L, 1L, 3L))
  expect_identical(actual$member, c(1L, 0L, 1L, 2L))
  expect_gt(reads, 0L)

  reads <- 0L
  native <- dplyr::collect(root$input)
  counted <- dplyr::collect(dplyr::count(root$input, g))
  expect_identical(native, root$source)
  expect_identical(counted$n, c(1L, 1L, 1L))
  expect_identical(reads, 0L)
})

test_that("generated Margin order avoids the caller namespace operator", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(
    g = c("a", "a"), h = c("x", "y"), x = c(1, 3)
  )
  source_bytes <- serialize(source, NULL)
  expected <- summarize_with_margins(
    source, value = sum(x), .grouping = rollup(g, h), .sort = "last"
  )

  for (install_before in c(TRUE, FALSE)) {
    root <- new.env(parent = globalenv())
    root$source <- source
    root$getExportedValue <- function(...) {
      stop("CALLER_EXPORT_LOOKUP_INTERCEPT")
    }
    helper_calls <- character()
    caller_calls <- character()
    dispatcher <- function(pkg, name) {
      package <- as.character(substitute(pkg))
      member <- as.character(substitute(name))
      if (identical(package, "base")) {
        if (member %in% c("is.na", "%in%")) {
          helper_calls <<- c(helper_calls, member)
          stop("GENERATED_ORDER_NAMESPACE_INTERCEPT")
        }
        if (identical(member, "sum")) {
          caller_calls <<- c(caller_calls, member)
        }
      }
      getExportedValue(package, member)
    }
    if (install_before) root$`::` <- dispatcher
    input <- evalq(dtplyr::lazy_dt(source), root)
    root$input <- input
    query <- evalq(summarize_with_margins(
      input, value = sum(x), .grouping = rollup(g, h), .sort = "last"
    ), root)
    expect_s3_class(query, "dtplyr_step")
    expect_identical(helper_calls, character())
    expect_identical(caller_calls, character())
    if (!install_before) root$`::` <- dispatcher

    actual <- dplyr::collect(query)
    expect_equal(as.data.frame(actual), as.data.frame(expected))
    expect_identical(helper_calls, character())
    expect_identical(serialize(root$source, NULL), source_bytes)

    helper_calls <- character()
    unsorted <- evalq(summarize_with_margins(
      input, value = sum(x), .grouping = rollup(g, h), .sort = "none"
    ), root)
    expected_unsorted <- summarize_with_margins(
      source, value = sum(x), .grouping = rollup(g, h), .sort = "none"
    )
    expect_equal(as.data.frame(dplyr::collect(unsorted)),
                 as.data.frame(expected_unsorted))
    expect_identical(helper_calls, character())

    # A namespace expression written by the caller retains its normal lookup.
    namespaced <- evalq(summarize_with_margins(
      input, value = base::sum(x), .grouping = rollup(g, h), .sort = "last"
    ), root)
    expect_identical(caller_calls, character())
    expect_identical(dplyr::collect(namespaced)$value, expected$value)
    expect_true("sum" %in% caller_calls)
    expect_identical(helper_calls, character())
  }
})
