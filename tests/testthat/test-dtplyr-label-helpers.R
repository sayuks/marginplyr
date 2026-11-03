test_that("included nesting keys ignore late caller conversion bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; this test requires only that backend.
  source <- data.table::data.table(g = "a", value = 7L)
  before <- serialize(source, NULL)
  root <- new.env(parent = globalenv())
  root$source <- source
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- nest_with_margins(input, .grouping = grouping_set(g), .keep = TRUE)
  expect_s3_class(query, "dtplyr_step")
  reads <- 0L
  root$as.character <- function(...) {
    reads <<- reads + 1L
    "MASK"
  }

  results <- list(
    dplyr::collect(query),
    nest_by_with_margins(input, .grouping = grouping_set(g), .keep = TRUE)
  )
  expect_s3_class(results[[2L]], "rowwise_df")
  for (result in results) {
    expect_identical(result$g, "a")
    expect_identical(lapply(result$data, as.list), list(list(g = "a", value = 7L)))
  }
  controls <- list(
    dplyr::collect(input),
    dplyr::collect(dplyr::summarise(input, n = dplyr::n(), .by = g)),
    dplyr::collect(dplyr::summarise(
      dplyr::group_by(input, g), data = list(dplyr::pick(dplyr::everything()))
    ))
  )
  for (control in controls) expect_identical(control$g, "a")
  expect_identical(as.list(controls[[3L]]$data[[1L]]), list(value = 7L))
  expect_identical(reads, 0L)
  root$as.character <- base::as.character
  expect_identical(dplyr::collect(query)$g, "a")
  expect_identical(serialize(source, NULL), before)
})

# Exercise the shared label path through all four public verbs.
label_helper_verbs <- list(
  expand = function(input, ...) expand_with_margins(input, ...),
  summarize = function(input, ...) {
    summarize_with_margins(input, n = dplyr::n(), ...)
  },
  nest = function(input, ...) nest_with_margins(input, .keep = TRUE, ...),
  nest_by = function(input, ...) nest_by_with_margins(input, .keep = TRUE, ...)
)

# Compare public values without backend-specific outer or cell classes.
label_helper_values <- function(result) {
  result <- tibble::as_tibble(dplyr::ungroup(dplyr::collect(result)))
  if ("data" %in% names(result)) {
    result$data <- lapply(result$data, tibble::as_tibble)
  }
  result
}

test_that("Margin labels ignore caller repetition and size helpers", {
  skip_if_suggest_absent("dtplyr")
  keys <- list(
    c("b", "a"), c(2L, 1L), factor(c("b", "a")),
    factor(c("b", NA_character_), exclude = NULL)
  )
  for (key in keys) {
    for (size in c(0L, 2L)) {
      source <- data.table::data.table(g = key[seq_len(size)], value = seq_len(size))
      before <- serialize(source, NULL)
      root <- new.env(parent = globalenv())
      root$source <- source
      root$as.character <- root$rep <- root$length <- function(...) {
        stop("caller label helper ran")
      }
      input <- evalq(dtplyr::lazy_dt(source), root)
      # NA_character_ conflicts with a declared NA factor level (ADR 0012).
      labels <- if (anyNA(levels(key))) list("Total", NULL) else {
        list("Total", NULL, NA_character_)
      }
      for (label in labels) {
        for (verb in label_helper_verbs) {
          run <- function(data) {
            verb(data, .grouping = rollup(g), .margin_label = label,
                 .sort = "last", .id = "set")
          }
          expected <- label_helper_values(run(tibble::as_tibble(source)))
          query <- run(input)
          actual <- label_helper_values(query)
          expect_identical(actual, expected)
          expect_identical(label_helper_values(query), expected)
          expect_identical(serialize(source, NULL), before)
        }
      }
    }
  }
})

test_that("label isolation preserves user expressions and collection boundaries", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = "a", value = 7L)
  visits <- 0L
  root$observe <- function(x) {
    visits <<- visits + base::length(x)
    base::rep(TRUE, base::length(x))
  }
  root$as.character <- function(...) "USER"
  root$rep <- function(...) "REPEAT"
  root$length <- function(...) 99L
  root$sum <- function(...) 77L
  input <- evalq(dplyr::filter(dtplyr::lazy_dt(source), observe(value)), root)
  summary <- evalq(summarize_with_margins(
    input, text = as.character(g[[1L]]), total = sum(value),
    repeated = rep("user", 1L), size = length(value), .grouping = rollup(g)
  ), list2env(list(input = input), parent = root))
  expect_s3_class(summary, "dtplyr_step")
  expect_identical(visits, 0L)
  result <- dplyr::collect(summary)
  expect_identical(result$g, c("a", "Total"))
  expect_identical(result$text, c("USER", "USER"))
  expect_identical(result$total, c(77L, 77L))
  expect_identical(result$repeated, c("REPEAT", "REPEAT"))
  expect_identical(result$size, c(99L, 99L))
  expect_gt(visits, 0L)

  visits <- 0L
  query <- nest_with_margins(input, .grouping = rollup(g))
  expect_s3_class(query, "dtplyr_step")
  expect_identical(visits, 0L)
  expect_identical(dplyr::collect(query)$g, c("a", "Total"))
  expect_gt(visits, 0L)
  visits <- 0L
  rowwise <- nest_by_with_margins(input, .grouping = rollup(g))
  expect_s3_class(rowwise, "rowwise_df")
  expect_identical(rowwise$g, c("a", "Total"))
  expect_gt(visits, 0L)
})

test_that("observed collisions ignore caller conversion and aggregate helpers", {
  skip_if_suggest_absent("dtplyr")
  for (helper in c("as.character", "sum")) {
    source <- data.table::data.table(g = "Total", value = 7L)
    before <- serialize(source, NULL)
    root <- new.env(parent = globalenv())
    root$source <- source
    reads <- 0L
    root[[helper]] <- function(...) {
      reads <<- reads + 1L
      if (helper == "sum") 0L else "MASK"
    }
    input <- evalq(dtplyr::lazy_dt(source), root)
    for (verb in label_helper_verbs) {
      expect_error(
        verb(input, .grouping = rollup(g), .check_margin_label = TRUE),
        "already present in grouping", class = "marginplyr_error"
      )
    }
    expect_identical(reads, 0L, info = helper)
    expect_identical(dplyr::collect(input)$g, "Total")
    expect_identical(serialize(source, NULL), before)
  }
})
