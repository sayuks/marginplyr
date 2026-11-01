test_that("nesting display helpers ignore late caller bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; no second optional backend is required.
  source <- data.table::data.table(g = c("a", "b"), row_id = 1:2)
  before <- serialize(source, NULL)

  for (helper in c("min", "seq_len")) {
    root <- new.env(parent = globalenv())
    root$source <- source
    input <- evalq(dtplyr::lazy_dt(source), root)
    query <- nest_with_margins(input, .grouping = grouping_set(g))
    expect_s3_class(query, "dtplyr_step")
    reads <- 0L
    root[[helper]] <- function(...) {
      reads <<- reads + 1L
      if (helper == "min") 0L else integer()
    }

    results <- list(
      dplyr::collect(query),
      nest_by_with_margins(input, .grouping = grouping_set(g))
    )
    expect_s3_class(results[[2L]], "rowwise_df")
    for (result in results) {
      expect_identical(result$g, c("a", "b"), info = helper)
      expect_identical(names(result), c("g", "data"))
      expect_identical(lapply(result$data, as.list), list(
        list(row_id = 1L), list(row_id = 2L)
      ))
    }
    expect_identical(reads, 0L, info = helper)

    controls <- list(
      dplyr::collect(input),
      dplyr::collect(dplyr::summarise(input, n = dplyr::n(), .by = g))
    )
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      controls <- c(controls, list(
        dplyr::collect(verb(input, .by = g)),
        verb(dplyr::collect(input), .grouping = grouping_set(g))
      ))
    }
    for (control in controls) expect_identical(control$g, c("a", "b"))
    expect_identical(reads, 0L, info = helper)

    root[[helper]] <- getExportedValue("base", helper)
    expect_identical(dplyr::collect(query)$g, c("a", "b"))
    expect_identical(serialize(source, NULL), before)
  }
})

# Compare public values and cell row counts independently of backend classes.
nest_display_values <- function(result, sort) {
  result <- dplyr::ungroup(result)
  if (sort == "none") result <- dplyr::arrange(result, .data$set, .data$g)
  list(
    outer = as.list(result[setdiff(names(result), "data")]),
    cells = lapply(result$data, function(cell) {
      list(rows = nrow(cell), values = as.list(cell))
    })
  )
}

test_that("caller helpers cannot change nesting keys or cells across options", {
  skip_if_suggest_absent("dtplyr")
  keys <- list(
    c("b", "a", "b", NA_character_),
    c(2L, 1L, 2L, NA_integer_),
    factor(c("b", "a", "b", NA_character_), levels = c("c", "a", "b"))
  )
  options <- expand.grid(
    key = seq_along(keys), size = c(0L, 4L), rollup = c(FALSE, TRUE),
    label = 1:3, keep = c(FALSE, TRUE), sort = c("none", "first", "last"),
    stringsAsFactors = FALSE
  )
  verbs <- list(nest_with_margins, nest_by_with_margins)
  for (i in seq_len(nrow(options))) {
    option <- options[i, ]
    input <- tibble::tibble(
      g = keys[[option$key]][seq_len(option$size)],
      row_id = seq_len(option$size), payload = letters[seq_len(option$size)]
    )
    # data.table is imported by dtplyr; this guard requires only dtplyr.
    source <- data.table::as.data.table(input)
    before <- serialize(source, NULL)
    root <- new.env(parent = globalenv())
    root$source <- source
    reads <- 0L
    root$min <- function(...) {
      reads <<- reads + 1L
      0L
    }
    root$seq_len <- function(...) {
      reads <<- reads + 1L
      integer()
    }
    lazy <- evalq(dtplyr::lazy_dt(source), root)
    spec <- if (option$rollup) rollup(g) else grouping_set(g)
    label <- list("Total", NULL, NA_character_)[[option$label]]
    for (verb in verbs) {
      run <- function(data) {
        verb(data, .grouping = spec, .margin_label = label,
             .keep = option$keep, .sort = option$sort, .id = "set")
      }
      expected <- run(input)
      actual <- dplyr::collect(run(lazy))
      info <- paste(i, paste(unlist(option), collapse = "/"))
      expect_identical(names(actual), c("g", "set", "data"), info = info)
      expect_identical(
        nest_display_values(actual, option$sort),
        nest_display_values(expected, option$sort), info = info
      )
      for (cell in actual$data) {
        expect_identical(
          names(cell), c("g"[option$keep], "row_id", "payload"), info = info
        )
        expect_identical(cell$payload, input$payload[cell$row_id], info = info)
        if (option$keep) {
          expect_identical(cell$g, input$g[cell$row_id], info = info)
        }
      }
      expect_identical(reads, 0L, info = info)
      expect_identical(serialize(source, NULL), before, info = info)
    }
  }
})

test_that("isolated display helpers preserve nesting collection boundaries", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c("a", "b"), row_id = 1:2)
  reads <- 0L
  root$observe <- function(value) {
    if (length(value) > 0L) reads <<- reads + 1L
    rep(TRUE, length(value))
  }
  root$min <- root$seq_len <- function(...) stop("caller helper ran")
  input <- evalq(dplyr::filter(dtplyr::lazy_dt(source), observe(row_id)), root)

  query <- nest_with_margins(input, .grouping = grouping_set(g))
  expect_s3_class(query, "dtplyr_step")
  expect_identical(reads, 0L)
  expect_identical(dplyr::collect(query)$g, c("a", "b"))
  expect_gt(reads, 0L)

  reads <- 0L
  rowwise <- nest_by_with_margins(input, .grouping = grouping_set(g))
  expect_s3_class(rowwise, "rowwise_df")
  expect_identical(dplyr::group_vars(rowwise), "g")
  expect_identical(rowwise$g, c("a", "b"))
  expect_gt(reads, 0L)
})
