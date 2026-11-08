test_that("dtplyr typed inspection does not classify populated S4 columns", {
  skip_if_suggest_absent("dtplyr")
  class <- "marginplyr_numeric_metadata_witness"
  methods::setClass(class, contains = "numeric")
  withr::defer(methods::removeClass(class))
  witness <- new.env(parent = emptyenv())
  witness$seen <- list()
  methods::setMethod("is.numeric", class, function(x) {
    witness$seen[[length(witness$seen) + 1L]] <- list(
      length = length(x), values = methods::as(x, "numeric")
    )
    TRUE
  })
  for (values in list(c(13, 29), c(101, 307))) {
    for (immutable in c(TRUE, FALSE)) {
      source <- data.table::data.table(
        a = methods::new(class, values), n = 1:2, label = c("x", "y")
      )
      before <- serialize(source, NULL)
      # Observe real method arguments: a correct plan alone hides source reads.
      witness$seen <- list()
      expect_true(is.numeric(source$a))
      expect_identical(witness$seen, list(list(length = 2L, values = values)))
      filter_rows <- integer()
      counted_filter <- function(x) {
        filter_rows <<- c(filter_rows, length(x))
        x > 1L
      }
      root <- dtplyr::lazy_dt(source, immutable = immutable)
      steps <- list(
        root = root,
        filter = dplyr::filter(root, counted_filter(n)),
        rename = dplyr::rename(root, renamed = n),
        alias = dplyr::mutate(root, copy = n),
        arithmetic = dplyr::mutate(root, copy = n + 1L)
      )
      for (step in steps) {
        witness$seen <- list()
        plan <- inspect_grouping(
          step, .grouping = grouping_set(where(is.character))
        )
        expect_identical(plan$included[[1L]], "(label)")
        expect_identical(witness$seen, list())
        expect_identical(
          inspect_grouping(step, .by = where(is.character)),
          inspect_grouping(step, .by = label)
        )
        expect_identical(witness$seen, list())
        expect_identical(filter_rows, integer())
        expect_identical(serialize(source, NULL), before)
      }
      witness$seen <- list()
      unsupported <- dplyr::mutate(root, derived = a + 1L)
      expect_identical(inspect_grouping(
        unsupported, .grouping = grouping_set(label)
      )$included, "(label)")
      expect_error(inspect_grouping(
        unsupported, .grouping = grouping_set(where(is.character))
      ), "Can't safely determine column metadata", fixed = TRUE,
      class = "marginplyr_error")
      expect_error(inspect_grouping(unsupported, .by = where(is.character)),
                   "dplyr::collect()", fixed = TRUE,
                   class = "marginplyr_error")
      expect_identical(witness$seen, list())
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("S4 metadata and explicit collection survive ordinary derivations", {
  skip_if_suggest_absent("dtplyr")
  class <- "marginplyr_preserved_numeric_metadata"
  methods::setClass(class, contains = "numeric")
  withr::defer(methods::removeClass(class))
  source <- data.table::data.table(
    a = methods::new(class, c(13, 29)), n = 1:2, label = c("x", "y")
  )
  before <- serialize(source, NULL)
  for (immutable in c(TRUE, FALSE)) {
    root <- dtplyr::lazy_dt(source, immutable = immutable)
    step <- dplyr::mutate(root, alias = a, copy = n, arithmetic = n + 1L)
    plan <- inspect_grouping(step, .grouping = grouping_set(where(isS4)))
    expect_identical(plan$included[[1L]], "(a, alias)")
    plan <- inspect_grouping(step, .grouping = grouping_set(where(is.integer)))
    expect_identical(plan$included[[1L]], "(n, copy, arithmetic)")
    expect_identical(
      inspect_grouping(step, .grouping = grouping_set(label))$included,
      "(label)"
    )
    expect_identical(serialize(source, NULL), before)
    explicit <- dplyr::collect(step)
    expect_identical(explicit$a, methods::new(class, c(13, 29)))
    expect_identical(explicit$alias, explicit$a)
    expect_identical(explicit$copy, 1:2)
    expect_identical(explicit$arithmetic, 2:3)
    if (immutable) expect_identical(serialize(source, NULL), before)
  }
})
