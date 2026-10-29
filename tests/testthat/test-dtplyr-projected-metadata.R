test_that("projected dtplyr types ignore discarded source names", {
  skip_if_suggest_absent("dtplyr")
  normalize <- function(x) {
    if (inherits(x, "dtplyr_step")) {
      x <- dplyr::collect(x)
    }
    x <- dplyr::ungroup(x)
    if ("data" %in% names(x)) {
      x$data <- lapply(x$data, tibble::as_tibble)
    }
    x
  }
  operations <- list(
    function(x) inspect_grouping(x, .by = flag, .grouping = rollup(value)),
    function(x) {
      inspect_grouping(
        x, .by = where(is.logical), .grouping = rollup(value)
      )
    },
    function(x) {
      inspect_grouping(
        x, .by = flag, .grouping = rollup(where(is.integer))
      )
    },
    function(x) {
      summarize_with_margins(
        x, total = sum(value), .by = where(is.logical),
        .grouping = rollup(where(is.integer)), .sort = "last"
      )
    },
    function(x) {
      expand_with_margins(
        x, .by = where(is.logical), .grouping = rollup(where(is.integer)),
        .sort = "last"
      )
    },
    function(x) {
      nest_with_margins(
        x, .by = where(is.logical), .grouping = rollup(where(is.integer)),
        .keep = TRUE, .sort = "last"
      )
    },
    function(x) {
      nest_by_with_margins(
        x, .by = where(is.logical), .grouping = rollup(where(is.integer)),
        .keep = TRUE, .sort = "last"
      )
    }
  )
  for (column in c("..1", "..2", "...")) {
    for (rows in c(0L, 1L, 3L)) {
      source <- data.table::data.table(
        special = rep("a", rows), value = seq_len(rows), flag = rep(TRUE, rows)
      )
      data.table::setnames(source, "special", column)
      before <- data.table::copy(source)
      root <- dtplyr::lazy_dt(source)
      for (step in list(
        dplyr::select(root, value, flag),
        dplyr::rename(root, ordinary = dplyr::all_of(column))
      )) {
        collected <- dplyr::collect(step)
        for (operation in operations) {
          expect_identical(normalize(operation(step)),
                           normalize(operation(collected)))
          expect_identical(source, before)
        }
      }
    }
  }
})

test_that("discarded names do not make typed planning run row predicates", {
  skip_if_suggest_absent("dtplyr")
  predicate <- function(x) stop("row predicate evaluated")
  for (column in c("..1", "..2", "...")) {
    source <- data.table::data.table(special = "a", value = 1:3, flag = TRUE)
    data.table::setnames(source, "special", column)
    before <- data.table::copy(source)
    for (immutable in c(TRUE, FALSE)) {
      root <- dtplyr::lazy_dt(source, immutable = immutable)
      for (step in list(
        dplyr::select(dplyr::filter(root, predicate(value)), value, flag),
        dplyr::filter(dplyr::select(root, value, flag), predicate(value))
      )) {
        expect_identical(inspect_grouping(
          step, .by = where(is.logical), .grouping = rollup(where(is.integer))
        ), inspect_grouping(tibble::tibble(value = 1:3, flag = TRUE),
                            .by = flag, .grouping = rollup(value)))
        if (immutable) {
          for (verb in list(summarize_with_margins, expand_with_margins,
                            nest_with_margins)) {
            expect_s3_class(verb(
              step, .by = where(is.logical),
              .grouping = rollup(where(is.integer))
            ), "dtplyr_step")
          }
        }
        expect_identical(source, before)
      }
    }
  }
})

test_that("visible unsupported dot-dot schemas remain refused", {
  skip_if_suggest_absent("dtplyr")
  for (column in c("..1", "..2", "...")) {
    source <- data.table::data.table(special = "a", value = 1L, flag = TRUE)
    data.table::setnames(source, "special", column)
    before <- data.table::copy(source)
    root <- dtplyr::lazy_dt(source)
    for (step in list(root, dplyr::filter(root, TRUE))) {
      expect_error(inspect_grouping(step, .by = where(is.logical)),
                   "Names can't be of the form", fixed = TRUE)
      for (verb in list(summarize_with_margins, expand_with_margins,
                        nest_with_margins, nest_by_with_margins)) {
        expect_error(verb(step, .grouping = rollup(value)),
                     "Names can't be of the form", fixed = TRUE)
      }
      expect_identical(source, before)
    }
  }
})
