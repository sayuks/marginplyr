dtplyr_list_dimensions <- function() {
  list(
    list_of = vctrs::list_of(1L, 2L),
    AsIs = I(list(1L, 2L)),
    blob = blob::blob(as.raw(1L), as.raw(2L)),
    bare = list(1L, 2L),
    dates = vctrs::list_of(as.Date("2026-01-01"), as.Date("2026-01-02")),
    frames = vctrs::list_of(data.frame(x = 1L), data.frame(x = 2L)),
    nested = vctrs::list_of(list(1L), list(2L))
  )
}

test_that("dtplyr list expansion agrees with local and backend controls", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("blob")
  for (values in dtplyr_list_dimensions()) {
    for (size in 0:2) {
      data <- tibble::tibble(
        group = values[seq_len(size)], payload = seq_len(size)
      )
      source <- dtplyr::lazy_dt(data)
      for (label in list(NULL, NA_character_)) {
        expand <- function(input) {
          expand_with_margins(
            input, .grouping = rollup(group),
            .margin_label = label, .id = "sid"
          )
        }
        query <- expand(source)
        expect_s3_class(query, "dtplyr_step")
        result <- dplyr::collect(query)
        control <- dplyr::union_all(
          dplyr::mutate(source, sid = 1L),
          dplyr::mutate(
            source, group = vctrs::vec_init(group, length(group)), sid = 2L
          )
        ) |>
          dplyr::collect() |>
          dplyr::relocate(group, sid)

        expect_identical(result, expand(data))
        expect_identical(result, control)
        expect_identical(result$payload, rep(seq_len(size), 2L))
        expect_identical(result$sid, rep(1:2, each = size))
        expect_identical(dplyr::collect(query), result)
      }
    }
  }
})

test_that("dtplyr list dimensions retain repeated grouping-set occurrences", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("blob")
  for (values in dtplyr_list_dimensions()) {
    data <- tibble::tibble(group = values, payload = 1:2)
    expand <- function(input) {
      expand_with_margins(
        input,
        .grouping = grouping_sets(grouping_set(group), rollup(group)),
        .duplicates = "keep", .margin_label = NULL, .id = "sid"
      )
    }
    result <- dplyr::collect(expand(dtplyr::lazy_dt(data)))
    expect_identical(result, expand(data))
    expect_identical(result$sid, rep(1:3, each = 2L))
    expect_identical(result$payload, rep(1:2, 3L))
  }
})

test_that("dtplyr retains list dimensions, fixed keys, and payloads", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("blob")
  for (values in dtplyr_list_dimensions()) {
    for (size in 0:2) {
      data <- tibble::tibble(
        group = values[seq_len(size)], key = seq_len(size)
      )
      for (label in list(NULL, NA_character_)) {
        operations <- list(
          function(input) {
            expand_with_margins(
              input, .grouping = grouping_set(group),
              .margin_label = label, .id = "sid"
            )
          },
          function(input) {
            expand_with_margins(
              input, .by = group, .grouping = rollup(key),
              .margin_label = label, .id = "sid"
            )
          },
          function(input) {
            expand_with_margins(
              input, .grouping = rollup(key),
              .margin_label = label, .id = "sid"
            )
          }
        )
        for (operation in operations) {
          query <- operation(dtplyr::lazy_dt(data))
          expect_s3_class(query, "dtplyr_step")
          expect_identical(dplyr::collect(query), operation(data))
        }
      }
    }
  }
})

# data.table does not group by list columns. Construction and the generated
# missing-value expression are the backend boundary this summary can prove.
test_that("dtplyr Margin summaries stage typed-missing list dimensions", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("blob")
  for (values in dtplyr_list_dimensions()) {
    for (size in 0:2) {
      data <- tibble::tibble(
        group = values[seq_len(size)], payload = seq_len(size)
      )
      for (label in list(NULL, NA_character_)) {
        summarize <- function(input) {
          summarize_with_margins(
            input, total = sum(payload), .grouping = rollup(group),
            .margin_label = label, .id = "sid"
          )
        }
        query <- summarize(dtplyr::lazy_dt(data))
        expect_s3_class(query, "dtplyr_step")
        expect_match(
          paste(deparse(dplyr::show_query(query)), collapse = "\n"),
          "vctrs::vec_init", fixed = TRUE
        )
      }
    }
  }
})

test_that("dtplyr list labels defer source work and leave inputs unchanged", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("blob")
  for (values in dtplyr_list_dimensions()) {
    data <- tibble::tibble(
      .N = values, payload = 1:2, ..marginplyr_label_1 = c("x", "y")
    )
    source <- dtplyr::lazy_dt(data)
    before <- dplyr::collect(source)
    seen <- integer()
    probe <- function(x) {
      seen <<- c(seen, length(x))
      x
    }
    filtered <- dplyr::filter(source, probe(payload) > 0L)
    query <- expand_with_margins(
      filtered, .grouping = rollup(.N), .margin_label = NULL, .id = "sid"
    )
    summary <- summarize_with_margins(
      filtered, total = sum(payload), .grouping = rollup(.N),
      .margin_label = NULL, .id = "sid"
    )
    expect_s3_class(query, "dtplyr_step")
    expect_s3_class(summary, "dtplyr_step")
    expect_identical(seen, integer())
    result <- dplyr::collect(query)
    expect_true(2L %in% seen)
    expect_identical(result, expand_with_margins(
      data, .grouping = rollup(.N), .margin_label = NULL, .id = "sid"
    ))
    expect_identical(dplyr::collect(query), result)
    expect_identical(dplyr::collect(source), before)
    expect_identical(data, before)
  }
})
