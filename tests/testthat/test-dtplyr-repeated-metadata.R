test_that("every repeated update is checked before unsafe expressions run", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    factor(x)
  }
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(g = seq_len(rows) + 10L, v = seq_len(rows))
    before <- data.table::copy(source)
    root <- dtplyr::lazy_dt(source)
    steps <- list(
      dplyr::mutate(root, g = v, g = factor(g)),
      dplyr::mutate(root, g = v, g = probe(g)),
      dplyr::mutate(root, g = v,
                    g = if (dplyr::n() > 0L) "text" else 0L)
    )
    for (step in steps) {
      expect_error(inspect_grouping(step, .by = where(is.numeric)),
                   "Can't safely determine column metadata",
                   class = "marginplyr_error")
      expect_error(inspect_grouping(
        step, .grouping = rollup(where(is.numeric))
      ), "Can't safely determine column metadata", class = "marginplyr_error")
      for (verb in list(expand_with_margins, nest_with_margins,
                        nest_by_with_margins)) {
        for (label in list(NULL, "Total")) {
          for (sort in c("none", "first", "last")) {
            expect_error(verb(step, .grouping = rollup(g), .id = "set",
                              .margin_label = label, .sort = sort),
                         "Can't safely determine column metadata",
                         class = "marginplyr_error")
          }
        }
      }
      expect_identical(seen, integer())
      expect_identical(source, before)
    }
  }
})

test_that("repeated safe updates retain the current column metadata", {
  skip_if_suggest_absent("dtplyr")
  normalize <- function(x, sort) {
    x <- dplyr::ungroup(x)
    if ("data" %in% names(x)) {
      x$data <- lapply(x$data, tibble::as_tibble)
    }
    # Nesting leaves result order unspecified when no Margin order is asked.
    if (sort == "none") {
      x <- dplyr::arrange(x, set, g)
    }
    x
  }
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      g = seq_len(rows) + 10L, v = seq_len(rows),
      f = factor(rep(c("b", "a"), length.out = rows),
                 levels = c("b", "a", "unused"))
    )
    before <- data.table::copy(source)
    root <- dtplyr::lazy_dt(source)
    steps <- list(
      dplyr::mutate(root, g = v, g = f),
      dplyr::mutate(root, g = f, g = g),
      dplyr::mutate(root, g = v, g = g / 2),
      dplyr::mutate(root, new = 1L, new = new + v),
      dplyr::mutate(root, new = "tag", alias = new),
      dplyr::mutate(root, new = 1L, alias = new / 2),
      dplyr::mutate(root, g = NULL, g = v)
    )
    for (step in steps) {
      collected <- dplyr::collect(step)
      saved_query <- utils::capture.output(dplyr::show_query(step))
      for (predicate in list(is.factor, is.numeric, is.integer, is.double,
                             is.character)) {
        expect_identical(
          inspect_grouping(step, .by = where(predicate)),
          inspect_grouping(collected, .by = where(predicate))
        )
        expect_identical(
          inspect_grouping(step, .grouping = grouping_set(where(predicate))),
          inspect_grouping(collected,
                           .grouping = grouping_set(where(predicate)))
        )
      }
      for (label in list(NULL, "Total")) {
        for (sort in c("none", "first", "last")) {
          expect_identical(dplyr::collect(expand_with_margins(
            step, .grouping = rollup(g), .margin_label = label,
            .id = "set", .sort = sort
          )), expand_with_margins(
            collected, .grouping = rollup(g), .margin_label = label,
            .id = "set", .sort = sort
          ))
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            actual <- verb(step, .grouping = rollup(g), .margin_label = label,
                           .id = "set", .sort = sort, .keep = TRUE)
            expected <- verb(collected, .grouping = rollup(g),
                             .margin_label = label, .id = "set",
                             .sort = sort, .keep = TRUE)
            expect_identical(normalize(dplyr::collect(actual), sort),
                             normalize(expected, sort))
          }
        }
      }
      expect_identical(dplyr::collect(step), collected)
      expect_identical(utils::capture.output(dplyr::show_query(step)),
                       saved_query)
      expect_identical(source, before)
    }
  }
})

test_that("later updates use the types and names established by earlier ones", {
  skip_if_suggest_absent("dtplyr")
  root <- dtplyr::lazy_dt(data.frame(g = 1:2, text = c("a", "b")))
  steps <- list(
    dplyr::mutate(root, g = text, g = g + 1L),
    dplyr::mutate(root, added = g, added = "text"),
    dplyr::mutate(root, added = 1L, added = "text")
  )
  for (step in steps) {
    expect_error(inspect_grouping(step, .by = where(is.numeric)),
                 "Can't safely determine column metadata",
                 class = "marginplyr_error")
  }
})

test_that("repeated factor copies retain declared label collisions", {
  skip_if_suggest_absent("dtplyr")
  root <- dtplyr::lazy_dt(data.frame(
    g = 1:2, v = 3:4, f = factor(c("a", "a"), levels = c("a", "Total"))
  ))
  step <- dplyr::mutate(root, g = v, g = f)
  for (verb in list(expand_with_margins, nest_with_margins,
                    nest_by_with_margins)) {
    expect_error(verb(step, .grouping = rollup(g)),
                 class = "marginplyr_error")
    expect_error(verb(dplyr::collect(step), .grouping = rollup(g)),
                 class = "marginplyr_error")
  }
})

test_that("NULL updates do not certify scalar writes to an existing factor", {
  skip_if_suggest_absent("dtplyr")
  root <- dtplyr::lazy_dt(data.frame(
    g = factor(c("a", "a"), levels = c("a", "unused")), v = 1:2
  ))
  step <- dplyr::mutate(root, g = NULL, g = "z")
  expect_error(inspect_grouping(step, .by = where(is.factor)),
               "Can't safely determine column metadata",
               class = "marginplyr_error")
})

test_that("scalar aliases cannot certify existing factor metadata", {
  skip_if_suggest_absent("dtplyr")
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      g = factor(rep("a", rows), levels = c("a", "unused")), v = seq_len(rows)
    )
    before <- data.table::copy(source)
    root <- dtplyr::lazy_dt(source)
    for (label in c("z", "Total")) {
      steps <- list(
        dplyr::mutate(root, added = !!label, g = added),
        dplyr::mutate(root, added = !!label, g = v, g = added),
        dplyr::mutate(root, added = 1L, alias = added, g = alias + 1L)
      )
      for (step in steps) {
        expect_error(inspect_grouping(step, .by = where(is.factor)),
                     "Can't safely determine column metadata",
                     class = "marginplyr_error")
        for (verb in list(expand_with_margins, nest_with_margins,
                          nest_by_with_margins)) {
          expect_error(verb(step, .grouping = rollup(g)),
                       "Can't safely determine column metadata",
                       class = "marginplyr_error")
        }
        expect_identical(source, before)
      }
    }
  }
})
