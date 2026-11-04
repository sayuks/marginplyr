# Display methods change presentation only; vctrs keeps ordinary code equality.
new_display_factor <- function(codes = c(1L, 2L), na_level = FALSE,
                               ordered = FALSE, mode = "decorate") {
  structure(codes, levels = c("a", "b", "unused",
                              if (na_level) NA_character_),
            class = c(paste0("display_factor_", mode),
                      if (ordered) "ordered", "factor"))
}

for (mode in c("decorate", "swap", "collapse", "margin")) {
  display_class <- paste0("display_factor_", mode)
  registerS3method("as.character", display_class, local({
    mapping <- switch(mode, decorate = c(a = "[a]", b = "[b]"),
                      swap = c(a = "b", b = "a"),
                      collapse = c(a = "a", b = "a"),
                      margin = c(a = "Total", b = "Total"))
    mapping <- c(mapping, unused = "unused")
    function(x, ...) {
      value <- as.character.factor(x)
      present <- !is.na(value)
      value[present] <- unname(mapping[value[present]])
      value
    }
  }), envir = asNamespace("base"))
  registerS3method("vec_ptype2", paste(display_class, display_class, sep = "."),
                   function(x, y, ...) vctrs::vec_slice(x, integer()),
                   envir = asNamespace("vctrs"))
  registerS3method("vec_cast", paste(display_class, display_class, sep = "."),
                   function(x, to, ...) x, envir = asNamespace("vctrs"))
}

# All four public seams cross the shared factor encoding/restoration path.
factor_value_verbs <- list(
  nest = nest_with_margins,
  nest_by = nest_by_with_margins,
  expand = expand_with_margins,
  summary = function(.data, ...) {
    summarize_with_margins(.data, rows = list(.data$row_id), ...)
  }
)

check_factor_display_details <- function(lazy = FALSE) {
  for (mode in c("decorate", "swap", "collapse", "margin")) {
    input <- tibble::tibble(g = new_display_factor(mode = mode), row_id = 1:2)
    before <- serialize(input, NULL)
    expect_identical(as.character(input$g[integer()]), character())
    expect_identical(as.character(new_display_factor(NA_integer_, mode = mode)),
                     NA_character_)
    source <- if (lazy) dtplyr::lazy_dt(input) else input
    source_before <- serialize(dplyr::collect(source), NULL)
    expect_identical(as.integer(dplyr::collect(source)$g), 1:2)
    native <- dplyr::collect(dplyr::summarise(
      source, rows = list(.data$row_id), .by = "g"
    ))
    expect_identical(as.integer(native$g), 1:2)
    expect_identical(native$rows, list(1L, 2L))
    native_nest <- dplyr::nest_by(input, !!!rlang::syms("g"), .keep = TRUE)
    expect_identical(as.integer(native_nest$g), 1:2)
    expect_identical(lapply(native_nest$data, function(cell) cell$row_id),
                     list(1L, 2L))
    expect_identical(as.integer(dplyr::collect(
      dplyr::union_all(source, source)
    )$g), rep(1:2, 2L))
    for (verb in factor_value_verbs) {
      for (plan in list(grouping_set("g"), rollup("g"))) {
        result <- dplyr::collect(verb(source, .grouping = plan, .id = "set",
                                      .check_margin_label = TRUE))
        detail <- result[result$set == 1L, ]
        expect_identical(as.integer(detail$g), 1:2, info = mode)
        expect_identical(as.character.factor(detail$g), c("a", "b"))
        expect_identical(levels(result$g), c("a", "b", "unused", "Total"))
        if ("data" %in% names(result)) {
          expect_identical(lapply(detail$data, function(cell) cell$row_id),
                           list(1L, 2L))
        }
      }
    }
    expect_identical(serialize(input, NULL), before)
    expect_identical(serialize(dplyr::collect(source), NULL), source_before)
  }
}

test_that("local Margin verbs preserve stored factor values", {
  check_factor_display_details()
})

test_that("dtplyr Margin verbs preserve stored factor values", {
  skip_if_suggest_absent("dtplyr")
  check_factor_display_details(lazy = TRUE)
})

# Check source codes as well as level values: NA level and missing code both
# display as NA, so a character-only assertion cannot establish this contract.
expect_source_factor <- function(actual, source, offset = 0L) {
  expect_identical(as.integer(actual), as.integer(source) + offset)
  expect_identical(as.character.factor(actual), as.character.factor(source))
  expect_identical(is.na(actual), is.na(source))
  expect_identical(is.ordered(actual), is.ordered(source))
}

check_factor_source_roles <- function(lazy = FALSE) {
  for (ordered in c(FALSE, TRUE)) {
    for (na_level in c(FALSE, TRUE)) {
      codes <- c(1L, 2L, 1L, if (na_level) 4L, NA_integer_)
      g <- new_display_factor(codes, na_level = na_level, ordered = ordered)
      for (empty in c(FALSE, TRUE)) {
        input <- tibble::tibble(
          g = g, h = "x", payload = g, row_id = seq_along(g)
        )
        if (empty) input <- input[integer(), ]
        before <- serialize(input, NULL)
        for (label in if (na_level) {
          list("Total", NULL)
        } else {
          list("Total", NULL, NA_character_)
        }) {
          for (position in c("first", "last")) {
            # Local frame and tibble inputs both reach the factor contract.
            source <- if (lazy) {
              dtplyr::lazy_dt(input)
            } else {
              if (position == "first") as.data.frame(input) else input
            }
            source_before <- serialize(dplyr::collect(source), NULL)
            for (keep in c(FALSE, TRUE)) {
              for (role in c("dimension", "fixed", "payload")) {
                for (verb in list(nest_with_margins, nest_by_with_margins)) {
                  plan <- if (role == "dimension") rollup("g") else rollup("h")
                  result <- if (role == "fixed") {
                    verb(source, .by = g, .grouping = plan, .id = "set",
                         .keep = keep, .margin_label = label,
                         .margin_label_position = position, .sort = position)
                  } else {
                    verb(source, .grouping = plan, .id = "set", .keep = keep,
                         .margin_label = label,
                         .margin_label_position = position, .sort = position)
                  }
                  result <- dplyr::collect(result)
                  if (empty) expect_identical(nrow(result), 0L)
                  labelled <- role == "dimension" && !is.null(label) &&
                    !is.na(label)
                  if (role != "payload") {
                    expected_levels <- levels(g)
                    if (labelled) {
                      expected_levels <- if (position == "first") {
                        c("Total", expected_levels)
                      } else {
                        c(expected_levels, "Total")
                      }
                    }
                    expect_identical(levels(result$g), expected_levels)
                    expect_identical(is.ordered(result$g), ordered)
                  }
                  detail_cells <- result$data[result$set == 1L]
                  detail_ids <- lapply(detail_cells, function(cell) cell$row_id)
                  if (!empty && role != "payload") {
                    expect_setequal(detail_ids, if (na_level) {
                      list(c(1L, 3L), 2L, 4L, 5L)
                    } else {
                      list(c(1L, 3L), 2L, 4L)
                    })
                  }
                  if (!empty && role == "payload") {
                    expect_identical(detail_ids, list(seq_along(g)))
                  }
                  for (i in seq_len(nrow(result))) {
                    cell <- result$data[[i]]
                    ids <- cell$row_id
                    expect_source_factor(cell$payload, input$payload[ids])
                    has_key <- role == "payload" || keep
                    expect_identical("g" %in% names(cell), has_key)
                    if (has_key) expect_source_factor(cell$g, input$g[ids])
                    omitted <- role == "dimension" && result$set[[i]] == 2L
                    if (role != "payload" && !omitted) {
                      offset <- as.integer(labelled && position == "first")
                      expect_source_factor(result$g[i], input$g[ids[[1L]]],
                                           offset = offset)
                    }
                    if (omitted) {
                      expect_identical(as.integer(result$g[i]), if (labelled) {
                        if (position == "first") 1L else nlevels(g) + 1L
                      } else {
                        NA_integer_
                      })
                    }
                  }
                  if (!empty && role != "fixed") {
                    total_row <- which(result$set == 2L)
                    expect_identical(result$data[[total_row]]$row_id,
                                     seq_along(g))
                    expect_identical(total_row, if (position == "first") {
                      1L
                    } else {
                      nrow(result)
                    })
                  }
                }
              }
            }
            expect_identical(serialize(dplyr::collect(source), NULL),
                             source_before)
          }
        }
        expect_identical(serialize(input, NULL), before)
      }
    }
  }
}

test_that("local nesting preserves factor codes in every source role", {
  check_factor_source_roles()
})

test_that("dtplyr nesting preserves factor codes in every source role", {
  skip_if_suggest_absent("dtplyr")
  check_factor_source_roles(lazy = TRUE)
})

check_factor_summary_expansion <- function(lazy = FALSE) {
  for (ordered in c(FALSE, TRUE)) {
    for (na_level in c(FALSE, TRUE)) {
      codes <- c(1L, 2L, if (na_level) 4L, NA_integer_)
      for (empty in c(FALSE, TRUE)) {
        g <- new_display_factor(codes, ordered = ordered, na_level = na_level)
        input <- tibble::tibble(g = g, row_id = seq_along(g))
        if (empty) input <- input[integer(), ]
        before <- serialize(input, NULL)
        source <- if (lazy) dtplyr::lazy_dt(input) else as.data.frame(input)
        for (label in list("Total", NULL)) {
          for (verb in factor_value_verbs[c("summary", "expand")]) {
            result <- dplyr::collect(verb(
              source, .grouping = rollup(g), .id = "set", .margin_label = label,
              .margin_label_position = "first", .sort = "last"
            ))
            detail <- result[result$set == 1L, ]
            expect_source_factor(detail$g, input$g,
                                 offset = as.integer(!is.null(label)))
            expect_identical(levels(result$g), if (is.null(label)) {
              levels(g)
            } else {
              c("Total", levels(g))
            })
            if (!empty) {
              if ("rows" %in% names(result)) {
                expect_identical(detail$rows, as.list(seq_along(g)))
              } else {
                expect_identical(detail$row_id, seq_along(g))
              }
            }
          }
        }
        expect_identical(serialize(input, NULL), before)
      }
    }
  }
}

test_that("summary and expansion preserve local source codes and NA levels", {
  check_factor_summary_expansion()
})

test_that("summary and expansion preserve dtplyr source codes and NA levels", {
  skip_if_suggest_absent("dtplyr")
  check_factor_summary_expansion(lazy = TRUE)
})

check_factor_collisions <- function(lazy = FALSE) {
  for (subclass in c(FALSE, TRUE)) {
    g <- new_display_factor()
    if (!subclass) class(g) <- "factor"
    input <- tibble::tibble(g = g, row_id = 1:2)
    source <- if (lazy) dtplyr::lazy_dt(input) else input
    for (verb in factor_value_verbs) {
      for (label in list(NULL, NA_character_)) {
        result <- dplyr::collect(verb(source, .grouping = grouping_set(g),
                                      .margin_label = label))
        expect_source_factor(result$g, g)
      }
      expect_error(
        verb(source, .grouping = rollup(g), .margin_label = "unused"),
        class = "marginplyr_error"
      )
      na_input <- tibble::tibble(
        g = new_display_factor(1L, na_level = TRUE), row_id = 1L
      )
      na_source <- if (lazy) dtplyr::lazy_dt(na_input) else na_input
      expect_error(verb(na_source, .grouping = rollup(g),
                        .margin_label = NA_character_),
                   class = "marginplyr_error")
    }
  }
}

test_that("local factor display keeps typed-missing and collision controls", {
  check_factor_collisions()
})

test_that("dtplyr factor display keeps typed-missing and collision controls", {
  skip_if_suggest_absent("dtplyr")
  check_factor_collisions(lazy = TRUE)
})
