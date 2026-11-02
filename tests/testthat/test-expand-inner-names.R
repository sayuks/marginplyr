test_that("expansion applies ordinary inner-name repair to summary payloads", {
  input <- tibble::tibble(g = "a", payload = summary(1)[1L])
  control <- suppressMessages(dplyr::union_all(
    input, dplyr::mutate(input, g = "Total")
  ))

  result <- suppressMessages(expand_with_margins(input, .grouping = rollup(g)))

  expect_identical(names(result), c("g", "payload"))
  expect_identical(result$g, c("a", "Total"))
  expect_identical(as.numeric(result$payload), c(1, 1))
  expect_identical(result$payload, control$payload)
})

test_that("public suffix names remain exact while inner names are repaired", {
  input <- tibble::tibble(
    g = "a", payload = summary(1)[1L], payload...1 = summary(2)[1L]
  )
  control <- suppressMessages(dplyr::bind_rows(
    input, dplyr::mutate(input, g = "Total")
  ))

  result <- suppressMessages(expand_with_margins(input, .grouping = rollup(g)))

  expect_identical(names(result), names(input))
  expect_identical(result$g, c("a", "Total"))
  expect_identical(result$payload, control[[2L]])
  expect_identical(result$payload...1, control[[3L]])
})

# Ordinary branch mutation and binding supply the class-combination oracle;
# neither the fixture nor the assertions restore attributes (ADR 0016).
expect_inner_name_expansion <- function(convert, kinds = c("summary", "table"),
                                        packed_columns = c(FALSE, TRUE)) {
  values <- list(summary = summary(1:3), table = table(c("a", "b", "b")))
  plans <- list(
    rollup = c(TRUE, FALSE),
    omitted_first = c(FALSE, TRUE),
    duplicate = c(TRUE, FALSE, TRUE)
  )
  for (kind in kinds) {
    for (size in c(0L, 1L, 2L)) {
      for (packed in packed_columns) {
        key <- vctrs::vec_slice(values[[kind]], seq_len(size))
        if (packed) key <- tibble::tibble(inner = key)
        input <- data.frame(g = rep("a", size), row = seq_len(size))
        input <- convert(input)
        input[["key"]] <- key
        expect_identical(is.data.frame(input$key), packed)
        untouched <- serialize(input, NULL)
        for (role in c("payload", "fixed", "dimension")) {
          dimension <- if (role == "dimension") "key" else "g"
          fixed <- if (role == "fixed") "key" else character()
          labels <- if (role == "dimension") {
            list(NULL, NA_character_)
          } else {
            list("Total")
          }
          for (label in labels) {
            for (plan_name in names(plans)) {
              included <- plans[[plan_name]]
              sets <- lapply(included, function(keep) {
                if (keep) {
                  grouping_set(dplyr::all_of(dimension))
                } else {
                  grouping_set()
                }
              })
              spec <- grouping_sets(!!!sets)
              branches <- lapply(seq_along(included), function(i) {
                branch <- input
                if (!included[[i]]) {
                  replacement <- if (is.null(label) || is.na(label)) {
                    vctrs::vec_init(input[[dimension]], size)
                  } else {
                    rep(label, size)
                  }
                  branch <- dplyr::mutate(branch, !!dimension := !!replacement)
                }
                dplyr::mutate(branch, set = i)
              })
              # Sequential union_all has its own class-transition limits for
              # three branches; bind_rows combines the whole ordinary list.
              control <- suppressMessages(dplyr::bind_rows(branches))
              control <- dplyr::relocate(
                control, dplyr::all_of(c(fixed, dimension, "set"))
              )
              control <- dplyr::arrange(control, .data$set, .data$row)
              for (sort in c("none", "last", "first")) {
                info <- paste(kind, size, packed, role, plan_name, sort)
                result <- suppressMessages(expand_with_margins(
                  input, .by = dplyr::all_of(fixed), .grouping = spec,
                  .margin_label = label, .duplicates = "keep",
                  .id = "set", .sort = sort
                ))
                result <- dplyr::arrange(result, .data$set, .data$row)
                expect_identical(names(result), names(control), info = info)
                expect_identical(
                  nrow(result), size * length(included), info = info
                )
                for (column in names(control)) {
                  expect_identical(
                    result[[column]], control[[column]], info = info
                  )
                }
              }
            }
          }
        }
        expect_identical(serialize(input, NULL), untouched)
      }
    }
  }
}

test_that("data-frame expansion follows ordinary vector-internal name rules", {
  expect_inner_name_expansion(identity)
})

test_that("tibble expansion follows ordinary vector-internal name rules", {
  expect_inner_name_expansion(tibble::as_tibble)
})

test_that("data-table expansion follows ordinary vector-internal name rules", {
  skip_if_suggest_absent("data.table")
  # Ordinary data.table binding cannot combine these table or packed columns.
  expect_inner_name_expansion(
    data.table::as.data.table, kinds = "summary", packed_columns = FALSE
  )
})
