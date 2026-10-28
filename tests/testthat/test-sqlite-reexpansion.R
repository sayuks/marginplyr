test_that("SQLite re-expansion carries only a sorted result's public columns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a"), value = 1:2), "reexpansion_source"
  )
  first <- expand_with_margins(
    source, .grouping = grouping_set("g"), .margin_label = NULL, .sort = "last"
  )
  query <- expand_with_margins(first)
  actual <- dplyr::arrange(dplyr::collect(query), .data$value)
  expect_identical(as.data.frame(actual),
                   data.frame(g = c("b", "a"), value = 1:2))
})

test_that("SQLite re-expansion retains copies, types, and public boundaries", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("b", "a"), value = 1:2,
                     missing_int = NA_integer_, missing_real = NA_real_)
  source <- dplyr::copy_to(con, data, "reexpansion_matrix")
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) dplyr::filter(source, FALSE) else source
    for (multiple in c(FALSE, TRUE)) {
      first_spec <- if (multiple) rollup("g") else grouping_set("g")
      for (first_sort in c("first", "last")) {
        first <- expand_with_margins(
          input, .grouping = first_spec, .margin_label = NULL,
          .sort = first_sort
        )
        local <- data
        if (multiple) {
          omitted <- data
          omitted$g <- NA_character_
          local <- rbind(local, omitted)
        }
        if (empty) local <- local[0L, ]
        for (spec in list(NULL, grouping_set("g"), rollup("g"))) {
          for (sort in c("none", "first", "last")) {
            for (id in list(NULL, "set")) {
              query <- testthat::with_mocked_bindings(
                expand_with_margins(
                  first, .grouping = spec, .margin_label = NULL,
                  .id = id, .sort = sort
                ),
                dbSendQuery = function(...) stop("Unexpected source read"),
                dbExecute = function(...) stop("Unexpected source command"),
                .package = "DBI"
              )
              expected <- expand_with_margins(
                local, .grouping = spec, .margin_label = NULL,
                .id = id, .sort = sort
              )
              expected <- as.data.frame(expected)
              canonical <- function(rows) {
                dplyr::arrange(as.data.frame(rows),
                               !!!rlang::syms(names(expected)))
              }
              saved <- dplyr::compute(query, name = "reexpansion_saved",
                                      overwrite = TRUE)
              expect_identical(DBI::dbListFields(con, "reexpansion_saved"),
                               names(expected))
              for (endpoint in list(query, saved)) {
                actual <- dplyr::collect(endpoint)
                expect_identical(canonical(actual), canonical(expected))
                # Only the new Margin order is observed; ties between copies
                # of an omitted key impose no payload order.
                if (sort != "none" && !is.null(spec)) {
                  expect_identical(actual$g, expected$g)
                  if (!is.null(id)) {
                    expect_identical(actual$set, expected$set)
                  }
                }
                for (n in c(0L, 1L)) {
                  limited <- dplyr::collect(endpoint, n = n)
                  expect_identical(names(limited), names(expected))
                  expect_identical(nrow(limited), min(n, nrow(expected)))
                  expect_identical(limited$missing_int,
                                   rep(NA_integer_, nrow(limited)))
                  expect_identical(limited$missing_real,
                                   rep(NA_real_, nrow(limited)))
                  if (!is.null(id)) expect_type(limited$set, "integer")
                  if (nrow(limited) > 0L) {
                    expect_identical(nrow(dplyr::anti_join(
                      limited, expected, by = names(expected), na_matches = "na"
                    )), 0L)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("SQLite re-expansion keeps special public names and current types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = c("b", "a"), value = 1:2)
  data[["TRUE"]] <- c(NA_integer_, NA_integer_)
  data[[".env"]] <- c(NA_real_, NA_real_)
  source <- dplyr::copy_to(con, data, "reexpansion_names")
  first <- expand_with_margins(
    source, .grouping = rollup("g"), .margin_label = NULL, .sort = "last"
  )
  query <- expand_with_margins(
    first, .grouping = rollup("g"), .margin_label = NULL, .id = "set"
  )
  expected <- expand_with_margins(
    expand_with_margins(data, .grouping = rollup("g"), .margin_label = NULL),
    .grouping = rollup("g"), .margin_label = NULL, .id = "set"
  )
  expect_selected_expansion(query, expected, "none")

  plain <- expand_with_margins(
    dplyr::select(source, dplyr::all_of(c("g", "value"))),
    .grouping = rollup("g"), .margin_label = NULL, .id = "prior"
  )
  changed <- dplyr::mutate(plain, g = nchar(.data$g))
  query <- expand_with_margins(
    changed, .grouping = grouping_set("g"), .margin_label = NULL, .sort = "last"
  )
  actual <- dplyr::collect(query)
  expect_identical(actual$g, c(1L, 1L, NA_integer_, NA_integer_))
})
