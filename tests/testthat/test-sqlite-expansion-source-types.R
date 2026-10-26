test_that("SQLite sorted one-set expansion keeps unchanged source types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a"), fixed = rep(NA_real_, 2L),
                    payload = rep(NA_integer_, 2L)),
    "expansion_source_types", temporary = TRUE
  )
  query <- expand_with_margins(
    source, .by = fixed, .grouping = grouping_set(g), .sort = "last"
  )
  result <- dplyr::collect(query)
  expect_identical(names(result), c("fixed", "g", "payload"))
  expect_identical(result$fixed, rep(NA_real_, 2L))
  expect_identical(result$g, c("a", "b"))
  expect_identical(result$payload, rep(NA_integer_, 2L))
})

test_that("SQLite expansion keeps source types at direct boundaries", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a", "a"), fixed = rep(NA_real_, 3L),
                    payload = rep(NA_integer_, 3L)),
    "expansion_source_matrix", temporary = TRUE
  )
  plans <- list(one = grouping_set(g), rollup = rollup(g))
  case <- 0L
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) dplyr::filter(source, FALSE) else source
    for (kind in names(plans)) {
      multiple <- identical(kind, "rollup")
      for (label in list("Total", NULL, NA_character_)) {
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid")) {
            info <- paste(empty, multiple, label, sort, id, collapse = " ")
            query <- expand_with_margins(
              input, .by = fixed, .grouping = plans[[kind]],
              .margin_label = label, .sort = sort, .id = id
            )
            expected_names <- c("fixed", "g", id, "payload")
            expect_identical(as.character(dplyr::tbl_vars(query)),
                             expected_names, info = info)
            expected_g <- if (identical(sort, "none")) {
              c("b", "a", "a")
            } else {
              c("a", "a", "b")
            }
            expected_id <- rep(1L, 3L)
            if (multiple) {
              margin <- rep(if (is.null(label)) NA_character_ else label, 3L)
              if (identical(sort, "first")) {
                expected_g <- c(margin, expected_g)
                expected_id <- c(rep(2L, 3L), expected_id)
              } else {
                expected_g <- c(expected_g, margin)
                expected_id <- c(expected_id, rep(2L, 3L))
              }
            }
            if (empty) {
              expected_g <- character()
              expected_id <- integer()
            }
            case <- case + 1L
            name <- paste0("expansion_source_result_", case)
            computed <- dplyr::compute(query, name = name)
            expect_identical(as.character(dplyr::tbl_vars(computed)),
                             expected_names, info = info)
            # The physical schema is the public materialization boundary;
            # tbl_vars() alone cannot reveal hidden columns in its table.
            expect_identical(DBI::dbListFields(con, name), expected_names,
                             info = info)
            for (endpoint in list(query, computed)) {
              for (n in c(Inf, 0L, 1L)) {
                result <- dplyr::collect(endpoint, n = n)
                rows <- seq_len(min(n, length(expected_g)))
                expect_identical(names(result), expected_names, info = info)
                if (identical(sort, "none")) {
                  if (is.infinite(n)) {
                    expect_identical(sort(result$g, na.last = TRUE),
                                     sort(expected_g, na.last = TRUE),
                                     info = info)
                  } else {
                    expect_true(all(result$g %in% expected_g), info = info)
                  }
                } else {
                  expect_identical(result$g, expected_g[rows], info = info)
                }
                expect_identical(result$fixed, rep(NA_real_, length(rows)),
                                 info = info)
                expect_identical(result$payload,
                                 rep(NA_integer_, length(rows)), info = info)
                if (!is.null(id)) {
                  if (identical(sort, "none")) {
                    if (is.infinite(n)) {
                      expect_identical(sort(result$sid), sort(expected_id),
                                       info = info)
                      set <- 2L - as.integer(result$g %in% c("a", "b"))
                      expect_identical(result$sid, set, info = info)
                    } else {
                      expect_true(all(result$sid %in% expected_id), info = info)
                    }
                  } else {
                    expect_identical(result$sid, expected_id[rows], info = info)
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

test_that("SQLite source-type preservation keeps construction lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", fixed = NA_real_, payload = NA_integer_),
    "expansion_lazy_source", temporary = TRUE
  )
  build <- function(sort) {
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) stop("unexpected source read"),
      .package = "DBI"
    )
    expand_with_margins(
      source, .by = fixed, .grouping = grouping_set(g), .sort = sort
    )
  }
  queries <- lapply(c("none", "first", "last"), build)
  DBI::dbExecute(
    con, "INSERT INTO expansion_lazy_source VALUES ('b', NULL, NULL)"
  )
  for (query in queries) {
    result <- dplyr::collect(query)
    expect_identical(result$g, c("a", "b"))
    expect_identical(result$fixed, rep(NA_real_, 2L))
    expect_identical(result$payload, rep(NA_integer_, 2L))
  }
})
