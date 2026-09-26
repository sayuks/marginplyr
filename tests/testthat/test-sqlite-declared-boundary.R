test_that("empty SQLite expansion keeps its integer Grouping set identifier", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(x = 1L), "declared_expansion_source", temporary = TRUE
  )
  empty <- dplyr::filter(source, x > 1L)
  query <- expand_with_margins(empty, .id = "set")

  for (result in list(dplyr::collect(query), dplyr::collect(query, n = 1L),
                      dplyr::collect(dplyr::compute(query)))) {
    expect_identical(names(result), c("set", "x"))
    expect_identical(result$x, integer())
    expect_identical(result$set, integer())
    expect_identical(nrow(result), 0L)
  }
  expect_identical(dplyr::collect(expand_with_margins(
    data.frame(x = integer()), .id = "set"
  ))$set, integer())
  expect_identical(DBI::dbGetQuery(
    con, "SELECT x FROM declared_expansion_source"
  )$x, 1L)
})

test_that("SQLite expansion declarations survive plans, labels, and order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(g = c("a", "b"), x = 1:2)
  source <- dplyr::copy_to(
    con, input, "declared_expansion_matrix", temporary = TRUE
  )
  empty <- dplyr::filter(source, x > 2L)
  case <- 0L
  for (label in list("Total", NULL, NA_character_)) {
    for (sort in c("none", "first", "last")) {
      for (plan in c("one", "rollup")) {
        case <- case + 1L
        query <- if (identical(plan, "one")) {
          expand_with_margins(
            empty, .grouping = grouping_set(g), .id = "set",
            .margin_label = label, .sort = sort
          )
        } else {
          expand_with_margins(
            empty, .grouping = rollup(g), .id = "set",
            .margin_label = label, .sort = sort
          )
        }
        computed <- dplyr::compute(
          query, name = paste0("declared_expansion_case_", case)
        )
        for (result in list(dplyr::collect(query),
                            dplyr::collect(computed))) {
          expect_identical(names(result), c("g", "set", "x"))
          expect_identical(result$g, character())
          expect_identical(result$set, integer())
          expect_identical(result$x, integer())
          expect_identical(nrow(result), 0L)
        }
        limited <- dplyr::collect(query, n = 1L)
        expect_identical(names(limited), c("g", "set", "x"))
        expect_identical(limited$set, integer())
        expect_identical(nrow(limited), 0L)
        expect_identical(DBI::dbListFields(
          con, paste0("declared_expansion_case_", case)
        ), c("g", "set", "x"))
      }
    }
  }
  populated <- dplyr::collect(expand_with_margins(source, .id = "set"))
  expect_identical(populated$set, c(1L, 1L))
  expect_identical(DBI::dbGetQuery(
    con, "SELECT g, x FROM declared_expansion_matrix ORDER BY x"
  ), input)
})

test_that("finite SQLite collection keeps all-missing shares double", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(NA_real_, 1)),
    "declared_share_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), share = share_of_total(z),
    .grouping = rollup(g), .sort = "last", .check_share_source = FALSE
  )
  prefix <- dplyr::collect(query, n = 1L)
  expect_identical(names(prefix), c("g", "z", "share"))
  expect_identical(prefix$g, "a")
  expect_identical(prefix$share, NA_real_)
  expect_identical(nrow(prefix), 1L)
  expect_identical(dplyr::collect(query, n = 0L)$share, double())
  expect_identical(dplyr::collect(query)$share,
                   c(NA_real_, 1, 1))
  expect_identical(dplyr::collect(dplyr::compute(query), n = 1L)$share,
                   NA_real_)
})

test_that("SQLite share prefixes retain declared types across share forms", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- data.frame(p = c("A", "A", "B"), g = c("x", "y", "z"),
                      v = c(2, -2, 3))
  source <- dplyr::copy_to(
    con, input, "declared_share_matrix", temporary = TRUE
  )
  for (sort in c("last", "first", "none")) {
    query <- summarize_with_margins(
      source, z = sum(v), parent = share_of_parent(z),
      total = share_of_total(z),
      dplyr::across(z, share_of_total, .names = "{.col}_across"),
      .by = p, .grouping = rollup(g), .sort = sort,
      .check_share_source = FALSE
    )
    full <- dplyr::collect(query)
    expect_identical(names(full),
                     c("p", "g", "z", "parent", "total", "z_across"))
    expect_identical(nrow(full), 5L)
    for (name in c("parent", "total", "z_across")) {
      expect_identical(typeof(full[[name]]), "double")
      expect_identical(dplyr::collect(query, n = 0L)[[name]], double())
    }
    if (identical(sort, "last")) {
      expect_identical(full$p, c("A", "A", "A", "B", "B"))
      expect_identical(full$g, c("x", "y", "Total", "z", "Total"))
      expect_identical(full$parent, c(NA_real_, NA_real_, 1, 1, 1))
      expect_identical(full$total, c(NA_real_, NA_real_, 1, 1, 1))
      expect_identical(full$z_across, full$total)
      for (n in 1:2) {
        prefix <- dplyr::collect(query, n = n)
        expect_identical(nrow(prefix), as.integer(n))
        for (name in c("parent", "total", "z_across")) {
          expect_identical(prefix[[name]], rep(NA_real_, n))
        }
      }
      mixed <- dplyr::collect(query, n = 3L)
      expect_identical(mixed$parent, c(NA_real_, NA_real_, 1))
      materialized <- dplyr::compute(
        query, name = "declared_share_materialized"
      )
      expect_identical(dplyr::collect(materialized, n = 1L)$parent,
                       NA_real_)
      expect_identical(DBI::dbListFields(
        con, "declared_share_materialized"
      ), names(full))
      selected <- dplyr::collect(dplyr::select(materialized, p, g, parent))
      expect_identical(names(selected), c("p", "g", "parent"))
      expect_identical(selected$parent, full$parent)
    }
  }
  expect_identical(DBI::dbGetQuery(
    con, "SELECT p, g, v FROM declared_share_matrix ORDER BY p, g"
  ), input)
})

test_that("missing mean shares retain double types without driver inference", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(p = c("A", "B"), g = c("a", "b"),
                    v = c(NA_real_, 1)),
    "declared_mean_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = mean(v), parent = share_of_parent(z),
    total = share_of_total(z), .by = p, .grouping = rollup(g),
    .sort = "last", .check_share_source = FALSE
  )
  prefix <- dplyr::collect(query, n = 1L)
  expect_identical(prefix$p, "A")
  expect_identical(prefix$g, "a")
  expect_identical(prefix$parent, NA_real_)
  expect_identical(prefix$total, NA_real_)
  expect_identical(dplyr::collect(dplyr::compute(query), n = 1L)$total,
                   NA_real_)
  expect_identical(DBI::dbGetQuery(con, "SELECT NULL AS share")$share,
                   NA)
})
