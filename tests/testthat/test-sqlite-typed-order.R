test_that("SQLite Margin order keeps an all-missing dimension typed", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_summary", temporary = TRUE
  )

  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )
  expect_identical(as.character(dplyr::tbl_vars(query)), c("g", "z"))
  result <- dplyr::collect(query)
  expect_identical(result$g, c(NA_character_, NA_character_))
  expect_equal(result$z, c(1, 1))
})

test_that("SQLite materializes a typed Margin order with public columns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_materialize", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )

  computed <- dplyr::compute(query, name = "typed_margin_result")
  expect_identical(DBI::dbListFields(con, "typed_margin_result"), c("g", "z"))
  result <- dplyr::collect(computed)
  expect_identical(result$g, c(NA_character_, NA_character_))
  expect_equal(result$z, c(1, 1))
})

test_that("SQLite expansion keeps a typed dimension and Margin order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_integer_, v = 1),
    "typed_order_expansion", temporary = TRUE
  )
  query <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL,
    .id = "set", .sort = "first"
  )

  for (result in list(
    dplyr::collect(query),
    dplyr::collect(dplyr::compute(query))
  )) {
    expect_identical(names(result), c("g", "set", "v"))
    expect_identical(result$g, c(NA_integer_, NA_integer_))
    expect_identical(result$set, c(2L, 1L))
    expect_equal(result$v, c(1, 1))
  }
})

test_that("SQLite contextual shares keep typed dimensions under Margin order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_real_, v = 1),
    "typed_order_shares", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), p = share_of_parent(z), t = share_of_total(z),
    .grouping = rollup(g), .margin_label = NA_character_,
    .id = "set", .sort = "last", .check_share_source = FALSE
  )

  for (result in list(
    dplyr::collect(query),
    dplyr::collect(dplyr::compute(query))
  )) {
    expect_identical(result$g, c(NA_real_, NA_real_))
    expect_identical(result$set, c(1L, 2L))
    expect_equal(result$z, c(1, 1))
    expect_equal(result$p, c(1, 1))
    expect_equal(result$t, c(1, 1))
  }
})

test_that("SQLite typed Margin order covers either contextual share alone", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_integer_, v = 1),
    "typed_order_single_share", temporary = TRUE
  )

  for (share in c("parent", "total")) {
    value <- if (identical(share, "parent")) {
      rlang::expr(share_of_parent(z))
    } else {
      rlang::expr(share_of_total(z))
    }
    query <- summarize_with_margins(
      source, z = sum(v), share = !!value,
      .grouping = rollup(g), .margin_label = NULL,
      .id = "set", .sort = "first", .check_share_source = FALSE
    )
    for (result in list(
      dplyr::collect(query),
      dplyr::collect(dplyr::compute(query))
    )) {
      expect_identical(result$g, c(NA_integer_, NA_integer_))
      expect_identical(result$set, c(2L, 1L))
      expect_equal(result$share, c(1, 1))
    }
  }
})

test_that("SQLite typed order hides internal columns from the R result", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_public", temporary = TRUE
  )
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )

  sql <- as.character(dbplyr::sql_render(query))
  expect_match(sql, "..marginplyr_order_", fixed = TRUE)
  expect_identical(last_sent_queries()$purpose, "result")
  expect_identical(last_sent_queries()$sql, sql)
  expect_identical(colnames(query), c("g", "z"))
  expect_false(any(grepl("..marginplyr_order_", capture.output(print(query)),
                         fixed = TRUE)))
  selected <- dplyr::select(query, z)
  expect_identical(colnames(selected), "z")
  expect_identical(names(dplyr::collect(selected)), "z")
  expect_false(grepl(
    "..marginplyr_order_", as.character(dbplyr::sql_render(selected)),
    fixed = TRUE
  ))
  expect_identical(
    names(dplyr::collect(dplyr::compute(selected))), "z"
  )
  expect_identical(names(dplyr::collect(query, n = 1L,
                                        warn_incomplete = FALSE)), c("g", "z"))
})

test_that("SQLite typed Margin labels retain all scalar source types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  values <- list(
    character = NA_character_, integer = NA_integer_, double = NA_real_
  )

  for (type in names(values)) {
    source <- dplyr::copy_to(
      con, data.frame(g = values[[type]], v = 1),
      paste0("typed_order_", type), temporary = TRUE
    )
    for (label in list(NULL, NA_character_)) {
      for (sort in c("first", "last")) {
        query <- summarize_with_margins(
          source, z = sum(v), .grouping = rollup(g),
          .margin_label = label, .id = "set", .sort = sort
        )
        info <- paste(type, if (is.null(label)) "NULL" else "NA", sort)
        for (result in list(
          dplyr::collect(query),
          dplyr::collect(dplyr::compute(query))
        )) {
          expect_identical(typeof(result$g), type, info = info)
          expect_true(all(is.na(result$g)), info = info)
          expect_identical(
            result$set,
            if (identical(sort, "first")) c(2L, 1L) else c(1L, 2L),
            info = info
          )
          expect_equal(result$z, c(1, 1), info = info)
        }
      }
    }
  }
})

test_that("SQLite refuses compute when every rowid name is shadowed", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_rowid_names", temporary = TRUE
  )
  query <- summarize_with_margins(
    source,
    rowid = sum(v), oid = sum(v) + 1, `_ROWID_` = sum(v) + 2,
    .grouping = rollup(g), .margin_label = NULL, .sort = "last"
  )
  before <- DBI::dbListTables(con)

  expect_error(
    dplyr::compute(query, name = "rowid_shadowed_result"),
    "rowid aliases", class = "marginplyr_error"
  )
  expect_identical(DBI::dbListTables(con), before)
  expect_identical(typeof(dplyr::collect(query)$g), "character")
})

test_that("SQLite typed Margin order keeps fixed partitions and missingness", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(fixed = c(2L, NA_integer_, 1L),
               g = NA_character_, v = c(1, 2, 3)),
    "typed_order_fixed", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), .by = fixed, .grouping = rollup(g),
    .margin_label = NULL, .id = "set", .sort = "last"
  )

  for (result in list(
    dplyr::collect(query),
    dplyr::collect(dplyr::compute(query))
  )) {
    expect_identical(result$fixed, c(1L, 1L, 2L, 2L, NA_integer_, NA_integer_))
    expect_identical(result$g, rep(NA_character_, 6L))
    expect_identical(result$set, rep(c(1L, 2L), 3L))
    expect_equal(result$z, c(3, 3, 1, 1, 2, 2))
  }
})

test_that("SQLite typed compute honors a persistent name and indexes", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_persistent", temporary = TRUE
  )
  DBI::dbWriteTable(con, "typed_margin_output", data.frame(old = 1))
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )

  computed <- dplyr::compute(
    query, name = "typed_margin_output", temporary = FALSE,
    overwrite = TRUE, indexes = list("g")
  )
  expect_identical(DBI::dbListFields(con, "typed_margin_output"), c("g", "z"))
  declared <- DBI::dbGetQuery(con, "PRAGMA table_info('typed_margin_output')")
  expect_identical(declared$type[[1L]], "TEXT")
  expect_true(nrow(DBI::dbGetQuery(
    con, "SELECT name FROM sqlite_master WHERE name = 'typed_margin_output'"
  )) == 1L)
  expect_true(nrow(DBI::dbGetQuery(
    con, "PRAGMA index_list('typed_margin_output')"
  )) >= 1L)
  expect_false(any(grepl("^marginplyr_order_", DBI::dbListTables(con))))
  expect_identical(dplyr::collect(computed)$g, c(NA_character_, NA_character_))
})

test_that("SQLite typed compute uses an available rowid alias", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_partial_rowid", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, rowid = sum(v), oid = sum(v) + 1,
    .grouping = rollup(g), .margin_label = NULL,
    .id = "set", .sort = "first"
  )

  computed <- dplyr::compute(
    query, name = "typed_order_partial_rowid_result", analyze = FALSE
  )
  expect_identical(dplyr::collect(computed)$set, c(2L, 1L))
  expect_identical(DBI::dbListFields(con, "typed_order_partial_rowid_result"),
                   c("g", "set", "rowid", "oid"))
})

test_that("SQLite internal names avoid case-insensitive collisions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(g = NA_character_, v = 1,
               `..MARGINPLYR_ORDER_1` = 7, check.names = FALSE),
    "typed_order_name_collision", temporary = TRUE
  )
  for (sort in c("first", "last")) {
    query <- expand_with_margins(
      source, .grouping = rollup(g), .margin_label = NULL,
      .id = "set", .sort = sort
    )
    for (result in list(
      dplyr::collect(query),
      dplyr::collect(dplyr::compute(query))
    )) {
      expect_identical(result$g, c(NA_character_, NA_character_))
      expect_equal(result$`..MARGINPLYR_ORDER_1`, c(7, 7))
      expect_identical(
        result$set,
        if (identical(sort, "first")) c(2L, 1L) else c(1L, 2L)
      )
    }
  }
})

test_that("SQLite typed compute removes temporary work after insertion fails", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_failed_compute", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )
  before <- DBI::dbListTables(con)

  expect_error(dplyr::compute(
    query, name = "failed_typed_margin_result",
    unique_indexes = list("z")
  ))
  expect_identical(DBI::dbListTables(con), before)
})
