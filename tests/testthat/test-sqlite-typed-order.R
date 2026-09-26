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

test_that("SQLite direct Margin collection accepts finite prefixes", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = 1:2, v = c(2, 4)),
    "typed_order_prefix", temporary = TRUE
  )
  old_options <- options(warn = 2)
  on.exit(options(old_options), add = TRUE)

  for (sort in c("first", "last")) {
    query <- summarize_with_margins(
      source, z = sum(v), .grouping = rollup(g),
      .margin_label = NULL, .sort = sort
    )
    expected_g <- if (identical(sort, "first")) {
      c(NA_integer_, 1L, 2L)
    } else {
      c(1L, 2L, NA_integer_)
    }
    expected_z <- if (identical(sort, "first")) c(6, 2, 4) else c(2, 4, 6)
    for (n in 0:4) {
      result <- dplyr::collect(query, n = n)
      expect_identical(names(result), c("g", "z"))
      expected_rows <- seq_len(min(n, 3L))
      expect_identical(result$g, expected_g[expected_rows])
      if (n == 0L) {
        expect_length(result$z, 0L)
      } else {
        expect_equal(result$z, expected_z[expected_rows])
      }
    }
    expect_identical(dplyr::collect(query, n = 1.9)$g, expected_g[1L])
  }
})

test_that("SQLite direct Margin collection validates limits like dbplyr", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = 1:2, v = c(2, 4)),
    "typed_order_limits", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )

  for (n in list(-1L, "1", c(1L, 2L))) {
    expect_error(dplyr::collect(source, n = n), "non-negative integer")
    expect_error(dplyr::collect(query, n = n), "non-negative integer")
  }
  expect_identical(dplyr::collect(query)$g, c(1L, 2L, NA_integer_))
  expect_identical(
    dplyr::collect(query, n = Inf, warn_incomplete = FALSE)$g,
    c(1L, 2L, NA_integer_)
  )
  expect_identical(
    dplyr::collect(query, n = 1L, warn_incomplete = FALSE)$g, 1L
  )
})

test_that("SQLite ordinary collection paths accept finite prefixes", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 4)),
    "typed_order_controls", temporary = TRUE
  )
  unsorted <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "none"
  )
  labelled <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = "All", .sort = "last"
  )
  old_options <- options(warn = 2)
  on.exit(options(old_options), add = TRUE)

  for (query in list(source, unsorted, labelled)) {
    expect_identical(nrow(dplyr::collect(query, n = 0L)), 0L)
    expect_identical(nrow(dplyr::collect(query, n = 1L)), 1L)
  }
})

test_that("SQLite unsorted identifiers keep typed dimensions in summaries", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  values <- list(
    character = NA_character_, integer = NA_integer_, double = NA_real_
  )

  for (type in names(values)) {
    source <- dplyr::copy_to(
      con,
      data.frame(fixed = c("a", "b"), g = rep(values[[type]], 2L),
                 v = c(1, 2)),
      paste0("unsorted_summary_", type), temporary = TRUE
    )
    for (label in list(NULL, NA_character_)) {
      for (fixed in c(FALSE, TRUE)) {
        query <- if (fixed) {
          summarize_with_margins(
            source, z = sum(v), .by = fixed, .grouping = rollup(g),
            .margin_label = label, .id = "sid", .sort = "none"
          )
        } else {
          summarize_with_margins(
            source, z = sum(v), .grouping = rollup(g),
            .margin_label = label, .id = "sid", .sort = "none"
          )
        }
        expected_names <- if (fixed) {
          c("fixed", "g", "sid", "z")
        } else {
          c("g", "sid", "z")
        }
        info <- paste(type, if (is.null(label)) "NULL" else "NA", fixed)
        expect_identical(as.character(dplyr::tbl_vars(query)), expected_names,
                         info = info)
        expect_identical(last_sent_queries()$purpose, "result", info = info)
        expect_identical(last_sent_queries()$sql,
                         as.character(dbplyr::sql_render(query)), info = info)
        result <- dplyr::collect(query)
        expect_identical(names(result), expected_names, info = info)
        expect_identical(nrow(result), if (fixed) 4L else 2L, info = info)
        expect_identical(result$g, rep(values[[type]], nrow(result)),
                         info = info)
        result <- if (fixed) {
          result[order(result$fixed, result$sid), ]
        } else {
          result[order(result$sid), ]
        }
        expect_identical(result$sid,
                         if (fixed) c(1L, 2L, 1L, 2L) else c(1L, 2L),
                         info = info)
        expect_equal(result$z,
                     if (fixed) c(1, 1, 2, 2) else c(3, 3), info = info)
      }
    }
  }
})

test_that("SQLite unsorted identifiers keep typed dimensions in expansions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  values <- list(
    character = NA_character_, integer = NA_integer_, double = NA_real_
  )

  for (type in names(values)) {
    source <- dplyr::copy_to(
      con,
      data.frame(fixed = c("a", "b"), g = rep(values[[type]], 2L),
                 v = c(1, 2)),
      paste0("unsorted_expansion_", type), temporary = TRUE
    )
    for (label in list(NULL, NA_character_)) {
      for (fixed in c(FALSE, TRUE)) {
        query <- if (fixed) {
          expand_with_margins(
            source, .by = fixed, .grouping = rollup(g),
            .margin_label = label, .id = "sid", .sort = "none"
          )
        } else {
          expand_with_margins(
            source, .grouping = rollup(g),
            .margin_label = label, .id = "sid", .sort = "none"
          )
        }
        info <- paste(type, if (is.null(label)) "NULL" else "NA", fixed)
        expected_names <- if (fixed) {
          c("fixed", "g", "sid", "v")
        } else {
          c("g", "sid", "fixed", "v")
        }
        expect_identical(as.character(dplyr::tbl_vars(query)),
                         expected_names, info = info)
        result <- dplyr::collect(query)
        expect_identical(names(result), expected_names, info = info)
        expect_identical(nrow(result), 4L, info = info)
        expect_identical(result$g, rep(values[[type]], 4L), info = info)
        result <- if (fixed) {
          result[order(result$fixed, result$sid), ]
        } else {
          result[order(result$sid, result$v), ]
        }
        expect_identical(result$sid,
                         if (fixed) c(1L, 2L, 1L, 2L) else c(1L, 1L, 2L, 2L),
                         info = info)
        expect_equal(result$v,
                     if (fixed) c(1, 1, 2, 2) else c(1, 2, 1, 2),
                     info = info)
      }
    }
  }
})

test_that("SQLite direct typed compute preserves its public schema", {
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
  expect_identical(dplyr::collect(computed)$g,
                   c(NA_character_, NA_character_))
  result <- dplyr::collect(query)
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

  expect_identical(dplyr::collect(dplyr::compute(query))$set, c(2L, 1L))
  result <- dplyr::collect(query)
  expect_identical(names(result), c("g", "set", "v"))
  expect_identical(result$g, c(NA_integer_, NA_integer_))
  expect_identical(result$set, c(2L, 1L))
  expect_equal(result$v, c(1, 1))
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

  expect_identical(dplyr::collect(dplyr::compute(query))$g,
                   c(NA_real_, NA_real_))
  result <- dplyr::collect(query)
  expect_identical(result$g, c(NA_real_, NA_real_))
  expect_identical(result$set, c(1L, 2L))
  expect_equal(result$z, c(1, 1))
  expect_equal(result$p, c(1, 1))
  expect_equal(result$t, c(1, 1))
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
    expect_identical(dplyr::collect(dplyr::compute(query))$set, c(2L, 1L))
    result <- dplyr::collect(query)
    expect_identical(result$g, c(NA_integer_, NA_integer_))
    expect_identical(result$set, c(2L, 1L))
    expect_equal(result$share, c(1, 1))
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
        expect_identical(typeof(dplyr::collect(dplyr::compute(query))$g),
                         type, info = info)
        result <- dplyr::collect(query)
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
})

test_that("SQLite stores numeric dimensions with text Margin labels as text", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  values <- list(integer = 1:2, double = c(1.5, 2.5))
  case <- 0L

  for (type in names(values)) {
    source <- dplyr::copy_to(
      con, data.frame(g = values[[type]], v = c(2, 4)),
      paste0("text_label_source_", type), temporary = TRUE
    )
    for (label in c("Total", "01")) {
      for (verb in c("summary", "expansion")) {
        for (plan in c("rollup", "one_set")) {
          for (sort in c("none", "first", "last")) {
            for (id in c(FALSE, TRUE)) {
              case <- case + 1L
              grouping <- if (identical(plan, "rollup")) {
                rollup(g)
              } else {
                grouping_set(g)
              }
              args <- list(
                .data = source, .grouping = grouping,
                .margin_label = label, .sort = sort,
                .id = if (id) "sid" else NULL
              )
              query <- if (identical(verb, "summary")) {
                do.call(
                  summarize_with_margins,
                  c(args, list(z = quote(sum(v))))
                )
              } else {
                do.call(expand_with_margins, args)
              }
              info <- paste(type, label, verb, plan, sort, id)
              direct <- dplyr::collect(query)
              name <- paste0("text_label_result_", case)
              saved <- dplyr::compute(query, name = name)
              materialized <- dplyr::collect(saved)
              reopened <- dplyr::tbl(
                con, DBI::Id(schema = "temp", table = name)
              )
              if (!identical(sort, "none")) {
                reopened <- dplyr::arrange(reopened, !!dbplyr::sql("rowid"))
              }
              reopened <- dplyr::collect(reopened)
              if (identical(sort, "none")) {
                by_columns <- function(x) {
                  dplyr::arrange(x, dplyr::across(dplyr::everything()))
                }
                direct <- by_columns(direct)
                materialized <- by_columns(materialized)
                reopened <- by_columns(reopened)
              }
              direct <- tibble::as_tibble(direct)
              materialized <- tibble::as_tibble(materialized)
              reopened <- tibble::as_tibble(reopened)
              expect_identical(materialized, direct, info = info)
              expect_identical(reopened, direct, info = info)
              expect_identical(typeof(direct$g), "character", info = info)
              declaration <- DBI::dbGetQuery(
                con, paste0("PRAGMA temp.table_info(\"", name, "\")")
              )
              expect_identical(declaration$type[declaration$name == "g"],
                               "TEXT", info = info)
              storage <- DBI::dbGetQuery(con, paste0(
                "SELECT typeof(g) AS kind, quote(g) AS spelling FROM temp.\"",
                name, "\""
              ))
              expect_true(all(storage$kind == "text"), info = info)
              expect_identical(
                sort(storage$spelling),
                sort(paste0("'", materialized$g, "'")),
                info = info
              )
            }
          }
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
    "all three rowid aliases", class = "marginplyr_error"
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

  expect_identical(dplyr::collect(dplyr::compute(query))$set,
                   rep(c(1L, 2L), 3L))
  result <- dplyr::collect(query)
  expect_identical(result$fixed, c(1L, 1L, 2L, 2L, NA_integer_, NA_integer_))
  expect_identical(result$g, rep(NA_character_, 6L))
  expect_identical(result$set, rep(c(1L, 2L), 3L))
  expect_equal(result$z, c(3, 3, 1, 1, 2, 2))
})

test_that("SQLite typed compute replaces a persistent target and its index", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = NA_character_, v = 1),
    "typed_order_persistent", temporary = TRUE
  )
  DBI::dbWriteTable(con, "typed_margin_output", data.frame(old = 1))
  DBI::dbExecute(con, paste0(
    "CREATE INDEX typed_margin_output_old ", "ON typed_margin_output(old)"
  ))
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .sort = "last"
  )

  computed <- dplyr::compute(
    query, name = "typed_margin_output", temporary = FALSE,
    overwrite = TRUE, indexes = list("g")
  )
  expect_identical(DBI::dbListFields(con, "typed_margin_output"), c("g", "z"))
  expect_true(nrow(DBI::dbGetQuery(
    con, "SELECT name FROM sqlite_master WHERE name = 'typed_margin_output'"
  )) == 1L)
  expect_identical(dplyr::collect(computed)$g,
                   c(NA_character_, NA_character_))
  expect_true(any(grepl("g", DBI::dbGetQuery(
    con, "PRAGMA index_list('typed_margin_output')"
  )$name)))
  expect_false(any(grepl("^marginplyr_order_", DBI::dbListTables(con))))
  expect_identical(dplyr::collect(query)$g, c(NA_character_, NA_character_))
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
  expect_true(DBI::dbExistsTable(con, "typed_order_partial_rowid_result"))
  expect_identical(dplyr::collect(computed)$set, c(2L, 1L))
  expect_identical(dplyr::collect(query)$set, c(2L, 1L))
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
    expect_identical(dplyr::collect(dplyr::compute(query))$set,
                     if (identical(sort, "first")) c(2L, 1L) else c(1L, 2L))
    result <- dplyr::collect(query)
    expect_identical(result$g, c(NA_character_, NA_character_))
    expect_equal(result$`..MARGINPLYR_ORDER_1`, c(7, 7))
    expect_identical(
      result$set,
      if (identical(sort, "first")) c(2L, 1L) else c(1L, 2L)
    )
  }
})

test_that("SQLite typed compute rolls back a failed unique index", {
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
  ), "UNIQUE constraint failed")
  expect_identical(DBI::dbListTables(con), before)
})
