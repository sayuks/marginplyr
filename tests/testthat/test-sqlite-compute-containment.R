test_that("dedicated SQLite compute refuses before touching destinations", {
  skip_if_suggest_absent("RSQLite", "DBI")

  for (sort in c("none", "last")) {
    for (destination in c("qualified", "shadowed")) {
      for (outer in c(FALSE, TRUE)) {
        con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        tryCatch({
          DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS other")
          source <- dplyr::copy_to(
            con, data.frame(g = c("a", "b"), v = c(2, 5)),
            "source", temporary = TRUE
          )
          DBI::dbExecute(con, "CREATE TABLE main.report (g TEXT, sid INTEGER, z REAL)")
          DBI::dbExecute(con, "INSERT INTO main.report VALUES ('keep', 99, -1)")
          DBI::dbExecute(con, "CREATE INDEX report_g ON report(g)")
          DBI::dbExecute(con, "ANALYZE main.report")
          DBI::dbExecute(con, "CREATE TABLE temp.report (g TEXT, sid INTEGER, z REAL)")
          DBI::dbExecute(con, "INSERT INTO temp.report VALUES ('temp', 98, -2)")
          DBI::dbExecute(con, "CREATE TABLE other.sentinel (value TEXT)")
          DBI::dbExecute(con, "INSERT INTO other.sentinel VALUES ('other')")
          DBI::dbExecute(con, "CREATE TABLE main.caller (value TEXT)")
          if (outer) {
            DBI::dbBegin(con)
            DBI::dbExecute(con, "INSERT INTO caller VALUES ('prior work')")
          }
          query <- summarize_with_margins(
            source, z = sum(v, na.rm = TRUE),
            .grouping = rollup(g), .id = "sid", .sort = sort
          )
          before <- lapply(c("main", "temp", "other"), function(schema) {
            DBI::dbGetQuery(con, paste0(
              "SELECT type, name, tbl_name, sql FROM ", schema,
              ".sqlite_master ORDER BY type, name"
            ))
          })
          stats_before <- DBI::dbGetQuery(
            con, "SELECT * FROM main.sqlite_stat1 ORDER BY tbl, idx"
          )
          name <- if (identical(destination, "qualified")) {
            dbplyr::in_schema("other", "report")
          } else {
            "report"
          }
          expect_error(
            dplyr::compute(query, name = name, temporary = FALSE,
                           analyze = TRUE),
            "temporarily disabled.*collect\\(\\)",
            class = "marginplyr_error",
            info = paste(sort, destination, outer)
          )
          after <- lapply(c("main", "temp", "other"), function(schema) {
            DBI::dbGetQuery(con, paste0(
              "SELECT type, name, tbl_name, sql FROM ", schema,
              ".sqlite_master ORDER BY type, name"
            ))
          })
          expect_identical(after, before)
          expect_identical(DBI::dbGetQuery(
            con, "SELECT * FROM main.sqlite_stat1 ORDER BY tbl, idx"
          ), stats_before)
          expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.report")$g,
                           "keep")
          expect_identical(DBI::dbGetQuery(con, "SELECT * FROM temp.report")$g,
                           "temp")
          expect_identical(DBI::dbGetQuery(con,
                                           "SELECT * FROM other.sentinel")$value,
                           "other")
          expect_identical(dplyr::collect(source)$g, c("a", "b"))
          expect_identical(dplyr::collect(source)$v, c(2, 5))
          expect_identical(DBI::dbGetQuery(con, "SELECT * FROM caller")$value,
                           if (outer) "prior work" else character())
          if (outer) {
            if (identical(sort, "last") &&
                identical(destination, "qualified")) {
              DBI::dbCommit(con)
              expect_identical(DBI::dbGetQuery(con,
                                               "SELECT * FROM caller")$value,
                               "prior work")
            } else {
              DBI::dbRollback(con)
              expect_identical(DBI::dbGetQuery(con,
                                               "SELECT * FROM caller")$value,
                               character())
            }
          } else {
            DBI::dbBegin(con)
            DBI::dbRollback(con)
          }
        }, finally = DBI::dbDisconnect(con))
      }
    }
  }
})

test_that("SQLite containment preserves direct collection and compute controls", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "containment_controls", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g),
    .id = "sid", .margin_label = NULL, .sort = "last"
  )
  full <- dplyr::collect(query)
  expect_identical(names(full), c("g", "sid", "z"))
  expect_identical(full$g, c("a", "b", NA_character_))
  expect_identical(full$sid, c(1L, 1L, 2L))
  expect_equal(full$z, c(2, 5, 7))
  expect_identical(dplyr::collect(query, n = 2L)$g, c("a", "b"))

  ordinary <- dplyr::summarise(source, z = sum(v))
  expect_equal(dplyr::collect(dplyr::compute(ordinary))$z, 7)
  downstream <- dplyr::select(query, z)
  expect_equal(dplyr::collect(dplyr::compute(downstream))$z,
               c(2, 5, 7))
  unsorted <- summarize_with_margins(
    source, z = sum(v), .grouping = rollup(g), .sort = "none"
  )
  expect_equal(sort(dplyr::collect(dplyr::compute(unsorted))$z),
               c(2, 5, 7))
})
