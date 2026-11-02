test_that("SQLite binary Boolean-word inputs with pronouns remain lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = "a", `TRUE` = 7L, `FALSE` = 19L, .env = "x",
                     check.names = FALSE)
  source <- dplyr::copy_to(con, data, "lazy_facts")
  keys <- dplyr::copy_to(con, data.frame(g = c("a", "b", "Total")), "keys")
  tables <- DBI::dbListTables(con)
  compose <- function(query, peer) {
    list(dplyr::inner_join(query, peer, by = "g"),
         dplyr::inner_join(peer, query, by = "g"),
         dplyr::full_join(query, peer, by = "g"),
         dplyr::full_join(peer, query, by = "g"),
         dplyr::union_all(query, source),
         dplyr::union_all(source, query),
         dplyr::intersect(query, source),
         dplyr::intersect(source, query))
  }
  build <- function() {
    unexpected <- function(...) stop("unexpected database execution")
    testthat::local_mocked_bindings(
      dbSendQuery = unexpected, dbSendStatement = unexpected,
      dbExecute = unexpected, dbGetQuery = unexpected,
      dbExistsTable = unexpected, .package = "DBI"
    )
    query <- expand_with_margins(source, .grouping = rollup(g), .sort = "last")
    derived <- compose(query, keys)
    for (cte in c(FALSE, TRUE)) {
      for (input in derived) {
        expect_s3_class(dbplyr::sql_render(
          input, sql_options = dbplyr::sql_options(cte = cte)
        ), "sql")
      }
    }
    derived
  }
  derived <- build()
  expect_identical(DBI::dbListTables(con), tables)
  DBI::dbExecute(con, "INSERT INTO lazy_facts VALUES ('b', 31, 43, 'y')")
  rows <- rbind(data, data.frame(g = "b", `TRUE` = 31L, `FALSE` = 43L,
                                 .env = "y", check.names = FALSE))
  margin <- rows
  margin$g <- "Total"
  expected <- rbind(rows, margin)
  union_rows <- rbind(expected, rows)
  targets <- c(rep(list(expected), 4L),
               list(union_rows, union_rows, rows, rows))
  canonical <- function(x) {
    dplyr::arrange(as.data.frame(x), .data$g, .data[["TRUE"]])
  }
  for (cte in c(FALSE, TRUE)) {
    # CTE set operations also lose Boolean names on ordinary SQLite inputs.
    for (i in if (cte) seq_len(4L) else seq_along(derived)) {
      actual <- dplyr::collect(
        derived[[i]], sql_options = dbplyr::sql_options(cte = cte)
      )
      expect_equal(canonical(actual), canonical(targets[[i]]))
    }
  }
})

test_that("SQLite Boolean-word summary outputs remain binary inputs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = c("a", "a"), v = c(7L, 19L)),
                           "summary_facts")
  query <- summarize_with_margins(
    source, `TrUe` = sum(v), .grouping = rollup(g), .sort = "first"
  )
  expected <- data.frame(g = c("a", "Total"), `TrUe` = c(26L, 26L),
                         check.names = FALSE)
  peer <- dplyr::copy_to(con, expected, "summary_peer")
  join <- function(x, y) dplyr::inner_join(x, y, by = c("g", "TrUe"))
  for (verb in list(join, dplyr::union_all, dplyr::intersect)) {
    for (right in c(FALSE, TRUE)) {
      x <- if (right) peer else query
      y <- if (right) query else peer
      actual <- dplyr::collect(verb(x, y))
      reference <- verb(expected, expected)
      expect_equal(dplyr::arrange(as.data.frame(actual), .data$g),
                   dplyr::arrange(reference, .data$g))
    }
  }
})
