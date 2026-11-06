test_that("SQLite expansion preserves a Boolean-word window source", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  root <- dplyr::copy_to(
    con, data.frame(g = "a", `TRUE` = 7L, check.names = FALSE), "window_source"
  )
  source <- dplyr::mutate(root, z = dplyr::row_number(g))
  expected <- tibble::tibble(g = "a", `TRUE` = 7L, z = 1L)
  expect_identical(dplyr::collect(source), expected)
  expect_identical(dplyr::collect(dplyr::select(source, dplyr::everything())),
                   expected)
  expect_identical(dplyr::collect(dplyr::mutate(source)), expected)
  expect_identical(dplyr::collect(expand_with_margins(source)), expected)
  expect_identical(dplyr::collect(expand_with_margins(
    source, .grouping = grouping_set(g)
  )), expected)
})

test_that("SQLite derived expansion constructs lazily and shares row copies", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(g = "a", `FaLsE` = 7L, rid = 1L, check.names = FALSE)
  root <- dplyr::copy_to(con, data, "lazy_derived_source")
  source <- dplyr::mutate(
    root, z = dplyr::row_number(rid),
    token = dbplyr::sql("hex(randomblob(16))")
  )
  original_query <- source$lazy_query
  tables <- DBI::dbListTables(con)
  build <- function() {
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) stop("unexpected source read"),
      dbExecute = function(...) stop("unexpected write"),
      .package = "DBI"
    )
    query <- expand_with_margins(
      source, .grouping = grouping_sets(grouping_set(g), grouping_set(),
                                        grouping_set(g)),
      .duplicates = "keep", .id = "TRUE", .sort = "last"
    )
    for (cte in c(FALSE, TRUE)) {
      dbplyr::sql_render(query, sql_options = dbplyr::sql_options(cte = cte))
    }
    query
  }
  query <- build()
  expect_identical(source$lazy_query, original_query)
  expect_identical(DBI::dbListTables(con), tables)
  expect_identical(DBI::dbReadTable(con, "lazy_derived_source"), data)
  DBI::dbExecute(con, "INSERT INTO lazy_derived_source VALUES ('b', 19, 2)")
  result <- dplyr::collect(query)
  expect_identical(names(result), c("g", "TRUE", "FaLsE", "rid", "z", "token"))
  expect_identical(result$g, c("a", "a", "b", "b", "Total", "Total"))
  expect_identical(result$`TRUE`, c(1L, 3L, 1L, 3L, 2L, 2L))
  expect_identical(result$FaLsE, c(7L, 7L, 19L, 19L, 7L, 19L))
  expect_identical(result$z, c(1L, 1L, 2L, 2L, 1L, 2L))
  for (rid in 1:2) {
    expect_length(unique(result$token[result$rid == rid]), 1L)
  }
  expect_identical(DBI::dbListTables(con), tables)
})

test_that("SQLite derived expansion keeps copies, labels and direct types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (name in c("TRUE", "FALSE", "TrUe", "FaLsE")) {
    data <- tibble::tibble(
      g = c("b", "a", NA_character_), v = c(7L, 2L, NA_integer_), rid = 1:3,
      text = rep(NA_character_, 3L), number = rep(NA_real_, 3L),
      `..marginplyr_input_1` = rep(NA_integer_, 3L)
    )
    names(data)[[2L]] <- name
    root <- dplyr::copy_to(con, data, "MARGINPLYR_BOOLEAN_SOURCE_1",
                           overwrite = TRUE)
    for (empty in c(FALSE, TRUE)) {
      input <- if (empty) dplyr::filter(root, FALSE) else root
      source <- dplyr::mutate(input, z = dplyr::row_number(rid))
      original <- dplyr::collect(source)
      plan <- grouping_sets(grouping_set(), grouping_set(dplyr::all_of(name)),
                            grouping_set(dplyr::all_of(name)))
      for (label in list(NULL, "Total")) {
        for (sort in c("none", "first", "last")) {
          info <- paste(name, empty, label, sort, collapse = " ")
          query <- expand_with_margins(
            source, .grouping = plan, .duplicates = "keep", .id = "set",
            .margin_label = label, .sort = sort
          )
          expected <- as.data.frame(expand_with_margins(
            original, .grouping = plan, .duplicates = "keep", .id = "set",
            .margin_label = label, .sort = sort
          ))
          options <- dbplyr::sql_options(cte = TRUE)
          for (cte in c(FALSE, TRUE)) {
            actual <- dplyr::collect(
              query, sql_options = dbplyr::sql_options(cte = cte)
            )
            expect_identical(as.data.frame(actual), expected, info = info)
          }
          computed <- dplyr::compute(query, name = "derived_result",
                                     overwrite = TRUE, analyze = FALSE,
                                     sql_options = options)
          expect_identical(DBI::dbListFields(con, "derived_result"),
                           names(expected), info = info)
          expect_identical(as.data.frame(dplyr::collect(computed)), expected,
                           info = info)
          for (n in c(0L, 1L)) {
            actual <- dplyr::collect(query, n = n, sql_options = options)
            prefix <- utils::head(expected, n)
            # No source type is inferred for an empty computed window column.
            if (n == 0L) prefix$z <- logical()
            expect_identical(as.data.frame(actual), prefix, info = info)
          }
          expect_identical(dplyr::collect(source), original, info = info)
        }
      }
    }
  }
})

test_that("SQLite first expansions retain executable derived input schemas", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (name in c("v", "TRUE", "FALSE", "TrUe", "FaLsE")) {
    for (size in c(0L, 1L, 3L)) {
      data <- tibble::tibble(
        g = c("b", "a", NA_character_)[seq_len(size)],
        v = c(7L, 2L, NA_integer_)[seq_len(size)], rid = seq_len(size),
        text = rep(NA_character_, size), number = rep(NA_real_, size),
        `..marginplyr_boolean_1` = rep(NA_integer_, size)
      )
      names(data)[[2L]] <- name
      root <- dplyr::copy_to(con, data, "derived_source", overwrite = TRUE)
      graphs <- list(
        root = root,
        scalar = dplyr::mutate(root, z = 1L),
        window = dplyr::mutate(root, z = dplyr::row_number(rid)),
        collapse = dplyr::collapse(dplyr::filter(root, TRUE)),
        union = dplyr::union_all(root, root),
        join = dplyr::left_join(
          root, dplyr::transmute(root, rid, j = rid + 1L), by = "rid"
        )
      )
      for (kind in names(graphs)) {
        info <- paste(name, size, kind)
        source <- graphs[[kind]]
        expected <- switch(kind,
          scalar = dplyr::mutate(data, z = 1L),
          window = dplyr::mutate(data, z = seq_len(size)),
          union = dplyr::bind_rows(data, data),
          join = dplyr::mutate(data, j = rid + 1L),
          data
        )
        # SQLite declares no type for these empty computed expressions.
        if (size == 0L && kind %in% c("scalar", "window", "join")) {
          expected[[if (kind == "join") "j" else "z"]] <- logical()
        }
        # These controls establish admission independently of expansion.
        for (control in list(source, dplyr::select(source, dplyr::everything()),
                             dplyr::mutate(source))) {
          expect_identical(dplyr::collect(control), expected, info = info)
        }
        queries <- list(expand_with_margins(source), expand_with_margins(
          source, .grouping = grouping_set(g)
        ))
        for (query in queries) {
          expect_identical(dplyr::collect(query), expected, info = info)
        }
      }
      safe <- dplyr::rename(root, payload = dplyr::all_of(name)) |>
        dplyr::mutate(z = dplyr::row_number(rid))
      expect_identical(dplyr::collect(expand_with_margins(safe)),
                       dplyr::collect(safe))
    }
  }
})
