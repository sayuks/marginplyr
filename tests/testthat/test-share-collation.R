# Two distinct spellings with one child each, using the declared collation.
sqlite_collation_source <- function(con, collation, parent_name = "parent") {
  DBI::dbExecute(con, paste0(
    "CREATE TABLE input (", DBI::dbQuoteIdentifier(con, parent_name),
    " TEXT COLLATE ", collation, ", child TEXT, amount INTEGER)"
  ))
  DBI::dbExecute(con, "INSERT INTO input VALUES (?, ?, ?)",
                 params = list("A", "x", 2L))
  DBI::dbExecute(con, "INSERT INTO input VALUES (?, ?, ?)",
                 params = list(if (collation == "RTRIM") "A " else "a",
                               "y", 5L))
  dplyr::tbl(con, "input")
}

test_that("SQLite Parent shares preserve source grouping equivalence", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (collation in c("BINARY", "NOCASE", "RTRIM")) {
    source <- sqlite_collation_source(con, collation)
    oracle <- DBI::dbGetQuery(con, paste(
      "SELECT D.child, D.total, CAST(D.total AS REAL)/P.total AS p",
      "FROM (SELECT parent, child, SUM(amount) total",
      "FROM input GROUP BY parent, child) D",
      "JOIN (SELECT parent, SUM(amount) total FROM input GROUP BY parent) P",
      "ON D.parent = P.parent ORDER BY D.child"
    ))
    query <- summarize_with_margins(
      source, total = sum(amount, na.rm = TRUE), p = share_of_parent(total),
      level = grouping_id(parent, child),
      .grouping = rollup(parent, child), .margin_label = NULL,
      .check_share_source = FALSE
    )
    result <- dplyr::collect(query)
    detail <- dplyr::arrange(result[result$level == 0L, ], .data$child)
    expect_identical(nrow(result), if (collation == "BINARY") 5L else 4L)
    expect_equal(detail[c("child", "total", "p")],
                 tibble::as_tibble(oracle), info = collation)
    expect_equal(detail$p, if (collation == "BINARY") c(1, 1) else c(2, 5) / 7)
    DBI::dbExecute(con, "DROP TABLE input")
  }
})

# An independent database oracle: ordinary grouping at each grain followed by
# joins on the actual source columns, before any Margin label or key rewrite.
collation_rollup_oracle <- function(source, sets, fixed = character()) {
  dimensions <- unique(unlist(sets))
  summaries <- lapply(sets, function(set) {
    source |>
      dplyr::group_by(!!!rlang::syms(c(fixed, set))) |>
      dplyr::summarise(
        total = sum(.data$amount, na.rm = TRUE), .groups = "drop"
      )
  })
  rows <- lapply(seq_along(sets), function(i) {
    summary <- summaries[[i]]
    coarser <- which(seq_along(sets) > i & lengths(sets) < length(sets[[i]]))
    targets <- c(p = if (length(coarser)) coarser[[1L]] else NA_integer_,
                 t = length(sets))
    for (share in names(targets)) {
      target <- targets[[share]]
      if (is.na(target) || length(sets[[i]]) == 0L) {
        summary <- dplyr::mutate(summary, "{share}" := 1.0)
      } else {
        keys <- c(fixed, sets[[target]])
        denominator <- dplyr::rename(summaries[[target]], denominator = "total")
        summary <- if (length(keys)) {
          dplyr::left_join(summary, denominator, by = keys, na_matches = "na")
        } else {
          dplyr::cross_join(summary, denominator)
        }
        summary <- summary |>
          dplyr::mutate(
            "{share}" := as.numeric(.data$total) / .data$denominator
          ) |>
          dplyr::select(-"denominator")
      }
    }
    bits <- 2^(rev(seq_along(dimensions)) - 1L)
    level <- sum(bits[!dimensions %in% sets[[i]]])
    summary |>
      dplyr::mutate(level = !!level) |>
      dplyr::select("level", "total", "p", "t") |>
      dplyr::collect()
  })
  dplyr::bind_rows(rows) |>
    dplyr::arrange(.data$level, .data$total)
}

collation_margin_summary <- function(source, grouping, dimensions,
                                     fixed = character(), label = NULL,
                                     id = NULL, sort = "none",
                                     duplicates = "drop", shares = TRUE) {
  share_exprs <- if (shares) {
    rlang::exprs(
      p = share_of_parent(!!rlang::sym("total")),
      t = share_of_total(!!rlang::sym("total"))
    )
  } else {
    list()
  }
  summarize_with_margins(
    source, level = grouping_id(!!!rlang::syms(dimensions)),
    total = sum(.data$amount, na.rm = TRUE), !!!share_exprs,
    .by = dplyr::all_of(fixed), .grouping = grouping,
    .margin_label = label, .id = id, .sort = sort,
    .duplicates = duplicates, .check_share_source = FALSE
  )
}

expect_collation_summary <- function(query, oracle, dimensions,
                                     fixed = character(), id = NULL,
                                     label = NULL) {
  result <- dplyr::collect(query)
  expect_identical(
    names(result), c(fixed, dimensions, id, "level", "total", "p", "t")
  )
  keys <- result[c(fixed, dimensions)]
  expect_true(all(vapply(keys, is.character, logical(1))))
  expect_type(result$level, "integer")
  expect_identical(typeof(result$total), typeof(oracle$total))
  if (!is.null(id)) expect_type(result[[id]], "integer")
  expect_type(result$p, "double")
  expect_type(result$t, "double")
  expect_equal(
    dplyr::arrange(tibble::as_tibble(result[c("level", "total", "p", "t")]),
                   .data$level, .data$total),
    oracle
  )
  # Representative spellings are unspecified; only absent keys have a label.
  for (i in seq_along(dimensions)) {
    bit <- 2^(length(dimensions) - i)
    omitted <- bitwAnd(as.integer(result$level), as.integer(bit)) != 0L
    values <- result[[dimensions[[i]]]][omitted]
    if (is.null(label) || is.na(label)) {
      expect_true(all(is.na(values)))
    } else {
      expect_true(all(values == label))
    }
  }
  invisible(result)
}

test_that("SQLite shares preserve collation across retrieval options", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sets <- list(c("parent", "child"), "parent", character())
  for (collation in c("NOCASE", "RTRIM")) {
    source <- sqlite_collation_source(con, collation)
    oracle <- collation_rollup_oracle(source, sets)
    for (label in list(NULL, NA_character_, "Margin")) {
      for (id in list(NULL, "set")) {
        for (sort in c("none", "first", "last")) {
          query <- collation_margin_summary(
            source, rollup(parent, child), c("parent", "child"),
            label = label, id = id, sort = sort
          )
          expect_s3_class(query, "tbl_lazy")
          direct <- expect_collation_summary(
            query, oracle, c("parent", "child"), id = id, label = label
          )
          computed <- dplyr::compute(query, name = "retrieved")
          materialized <- expect_collation_summary(
            computed, oracle, c("parent", "child"), id = id, label = label
          )
          expect_equal(
            tibble::as_tibble(materialized[c(id, "level", "total", "p", "t")]),
            tibble::as_tibble(direct[c(id, "level", "total", "p", "t")])
          )
          DBI::dbExecute(con, "DROP TABLE retrieved")
        }
      }
    }
    aggregate <- collation_margin_summary(
      source, rollup(parent, child), c("parent", "child"), shares = FALSE
    )
    expect_equal(
      dplyr::collect(aggregate) |>
        dplyr::select("level", "total") |>
        dplyr::arrange(.data$level, .data$total),
      oracle[c("level", "total")]
    )
    DBI::dbExecute(con, "DROP TABLE input")
  }
})

test_that("SQLite Parent shares use the equivalence of each derived input", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (collation in c("NOCASE", "RTRIM")) {
    source <- sqlite_collation_source(con, collation, parent_name = "original")
    DBI::dbExecute(con, paste(
      "CREATE VIEW direct_view AS",
      "SELECT original AS parent, child, amount FROM input"
    ))
    renamed <- source |>
      dplyr::select("original", "child", "amount") |>
      dplyr::rename(parent = "original")
    forms <- list(
      renamed = renamed,
      view = dplyr::tbl(con, "direct_view"),
      cast = dplyr::mutate(renamed, parent = as.character(.data$parent)),
      collated = dplyr::mutate(renamed, parent = !!dbplyr::sql(paste0(
        "(CASE WHEN 1=1 THEN parent ELSE NULL END) COLLATE ", collation
      ))),
      caller_case = dplyr::mutate(renamed, parent = dplyr::if_else(
        .data$amount > 0L, .data$parent, NA_character_
      )),
      caller_concat = dplyr::mutate(renamed, parent = paste0(.data$parent, ""))
    )
    sets <- list(c("parent", "child"), "parent", character())
    for (source in forms) {
      oracle <- collation_rollup_oracle(source, sets)
      query <- collation_margin_summary(
        source, rollup(parent, child), c("parent", "child")
      )
      expect_collation_summary(query, oracle, c("parent", "child"))
      expect_collation_summary(dplyr::compute(query, name = "derived"),
                               oracle, c("parent", "child"))
      DBI::dbExecute(con, "DROP TABLE derived")
    }
    DBI::dbExecute(con, "DROP VIEW direct_view")
    DBI::dbExecute(con, "DROP TABLE input")
  }
})

test_that("SQLite composite Parents preserve collated and missing keys", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE input (fixed TEXT COLLATE NOCASE,",
    "parent TEXT COLLATE NOCASE,",
    "included TEXT COLLATE RTRIM, child TEXT, amount INTEGER)"
  ))
  data <- data.frame(
    fixed = c("F", "f", "F", "f", "other", "OTHER", NA, NA, NA),
    parent = c("A", "a", NA, NA, "A", "a", "A", "a", NA),
    included = c("K", "K ", NA, NA, "K", "K ", "K", "K ", NA),
    child = c("x", "y", "z", NA, "x", "y", "x", "y", NA),
    amount = c(2L, 5L, 11L, 17L, 23L, 31L, 41L, 47L, 59L)
  )
  DBI::dbAppendTable(con, "input", data)
  source <- dplyr::tbl(con, "input")
  dimensions <- c("parent", "included", "child")
  for (duplicates in c("drop", "keep")) {
    grouping <- if (duplicates == "keep") {
      rollup(grouping_set(parent, included), child, child)
    } else {
      rollup(grouping_set(parent, included), child)
    }
    sets <- list(dimensions, c("parent", "included"), character())
    if (duplicates == "keep") sets <- c(sets[1L], sets)
    oracle <- collation_rollup_oracle(source, sets, fixed = "fixed")
    for (label in list(NULL, "Margin")) {
      query <- collation_margin_summary(
        source, grouping, dimensions, fixed = "fixed", label = label,
        id = "set", duplicates = duplicates
      )
      result <- expect_collation_summary(
        query, oracle, dimensions, fixed = "fixed", id = "set", label = label
      )
      expect_equal(
        as.integer(table(result$set)),
        if (duplicates == "keep") c(9L, 9L, 5L, 3L) else c(9L, 5L, 3L)
      )
    }
  }
})
