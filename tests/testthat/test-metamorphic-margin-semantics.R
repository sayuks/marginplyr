# These are public-seam relations, not examples of the private Grouping plan.
# Each case varies a documented input boundary without making test generation a
# second implementation of Margin semantics.
metamorphic_case_table <- function() {
  populated <- data.frame(
    region = c("East", "East", NA_character_, "West"),
    store = factor(
      c("A", "A", "A", "B"),
      levels = c("A", "B", "unused")
    ),
    amount = c(2, 2, 3, 5)
  )

  list(
    "duplicate, missing, and unused factor level" = populated,
    "empty input with an unused factor level" = populated[0, ]
  )
}

metamorphic_product_case <- function() {
  data.frame(
    region = c("East", "East", NA_character_, "West"),
    store = c("A", "A", "A", "B"),
    channel = c("web", "web", "web", "store"),
    amount = c(2, 2, 3, 5)
  )
}

metamorphic_label <- function(case, transformation, api, backend) {
  paste(
    paste0("source case: ", case),
    paste0("transformation: ", transformation),
    paste0("API: ", api),
    paste0("backend: ", backend),
    sep = "; "
  )
}

metamorphic_public_result <- function(code,
                                      case,
                                      transformation,
                                      api,
                                      backend) {
  code <- rlang::enquo(code)
  tryCatch(
    rlang::eval_tidy(code),
    error = function(condition) {
      testthat::fail(paste(
        metamorphic_label(case, transformation, api, backend),
        conditionMessage(condition),
        sep = "\n"
      ))
    }
  )
}

# Margin-operation row order is unspecified by default. `vec_order()` orders
# every visible value without converting its type, so identical() below remains
# a type-sensitive multiset comparison.
metamorphic_multiset <- function(result) {
  result <- tibble::as_tibble(result)[sort(names(result))]
  result[vctrs::vec_order(result), , drop = FALSE]
}

expect_metamorphic_multiset <- function(actual,
                                        expected,
                                        case,
                                        transformation,
                                        api,
                                        backend) {
  expect_identical(
    metamorphic_multiset(actual),
    metamorphic_multiset(expected),
    label = metamorphic_label(case, transformation, api, backend)
  )
}

# The public list format carries exact column vectors. Constructor transforms
# may change occurrence and bit numbering, so a plan relation compares its
# fixed/included/omitted multiset rather than either documented identifier.
metamorphic_plan_multiset <- function(plan) {
  rows <- lapply(seq_len(nrow(plan)), function(index) {
    list(
      fixed = sort(plan$fixed[[index]]),
      included = sort(plan$included[[index]]),
      omitted = sort(plan$omitted[[index]])
    )
  })
  keys <- vapply(
    rows,
    function(row) paste(unlist(row, use.names = FALSE), collapse = "\r"),
    character(1)
  )
  rows[order(keys)]
}

expect_metamorphic_plan <- function(actual,
                                    expected,
                                    case,
                                    transformation,
                                    backend) {
  expect_identical(
    metamorphic_plan_multiset(actual),
    metamorphic_plan_multiset(expected),
    label = metamorphic_label(
      case,
      transformation,
      "inspect_grouping(.format = \"list\")",
      backend
    )
  )
}

metamorphic_summary <- function(input, grouping) {
  summarize_with_margins(
    input,
    total = sum(.data$amount),
    rows = dplyr::n(),
    .grouping = grouping,
    .margin_label = NULL
  ) |>
    dplyr::collect()
}

metamorphic_direct_summary <- function(input, grouping) {
  summarize_with_margins(
    input,
    total = sum(.data$amount),
    rows = dplyr::n(),
    .grouping = grouping,
    .margin_label = NULL,
    .id = "set"
  ) |>
    dplyr::collect()
}

metamorphic_expanded_summary <- function(input, grouping) {
  expand_with_margins(
    input,
    .grouping = grouping,
    .margin_label = NULL,
    .id = "set"
  ) |>
    dplyr::group_by(.data$region, .data$store, .data$set) |>
    dplyr::summarise(
      total = sum(.data$amount),
      rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect()
}

metamorphic_rollup <- function() {
  rlang::inject(
    rollup(!!rlang::sym("region"), !!rlang::sym("store"))
  )
}

metamorphic_empty_grouping <- function() {
  rlang::inject(
    grouping_sets(
      grouping_set(!!rlang::sym("region"), !!rlang::sym("store")),
      grouping_set(!!rlang::sym("region"))
    )
  )
}

expect_expansion_equivalence <- function(input,
                                         case,
                                         backend,
                                         grouping = metamorphic_rollup()) {
  expect_metamorphic_multiset(
    metamorphic_public_result(
      metamorphic_direct_summary(input, grouping),
      case,
      expansion_relation,
      "summarize_with_margins()",
      backend
    ),
    metamorphic_public_result(
      metamorphic_expanded_summary(input, grouping),
      case,
      expansion_relation,
      "expand_with_margins() then dplyr::summarise()",
      backend
    ),
    case,
    expansion_relation,
    "summarize_with_margins() and expand_with_margins()",
    backend
  )
}

expansion_relation <- paste(
  "direct additive summary equals expand then summarise",
  "by visible keys and .id"
)

test_that(
  "constructor algebra preserves Grouping plans and additive summaries",
  {
    transformations <- list(
      "rollup equals ordered explicit grouping sets" = list(
        rollup(region, store),
        grouping_sets(
          grouping_set(region, store),
          grouping_set(region),
          grouping_set()
        )
      ),
      "cube equals ordered explicit grouping sets" = list(
        cube(region, store),
        grouping_sets(
          grouping_set(region, store),
          grouping_set(region),
          grouping_set(store),
          grouping_set()
        )
      ),
      "two rollups product equals cube" = list(
        cube(region, store),
        grouping_spec(rollup(region), rollup(store))
      )
    )

    for (case in names(metamorphic_case_table())) {
      input <- metamorphic_case_table()[[case]]
      for (transformation in names(transformations)) {
        pair <- transformations[[transformation]]
        expect_metamorphic_plan(
          metamorphic_public_result(
            inspect_grouping(
              input,
              .grouping = pair[[1L]],
              .format = "list"
            ),
            case,
            transformation,
            "inspect_grouping(.format = \"list\")",
            "local"
          ),
          metamorphic_public_result(
            inspect_grouping(
              input,
              .grouping = pair[[2L]],
              .format = "list"
            ),
            case,
            transformation,
            "inspect_grouping(.format = \"list\")",
            "local"
          ),
          case,
          transformation,
          "local"
        )
        expect_metamorphic_multiset(
          metamorphic_public_result(
            metamorphic_summary(input, pair[[1L]]),
            case,
            transformation,
            "summarize_with_margins()",
            "local"
          ),
          metamorphic_public_result(
            metamorphic_summary(input, pair[[2L]]),
            case,
            transformation,
            "summarize_with_margins()",
            "local"
          ),
          case,
          transformation,
          "summarize_with_margins()",
          "local"
        )
      }
    }
  }
)

test_that("empty input preserves the direct and expansion relation locally", {
  expect_expansion_equivalence(
    metamorphic_case_table()[[2L]],
    "empty input with an unused factor level",
    "local",
    metamorphic_empty_grouping()
  )
})

test_that("product associativity and commutativity preserve public semantics", {
  input <- metamorphic_product_case()
  transformations <- list(
    "product commutativity" = list(
      grouping_spec(rollup(region), rollup(store)),
      grouping_spec(rollup(store), rollup(region))
    ),
    "product associativity" = list(
      grouping_spec(
        grouping_spec(rollup(region), grouping_set(store)),
        grouping_set(channel)
      ),
      grouping_spec(
        rollup(region),
        grouping_spec(grouping_set(store), grouping_set(channel))
      )
    )
  )

  for (transformation in names(transformations)) {
    pair <- transformations[[transformation]]
    expect_metamorphic_plan(
      metamorphic_public_result(
        inspect_grouping(input, .grouping = pair[[1L]], .format = "list"),
        "duplicate, missing, and three-dimensional input",
        transformation,
        "inspect_grouping(.format = \"list\")",
        "local"
      ),
      metamorphic_public_result(
        inspect_grouping(input, .grouping = pair[[2L]], .format = "list"),
        "duplicate, missing, and three-dimensional input",
        transformation,
        "inspect_grouping(.format = \"list\")",
        "local"
      ),
      "duplicate, missing, and three-dimensional input",
      transformation,
      "local"
    )
    expect_metamorphic_multiset(
      metamorphic_public_result(
        metamorphic_summary(input, pair[[1L]]),
        "duplicate, missing, and three-dimensional input",
        transformation,
        "summarize_with_margins()",
        "local"
      ),
      metamorphic_public_result(
        metamorphic_summary(input, pair[[2L]]),
        "duplicate, missing, and three-dimensional input",
        transformation,
        "summarize_with_margins()",
        "local"
      ),
      "duplicate, missing, and three-dimensional input",
      transformation,
      "summarize_with_margins()",
      "local"
    )
  }
})

test_that(
  "direct additive summaries equal expanded aggregation by occurrence",
  {
    input <- metamorphic_case_table()[[1L]]
    expect_expansion_equivalence(
      input,
      "duplicate, missing, and unused factor level",
      "local"
    )
  }
)

test_that("dtplyr keeps direct and expanded additive summaries equivalent", {
  skip_if_suggest_absent("dtplyr")

  input <- dtplyr::lazy_dt(metamorphic_case_table()[[1L]])
  expect_expansion_equivalence(
    input,
    "duplicate, missing, and unused factor level",
    "dtplyr"
  )
})

test_that("RSQLite keeps direct and expanded additive summaries equivalent", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    transform(metamorphic_case_table()[[1L]], store = as.character(store)),
    "metamorphic_sqlite_input",
    temporary = TRUE
  )
  expect_expansion_equivalence(
    input,
    "duplicate and source-missing keys",
    "RSQLite"
  )
})

test_that(
  "RSQLite preserves the direct and expansion relation for empty input",
  {
    skip_if_suggest_absent("RSQLite", "DBI")

    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    input <- dplyr::copy_to(
      con,
      data.frame(region = character(), store = character(), amount = numeric()),
      "metamorphic_sqlite_empty_input",
      temporary = TRUE
    )
    expect_expansion_equivalence(
      input,
      "empty character-key input",
      "RSQLite",
      metamorphic_empty_grouping()
    )
  }
)

test_that("DuckDB keeps direct and expanded additive summaries equivalent", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con,
    metamorphic_case_table()[[1L]],
    "metamorphic_duckdb_input",
    temporary = TRUE
  )
  expect_expansion_equivalence(
    input,
    "duplicate, missing, and unused factor level",
    "DuckDB"
  )
})

test_that(
  "DuckDB preserves the direct and expansion relation for empty input",
  {
    skip_if_suggest_absent("duckdb", "DBI")

    con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    input <- dplyr::copy_to(
      con,
      data.frame(region = character(), store = character(), amount = numeric()),
      "metamorphic_duckdb_empty_input",
      temporary = TRUE
    )
    expect_expansion_equivalence(
      input,
      "empty character-key input",
      "DuckDB",
      metamorphic_empty_grouping()
    )
  }
)

test_that(
  "Arrow keeps supported direct and expanded additive summaries equivalent",
  {
    skip_if_suggest_absent("arrow")

    # Arrow cannot unify the factor dictionaries that the factor-restoration
    # path creates, so this supported Arrow operation uses the corresponding
    # character-key case. The local cases above retain the unused factor level.
    input <- arrow::Table$create(
      transform(metamorphic_case_table()[[1L]], store = as.character(store))
    )
    expect_expansion_equivalence(
      input,
      "duplicate and source-missing keys",
      "Arrow"
    )
  }
)

test_that("RSQLite preserves typed missing values across grouping-set order", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cases <- list(
    "zero-row character key" = data.frame(a = character(), amount = numeric()),
    "one-row all-missing key" = data.frame(a = NA_character_, amount = 1)
  )
  specifications <- list(
    grouping_sets(grouping_set(a), grouping_set()),
    grouping_sets(grouping_set(), grouping_set(a))
  )
  margin_labels <- list(`NULL` = NULL, `NA_character_` = NA_character_)

  for (case in names(cases)) {
    input <- dplyr::copy_to(
      con,
      cases[[case]],
      paste0("metamorphic_sqlite_", gsub("[^a-z]+", "_", tolower(case))),
      temporary = TRUE,
      overwrite = TRUE
    )
    for (margin_label_name in names(margin_labels)) {
      margin_label <- margin_labels[[margin_label_name]]
      case_label <- paste(case, "Margin label", margin_label_name)
      summaries <- lapply(specifications, function(grouping) {
        metamorphic_public_result(
          summarize_with_margins(
            input,
            total = sum(.data$amount),
            rows = dplyr::n(),
            .grouping = grouping,
            .margin_label = margin_label
          ) |>
            dplyr::collect(),
          case_label,
          "grouping-set occurrence permutation",
          "summarize_with_margins()",
          "RSQLite"
        )
      })
      expansions <- lapply(specifications, function(grouping) {
        metamorphic_public_result(
          expand_with_margins(
            input,
            .grouping = grouping,
            .margin_label = margin_label
          ) |>
            dplyr::collect(),
          case_label,
          "grouping-set occurrence permutation",
          "expand_with_margins()",
          "RSQLite"
        )
      })

      expected_summary_a <- if (identical(case, "zero-row character key")) {
        NA_character_
      } else {
        c(NA_character_, NA_character_)
      }
      expected_expansion_a <- if (identical(case, "zero-row character key")) {
        character()
      } else {
        c(NA_character_, NA_character_)
      }
      for (index in seq_along(specifications)) {
        expect_identical(
          summaries[[index]]$a,
          expected_summary_a,
          label = metamorphic_label(
            case_label,
            "grouping-set occurrence permutation",
            "summarize_with_margins() character-key type",
            "RSQLite"
          )
        )
        expect_identical(
          expansions[[index]]$a,
          expected_expansion_a,
          label = metamorphic_label(
            case_label,
            "grouping-set occurrence permutation",
            "expand_with_margins() character-key type",
            "RSQLite"
          )
        )
      }

      expect_metamorphic_multiset(
        summaries[[1L]],
        summaries[[2L]],
        case_label,
        "grouping-set occurrence permutation",
        "summarize_with_margins()",
        "RSQLite"
      )
      expect_metamorphic_multiset(
        expansions[[1L]],
        expansions[[2L]],
        case_label,
        "grouping-set occurrence permutation",
        "expand_with_margins()",
        "RSQLite"
      )
    }
  }
})

# The positive integer measure keeps conservation separate from the missing- and
# zero-denominator examples in `test-share.R`. The first and last rows are a
# duplicate, and both the fixed and variable keys carry a missing value.
share_metamorphic_input <- function() {
  data.frame(
    fixed = c("north", "north", NA_character_, NA_character_, "north"),
    region = c("east", NA_character_, "west", "west", "east"),
    store = c("a", "a", NA_character_, "b", "a"),
    measure = c(2L, 3L, 5L, 7L, 2L)
  )
}

share_metamorphic_summary <- function(input, check_share_source = TRUE) {
  rlang::inject(summarize_with_margins(
    input,
    measure_total = sum(.data$measure),
    parent_share = share_of_parent(!!rlang::sym("measure_total")),
    total_share = share_of_total(!!rlang::sym("measure_total")),
    .by = !!rlang::sym("fixed"),
    .grouping = rollup(!!rlang::sym("region"), !!rlang::sym("store")),
    .id = "occurrence",
    .margin_label = NULL,
    .check_share_source = check_share_source
  )) |>
    dplyr::collect()
}

expect_share_conservation <- function(result, case, backend) {
  total_partitions <- result |>
    dplyr::group_by(.data$fixed, .data$occurrence) |>
    dplyr::summarise(share_sum = sum(.data$total_share), .groups = "drop")
  expect_equal(
    total_partitions$share_sum,
    rep(1, nrow(total_partitions)),
    label = metamorphic_label(
      case,
      "Total shares sum to one per fixed-key and grouping occurrence",
      "summarize_with_margins()",
      backend
    )
  )

  # `rollup(region, store)` orders details, regional subtotals, then Grand
  # totals. The first two occurrences consequently have the stated immediate
  # parents, while each Grand-total row is its own Parent share of one.
  detail_parents <- result |>
    dplyr::filter(.data$occurrence == 1L) |>
    dplyr::group_by(.data$fixed, .data$region) |>
    dplyr::summarise(share_sum = sum(.data$parent_share), .groups = "drop")
  subtotal_parents <- result |>
    dplyr::filter(.data$occurrence == 2L) |>
    dplyr::group_by(.data$fixed) |>
    dplyr::summarise(share_sum = sum(.data$parent_share), .groups = "drop")
  parent_partitions <- dplyr::bind_rows(detail_parents, subtotal_parents)
  expect_equal(
    parent_partitions$share_sum,
    rep(1, nrow(parent_partitions)),
    label = metamorphic_label(
      case,
      "Parent shares sum to one beneath each immediate parent",
      "summarize_with_margins()",
      backend
    )
  )
}

expect_share_relations <- function(input,
                                   case,
                                   backend,
                                   check_share_source = TRUE) {
  baseline <- metamorphic_public_result(
    share_metamorphic_summary(input, check_share_source),
    case,
    "identity",
    "summarize_with_margins()",
    backend
  )
  expect_share_conservation(baseline, case, backend)

  scale <- 5L
  scaled <- metamorphic_public_result(
    share_metamorphic_summary(
      dplyr::mutate(input, measure = .data$measure * scale),
      check_share_source
    ),
    case,
    "multiply every positive source measure by one nonzero constant",
    "summarize_with_margins()",
    backend
  )
  expect_metamorphic_multiset(
    scaled,
    dplyr::mutate(baseline, measure_total = .data$measure_total * scale),
    case,
    "source summary scales while Parent and Total shares stay unchanged",
    "summarize_with_margins()",
    backend
  )

  duplicated <- metamorphic_public_result(
    share_metamorphic_summary(
      dplyr::union_all(input, input),
      check_share_source
    ),
    case,
    "duplicate every source row",
    "summarize_with_margins()",
    backend
  )
  expect_metamorphic_multiset(
    duplicated,
    dplyr::mutate(baseline, measure_total = .data$measure_total * 2L),
    case,
    "additive summary doubles while Parent and Total shares stay unchanged",
    "summarize_with_margins()",
    backend
  )
}

test_that("local shares conserve under scale and duplication", {
  expect_share_relations(
    share_metamorphic_input(),
    "duplicate rows with fixed and variable missing keys",
    "local"
  )
})

test_that("dtplyr shares conserve under scale and duplication", {
  skip_if_suggest_absent("dtplyr")

  expect_share_relations(
    dtplyr::lazy_dt(share_metamorphic_input()),
    "duplicate rows with fixed and variable missing keys",
    "dtplyr"
  )
})

test_that("RSQLite shares conserve under scale and duplication", {
  skip_if_suggest_absent("RSQLite", "DBI")

  source <- share_metamorphic_input()
  # SQLite converts ineligible sources, but this known `sum(integer)` source is
  # eligible before the test asks to bypass the dialect check.
  expect_type(source$measure, "integer")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    source,
    "metamorphic_share_sqlite_input",
    temporary = TRUE
  )

  expect_share_relations(
    input,
    "duplicate rows with fixed and variable missing keys",
    "RSQLite",
    check_share_source = FALSE
  )
})

test_that("DuckDB shares conserve under scale and duplication", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con,
    share_metamorphic_input(),
    "metamorphic_share_duckdb_input",
    temporary = TRUE
  )

  expect_share_relations(
    input,
    "duplicate rows with fixed and variable missing keys",
    "DuckDB"
  )
})
