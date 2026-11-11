# Expected shares from ordinary dplyr summaries at each grain, one row per
# fixed key and dimension combination. `sets` lists each set's dimensions and
# `parents` the dimensions of the set holding each set's parent denominator.
logical_name_share_oracle <- function(data, keys, dimensions, sets, parents) {
  grain <- function(by) {
    dplyr::summarise(
      dplyr::group_by(data, dplyr::across(dplyr::all_of(by))),
      value = sum(v), .groups = "drop"
    )
  }
  rows <- lapply(seq_along(sets), function(index) {
    set <- sets[[index]]
    cells <- grain(c(keys, set))
    parent <- dplyr::rename(
      grain(c(keys, parents[[index]])), parent_value = "value"
    )
    total <- dplyr::rename(grain(keys), total_value = "value")
    cells <- dplyr::left_join(cells, parent, by = c(keys, parents[[index]]))
    cells <- dplyr::left_join(cells, total, by = keys)
    for (omitted in setdiff(dimensions, set)) {
      cells[[omitted]] <- "(all)"
    }
    cells$parent <- cells$value / cells$parent_value
    cells$total <- cells$value / cells$total_value
    cells[c(keys, dimensions, "value", "parent", "total")]
  })
  dplyr::bind_rows(rows)
}

test_that("dtplyr shares keep fixed keys named T and F as partitions", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(
    T = c("a", "a", "a", "a", "b", "b", "b", "b"),
    F = c("p", "p", "q", "q", "p", "p", "q", "q"),
    g = c("x", "y", "x", "y", "x", "x", "y", "y"),
    h = c("m", "n", "n", "m", "m", "n", "m", "n"),
    v = c(1, 2, 3, 4, 5, 6, 7, 9)
  )
  before <- data.table::copy(input)
  plans <- list(
    rollup_one = list(
      grouping = rlang::quo(rollup(g)), dimensions = "g",
      sets = list("g", character()), parents = list(character(), character()),
      helpers = c("share_of_parent", "share_of_total")
    ),
    rollup_two = list(
      grouping = rlang::quo(rollup(g, h)), dimensions = c("g", "h"),
      sets = list(c("g", "h"), "g", character()),
      parents = list("g", character(), character()),
      helpers = c("share_of_parent", "share_of_total")
    ),
    cube = list(
      grouping = rlang::quo(cube(g)), dimensions = "g",
      sets = list("g", character()), parents = list(character(), character()),
      helpers = "share_of_total"
    )
  )
  for (keys in list("T", "F", c("T", "F"), c("F", "T"))) {
    for (plan_name in names(plans)) {
      plan <- plans[[plan_name]]
      expected <- logical_name_share_oracle(
        tibble::as_tibble(input), keys, plan$dimensions, plan$sets,
        plan$parents
      )
      for (helper in plan$helpers) {
        column <- if (identical(helper, "share_of_parent")) "parent" else "total"
        for (style in c("direct", "across")) {
          share <- if (identical(style, "direct")) {
            rlang::exprs(share = !!rlang::call2(helper, rlang::sym("value")))
          } else {
            rlang::exprs(dplyr::across(
              dplyr::all_of("value"), !!rlang::sym(helper), .names = "share"
            ))
          }
          for (sort in c("first", "last")) {
            info <- paste(paste(keys, collapse = ","), plan_name, helper,
                          style, sort)
            query <- summarize_with_margins(
              dtplyr::lazy_dt(input, immutable = TRUE), value = sum(v),
              !!!share, .by = dplyr::all_of(keys), .grouping = !!plan$grouping,
              .margin_label = "(all)", .id = "set", .sort = sort
            )
            expect_s3_class(query, "dtplyr_step")
            actual <- tibble::as_tibble(dplyr::collect(query))
            expect_identical(
              names(actual),
              c(keys, plan$dimensions, "set", "value", "share"),
              info = info
            )
            for (key in keys) {
              expect_type(actual[[key]], "character")
            }
            # Each Margin pattern carries one set identifier.
            pattern <- do.call(paste, lapply(
              plan$dimensions, function(name) actual[[name]] == "(all)"
            ))
            expect_true(all(lengths(tapply(
              actual$set, pattern, unique, simplify = FALSE
            )) == 1L), info = info)
            order_rows <- function(data) {
              data[vctrs::vec_order(data[c(keys, plan$dimensions)]), ,
                   drop = FALSE]
            }
            actual <- order_rows(actual)
            reference <- order_rows(expected)
            expect_identical(
              actual[c(keys, plan$dimensions)],
              reference[c(keys, plan$dimensions)], info = info
            )
            expect_equal(actual$value, reference$value, info = info)
            expect_equal(actual$share, reference[[column]], info = info)
            expect_identical(input, before, info = info)
          }
        }
      }
    }
  }
})
