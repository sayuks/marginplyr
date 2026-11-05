test_that("enclosing coverage preserves nested product occurrence order", {
  input <- tibble::tibble(a = c("A", "A"), b = "B", c = "C", value = 1:2)
  wrapped <- grouping_sets(
    grouping_spec(cube(b, a), grouping_sets(c, grouping_set())),
    grouping_set(c),
    grouping_spec(grouping_set(), cube(a))
  )
  required <- grouping_sets(grouping_set(a), grouping_set(b))
  specifications <- list(
    grouping_spec(!!wrapped, !!required),
    grouping_spec(!!required, !!wrapped)
  )
  included <- list(
    list(c("b", "a", "c"), c("b", "a"), c("b", "c"), "b",
         c("a", "c"), "a"),
    list(c("a", "b", "c"), c("a", "b"), c("a", "c"), "a",
         c("b", "c"), "b")
  )
  outer <- list(
    tibble::tibble(
      b = c("B", "B", "B", "B", "Total", "Total"),
      a = c("A", "A", "Total", "Total", "A", "A"),
      c = c("C", "Total", "C", "Total", "C", "Total"), set = 1:6
    ),
    tibble::tibble(
      a = c("A", "A", "A", "A", "Total", "Total"),
      b = c("B", "B", "Total", "Total", "B", "B"),
      c = c("C", "Total", "C", "Total", "C", "Total"), set = 1:6
    )
  )
  payload <- list(input[c("b", "a", "c", "value")], input)

  for (i in seq_along(specifications)) {
    plan <- inspect_grouping(
      input, .grouping = specifications[[i]], .duplicates = "drop",
      .format = "list"
    )
    expect_identical(plan$included, included[[i]])
    expect_identical(plan$set_id, 1:6)
    expect_identical(plan$grouping_id, 0:5)

    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(
        input, .grouping = specifications[[i]], .duplicates = "drop",
        .id = "set", .sort = "none", .keep = TRUE
      )
      result <- tibble::as_tibble(dplyr::arrange(result, .data$set))
      expect_identical(result[names(outer[[i]])], outer[[i]])
      expect_identical(result$data, rep(list(payload[[i]]), 6L))
    }
  }
})
