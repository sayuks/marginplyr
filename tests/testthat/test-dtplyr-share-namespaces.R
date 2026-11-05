test_that("dtplyr shares retain displayed ratios under namespace dispatch", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), ratio = share_of_total(value),
    .grouping = rollup(g), .sort = "last"
  )
  root$`::` <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (identical(package, "base") && identical(member, "as.double")) {
      return(function(x) rep(10, length(x)))
    }
    getExportedValue(package, member)
  }
  result <- dplyr::collect(query)
  expect_identical(result$value, c(1, 3, 4))
  expect_identical(result$ratio, c(0.25, 0.75, 1))
})
