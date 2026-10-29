test_that("arranged Arrow summaries retain public keys and Margin order", {
  skip_if_suggest_absent("arrow")
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path))
  for (empty in c(FALSE, TRUE)) {
    data <- data.frame(g = c("b", "a"), v = c(3, 1))
    if (empty) data <- data[FALSE, ]
    arrow::write_parquet(data, path)
    sources <- list(table = arrow::arrow_table(data),
                    dataset = arrow::open_dataset(path))
    for (shape in names(sources)) {
      for (descending in c(FALSE, TRUE)) {
        source <- sources[[shape]]
        arranged <- if (descending) {
          dplyr::arrange(source, dplyr::desc(g))
        } else {
          dplyr::arrange(source, g)
        }
        ordinary <- dplyr::collect(dplyr::summarize(
          arranged, total = sum(v), .by = g
        ))
        expected <- data.frame(g = c("a", "b"), total = c(1, 3))
        if (empty) expected <- expected[FALSE, ]
        expect_identical(arrow_partition_bag(ordinary), expected)
        for (input in list(arranged, source, dplyr::collapse(arranged))) {
          for (role in c("fixed", "included", "rollup")) {
            for (sort in c("none", "first", "last")) {
              info <- paste(empty, shape, descending, role, sort)
              query <- if (role == "fixed") {
                summarize_with_margins(
                  input, total = sum(v), .by = g, .sort = sort
                )
              } else {
                grouping <- if (role == "included") {
                  grouping_set(g)
                } else {
                  rollup(g)
                }
                summarize_with_margins(
                  input, total = sum(v), .grouping = grouping,
                  .margin_label = NULL, .sort = sort
                )
              }
              want <- expected
              if (role == "rollup") {
                total <- data.frame(
                  g = NA_character_, total = if (empty) 0 else 4
                )
                want <- if (sort == "first") {
                  rbind(total, want)
                } else {
                  rbind(want, total)
                }
              }
              expect_s3_class(query, "arrow_dplyr_query")
              expect_identical(names(query), c("g", "total"), info = info)
              actual <- dplyr::collect(query)
              expect_identical(arrow_partition_bag(actual),
                               arrow_partition_bag(want), info = info)
              if (sort != "none") {
                expect_identical(as.data.frame(actual), want, info = info)
              }
            }
          }
        }
      }
    }
  }
})
