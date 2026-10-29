suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
assert_expected <- "--assert" %in% commandArgs(trailingOnly = TRUE)
failures <- character()
checks <- 0L
capture <- function(expr) {
  tryCatch(force(expr), error = identity)
}
for (empty in c(FALSE, TRUE)) {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  if (empty) data <- data[FALSE, ]
  directory <- tempfile("margin-arrow-order-")
  dir.create(directory)
  arrow::write_parquet(data, file.path(directory, "data.parquet"))
  sources <- list(table = arrow::arrow_table(data),
                  dataset = arrow::open_dataset(directory))
  for (kind in names(sources)) for (descending in c(FALSE, TRUE)) {
    source <- if (descending) {
      dplyr::arrange(sources[[kind]], dplyr::desc(g))
    } else {
      dplyr::arrange(sources[[kind]], g)
    }
    ordinary <- dplyr::collect(dplyr::summarise(
      source, total = sum(x), .by = g
    ))
    expected <- data.frame(g = data$g, total = data$x)
    normalize <- function(value) {
      value <- as.data.frame(value)
      value <- value[order(value$g), , drop = FALSE]
      rownames(value) <- NULL
      value
    }
    stopifnot(identical(names(ordinary), c("g", "total")),
              identical(normalize(ordinary), normalize(expected)))
    for (fixed in c(FALSE, TRUE)) for (sorting in c("none", "first", "last")) {
      invoke <- function(input) {
        if (fixed) {
          summarize_with_margins(input, total = sum(x), .by = g,
                                 .sort = sorting)
        } else {
          summarize_with_margins(input, total = sum(x),
                                 .grouping = grouping_set(g), .sort = sorting)
        }
      }
      key <- paste(empty, kind, descending, fixed, sorting, sep = "/")
      query <- invoke(source)
      stopifnot(inherits(query, "arrow_dplyr_query"))
      actual <- capture(dplyr::collect(query))
      collapsed <- dplyr::collect(invoke(dplyr::collapse(source)))
      stopifnot(identical(names(collapsed), c("g", "total")),
                identical(normalize(collapsed), normalize(expected)))
      checks <- checks + 1L
      if (inherits(actual, "error")) {
        stopifnot(grepl("Invalid sort key column", conditionMessage(actual),
                        fixed = TRUE))
        failures <- c(failures, key)
        cat("FAIL", key, strsplit(conditionMessage(actual), "\n")[[1L]][[1L]],
            "\n")
      } else {
        stopifnot(isTRUE(all.equal(as.data.frame(actual),
                                   as.data.frame(collapsed))))
        cat("PASS", key, "\n")
      }
    }
  }
  unlink(directory, recursive = TRUE)
}
cat("Compared", checks, "cases;", length(failures),
    "arranged-input failures; ordinary and collapse controls passed.\n")
if (assert_expected && length(failures)) {
  stop("Arranged Arrow summaries must execute without private sort-key errors.",
       call. = FALSE)
}
