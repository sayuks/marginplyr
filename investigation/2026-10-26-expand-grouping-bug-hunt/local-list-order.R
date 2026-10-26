# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
assert_mode <- "--assert" %in% commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(g = list(NA_integer_, NULL), payload = 1:2)
stopifnot(identical(vctrs::vec_detect_missing(input$g), c(FALSE, TRUE)))
stopifnot(identical(dplyr::arrange(input, g)$payload, 1:2))
verbs <- list(
  expand = function(data, sort) {
    expand_with_margins(data, .by = g, .sort = sort)
  },
  summarize = function(data, sort) {
    summarize_with_margins(data, payload = sum(payload), .by = g, .sort = sort)
  },
  nest = function(data, sort) {
    nest_with_margins(data, .by = g, .sort = sort)
  },
  nest_by = function(data, sort) {
    nest_by_with_margins(data, .by = g, .sort = sort)
  }
)
failures <- 0L
for (name in names(verbs)) {
  for (sort in c("first", "last")) {
    got <- verbs[[name]](input, sort)
    missing <- vctrs::vec_detect_missing(got$g)
    passed <- identical(missing, c(FALSE, TRUE))
    cat(name, sort, "missing flags:", missing, "contract:", passed, "\n")
    failures <- failures + as.integer(!passed)
    # A classed list follows vctrs missingness in its is.na() method.
    control <- input
    control$g <- vctrs::list_of(NA_integer_, NULL)
    expected <- verbs[[name]](control, sort)
    stopifnot(identical(vctrs::vec_detect_missing(expected$g), c(FALSE, TRUE)))
  }
}
for (sort in c("first", "last")) {
  got <- expand_with_margins(
    input, .grouping = rollup(g), .margin_label = NULL,
    .id = "set", .sort = sort
  )
  stopifnot(nrow(got) == 4L, identical(sort(got$payload), c(1L, 1L, 2L, 2L)))
  detail <- got[got$set == 1L, , drop = FALSE]
  passed <- identical(detail$payload, 1:2)
  cat("dimension", sort, "detail payload:", detail$payload, "contract:", passed, "\n")
  failures <- failures + as.integer(!passed)
}
cat("Desired-contract failures:", failures, "\n")
if (assert_mode) stopifnot(failures == 0L)
