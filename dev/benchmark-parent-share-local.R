benchmark_parent_share_local <- function(iterations = 3L,
                                         rows = 100000L,
                                         seed = 27L) {
  stopifnot(
    length(iterations) == 1L,
    iterations >= 1L,
    length(rows) == 1L,
    rows >= 1L,
    length(seed) == 1L
  )

  set.seed(seed)
  data <- data.frame(
    division = sample(50L, rows, replace = TRUE),
    region = sample(20L, rows, replace = TRUE),
    store = sample(10L, rows, replace = TRUE),
    item = sample(5L, rows, replace = TRUE),
    revenue = stats::runif(rows)
  )
  run <- function() {
    invisible(marginplyr::summarize_with_margins(
      data,
      revenue = sum(revenue),
      revenue_share = marginplyr::share_of_parent(revenue),
      .grouping = marginplyr::rollup(division, region, store, item),
      .margin_label = NULL
    ))
  }

  run()
  elapsed <- replicate(
    iterations,
    unname(system.time(run())[["elapsed"]])
  )
  data.frame(
    rows = rows,
    grouping_sets = 5L,
    iteration = seq_along(elapsed),
    elapsed_seconds = elapsed
  )
}

print(benchmark_parent_share_local())
