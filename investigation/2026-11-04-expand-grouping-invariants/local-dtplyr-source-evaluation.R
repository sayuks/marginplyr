pkgload::load_all(".", quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)

probe <- function(shape, single, label, sort) {
  calls <- 0L
  first_read <- function(x) {
    calls <<- calls + 1L
    x == 19L & calls == 1L
  }
  input <- data.table::data.table(g = "a", payload = 19L)
  before <- data.table::copy(input)
  root <- dtplyr::lazy_dt(input)
  src <- switch(shape,
    filtered = filter(root, first_read(payload)),
    projected = select(filter(root, first_read(payload)), payload, g),
    cached_after = compute(filter(root, first_read(payload)), name = "selected"),
    cached_before = filter(compute(root, name = "original"), first_read(payload))
  )

  one_read <- collect(src)
  stopifnot(calls == 1L, nrow(one_read) == 1L, one_read$payload == 19L)
  calls <- 0L

  # Copy a single source answer explicitly. No Margin verb supplies this oracle.
  expected <- transmute(one_read, g, sid = 1L, payload)
  if (!single) {
    omitted <- mutate(expected, g = if (is.null(label)) NA_character_ else label,
                      sid = 2L)
    expected <- bind_rows(expected, omitted)
  }
  if (sort == "first" && !single) {
    expected <- expected[2:1, ]
  }
  specification <- if (single) grouping_set(g) else rollup(g)
  result <- expand_with_margins(
    src, .grouping = specification, .margin_label = label,
    .sort = sort, .id = "sid"
  )
  stopifnot(inherits(result, "dtplyr_step"), calls == 0L)
  actual <- collect(result)
  stopifnot(identical(input, before))
  if (sort == "none") {
    actual <- arrange(actual, sid)
  }
  cat(shape, if (single) "single" else "rollup", sort,
      if (is.null(label)) "NULL" else label,
      "source calls", calls, "expected/actual rows", nrow(expected), nrow(actual),
      "actual ids", paste(actual$sid, collapse = ","), "\n")
  identical(actual, expected) && calls == 1L
}

matches <- logical()
for (shape in c("filtered", "projected", "cached_after", "cached_before")) {
  for (single in c(TRUE, FALSE)) {
    for (label in list(NULL, "Total")) {
      for (sort in c("none", "first", "last")) {
        matches <- c(matches, probe(shape, single, label, sort))
      }
    }
  }
}
cat("Matching source evaluations:", sum(matches), "of", length(matches), "\n")
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(all(matches))
}
