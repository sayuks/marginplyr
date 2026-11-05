suppressMessages(pkgload::load_all(".", quiet = TRUE))
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

probe <- function(operation, graph, mutate_source = FALSE) {
  reads <- integer()
  source <- data.table::data.table(g = c("a", "b"), row = 1:2)
  head <- function(x, n) {
    reads <<- c(reads, nrow(x))
    if (mutate_source) {
      data.table::set(x, j = "g", value = "changed")
    }
    zero <- utils::head(x, n)
    data.table::set(zero, j = "g", value = integer())
    zero
  }
  query <- switch(
    graph,
    root = dtplyr::lazy_dt(source),
    filtered = dplyr::filter(dtplyr::lazy_dt(source), TRUE),
    mutable = dtplyr::lazy_dt(source, immutable = FALSE)
  )
  stopifnot(identical(dplyr::collect(query)$g, c("a", "b")))
  groups <- grouping_set(dplyr::where(is.character))
  result <- switch(
    operation,
    named = inspect_grouping(query, .grouping = grouping_set(g)),
    typed = inspect_grouping(query, .grouping = groups),
    fixed = inspect_grouping(query, .by = dplyr::where(is.character)),
    nest = nest_with_margins(query, .grouping = groups),
    rowwise = nest_by_with_margins(query, .grouping = groups)
  )
  construction_reads <- reads
  if (inherits(result, "dtplyr_step")) {
    result <- dplyr::collect(result)
  }
  nesting <- operation %in% c("nest", "rowwise")
  observed <- if (nesting) names(result) else if (operation == "fixed") {
    result$fixed
  } else {
    result$included
  }
  cat(operation, graph, mutate_source, "source sizes",
      toString(construction_reads), "columns", toString(observed),
      "rows", nrow(result), "source after", toString(source$g), "\n")
  expected <- if (nesting) c("g", "data") else "(g)"
  identical(observed, expected) && !length(construction_reads) &&
    identical(source$g, c("a", "b")) &&
    nrow(result) == if (nesting) 2L else 1L
}

controls <- c(
  probe("named", "root"),
  probe("typed", "filtered"),
  probe("fixed", "filtered"),
  probe("nest", "filtered"),
  probe("rowwise", "filtered"),
  probe("typed", "mutable")
)
stopifnot(all(controls))
observations <- c(
  probe("typed", "root"),
  probe("fixed", "root"),
  probe("nest", "root"),
  probe("rowwise", "root"),
  probe("typed", "root", mutate_source = TRUE)
)
if (assert && !all(observations)) {
  stop("Immutable-root preparation did not preserve the schema and source.")
}
