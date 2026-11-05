suppressMessages(pkgload::load_all(".", quiet = TRUE))
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

reads <- integer()
record <- function(values) {
  reads <<- c(reads, length(values))
  values
}
local_source <- tibble::new_tibble(
  list(g = c("a", "b"), .rows = 1:2, p = 1:2), nrow = 2L
)
root <- dtplyr::lazy_dt(local_source)
safe <- dplyr::filter(root, TRUE)
unsafe <- dplyr::mutate(root, p = record(p))
stopifnot(identical(dplyr::collect(unsafe), local_source))
stopifnot(identical(reads, 2L))
reads <- integer()

checked <- function(label, expected, expr) {
  result <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(result, "marginplyr_error"))
  first_line <- strsplit(conditionMessage(result), "\n", fixed = TRUE)[[1L]][1L]
  cat(label, first_line, "\n")
  grepl(expected, first_line, fixed = TRUE)
}
cases <- function(source, label, metadata_refusal = FALSE) {
  diagnose <- function(label, expected, expr) {
    if (metadata_refusal) expected <- "Can't safely determine column metadata"
    checked(label, expected, expr)
  }
  c(
    diagnose(paste(label, "list name overlaps fixed key"),
            "must not be a grouping column",
            nest_with_margins(source, .by = g, .key = "g")),
    diagnose(paste(label, "list name overlaps dimension"),
            "must not be a grouping column",
            nest_with_margins(source, .grouping = rollup(g), .key = "g")),
    diagnose(paste(label, "rowwise fixed key"),
            "cannot be a row-wise identifier",
            nest_by_with_margins(source, .by = .rows)),
    diagnose(paste(label, "rowwise dimension"),
            "cannot be a row-wise identifier",
            nest_by_with_margins(source, .grouping = grouping_set(.rows))),
    diagnose(paste(label, "rowwise set identifier"),
            "cannot be a row-wise identifier",
            nest_by_with_margins(dplyr::rename(source, outer = .rows),
                                 .id = ".rows"))
  )
}

stopifnot(all(cases(local_source, "local")), all(cases(safe, "safe")))
observations <- cases(unsafe, "unsafe", metadata_refusal = !assert)
stopifnot(length(reads) == 0L)
stopifnot(checked("unsafe with valid list name",
                  "Can't safely determine column metadata",
                  nest_with_margins(unsafe, .by = g, .key = "nested")))
if (!all(observations)) {
  stop("Nesting admission did not give the locally resolvable diagnostic.")
}
