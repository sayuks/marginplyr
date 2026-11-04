# Dated control for callback sensitivity and native secondary-index writes.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
source <- data.table::data.table(g = c("a", "b"), k = 1:2)
callbacks <- 0L
counted <- function(x) { callbacks <<- callbacks + 1L; rep(TRUE, length(x)) }
q <- dtplyr::lazy_dt(source) |> filter(counted(k))
inspect_grouping(q, .grouping = rollup(where(is.character)))
lazy <- nest_with_margins(q, .grouping = rollup(g))
stopifnot(callbacks == 0L)
collect(q)
stopifnot(callbacks == 1L)
cat("Effectful-filter control: construction 0 callbacks; explicit native collection 1\n")
for (automatic_index in c(TRUE, FALSE)) {
  options(datatable.auto.index = automatic_index)
  input <- data.table::data.table(g = c("a", "b"), k = 1:2)
  before <- serialize(input, NULL)
  native <- dtplyr::lazy_dt(input) |> filter(k == 2L) |> collect()
  changed <- !identical(before, serialize(input, NULL))
  stopifnot(identical(changed, automatic_index), identical(native$g, "b"))
  cat("Native oracle auto.index", automatic_index, ": serialized source changed", changed, "; source values retained\n")
}
cat(R.version.string, "\n")
print(Sys.info()[c("sysname", "release", "machine")])
