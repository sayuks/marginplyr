# Dated classifier for the finite graph matrix's conservative refusals.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (size in c(0L, 1L, 4L)) {
  source <- data.table::data.table(
    k = rep(c(1L, 1L, 2L, NA_integer_), length.out = size),
    g = rep(c("a", NA_character_, "b", "a"), length.out = size),
    row_id = seq_len(size), v = as.double(seq_len(size))
  )
  q <- dtplyr::lazy_dt(source) |>
    mutate(z = (v / 2 + row_id) * -1) |>
    rename(value = z)
  answer <- tryCatch(inspect_grouping(q, .by = where(is.character), .format = "list"), error = identity)
  stopifnot(inherits(answer, "marginplyr_error"), grepl("Can't safely determine column metadata", conditionMessage(answer), fixed = TRUE))
  cat(size, "arithmetic", class(answer)[[1L]], conditionMessage(answer), "\n")
}
