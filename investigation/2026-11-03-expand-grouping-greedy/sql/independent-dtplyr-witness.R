Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
  key <- stats::dist(matrix(c(3, 8), ncol = 1L))
  source <- data.table::as.data.table(tibble(g = key, payload = 19L))
  before <- data.table::copy(source)
  input <- dtplyr::lazy_dt(source, immutable = TRUE)
  missing <- vctrs::vec_init(key, 1L)
  control <- collect(union_all(input, mutate(input, g = !!missing)))
  stopifnot(inherits(control$g, "dist"), nrow(control) == 2L,
            identical(as.numeric(control$g), c(5, NA_real_)))
  result <- tryCatch(collect(expand_with_margins(input,
    .grouping = grouping_sets(grouping_set(), grouping_set(g)),
    .margin_label = NA_character_)), error = identity)
  stopifnot(identical(source, before))
  cat("Independent ordinary dist control succeeded with typed values 5, NA.\n")
  cat("Independent expansion:", if (inherits(result, "error")) conditionMessage(result) else "succeeded", "\n")
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(!inherits(result, "error"), inherits(result$g, "dist"),
              identical(sort(as.numeric(result$g), na.last = TRUE), c(5, NA_real_)))
  }
})
