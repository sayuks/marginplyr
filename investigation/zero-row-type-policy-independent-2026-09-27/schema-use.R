args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else "."
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source("inst/suggests/guard.R")
stopifnot(marginplyr_suggest_available("arrow"))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
source <- dplyr::copy_to(con,
  data.frame(p = "A", g = "a", h = "b", v = 2), "source")
summarize <- function(input) {
  summarize_with_margins(input, mask = grouping_id(g, h),
    .by = p, .grouping = rollup(g, h), .id = "sid",
    .margin_label = NULL)
}
full <- dplyr::collect(summarize(source))
empty <- dplyr::collect(summarize(dplyr::filter(source, FALSE)))
cat("full/empty mask types:", typeof(full$mask), typeof(empty$mask), "\n")
for (typed in c(FALSE, TRUE)) {
  path <- tempfile("marginplyr-empty-schema-")
  dir.create(path)
  x <- empty[c("sid", "mask")]
  if (typed) {
    x$sid <- as.integer(x$sid)
    x$mask <- as.integer(x$mask)
  }
  arrow::write_parquet(x, file.path(path, "0-empty.parquet"))
  arrow::write_parquet(full[c("sid", "mask")], file.path(path, "1-full.parquet"))
  cat("\nrepaired empty:", typed, "\n")
  print(arrow::read_parquet(file.path(path, "0-empty.parquet"), as_data_frame = FALSE)$schema)
  for (unify in c(FALSE, TRUE)) {
    cat("unify_schemas:", unify, "\n")
    tryCatch({
      dataset <- arrow::open_dataset(path, unify_schemas = unify)
      print(dataset$schema)
      print(dplyr::collect(dataset))
    }, error = function(err) cat("ERROR:", conditionMessage(err), "\n"))
  }
}
DBI::dbDisconnect(con)
