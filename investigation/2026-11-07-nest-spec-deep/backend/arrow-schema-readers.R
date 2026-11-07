Sys.setenv(TESTTHAT_PARALLEL = "false")
source_root <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE))))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
fixtures <- list(
  boolean = function() arrow::Array$create(c(TRUE, NA), type = arrow::boolean()),
  int8 = function() arrow::Array$create(1:2, type = arrow::int8()),
  int64 = function() arrow::Array$create(1:2, type = arrow::int64()),
  float32 = function() arrow::Array$create(c(1.5, NA), type = arrow::float32()),
  decimal = function() arrow::Array$create(c(1.5, NA), type = arrow::decimal128(12, 2)),
  utf8 = function() arrow::Array$create(c("a", NA)),
  binary = function() arrow::Array$create(list(as.raw(1:2), NULL), type = arrow::binary()),
  date32 = function() arrow::Array$create(as.Date("2020-01-01") + 0:1),
  timestamp = function() arrow::Array$create(as.POSIXct("2020-01-01", tz = "Asia/Tokyo") + 0:1, type = arrow::timestamp("ns", "Asia/Tokyo")),
  duration = function() arrow::Array$create(as.difftime(1:2, units = "hours"), type = arrow::duration("ns")),
  factor = function() arrow::Array$create(factor(c("a", "b"), levels = c("a", "b", "unused"))),
  ordered = function() arrow::Array$create(ordered(c("a", "b"))),
  list = function() arrow::Array$create(list(1:2, 3:4), type = arrow::list_of(arrow::int32())),
  fixed_list = function() arrow::Array$create(list(1:2, 3:4), type = arrow::fixed_size_list_of(arrow::int32(), 2L)),
  struct = function() arrow::Array$create(data.frame(a = 1:2, b = c("a", "b")))
)
predicates <- list(character = is.character, integer = is.integer,
                   numeric = is.numeric, list = is.list, packed = is.data.frame,
                   date = function(x) inherits(x, "Date"),
                   difftime = function(x) inherits(x, "difftime"), factor = is.factor)
results <- list(); k <- 0L; readers <- 0L
for (type in names(fixtures)) {
  tab <- arrow::Table$create(key_chr = c("a", "b"), typed = fixtures[[type]]())
  oracle <- as.data.frame(tab)[0, , drop = FALSE]
  for (shape in c("table", "batch", "query", "reader", "reader_query")) {
    for (pn in names(predicates)) for (slot in c("dimension", "fixed")) {
      pred <- predicates[[pn]]
      reader <- NULL
      input <- switch(shape,
        table = tab,
        batch = arrow::as_record_batch(tab),
        query = select(tab, everything()),
        reader = {
          reader <- arrow::RecordBatchReader$create(arrow::as_record_batch(tab))
          reader
        },
        reader_query = {
          reader <- arrow::RecordBatchReader$create(arrow::as_record_batch(tab))
          select(reader, everything())
        })
      selected <- names(oracle)[vapply(oracle, pred, logical(1))]
      got <- if (slot == "dimension") {
        inspect_grouping(input, .grouping = grouping_set(where(pred)), .format = "list")$included[[1L]]
      } else {
        inspect_grouping(input, .by = where(pred), .format = "list")$fixed[[1L]]
      }
      if (!is.null(reader)) {
        remaining <- as.data.frame(reader)
        stopifnot(nrow(remaining) == 2L, identical(remaining$key_chr, c("a", "b")))
        readers <- readers + 1L
      }
      k <- k + 1L
      results[[k]] <- data.frame(type, shape, predicate = pn, slot,
                                selected = paste(got, collapse = ","),
                                expected = paste(selected, collapse = ","),
                                match = identical(got, selected))
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(out, "arrow-schema-readers.csv"), row.names = FALSE)
print(table(results$match))
cat(readers, "reader instances retained all source rows\n")
print(subset(results, !match), row.names = FALSE)
stopifnot(all(results$match))
