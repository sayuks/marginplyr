# Run with an unused output directory and an optional isolated library.
# The --arrow-only mode does not load marginplyr.
args <- commandArgs(TRUE)
arrow_only <- "--arrow-only" %in% args
args <- setdiff(args, "--arrow-only")
stopifnot(length(args) %in% 1:2)
if (length(args) == 2L) .libPaths(c(args[[2L]], .libPaths()))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(dplyr))
stopifnot(
  packageVersion("arrow") == "25.0.1",
  packageVersion("dplyr") == "1.2.1"
)
set_cpu_count(1L)
set_io_thread_count(2L)
options(arrow.use_threads = FALSE)

out <- args[[1L]]
stopifnot(!file.exists(out))
dir.create(file.path(out, "data", "p=1"), recursive = TRUE)
file <- file.path(out, "data", "p=1", "one.parquet")
write_parquet(
  data.frame(row_id = 1L), file,
  chunk_size = 1L, compression = "uncompressed"
)
declared_schema <- schema(row_id = int32(), p = int32())
ds <- open_dataset(
  file.path(out, "data"), schema = declared_schema,
  partitioning = hive_partition(p = int32())
)
reader <- ParquetFileReader$create(file)
stopifnot(
  length(ds$files) == 1L, reader$num_rows == 1L,
  reader$num_row_groups == 1L,
  reader$GetSchema()$Equals(schema(row_id = int32())),
  ds$schema$Equals(declared_schema)
)
source <- collect(ds)
stopifnot(identical(source$row_id, 1L), identical(source$p, 1L))
expected <- data.frame(p = c(1L, NA_integer_), occurrence = 1:2, row_id = c(1L, 1L))
observed_wrong <- expected
observed_wrong$p <- c(1L, 1L)

# Compare a row bag, preserving multiplicity and integer types.
same_rows <- function(actual, expected) {
  normalize <- function(x) {
    x <- as.data.frame(x[, names(expected), drop = FALSE])
    x <- x[order(x$occurrence, x$row_id, x$p, na.last = TRUE), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  identical(normalize(actual), normalize(expected))
}
missing <- Scalar$create(NA_integer_, type = int32())
union_query <- union_all(
  mutate(ds, occurrence = 1L),
  mutate(ds, p = !!missing, occurrence = 2L)
)
before <- collect(union_query)
projected <- select(union_query, p, occurrence, row_id)
after <- collect(projected)
after_compute <- collect(compute(projected))
stopifnot(same_rows(before, expected))
stopifnot(
  !same_rows(before[-1L, ], expected),
  !same_rows(rbind(before, before[1L, ]), expected),
  !same_rows(observed_wrong, expected)
)
evidence <- list(
  source = source, expected = expected,
  union_before_projection = before,
  union_after_projection = after,
  union_after_compute = after_compute,
  dataset_schema = ds$schema$ToString(),
  parquet_schema = reader$GetSchema()$ToString(),
  file_count = length(ds$files), row_groups = reader$num_row_groups,
  file_hash = unname(tools::md5sum(file)),
  versions = c(arrow = as.character(packageVersion("arrow")),
               dplyr = as.character(packageVersion("dplyr")))
)
violation <- same_rows(after, observed_wrong) && same_rows(after_compute, observed_wrong)

if (!arrow_only) {
  suppressPackageStartupMessages(library(marginplyr))
  if (length(args) == 2L) {
    stopifnot(startsWith(find.package("marginplyr"), normalizePath(args[[2L]])))
  }
  query <- expand_with_margins(
    ds, .grouping = rollup(p), .id = "occurrence", .margin_label = NULL
  )
  actual <- collect(query)
  computed <- collect(compute(query))
  repeated <- collect(query)
  table_control <- collect(expand_with_margins(
    Table$create(source, schema = declared_schema),
    .grouping = rollup(p), .id = "occurrence", .margin_label = NULL
  ))
  summary <- collect(summarize_with_margins(
    ds, n = n(), mask = grouping_id(),
    .grouping = rollup(p), .id = "occurrence", .margin_label = NULL
  ))
  summary_expected <- data.frame(
    p = c(1L, NA_integer_), occurrence = 1:2, n = c(1, 1), mask = c(0, 1)
  )
  summary <- as.data.frame(summary[order(summary$occurrence), ])
  stopifnot(
    same_rows(table_control, expected),
    identical(summary$p, summary_expected$p),
    identical(summary$occurrence, summary_expected$occurrence),
    all(summary$n == summary_expected$n), all(summary$mask == summary_expected$mask)
  )
  evidence$margin_actual <- actual
  evidence$margin_computed <- computed
  evidence$margin_repeated <- repeated
  evidence$table_control <- table_control
  evidence$summary_control <- summary
  evidence$versions <- c(evidence$versions,
                        marginplyr = as.character(packageVersion("marginplyr")))
  violation <- violation && same_rows(actual, observed_wrong) &&
    same_rows(computed, observed_wrong) && same_rows(actual, repeated)
} else {
  stopifnot(!"marginplyr" %in% loadedNamespaces())
}

stopifnot(identical(unname(tools::md5sum(file)), evidence$file_hash))
evidence$violation_reproduced <- violation
saveRDS(evidence, file.path(out, "evidence.rds"))
capture.output(evidence, file = file.path(out, "evidence.txt"))
print(evidence)
cat("VIOLATION REPRODUCED:", violation, "\n")
# Exit zero witnesses the historical defect, rather than a repaired result.
stopifnot(violation)
