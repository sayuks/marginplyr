args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
ordinary_subset <- utils::getS3method("[", "data.table", envir = asNamespace("data.table"))
registry <- get(".__S3MethodsTable__.", envir = baseenv())
registered_before <- get("[.data.table", envir = registry)

probe <- function(mask, position, consumer, graph = "arithmetic") {
  source <- data.table::data.table(g = c("a", "b"), v = 1:2)
  original <- serialize(source, NULL)
  seen <- integer()
  if (mask) {
    `[.data.table` <- function(x, ...) {
      seen <<- c(seen, nrow(x))
      value <- ordinary_subset(x, ...)
      if (nrow(x) == 0L && is.data.frame(value) && "g" %in% names(value)) {
        data.table::set(value, j = "g", value = integer())
      }
      value
    }
  }
  input <- dtplyr::lazy_dt(source)
  if (graph == "arithmetic") {
    input <- mutate(input, v = v + 1L)
  } else if (graph == "filter") {
    input <- filter(input, v > 0L)
  }
  native <- collect(input)
  stopifnot(identical(native$g, c("a", "b")))
  native_seen <- seen
  seen <- integer()
  fun <- switch(consumer, inspect = inspect_grouping,
                nest = nest_with_margins, rowwise = nest_by_with_margins)
  value <- if (position == "dimension") {
    fun(input, .grouping = grouping_set(where(is.character)))
  } else if (position == "fixed") {
    fun(input, .by = where(is.character), .grouping = grouping_set())
  } else {
    fun(input, .grouping = grouping_set(g))
  }
  construction_seen <- seen
  if (consumer == "nest") value <- collect(value)
  good <- if (consumer == "inspect") {
    column <- if (position == "fixed") "fixed" else "included"
    identical(value[[column]], "(g)")
  } else {
    "g" %in% names(value) && nrow(value) == 2L && identical(sort(value$g), c("a", "b"))
  }
  stopifnot(identical(original, serialize(source, NULL)))
  stopifnot(identical(registered_before, get("[.data.table", envir = registry)))
  data.frame(mask, graph, position, consumer, good,
             native_rows = paste(native_seen, collapse = ","),
             construction_rows = paste(construction_seen, collapse = ","),
             all_margin_rows = paste(seen, collapse = ","))
}

minimal <- do.call(rbind, lapply(c(FALSE, TRUE), function(mask) {
  do.call(rbind, lapply(c("dimension", "fixed"), function(position) {
    do.call(rbind, lapply(c("inspect", "nest", "rowwise"), function(consumer) {
      probe(mask, position, consumer)
    }))
  }))
}))
print(minimal, row.names = FALSE)
stopifnot(all(minimal$good[!minimal$mask]))
if ("--assert" %in% args) stopifnot(all(minimal$good))

if ("--matrix" %in% args) {
  matrix <- expand.grid(mask = c(FALSE, TRUE),
                        graph = c("root", "arithmetic", "filter"),
                        position = c("dimension", "fixed", "name"),
                        consumer = c("inspect", "nest", "rowwise"), stringsAsFactors = FALSE)
  results <- do.call(rbind, lapply(seq_len(nrow(matrix)), function(i) {
    do.call(probe, as.list(matrix[i, ]))
  }))
  print(results, row.names = FALSE)
  write.csv(results, file = Sys.getenv("MARGINPLYR_MATRIX_OUTPUT", "/tmp/bracket-method-matrix.csv"), row.names = FALSE)
  print(with(results, table(mask, graph, good)))
}
cat("R:", as.character(getRversion()), "\n")
for (pkg in c("dplyr", "dtplyr", "data.table", "tidyselect", "vctrs")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
