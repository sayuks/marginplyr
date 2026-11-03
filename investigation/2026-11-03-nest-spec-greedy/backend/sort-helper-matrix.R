# Finite late-binding matrix for the shared dtplyr Margin-order expressions.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
outfile <- if (length(args)) args[[1L]] else "sort-helper-results.csv"
normalize <- function(out) {
  if (inherits(out, "dtplyr_step")) out <- dplyr::collect(out)
  out <- dplyr::ungroup(out)
  out$data <- lapply(out$data, function(cell) tibble::as_tibble(cell, .name_repair = "minimal"))
  tibble::as_tibble(out, .name_repair = "minimal")
}
cell_set <- function(out) {
  sort(vapply(seq_len(nrow(out)), function(i) {
    paste(as.character(serialize(as.list(out[i, , drop = FALSE]), NULL)), collapse = "")
  }, ""))
}
results <- list()
for (helper in c("is.na", "%in%")) for (key in c("g", ".SD")) {
  for (type in c("character", "integer", "factor")) for (size in c(0L, 3L)) {
    vals <- switch(type, character = c("b", NA_character_, "a"),
                   integer = c(2L, NA_integer_, 1L),
                   factor = factor(c("b", NA_character_, "a"), levels = c("b", "a", "unused")))
    vals <- vals[seq_len(size)]
    for (role in c("fixed", "dimension")) for (order in c("none", "last", "first")) {
      for (keep in c(FALSE, TRUE)) for (verb in c("nest", "nest_by")) {
        root <- new.env(parent = globalenv())
        root$source <- data.table::data.table(value = vals, row_id = seq_len(size))
        data.table::setnames(root$source, "value", key)
        before <- serialize(root$source, NULL)
        input <- evalq(dtplyr::lazy_dt(source), root)
        spec <- if (role == "dimension") rlang::inject(rollup(!!rlang::sym(key))) else NULL
        fixed <- if (role == "fixed") key else character()
        fun <- if (verb == "nest") nest_with_margins else nest_by_with_margins
        run <- function(x) fun(x, .grouping = spec, .by = dplyr::all_of(fixed),
                               .id = "set", .sort = order, .keep = keep,
                               .margin_label = NULL)
        expected <- normalize(run(input))
        local <- normalize(run(tibble::as_tibble(root$source, .name_repair = "minimal")))
        stopifnot(if (order == "none") identical(cell_set(expected), cell_set(local)) else identical(expected, local))
        query <- if (verb == "nest") run(input) else NULL
        reads <- 0L
        root[[helper]] <- if (helper == "is.na") {
          function(x) { reads <<- reads + 1L; !base::is.na(x) }
        } else {
          function(x, table) { reads <<- reads + 1L; !base::`%in%`(x, table) }
        }
        actual <- normalize(if (verb == "nest") query else run(input))
        unchanged_source <- identical(serialize(root$source, NULL), before)
        unchanged_cells <- identical(cell_set(actual), cell_set(expected))
        matched <- identical(actual, expected)
        stopifnot(unchanged_source, unchanged_cells)
        results[[length(results) + 1L]] <- data.frame(helper, key, type, size, role,
          order, keep, verb, matched, callback_reads = reads, unchanged_source, unchanged_cells)
      }
    }
  }
}
result <- dplyr::bind_rows(results)
write.csv(result, outfile, row.names = FALSE)
print(with(result, table(helper, matched)))
print(with(result, table(helper, order, matched)))
cat("Completed comparisons:", nrow(result), "\n")
cat("Unchanged source/cell-set:", all(result$unchanged_source), all(result$unchanged_cells), "\n")
cat("Baseline: 4b34fdecb2d8a41034b3d9b5d5af8e51592555e1\n")
print(sessionInfo())
for (package in c("dplyr", "dtplyr", "data.table", "vctrs", "tibble", "tidyr")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
