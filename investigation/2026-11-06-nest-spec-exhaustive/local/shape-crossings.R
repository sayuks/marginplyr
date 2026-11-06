args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args)) args[[1L]] else "."
pkgload::load_all(source_root, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
cat("SHA:", system2("git", c("-C", shQuote(source_root), "rev-parse", "HEAD"), stdout = TRUE), "\n")
cat(R.version.string, "\n")
base <- c(1, 1, 2, NA_real_)
shapes <- list(
  array4 = array(rep(base, 4), c(4, 2, 1, 2)),
  matrix0 = matrix(numeric(), 4, 0),
  array0 = array(numeric(), c(4, 0, 2)),
  logical4 = array(rep(base > 1, 4), c(4, 2, 1, 2)),
  character4 = array(rep(as.character(base), 4), c(4, 2, 1, 2)),
  complex4 = array(rep(as.complex(base), 4), c(4, 2, 1, 2)),
  list4 = array(rep(list(1L, 1L, 2L, NULL), 4), c(4, 2, 1, 2)),
  list0 = array(list(), c(4, 0, 2)),
  packed0 = tibble::new_tibble(list(), nrow = 4L),
  packed_array = tibble::tibble(a = array(rep(base, 2), c(4, 1, 2))),
  packed_nested = tibble::tibble(a = tibble::tibble(b = tibble::tibble(c = base))),
  packed_na_factor = tibble::tibble(a = structure(c(1L, 2L, NA_integer_, 1L), levels = c("a", NA), class = "factor")),
  packed_matrix0 = tibble::tibble(a = matrix(numeric(), 4, 0)),
  asis_matrix = I(matrix(rep(base, 2), 4, 2)),
  asis_array = I(array(rep(base, 4), c(4, 2, 1, 2))),
  asis_list = I(list(1L, 1L, 2L, NULL))
)
results <- list()
for (family in names(shapes)) for (empty in c(FALSE, TRUE)) for (by in c(FALSE, TRUE)) {
  for (verb in c("nest_with_margins", "nest_by_with_margins")) for (keep in c(FALSE, TRUE)) {
    for (sort in c("none", "first", "last")) {
      data <- tibble::tibble(f = c("x", "y", "x", "y"), g = shapes[[family]], rid = seq_len(4))
      if (empty) data <- vctrs::vec_slice(data, integer())
      before <- serialize(data, NULL)
      got <- tryCatch(do.call(get(verb), c(list(data, .grouping = rollup(g), .margin_label = NULL,
                           .id = "sid", .keep = keep, .sort = sort), if (by) list(.by = rlang::expr(f)))), error = identity)
      expected <- character()
      for (sid in 1:2) {
        keys <- c(if (by) "f", if (sid == 1) "g")
        loc <- if (length(keys)) vctrs::vec_group_loc(data[keys])$loc else if(nrow(data)) list(seq_len(nrow(data))) else list()
        for (ids in loc) expected <- c(expected, paste(sid, paste(data$rid[ids], collapse = ","), sep = ":"))
      }
      actual <- if (inherits(got, "error")) character() else vapply(seq_len(nrow(got)), function(i) paste(got$sid[[i]], paste(got$data[[i]]$rid, collapse = ","), sep = ":"), character(1))
      ok <- !inherits(got, "error") && identical(sort(expected), sort(actual)) && identical(serialize(data, NULL), before)
      if (ok) for (cell in got$data) {
        wanted <- if (keep) c(if (by) "f", "g", if (!by) "f", "rid") else c(if (!by) "f", "rid")
        ok <- ok && identical(names(cell), wanted)
        for (nm in wanted) ok <- ok && identical(cell[[nm]], vctrs::vec_slice(data[[nm]], cell$rid))
      }
      results[[length(results) + 1L]] <- data.frame(family, empty, by, verb, keep, sort, ok,
        error = if (inherits(got, "error")) conditionMessage(got) else "")
    }
  }
}
results <- do.call(rbind, results)
path <- if (length(args) > 1L) args[[2L]] else file.path(tempdir(), "shape-crossings.csv")
utils::write.csv(results, path, row.names = FALSE)
cat("comparisons:", nrow(results), "matches:", sum(results$ok), "failures:", sum(!results$ok), "\n")
if (!all(results$ok)) print(unique(results[!results$ok, c("family", "error")]), row.names = FALSE)
stopifnot(all(results$ok))
