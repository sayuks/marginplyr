# Compare direct and repeated materialization with a separately built local result.
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else getwd()
pkgload::load_all(root, quiet = TRUE)
canonical_cell <- function(x) {
  x <- as.data.frame(x, optional = TRUE)
  rownames(x) <- NULL
  attributes(x) <- attributes(x)[c("names", "row.names", "class")]
  x
}
canonical <- function(x) {
  x <- if (inherits(x, "dtplyr_step")) dplyr::as_tibble(x, .name_repair = "minimal") else dplyr::collect(x)
  x <- as.data.frame(x, optional = TRUE)
  for (i in seq_along(x)) {
    if (is.list(x[[i]]) && all(vapply(x[[i]], is.data.frame, logical(1)))) {
      x[[i]] <- lapply(x[[i]], canonical_cell)
    }
  }
  rownames(x) <- NULL
  x
}
names_to_check <- c("data", "..1", "...", "..-1", ".. 2", ".N", ".SD", ".SDall", ".xSD", ".iSD", ".rows", "set", "TRUE", "F", "..marginplyr_nest_1", "..marginplyr_sort_1")
count <- 0L
for (nr in c(0L, 1L, 6L)) {
  x <- tibble::tibble(g = c("b", "a", NA_character_, "a", "b", "a")[seq_len(nr)], h = c("y", "x", "y", "y", NA_character_, "x")[seq_len(nr)], row_id = seq_len(nr), payload = as.list(seq_len(nr)))
  for (key in names_to_check) for (id in c("set", "..2", ".SDall")) {
    if (key == id) next
    for (sort in c("last", "first")) for (keep in c(FALSE, TRUE)) {
      local <- canonical(nest_with_margins(x, .grouping = cube(g, h), .key = key, .id = id, .sort = sort, .keep = keep))
      z <- nest_with_margins(dtplyr::lazy_dt(x), .grouping = cube(g, h), .key = key, .id = id, .sort = sort, .keep = keep)
      for (mode in c("collect", "compute", "repeat-collect", "repeat-compute")) {
        got <- canonical(if (grepl("compute", mode, fixed = TRUE)) dplyr::compute(z) else z)
        if (!identical(local, got)) {
          cat("MISMATCH", nr, key, id, sort, keep, mode, "\n")
          print(all.equal(local, got))
          quit(status = 1L)
        }
        count <- count + 1L
      }
    }
  }
}
cat("Materialization observations matched:", count, "\n")
