args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args)) args[[1L]] else "."
pkgload::load_all(source_root, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
cat("SHA:", system2("git", c("-C", shQuote(source_root), "rev-parse", "HEAD"), stdout = TRUE), "\n")
cat(R.version.string, "\n")
name_pool <- c("g", ".rows", ".by", ".groups", ".data", ".env",
               "..01", ".SD", ".N", ".I", "T", "F", "key", "check.names",
               "keep.rownames", "stringsAsFactors", "{x}", "a\nb", "a\tb",
               "a b", "a/b", "é", "日本語", "..marginplyr_nest_1",
               "..marginplyr_cell_1", "..marginplyr_alias_1")
rows <- list()
i <- 0L
for (name in name_pool) for (verb in c("nest_with_margins", "nest_by_with_margins")) {
  if (verb == "nest_by_with_margins" && name == ".rows") next
  for (keep in c(FALSE, TRUE)) for (sort in c("none", "first", "last")) {
    for (empty in c(FALSE, TRUE)) {
      payload_names <- setdiff(name_pool, name)
      vals <- c(list(c("b", "a", "b")), lapply(seq_along(payload_names), function(j) j * 10L + 1:3))
      names(vals) <- c(name, payload_names)
      data <- tibble::new_tibble(vals, nrow = 3L)
      if (empty) data <- vctrs::vec_slice(data, integer())
      before <- serialize(data, NULL)
      spec <- grouping_sets(grouping_set(dplyr::all_of(!!name)), grouping_set())
      got <- tryCatch(do.call(get(verb), list(data, .grouping = spec, .id = "set_id", .keep = keep,
                                            .key = "cells", .sort = sort)), error = identity)
      error <- if (inherits(got, "error")) conditionMessage(got) else ""
      ok <- !inherits(got, "error")
      if (ok) {
        ok <- identical(names(got), c(name, "set_id", "cells")) && identical(serialize(data, NULL), before)
        if (verb == "nest_by_with_margins") ok <- ok && identical(dplyr::group_vars(got), c(name, "set_id"))
        expected <- if (keep) c(name, payload_names) else payload_names
        for (j in seq_len(nrow(got))) {
          ids <- if (got$set_id[[j]] == 2L) seq_len(nrow(data)) else which(data[[name]] == got[[name]][[j]])
          cell <- got$cells[[j]]
          ok <- ok && identical(names(cell), expected) && nrow(cell) == length(ids)
          for (nm in expected) ok <- ok && identical(cell[[nm]], data[[nm]][ids])
        }
        ok <- ok && nrow(got) == if (empty) 0L else 3L
      }
      i <- i + 1L
      rows[[i]] <- data.frame(name = name, verb = verb, keep = keep, sort = sort, empty = empty,
                              ok = ok, error = error)
    }
  }
}
results <- do.call(rbind, rows)
path <- if (length(args) > 1L) args[[2L]] else file.path(tempdir(), "name-crossings.csv")
utils::write.csv(results, path, row.names = FALSE)
cat("comparisons:", nrow(results), "matches:", sum(results$ok), "failures:", sum(!results$ok), "\n")
if (any(!results$ok)) print(utils::head(results[!results$ok, ], 20L), row.names = FALSE)
stopifnot(all(results$ok))
