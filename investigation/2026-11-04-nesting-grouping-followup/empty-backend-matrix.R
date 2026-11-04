pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
call <- function(data, mode, ...) {
  if (mode == "empty_spec") nest_by_with_margins(data, .grouping = grouping_set(), ...)
  else if (mode == "fixed") nest_by_with_margins(data, .by = g, ...)
  else if (mode == "spec") nest_by_with_margins(data, .grouping = grouping_set(g), ...)
  else nest_by_with_margins(data, ...)
}
for (mode in c("none", "empty_spec", "fixed", "spec")) {
  for (size in c(0L, 2L)) {
    for (id in list(NULL, "sid", "...", "..1", ".rows", ".SD", "N")) {
      for (key in c("data", "....", "..10", ".SD", ".rows")) {
        for (keep in c(FALSE, TRUE)) {
          tbl <- tibble(g = seq_len(size), x = seq_len(size))
          src <- dtplyr::lazy_dt(tbl)
          actual <- tryCatch(call(src, mode, .id = id, .key = key, .keep = keep), error = identity)
          expected <- tryCatch(call(tbl, mode, .id = id, .key = key, .keep = keep), error = identity)
          if (inherits(actual, "error") && !inherits(expected, "error")) {
            cat("LAZY ONLY", mode, size, paste0(id, collapse = ""), key, keep, conditionMessage(actual), "\n")
          } else if (!inherits(actual, "error") && inherits(expected, "error")) {
            cat("LOCAL ONLY", mode, size, paste0(id, collapse = ""), key, keep, conditionMessage(expected), "\n")
          } else if (inherits(actual, "error") && !identical(conditionMessage(actual), conditionMessage(expected))) {
            cat("ERROR MISMATCH", mode, size, paste0(id, collapse = ""), key, keep, "\n", conditionMessage(actual), "\n", conditionMessage(expected), "\n")
          } else if (!inherits(actual, "error")) {
            a <- ungroup(actual)
            b <- ungroup(expected)
            a[[key]] <- lapply(a[[key]], as.list)
            b[[key]] <- lapply(b[[key]], as.list)
            if (!identical(a,b)) cat("RESULT MISMATCH", mode, size, paste0(id, collapse = ""), key, keep, "\n")
          }
        }
      }
    }
  }
}
for (nm in c("...", "..1", ".SD", ".N", ".rows", "f", "T")) {
  for (x in list(integer(), 1:2)) {
    dt <- data.table::data.table(x = x)
    data.table::setnames(dt, "x", nm)
    q <- dtplyr::lazy_dt(dt)
    for (v in list(nest_with_margins, nest_by_with_margins)) {
      got <- tryCatch({
        res <- v(q)
        if (inherits(res,"dtplyr_step")) tibble::as_tibble(res,.name_repair="minimal") else res
      }, error = identity)
      cat(nm,length(x),identical(v,nest_by_with_margins),if (inherits(got,"error")) conditionMessage(got) else paste(nrow(got), if (nrow(got) > 0L) names(got$data[[1L]])), "\n")
    }
  }
}
