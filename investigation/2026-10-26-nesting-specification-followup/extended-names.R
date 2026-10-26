# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(TRUE)
root <- if (length(args)) args[[1]] else "."
pkgload::load_all(root, quiet = TRUE)
names_to_try <- c(".env", ".data", "..", "._", "_DT1", "_DT2", "..cols", ".x", ".y", "i.g", "x.g", "i", "j", "by", "key", "check.names", "keep.rownames", "stringsAsFactors", "sorted", "NA", "TRUE", "FALSE", "NULL", "Inf", "NaN", "if", "function", "return", "c", "list", "data.table", "list(1)", "x y", "x\ny", "x\ty", "x\ry", "`", "\\", "{x}", "😸", "é", "e\u0301", ".SDcols", ".N", ".I", ".SD", ".BY", ".EACHI", ".GRP", ".NGRP", "T", "F", ".Last.updated", ".rows", "data", "set", "..marginplyr_nest_1", "..marginplyr_column_1", "..marginplyr_dtplyr_column_1", "..marginplyr_cell_1")
normalize <- function(x, key) {
  x <- dplyr::ungroup(x)
  out <- lapply(seq_len(nrow(x)), function(i) {
    cell <- tibble::as_tibble(x[[key]][[i]])
    if (nrow(cell) > 1L && ncol(cell) > 0L) cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
    list(outer = as.list(x[i, setdiff(names(x), key), drop = FALSE]), cols = names(cell), rows = nrow(cell), cell = as.list(cell))
  })
  out[order(vapply(out, function(r) paste(capture.output(dput(r)), collapse = ""), character(1)))]
}
failures <- list()
calls <- 0L
for (nm in names_to_try) for (role in c("payload", "dimension", "fixed", "id", "key")) for (keep in c(FALSE, TRUE)) for (size in c(0L, 3L)) for (verb in c("nest_with_margins", "nest_by_with_margins")) {
  if (nm == ".rows" && verb == "nest_by_with_margins" && role %in% c("fixed", "dimension", "id")) next
  x <- tibble::tibble(g = c("a", NA, "a")[seq_len(size)], h = c("x", "x", "y")[seq_len(size)], p = c(1L, 2L, 1L)[seq_len(size)])
  column <- switch(role, payload = "p", dimension = "g", fixed = "h", NULL)
  if (!is.null(column)) {
    if (nm %in% setdiff(names(x), column)) next
    names(x)[names(x) == column] <- nm
  }
  g <- if (role == "dimension") nm else "g"
  h <- if (role == "fixed") nm else "h"
  key <- if (role == "key") nm else "data"
  id <- if (role == "id") nm else "set"
  if (key %in% c(g, h, id) || id %in% names(x)) next
  # Forward selection quosures by direct injection into public calls.
  run <- function(source) {
    fun <- get(verb)
    fun(source, .by=dplyr::all_of(h), .grouping=rollup(dplyr::all_of(g)), .keep=keep, .key=key, .id=id, .margin_label=NULL, .sort="last")
  }
  local <- tryCatch(run(x), error = identity)
  lazy <- tryCatch(dplyr::collect(run(dtplyr::lazy_dt(x))), error = identity)
  calls <- calls + 2L
  if (inherits(local, "error") || inherits(lazy, "error")) {
    failures[[length(failures)+1L]] <- list(nm=nm, role=role, keep=keep, size=size, verb=verb, local=if(inherits(local,"error")) conditionMessage(local), lazy=if(inherits(lazy,"error"))conditionMessage(lazy))
  } else if (!identical(normalize(local,key), normalize(lazy,key))) {
    failures[[length(failures)+1L]] <- list(nm=nm, role=role, keep=keep, size=size, verb=verb, local=normalize(local,key), lazy=normalize(lazy,key))
  }
}
cat("calls:",calls,"failures:",length(failures),"\n")
if (length(failures)) print(failures)

stopifnot(length(failures) == 0L)
