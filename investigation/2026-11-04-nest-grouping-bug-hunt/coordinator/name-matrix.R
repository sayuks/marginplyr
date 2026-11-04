args <- commandArgs(trailingOnly = TRUE)
source <- if (length(args)) args[[1L]] else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
cat(R.version.string, "\n")
for (p in c("dplyr", "dtplyr", "data.table", "vctrs", "tibble")) {
  cat(p, as.character(packageVersion(p)), "\n")
}
spellings <- c(".data", ".env", ".rows", ".SD", ".SDall", ".xSD", ".iSD", ".ROW", ".", ".N", ".I", ".BY", ".GRP",
  ".EACHI", ".NGRP", "T", "F", "TRUE", "FALSE", "NA", "NaN", "Inf", "NULL",
  "...", "..1", "..2", "..g", "_DT1", "V1", "data", "set_id", "list", "n",
  "..marginplyr_nest_1", "..marginplyr_column_1", "..marginplyr_alias_1",
  "..marginplyr_cell_1", "a b", "a\nb", "a\rb", "a\tb", "a`b", "地域", "é")
capture <- function(f) tryCatch(f(), error = identity)
normalize <- function(x, columns, key, id = "occ") {
  x <- if (inherits(x, "dtplyr_step")) dplyr::as_tibble(x, .name_repair = "minimal") else dplyr::collect(x)
  x <- as.data.frame(x, check.names = FALSE)
  cells <- lapply(x[[key]], function(cell) {
    cell <- as.data.frame(cell, check.names = FALSE)
    names(cell) <- c("fixed", "g", "value")[match(names(cell), columns)]
    cell <- cell[order(cell$value), , drop = FALSE]
    rownames(cell) <- NULL
    cell
  })
  names(x)[match(c(columns[1:2], id, key), names(x))] <- c("fixed", "g", "occ", "cell")
  x$cell <- cells
  x <- x[c("fixed", "g", "occ", "cell")]
  rownames(x) <- NULL
  x
}
rows <- list()
for (spelling in spellings) for (role in c("fixed", "dimension", "payload", "key", "id")) {
  cols <- c("fixed", "g", "value")
  key <- "cells"
  id <- "occ"
  if (role == "key") key <- spelling else if (role == "id") id <- spelling else cols[match(role, c("fixed", "dimension", "payload"))] <- spelling
  for (kind in c("local", "dtplyr")) for (verb in c("nest_with_margins", "nest_by_with_margins")) for (keep in c(FALSE, TRUE)) {
    if (verb == "nest_by_with_margins" && role %in% c("fixed", "dimension", "id") && spelling == ".rows") next
    d <- tibble::tibble(fixed = c("f", "f", NA), g = c("b", "a", "a"), value = 1:3)
    names(d) <- cols
    before <- serialize(d, NULL)
    src <- if (kind == "local") d else dtplyr::lazy_dt(d)
    fun <- get(verb)
    ans <- capture(function() {
      result <- fun(src, .by = dplyr::all_of(cols[[1]]), .grouping = rollup(dplyr::all_of(cols[[2]])),
        .key = key, .id = id, .keep = keep, .sort = "last")
      normalize(result, cols, key, id)
    })
    control <- tibble::tibble(fixed = c("f", "f", NA), g = c("b", "a", "a"), value = 1:3)
    ref <- normalize(fun(control, .by = fixed, .grouping = rollup(g), .key = "cells", .id = "occ", .keep = keep, .sort = "last"), c("fixed", "g", "value"), "cells")
    status <- if (inherits(ans, "error")) "error" else if (identical(ans, ref)) "match" else "mismatch"
    stopifnot(identical(before, serialize(d, NULL)))
    i <- length(rows) + 1L
    rows[[i]] <- data.frame(spelling = spelling, role = role, kind = kind, verb = verb, keep = keep,
      status = status, diagnostic = if (inherits(ans, "error")) conditionMessage(ans) else "", check.names = FALSE)
    if (status != "match") cat(i, encodeString(spelling), role, kind, verb, keep, status, if(inherits(ans,"error")) class(ans)[1] else "", "\n")
  }
}
results <- do.call(rbind, rows)
print(with(results, table(kind, status)))
out <- Sys.getenv("HUNT_OUTPUT")
if (nzchar(out)) { saveRDS(results, out); cat("Saved", out, "\n") }
stopifnot(!any(results$status == "mismatch"))
