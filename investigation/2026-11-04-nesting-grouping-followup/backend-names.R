pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data.table::setDTthreads(1L)
names_to_test <- c(
  ".", ".i", ".j", ".SD", ".SDall", ".N", ".I", ".BY", ".GRP", ".NGRP", ".EACHI",
  ".ROW", ".xSD", ".iSD", "i.g", "x.g", ".rows", ".data", ".env", "T", "F",
  "TRUE", "FALSE", "NA", "NULL", "Inf", "NaN", "if", "function", "as.data.table",
  "list", "copy", "j", "key", "setnames", "check.names", "with", "on",
  "nomatch", "mult", "allow.cartesian", "which", "a::b", "a$b", "a b", "a\u0009b",
  "a\u000ab", "a`b", "X)", "%x%", "_DT1", "..marginplyr_nest_1",
  "..marginplyr_cell_1", "..marginplyr_dtplyr_column_1", "...", "..1",
  "..+1", "..01", "..-1", ".. 1", "年"
)
run <- function(v, source, role, nm, keep) {
  grp <- if (role == "dimension") nm else if (role == "fixed") "value" else "g"
  fixed <- if (role == "fixed") nm else character()
  ans <- v(source, .by = all_of(fixed), .grouping = rollup(all_of(grp)),
           .key = if (role == "key") nm else "cells",
           .id = if (role == "id") nm else "set",
           .keep = keep, .sort = "last")
  if (inherits(ans, "dtplyr_step")) ans <- tibble::as_tibble(ans, .name_repair = "minimal")
  key <- if (role == "key") nm else "cells"
  groups <- group_vars(ans)
  ans <- as.list(ungroup(ans))
  ans[[key]] <- lapply(ans[[key]], function(cell) list(dim(cell), as.list(cell)))
  list(ans, groups)
}
observe <- function(x) {
  warnings <- character()
  result <- withCallingHandlers(tryCatch(x, error = identity), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warnings)
}
counts <- c(match = 0L, errors_both = 0L, lazy_only = 0L, local_only = 0L,
            value_mismatch = 0L, warning_mismatch = 0L)
for (nm in names_to_test) for (role in c("dimension","fixed","payload","id","key")) {
  data <- tibble(g = c("b","a"), value = 1:2)
  if (role %in% c("fixed","dimension")) names(data)[1L] <- nm
  if (role == "payload") names(data)[2L] <- nm
  root <- dtplyr::lazy_dt(data)
  source <- serialize(root[["parent"]], NULL)
  for (keep in c(FALSE, TRUE)) for (verb in c("nest_with_margins","nest_by_with_margins")) {
    expected <- observe(run(get(verb), data, role, nm, keep))
    actual <- observe(run(get(verb), root, role, nm, keep))
    status <- if (inherits(actual$result, "error")) {
      if (inherits(expected$result, "error")) "errors_both" else "lazy_only"
    } else if (inherits(expected$result, "error")) {
      "local_only"
    } else if (!identical(actual$result, expected$result)) {
      "value_mismatch"
    } else if (!identical(actual$warnings, expected$warnings)) {
      "warning_mismatch"
    } else "match"
    counts[[status]] <- counts[[status]] + 1L
    if (!status %in% c("match","errors_both")) {
      cat(status, encodeString(nm), role, keep, verb, "\n")
      if (inherits(actual$result, "error")) cat("lazy:", conditionMessage(actual$result), "\n")
      if (inherits(expected$result, "error")) cat("local:", conditionMessage(expected$result), "\n")
      if (status == "value_mismatch") print(all.equal(actual$result, expected$result))
      if (status == "warning_mismatch") print(list(lazy = actual$warnings, local = expected$warnings))
    }
    stopifnot(identical(source, serialize(root[["parent"]], NULL)))
  }
}
print(counts)
