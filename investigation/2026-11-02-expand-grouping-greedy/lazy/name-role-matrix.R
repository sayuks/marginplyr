# Dated independent branch oracle; run from a repository root.
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-02-expand-grouping-greedy/lazy")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(expr, error = identity)
canon <- function(x) {
  x <- as.data.frame(x)
  if (nrow(x)) x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
nms <- c(".SDcols", ".Last.updated", ".env", ".data", ".", "..", "..g", "i.g", "x.g", "copy", "setnames", "on", "key", "names", "NA", "NaN", "Inf", "TRUE", "FALSE", "a`b", "a\nb", "a\tb", "é", "地域", "..marginplyr_dtplyr_column_1")
records <- list(); k <- 0L
for (nm in nms) for (role in c("dimension", "fixed", "payload", "id")) for (n in c(0L, 3L)) {
  dat <- tibble(g = c("b", "a", NA_character_)[seq_len(n)], b = c("y", "x", "x")[seq_len(n)], rid = seq_len(n), value = (1:3)[seq_len(n)])
  old <- switch(role, dimension = "g", fixed = "b", payload = "value", id = "")
  if (nzchar(old)) names(dat)[names(dat) == old] <- nm
  g <- if (role == "dimension") nm else "g"
  b <- if (role == "fixed") nm else "b"
  id <- if (role == "id") nm else "sid"
  for (backend in c("dtplyr", "arrow")) for (shape in c("root", "filtered")) {
    original <- if (backend == "dtplyr") data.table::as.data.table(dat) else dat
    before <- if (backend == "dtplyr") data.table::copy(original) else original
    src <- capture(if (backend == "dtplyr") dtplyr::lazy_dt(original) else arrow::Table$create(original))
    if (shape == "filtered" && !inherits(src, "error")) src <- capture(filter(src, rid >= 0L))
    source <- if (inherits(src, "error")) src else capture(collect(src))
    for (label in c("null", "text")) for (sort in c("none", "first", "last")) {
      lab <- if (label == "null") NULL else "Total"
      expected <- dat
      expected[[id]] <- rep(1L, n)
      omitted <- dat
      omitted[[g]] <- rep(if (is.null(lab)) NA_character_ else lab, n)
      omitted[[id]] <- rep(2L, n)
      expected <- bind_rows(expected, omitted)
      expected <- expected[c(b, g, id, setdiff(names(expected), c(b, g, id)))]
      if (sort != "none" && nrow(expected)) {
        order_args <- list(is.na(expected[[b]]), expected[[b]], if (sort == "first") -expected[[id]] else expected[[id]], is.na(expected[[g]]), expected[[g]])
        expected <- expected[do.call(order, order_args), ]
      }
      actual <- if (inherits(source, "error")) source else capture(collect(expand_with_margins(src, .by = all_of(b), .grouping = rollup(all_of(g)), .margin_label = lab, .id = id, .sort = sort)))
      status <- if (inherits(source, "error")) "source_error" else if (inherits(actual, "error")) "package_error" else if (!isTRUE(all.equal(canon(actual), canon(expected)))) "value_diff" else if (sort != "none" && !isTRUE(all.equal(as.data.frame(actual[c(b,g,id)]), as.data.frame(expected[c(b,g,id)]), check.attributes = FALSE))) "order_diff" else "pass"
      if (!identical(original, before)) stop("Source changed")
      k <- k + 1L
      records[[k]] <- data.frame(name = nm, role = role, n = n, backend = backend, shape = shape, label = label, sort = sort, status = status, message = if (inherits(actual, "error")) conditionMessage(actual) else "")
      if (status %in% c("package_error", "value_diff", "order_diff")) cat("LEAD", k, nm, role, n, backend, shape, label, sort, status, records[[k]]$message, "\n")
    }
  }
  if (k %% 480L == 0L) cat("Completed", k, "\n")
}
res <- bind_rows(records)
write.csv(res, file.path(out, "name-role-results.csv"), row.names = FALSE)
print(table(res$backend, res$status))
