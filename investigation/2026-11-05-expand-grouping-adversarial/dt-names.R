suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
Sys.setenv(TESTTHAT_PARALLEL = "false")
names_to_test <- c(".N", ".I", ".SD", ".BY", ".GRP", ".NGRP", ".EACHI", ".", ".data", ".env",
                   "...", "..1", "..01", "..0", "a b", "NA", "TRUE", "if", "copy", "length",
                   "if_else", "dplyr", "base", ".x", "..marginplyr_label_1")
capture <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) {
  paste(class(e)[[1L]], gsub("[\r\n]+", " ", conditionMessage(e)))
})
results <- list()
for (nm in names_to_test) for (role in c("dimension", "fixed", "payload", "id")) {
  dat <- data.frame(g = c(2L, 1L, NA_integer_), idx = 1:3)
  id <- if (role == "id") nm else "id"
  if (role != "id") dat[[nm]] <- c(3L, 1L, 2L)
  dimension <- if (role == "dimension") nm else "g"
  fixed <- if (role == "fixed") nm else character()
  for (label in list(NULL, "Z")) for (sort in c("none", "last", "first")) {
    run <- function(d) expand_with_margins(
      d, .by = dplyr::all_of(fixed), .grouping = rollup(dplyr::all_of(dimension)),
      .margin_label = label, .sort = sort, .id = id
    )
    canon <- function(d) {
      if (!is.data.frame(d)) return(d)
      if (sort == "none") d <- d[order(d[[id]], d[["idx"]]), , drop = FALSE]
      rownames(d) <- NULL
      as.list(d)
    }
    eager <- capture(canon(run(dat)))
    native <- capture(as.list(as.data.frame(dplyr::collect(dtplyr::lazy_dt(dat)))))
    observed <- capture(canon(dplyr::collect(run(dtplyr::lazy_dt(dat)))))
    results[[length(results) + 1L]] <- data.frame(
      name = nm, role = role, label = if (is.null(label)) "NULL" else label, sort,
      result = if (!identical(native, as.list(dat))) "source"
               else if (!is.list(eager)) paste("eager", eager)
               else if (identical(eager, observed)) "match"
               else if (is.list(observed)) "different" else observed
    )
  }
}
res <- do.call(rbind, results)
print(table(res$result))
print(unique(res[res$result != "match", c("name", "role", "result")]), row.names = FALSE)
