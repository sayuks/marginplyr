Sys.setenv(TESTTHAT_PARALLEL = "false")
source_path <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_path, quiet = TRUE)
library(dplyr)
output <- commandArgs(trailingOnly = TRUE)
if (!length(output)) output <- tempfile(fileext = ".csv")
names_to_try <- c("plain", ".SD", ".N", ".I", ".BY", ".GRP", ".NGRP",
                  ".EACHI", ".rows", "T", "F", "...", "..1", ".data",
                  ".env", "key", "check.names", "keep.rownames",
                  "stringsAsFactors", "..marginplyr_nest_1", "a b", "東京")
canonical <- function(x, key) {
  x <- as_tibble(ungroup(x), .name_repair = "minimal")
  x[[key]] <- lapply(x[[key]], function(y) as_tibble(y, .name_repair = "minimal"))
  x
}
run <- function(x, g, key, verb, keep, graph) {
  if (graph == "filter") x <- filter(x, TRUE)
  if (graph == "cache") x <- compute(x)
  expr <- rlang::expr((!!get(verb))(
    x, .grouping = rollup(!!rlang::sym(g)), .keep = !!keep,
    .key = !!key, .sort = "last", .id = "set"))
  ans <- rlang::eval_tidy(expr)
  if (inherits(ans, "dtplyr_step")) ans <- collect(ans)
  canonical(ans, key)
}
rows <- list()
for (name in names_to_try) for (role in c("group", "payload", "key")) {
  g <- if (role == "group") name else "g"
  payload <- if (role == "payload") name else "row"
  key <- if (role == "key") name else "cells"
  input <- tibble::new_tibble(setNames(list(c("b", "a", NA, "a"), 1:4),
                                      c(g, payload)), nrow = 4L)
  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    for (keep in c(FALSE, TRUE)) for (graph in c("root", "filter", "cache")) {
      local <- tryCatch(run(input, g, key, verb, keep, "root"), error = identity)
      lazy <- dtplyr::lazy_dt(input)
      original <- serialize(lazy[["parent"]], NULL)
      actual <- tryCatch(run(lazy, g, key, verb, keep, graph), error = identity)
      source_same <- identical(original, serialize(lazy[["parent"]], NULL))
      status <- if (inherits(local, "error")) "local_error" else
        if (inherits(actual, "error")) "lazy_error" else
          if (!isTRUE(all.equal(local, actual))) "mismatch" else "match"
      detail <- if (inherits(actual, "error")) conditionMessage(actual) else
        if (status == "mismatch") paste(all.equal(local, actual), collapse = "; ") else ""
      rows[[length(rows) + 1L]] <- data.frame(name, role, verb, keep, graph,
                                            status, source_same, detail)
      if (status != "match" || !source_same) print(tail(rows, 1L))
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, output[[1L]], row.names = FALSE)
print(table(results$status))
print(table(results$source_same))
print(sessionInfo())
