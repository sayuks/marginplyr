output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(marginplyr)
library(dplyr, warn.conflicts = FALSE)
cat("Investigated SHA: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
print(sessionInfo())
candidate_names <- c("data", ".rows", ".data", ".env", "...", "..1", "..0",
                     ".N", ".SD", ".BY", "T", "F", "NA", "NULL", ":=",
                     "a b", "a\nb", "a\tb", "日本語", "..marginplyr_nest_1")
x <- tibble(f = c("a", "a", "a", "b"), g = c("x", "x", NA, "y"),
            h = c(1L, 2L, 1L, 1L), row = 1:4)
results <- list()
index <- 0L
for (kind in c("local", "dtplyr")) {
  source <- if (kind == "local") x else dtplyr::lazy_dt(x)
  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    fn <- get(verb)
    for (key in candidate_names) for (id in c("set", ".rows", "..2", ".env", ".SD")) {
      if (key == id) next
      for (keep in c(FALSE, TRUE)) {
        index <- index + 1L
        ans <- tryCatch(fn(source, .by = f, .grouping = rollup(g, h),
                           .key = key, .id = id, .keep = keep,
                           .sort = "last"), error = identity)
        expected_refusal <- verb == "nest_by_with_margins" && id == ".rows"
        if (inherits(ans, "error")) {
          status <- if (expected_refusal && inherits(ans, "marginplyr_error")) {
            "expected_refusal"
          } else "unexpected_error"
          detail <- conditionMessage(ans)
        } else {
          ans <- as_tibble(ans, .name_repair = "minimal")
          expected <- fn(x, .by = f, .grouping = rollup(g, h), .id = "set",
                         .key = "data", .keep = keep, .sort = "last")
          expected <- as_tibble(expected)
          outer <- as.data.frame(ans[setdiff(names(ans), key)])
          names(outer)[names(outer) == id] <- "set"
          expected_outer <- as.data.frame(expected[setdiff(names(expected), "data")])
          cells_ok <- identical(lapply(ans[[key]], as.data.frame),
                                lapply(expected$data, as.data.frame))
          outer_ok <- identical(outer, expected_outer)
          groups_ok <- if (verb == "nest_by_with_margins") {
            identical(group_vars(fn(source, .by = f, .grouping = rollup(g, h),
                                    .key = key, .id = id, .keep = keep)),
                      c("f", "g", "h", id))
          } else TRUE
          status <- if (!expected_refusal && cells_ok && outer_ok && groups_ok) "match" else "mismatch"
          detail <- paste("outer", outer_ok, "cells", cells_ok, "groups", groups_ok)
        }
        results[[index]] <- data.frame(kind, verb, key, id, keep, status, detail)
      }
    }
  }
}
results <- bind_rows(results)
write.csv(results, file.path(output_dir, "output-names.csv"), row.names = FALSE)
print(table(results$kind, results$status))
print(results[!results$status %in% c("match", "expected_refusal"), ])
stopifnot(all(results$status %in% c("match", "expected_refusal")))
