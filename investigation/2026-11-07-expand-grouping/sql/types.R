# Run from an archived package root. Outputs go to MARGINPLYR_HUNT_OUTPUT.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
outdir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "/private/tmp/marginplyr-sql-types")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
observe <- function(x) tryCatch(force(x), error = identity)
canonical <- function(x) {
  if (inherits(x, "error")) return(x)
  x <- as.data.frame(x)
  x <- x[order(x$sid, x$rid), , drop = FALSE]
  rownames(x) <- NULL
  x
}
records <- list()
failures <- list()
n <- 0L
for (extended in c(FALSE, TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = extended)
  for (decl in c("INTEGER", "REAL", "TEXT", "BLOB", "DATE", "DATETIME", "TIMESTAMP", "BOOLEAN", "NUMERIC", "INT64")) {
    DBI::dbExecute(con, "DROP TABLE IF EXISTS src")
    DBI::dbExecute(con, paste0("CREATE TABLE src (rid INTEGER, g ", decl, ", p ", decl, ")"))
    literal <- if (decl == "TEXT") "'b'" else if (decl == "BLOB") "X'0102'" else "2"
    third <- if (decl == "BLOB") "X'00FF'" else "1"
    DBI::dbExecute(con, paste0("INSERT INTO src VALUES (1,", literal, ",", literal, "), (2,NULL,NULL), (3,", third, ",", third, ")"))
    root <- tbl(con, "src")
    for (shape in c("root", "window", "collapse", "head", "inline")) {
      src <- switch(shape, root = root, window = mutate(root, z = row_number(rid)), collapse = collapse(root), head = head(root, 2), inline = dbplyr::copy_inline(con, collect(root)))
      base <- observe(collect(src))
      if (inherits(base, "error")) next
      for (rows in c("all", "empty")) {
        x <- if (rows == "all") src else filter(src, FALSE)
        b <- collect(x)
        for (family in c("identity", "rollup", "omitted_first", "duplicates")) {
          spec <- switch(family, identity = grouping_set(g), rollup = rollup(g), omitted_first = grouping_sets(grouping_set(), grouping_set(g)), duplicates = grouping_sets(grouping_set(), grouping_set(g), grouping_set()))
          for (sort in c("none", "first", "last")) {
            actual_query <- observe(expand_with_margins(x, .grouping = spec, .margin_label = NULL, .duplicates = "keep", .sort = sort, .id = "sid"))
            actual <- if (inherits(actual_query, "error")) actual_query else observe(collect(actual_query))
            pieces <- lapply(seq_along(if (family == "duplicates") 1:3 else if (family == "identity") 1 else 1:2), function(i) {
              omitted <- if (family == "identity") FALSE else if (family == "rollup") i == 2L else i != 2L
              z <- b
              if (omitted) z <- mutate(z, g = vctrs::vec_init(g, n()))
              mutate(z, sid = as.integer(i))
            })
            expected <- observe(Reduce(union_all, pieces))
            if (!inherits(expected, "error")) expected <- select(expected, g, sid, everything())
            comparison <- if (inherits(expected, "error")) "ordinary-error" else if (inherits(actual, "error")) "package-error" else if (identical(canonical(actual), canonical(expected))) "match" else "mismatch"
            n <- n + 1L
            records[[n]] <- data.frame(extended, decl, shape, rows, family, sort, comparison, diagnostic = if (inherits(actual, "error")) conditionMessage(actual) else "")
            if (comparison %in% c("package-error", "mismatch")) failures[[as.character(n)]] <- list(case = records[[n]], base = b, expected = expected, actual = actual)
          }
        }
      }
    }
  }
  DBI::dbDisconnect(con)
  cat("Completed extended_types =", extended, "with", n, "comparisons\n")
}
result <- do.call(rbind, records)
write.csv(result, file.path(outdir, "types.csv"), row.names = FALSE)
saveRDS(failures, file.path(outdir, "types-failures.rds"))
print(table(result$extended, result$comparison))
print(unique(result[result$comparison != "match", c("extended", "decl", "shape", "comparison")]))
writeLines(capture.output(sessionInfo()), file.path(outdir, "environment.txt"))
