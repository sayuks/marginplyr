# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On an Arrow input with .check_margin_label = TRUE the label collision query runs before the Arrow share refusal.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr); library(arrow)})
built <- new.env(); built$n <- 0
invisible(suppressMessages(trace("do_arrow_summarize", where = asNamespace("arrow"), print = FALSE,
  tracer = quote(assign("n", get("n", envir = .GlobalEnv$built) + 1, envir = .GlobalEnv$built)))))
first_line <- function(e) strsplit(conditionMessage(e), "\n")[[1]][1]
probe <- function(label, expr) {
  built$n <- 0
  msg <- tryCatch({force(expr); "returned"}, error = function(e) paste0("<", class(e)[1], "> ", first_line(e)))
  cat(sprintf("%-62s Arrow summarise() built: %d\n   -> %s\n", label, built$n, msg))
}
clean <- arrow::Table$create(data.frame(group = c("x", "y"), value = 1:2))
colliding <- arrow::Table$create(data.frame(group = c("Total", "y"), value = 1:2))
cat("EXPECTED (ADR 0005): the Arrow share refusal comes before any Margin-label validation query;\n",
    "  'The existing Arrow schema snapshot is the only backend read performed for that rejection.'\n",
    "  So every share call below: 'Arrow backends do not support ... shares', with no Arrow summarise() built.\n\n")
probe("OBSERVED share_of_total, .check_margin_label = TRUE, clean data",
      summarize_with_margins(clean, total = sum(value), s = share_of_total(total), .grouping = rollup(group), .check_margin_label = TRUE))
probe("OBSERVED share_of_total, .check_margin_label = TRUE, 'Total' in data",
      summarize_with_margins(colliding, total = sum(value), s = share_of_total(total), .grouping = rollup(group), .check_margin_label = TRUE))
probe("OBSERVED share_of_parent, .check_margin_label = TRUE, 'Total' in data",
      summarize_with_margins(colliding, total = sum(value), s = share_of_parent(total), .grouping = rollup(group), .check_margin_label = TRUE))
probe("CONTROL  share_of_total, default .check_margin_label (FALSE)",
      summarize_with_margins(colliding, total = sum(value), s = share_of_total(total), .grouping = rollup(group)))
probe("CONTROL  a share planning refusal (no Grand total set), check TRUE",
      summarize_with_margins(colliding, total = sum(value), s = share_of_total(total), .grouping = grouping_sets(group), .check_margin_label = TRUE))
cat("\nAfter following the collision remedy (another label), the call is refused for the share anyway:\n")
probe("  .margin_label = 'All', check TRUE",
      summarize_with_margins(colliding, total = sum(value), s = share_of_total(total), .grouping = rollup(group), .margin_label = "All", .check_margin_label = TRUE))
invisible(0)

cat("\nArrow file dataset whose file is removed after the dataset is opened (the read reaches the file):\n")
dir <- file.path(tempdir(), "cand2-ds"); unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
arrow::write_parquet(data.frame(group = c("x", "y"), value = 1:2), file.path(dir, "part.parquet"))
ds <- arrow::open_dataset(dir)
unlink(file.path(dir, "part.parquet"))
probe("OBSERVED dataset, share_of_total, .check_margin_label = TRUE",
      summarize_with_margins(ds, total = sum(value), s = share_of_total(total), .grouping = rollup(group), .check_margin_label = TRUE))
probe("CONTROL  dataset, share_of_total, default check",
      summarize_with_margins(ds, total = sum(value), s = share_of_total(total), .grouping = rollup(group)))
unlink(dir, recursive = TRUE)
