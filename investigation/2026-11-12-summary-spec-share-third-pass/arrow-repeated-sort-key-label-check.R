# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# .check_margin_label = TRUE over an Arrow input whose ordering names one
# column twice fails at construction; the same call with the check off
# succeeds. Sibling of #1585.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
d <- data.frame(g = c("a", "a", "b"), w = c(2L, 1L, 1L), v = c(3L, 1L, 2L))
show <- function(label, expr) {
  out <- tryCatch(
    paste(capture.output(print(as.data.frame(collect(expr)))), collapse = "\n"),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  cat("## ", label, "\n", out, "\n", sep = "")
}
for (shape in c("arrange(v) |> arrange(w, v)", "arrange(v, desc(v))")) {
  cat("=====", shape, "\n")
  x <- arrow::arrow_table(d)
  x <- if (startsWith(shape, "arrange(v) |>")) arrange(arrange(x, v), w, v) else arrange(x, v, desc(v))
  show("native collect of the input", x)
  show("Margin summary, .check_margin_label = FALSE",
       summarize_with_margins(x, s = sum(v), .grouping = rollup(g), .check_margin_label = FALSE))
  show("Margin summary, .check_margin_label = TRUE",
       summarize_with_margins(x, s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE))
}
cat("===== controls\n")
show("single arrange(v), check on (the #1585 shape)",
     summarize_with_margins(arrange(arrow::arrow_table(d), v), s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE))
show("arrange(g) |> arrange(w, g): repeated key is the checked dimension",
     summarize_with_margins(arrange(arrange(arrow::arrow_table(d), g), w, g), s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE))
con <- DBI::dbConnect(duckdb::duckdb()); DBI::dbWriteTable(con, "d", d)
show("DuckDB arrange(v) |> arrange(w, v), check on",
     suppressWarnings(summarize_with_margins(arrange(arrange(tbl(con, "d"), v), w, v), s = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE)))
show("native Arrow: the same input after select(g)", select(arrange(arrange(arrow::arrow_table(d), v), w, v), g))
