# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
repo <- "."
suppressMessages(pkgload::load_all(repo, quiet = TRUE)); suppressMessages(library(dplyr))
d <- data.frame(year = c(2025L, 2026L), region = c("n", "s"), v = c(1, 2))
for (s in c("simulate_mssql", "simulate_oracle", "simulate_redshift", "simulate_snowflake", "simulate_teradata", "simulate_postgres")) {
  X <- dbplyr::tbl_lazy(d, getExportedValue("dbplyr", s)(), name = "d")
  for (cl in list(int = quote(summarize_with_margins(X, s = sum(v, na.rm = TRUE), .grouping = rollup(year))),
                  chr = quote(summarize_with_margins(X, s = sum(v, na.rm = TRUE), .grouping = rollup(region))),
                  one = quote(summarize_with_margins(X, s = sum(v, na.rm = TRUE), .grouping = grouping_set(year))),
                  nul = quote(summarize_with_margins(X, s = sum(v, na.rm = TRUE), .grouping = rollup(year), .margin_label = NULL)))) {
    sql <- as.character(dbplyr::sql_render(eval(cl)))
    cat("----", s, deparse(cl[[4]]), if (length(cl) > 4) "label NULL", "\n"); cat(sub("UNION ALL.*UNION ALL", "UNION ALL ... UNION ALL", gsub("\\s+", " ", sql)), "\n")
  }
}
