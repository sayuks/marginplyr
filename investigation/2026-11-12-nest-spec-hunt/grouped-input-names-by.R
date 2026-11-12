# Investigated SHA: 11471772e4a8a8518a689c538c2bc2929bd5d53d
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A grouped input supplies its fixed keys implicitly, yet two refusals about
# those keys name `.by`, an argument the caller did not write.
pkgload::load_all(".", quiet = TRUE)
d <- tibble::tibble(year = c(1L, 1L, 2L), region = c("a", "b", "a"), v = 1:3)
gd <- dplyr::group_by(d, year)
e <- function(label, expr) {
  r <- tryCatch({ expr; "ok" }, error = function(e) conditionMessage(e))
  cat("## ", label, "\n", r, "\n", sep = "")
}
verbs <- list(nest_with_margins = nest_with_margins,
              nest_by_with_margins = nest_by_with_margins,
              summarize_with_margins = summarize_with_margins,
              expand_with_margins = expand_with_margins)
for (name in names(verbs)) {
  e(paste(name, "grouped, grouping column in .grouping"),
    verbs[[name]](gd, .grouping = rollup(year, region)))
  e(paste(name, "grouped, .margin_label names the grouping column"),
    verbs[[name]](gd, .grouping = rollup(region), .margin_label = c(region = "All", year = "All")))
}
e("inspect_grouping grouped, grouping column in .grouping", inspect_grouping(gd, .grouping = rollup(year, region)))
e("control: explicit .by", nest_with_margins(d, .by = year, .grouping = rollup(year, region)))
e("control: tidyr::nest() on a grouped input with .by", tidyr::nest(gd, .by = year))
