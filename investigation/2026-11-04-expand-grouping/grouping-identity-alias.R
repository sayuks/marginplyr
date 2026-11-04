pkgload::load_all(".", quiet = TRUE)
data <- tibble::tibble(a = 1, b = 2)
calls <- alist(
  grouping = inspect_grouping(data, .grouping = grouping_set(c(a = a))),
  grouped_different = inspect_grouping(data, .grouping = grouping_set(c(b = a))),
  grouped_all_of = inspect_grouping(data, .grouping = grouping_set(dplyr::all_of(c(a = "a")))),
  grouped_outer = inspect_grouping(data, .grouping = grouping_set(a = a)),
  grouped_without = inspect_grouping(data, .grouping = grouping_set(c(a))),
  by = inspect_grouping(data, .by = c(a = a)),
  by_different = inspect_grouping(data, .by = c(b = a)),
  by_without = inspect_grouping(data, .by = c(a)),
  expanded = expand_with_margins(data, .grouping = rollup(c(a = a)), .id = "id")
)
for (nm in names(calls)) {
  cat(nm, "\n")
  print(tryCatch(eval(calls[[nm]]), error = function(cnd) paste(class(cnd)[1L], conditionMessage(cnd))))
}
