Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
root <- "investigation/2026-11-03-expand-grouping-greedy/lazy"
workspace <- tempfile("marginplyr-union-dataset-")
dir.create(workspace)
fixtures <- c("ordinary", "..marginplyr_arrow_id_1", "..marginplyr_arrow_dimension_1", "..marginplyr_arrow_boundary_1")
rows <- list()
for (hidden in fixtures) {
 data <- tibble(g = c("c", "a", NA_character_), k = c(NA_integer_, 2L, 1L), rid = 1:3)
 data[[hidden]] <- c("x", "y", "z")
 paths <- file.path(workspace, paste0(match(hidden, fixtures), c("-a", "-b")))
 for (i in 1:2) {
  dir.create(paths[[i]])
  arrow::write_dataset(data[if (i == 1L) 1:2 else 3L, ], paths[[i]], partitioning = c(hidden, "g"))
 }
 options <- list(selector_ignore_prefixes = character())
 ds1 <- arrow::open_dataset(paths[[1]], factory_options = options, schema = arrow::schema(arrow::Table$create(data)))
 ds2 <- arrow::open_dataset(paths[[2]], factory_options = options, schema = arrow::schema(arrow::Table$create(data)))
 ud <- arrow::open_dataset(list(ds1, ds2))
 memory <- arrow::InMemoryDataset$create(arrow::Table$create(data))
 mix <- arrow::open_dataset(list(ds1, memory))
 nested <- arrow::open_dataset(list(ud, mix))
 right <- arrow::Table$create(tibble(rid = 1:3, extra = c(10L, 20L, 30L)))
 shapes <- list(
  union_files = select(ud, g, k, rid),
  union_mixed = select(mix, g, k, rid),
  union_nested = select(nested, g, k, rid),
  union_filtered = filter(select(nested, g, k, rid), rid > 1L),
  union_ordered = arrange(select(nested, g, k, rid), desc(rid)),
  union_renamed = rename(rename(select(nested, g, k, rid), temp = g), g = temp),
  join_left = left_join(select(nested, g, k, rid), right, by = "rid"),
  join_right = left_join(right, select(nested, g, k, rid), by = "rid"),
  union_query = union_all(select(ud, g, k, rid), select(mix, g, k, rid)),
  union_after_join = union_all(left_join(select(ud, g, k, rid), right, by = "rid"), left_join(select(mix, g, k, rid), right, by = "rid"))
 )
 for (shape in names(shapes)) {
  input <- shapes[[shape]]
  raw <- tryCatch(collect(input), error = identity)
  if (inherits(raw, "error")) {
   rows[[length(rows)+1L]] <- data.frame(hidden, shape, label = "source", sort = "source", id = "source", fixed = FALSE, outcome = "source-error", detail = conditionMessage(raw))
   next
  }
  for (label in c("NULL", "text")) for (sort in c("none", "last", "first")) for (id in c("set", hidden)) for (fixed in c(FALSE, TRUE)) {
   expected <- bind_rows(mutate(raw, !!id := 1L), mutate(raw, g = if (label == "NULL") NA_character_ else "Total", !!id := 2L))
   names_expected <- c(if(fixed) "k", "g", id, setdiff(names(raw), c(if(fixed) "k", "g")))
   expected <- expected[, names_expected]
   actual <- tryCatch(if (fixed) {
    collect(expand_with_margins(input, .by = k, .grouping = rollup(g), .margin_label = if(label == "NULL") NULL else "Total", .id = id, .sort = sort))
   } else {
    collect(expand_with_margins(input, .grouping = rollup(g), .margin_label = if(label == "NULL") NULL else "Total", .id = id, .sort = sort))
   }, error = identity)
   outcome <- if (inherits(actual, "error")) "package-error" else {
    canonical <- function(x) x[order(x[[id]], x$rid), , drop = FALSE]
    if (!isTRUE(all.equal(canonical(actual), canonical(expected)))) "value-type-mismatch" else if(sort == "none") "match" else {
     bit <- expected[[id]] == 2L
     args <- c(if(fixed) list(is.na(expected$k), expected$k), list(if(sort == "first") !bit else bit, is.na(expected$g), expected$g, expected[[id]]))
     key <- expected[do.call(order, args), , drop = FALSE]
     if (identical(actual[, c(if(fixed) "k", "g", id)], key[, c(if(fixed) "k", "g", id)])) "match" else "order-mismatch"
    }
   }
   rows[[length(rows)+1L]] <- data.frame(hidden, shape, label, sort, id, fixed, outcome, detail = if(inherits(actual,"error")) conditionMessage(actual) else "")
  }
 }
}
results <- bind_rows(rows)
write.csv(results, file.path(root, "union-dataset-graphs.csv"), row.names = FALSE)
print(count(results, shape, outcome))
print(filter(results, outcome != "match"))
unlink(workspace, recursive = TRUE)
stopifnot(all(results$outcome == "match"))
