suppressPackageStartupMessages({
  pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."), quiet = TRUE)
  library(dplyr)
})
left <- arrow::arrow_table(a = 1L, b = "left payload")
right <- arrow::arrow_table(b = 1L)
query <- right_join(left, right, by = c(a = "b"), keep = FALSE)
schema_names <- names(select(as.data.frame(arrow::schema(query)), where(is.integer)))
arrow_names <- names(select(query, where(is.integer)))
plan <- inspect_grouping(query, .grouping = grouping_set(where(is.integer)), .format = "list")
fixed <- inspect_grouping(query, .by = where(is.integer), .format = "list")
collected_names <- names(select(collect(query), where(is.integer)))
stopifnot(identical(schema_names, "a"), identical(arrow_names, "a"),
          identical(plan$included[[1L]], "a"), identical(fixed$fixed[[1L]], "a"),
          identical(collected_names, c("a", "b")))
name_only <- inspect_grouping(query, .by = b, .format = "list")
kept <- right_join(left, right, by = c(a = "b"), keep = TRUE)
resolved <- inspect_grouping(kept, .by = where(is.integer), .format = "list")
stopifnot(identical(name_only$fixed[[1L]], "b"),
          identical(resolved$fixed[[1L]], c("a", "b.y")),
          identical(names(select(collect(kept), where(is.integer))), c("a", "b.y")))
cat("schema/native lazy select/Grouping dimension/fixed key: a\n")
cat("native collection then select: a, b\n")
cat("explicit-name and keep=TRUE controls passed\n")
