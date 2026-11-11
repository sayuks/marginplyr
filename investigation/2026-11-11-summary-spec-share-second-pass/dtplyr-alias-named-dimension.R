# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(dplyr, warn.conflicts = FALSE)
nm <- "..marginplyr_dtplyr_column_1"
d <- data.frame(k = c("p", "q", "p"), v = c(1, 2, 4)); names(d)[1] <- nm
show <- function(label, expr) cat(label, ":", tryCatch({ r <- as.data.frame(collect(expr)); paste(nrow(r), "rows; names", paste(names(r), collapse = ",")) }, error = function(e) paste("ERROR", conditionMessage(e))), "\n")
# expected (plain dplyr per grain): (k) -> p=5, q=2 ; () -> 7 : 3 rows
print(summarise(d, s = sum(v), .by = all_of(nm))); print(summarise(d, s = sum(v)))
show("local data frame  ", summarize_with_margins(d, s = sum(v), .grouping = rollup(all_of(nm))))
show("data.table        ", summarize_with_margins(data.table::as.data.table(d), s = sum(v), .grouping = rollup(all_of(nm))))
x <- dtplyr::lazy_dt(data.table::as.data.table(d))
show("native dtplyr .by ", summarise(x, s = sum(v), .by = all_of(nm)))
show("dtplyr rollup     ", summarize_with_margins(x, s = sum(v), .grouping = rollup(all_of(nm))))
show("dtplyr cube       ", summarize_with_margins(x, s = sum(v), .grouping = cube(all_of(nm))))
show("dtplyr sets       ", summarize_with_margins(x, s = sum(v), .grouping = grouping_sets(grouping_set(all_of(nm)), grouping_set())))
show("dtplyr set (never omitted)", summarize_with_margins(x, s = sum(v), .grouping = grouping_set(all_of(nm))))
show("dtplyr .by        ", summarize_with_margins(x, s = sum(v), .by = all_of(nm)))
d2 <- d; names(d2)[1] <- "..marginplyr_dtplyr_column_2"
show("dtplyr rollup, suffix _2", summarize_with_margins(dtplyr::lazy_dt(data.table::as.data.table(d2)), s = sum(v), .grouping = rollup(all_of("..marginplyr_dtplyr_column_2"))))
