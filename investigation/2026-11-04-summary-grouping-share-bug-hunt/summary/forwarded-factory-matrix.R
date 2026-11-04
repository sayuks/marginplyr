pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out_dir <- "investigation/2026-11-04-summary-grouping-share-bug-hunt/summary"
data <- data.frame(g = c("a", "a", "b"), x = 1:3)
inputs <- list(data.frame = identity, tibble = tibble::as_tibble,
               data.table = data.table::as.data.table)
refs <- c("..1", "..2", "..01", "..001", "..02", "..+1", ".. 1", "..\t1")
run <- function(verb, source, ref, unpack) {
  calls <- 0L
  factory <- function() {
    calls <<- calls + 1L
    offset <- calls
    function(x) data.frame(value = sum(x) + offset)
  }
  wrapper <- function(...) {
    expr <- rlang::expr(dplyr::across(x, !!as.name(ref), .unpack = unpack))
    verb(source, !!expr, .by = g)
  }
  result <- wrapper(factory(), factory())
  list(result = as.data.frame(result), calls = calls)
}
results <- list()
for (input in names(inputs)) {
  source <- inputs[[input]](data)
  for (ref in refs) {
    for (unpack in list(TRUE, "{inner}", FALSE)) {
      ordinary <- run(dplyr::summarise, source, ref, unpack)
      margin <- run(marginplyr::summarize_with_margins, source, ref, unpack)
      results[[length(results) + 1L]] <- data.frame(
        input, ref, unpack = as.character(unpack),
        ordinary_calls = ordinary$calls, margin_calls = margin$calls,
        ordinary_values = paste(unlist(ordinary$result[-1L]), collapse = ","),
        margin_values = paste(unlist(margin$result[-1L]), collapse = ","),
        outcome = if (identical(ordinary, margin)) "agree" else "value-difference")
    }
  }
}
result <- dplyr::bind_rows(results)
write.csv(result, file.path(out_dir, "forwarded-factory-results.csv"), row.names = FALSE)
print(with(result, table(unpack, outcome)))
stopifnot(nrow(result) == 72L,
          sum(result$outcome == "value-difference") == 36L,
          sum(result$outcome == "agree") == 36L)
