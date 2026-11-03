options(lifecycle_verbosity = "quiet", width = 120)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
artifact <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/summary"
forms <- c(
  first = '(function(...) sum(pick(..1)))(x)',
  second = '(function(...) sum(pick(..2)))(y, x)',
  two = '(function(...) sum(pick(..1, ..2)))(x, y)',
  named = '(function(...) sum(pick(..1)))(cols = x)',
  namespace = '(function(...) sum(dplyr::pick(..1)))(x)',
  head_paren = '(function(...) sum((pick)(..1)))(x)',
  whole_paren = '(function(...) sum((pick(..1))))(x)',
  qualified_paren = '(function(...) sum((dplyr::pick)(..1)))(x)',
  all_dots = '(function(...) sum(pick(...)))(x)',
  literal = '(function(...) sum(pick(x)))(x)',
  wrapped = '(function(...) sum(pick(c(...))))(x)',
  selection_wrapper = '(function(...) sum(pick(all_of(...))))("x")',
  across_first = '(function(...) sum(across(..1, sum)))(x)',
  across_dots = '(function(...) sum(across(..., sum)))(x)'
)
cap <- function(f) tryCatch(list(ok = TRUE, value = f()), error = function(cnd) list(ok = FALSE, class = class(cnd), text = conditionMessage(cnd)))
rows <- list()
for (kind in c("frame", "tibble", "table")) {
  for (partition in c("none", "fixed", "rollup")) {
    for (preceded in c(FALSE, TRUE)) for (named in c(FALSE, TRUE)) for (form in names(forms)) {
      data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
      data <- switch(kind, frame = data, tibble = as_tibble(data), table = data.table::as.data.table(data))
      e <- str2lang(forms[[form]])
      dots <- list(rlang::new_quosure(e, environment()))
      if (named) names(dots) <- "value"
      if (preceded) dots <- c(list(prior = rlang::quo(1L)), dots)
      keys <- if (partition == "fixed") "g" else character()
      ordinary <- cap(function() summarise(data, !!!dots, .by = all_of(keys)))
      margin <- cap(function() {
        out <- if (partition == "rollup") {
          summarize_with_margins(data, !!!dots, .grouping = rollup(g), .id = "set")
        } else summarize_with_margins(data, !!!dots, .by = all_of(keys))
        out
      })
      if (partition == "rollup" && ordinary$ok && margin$ok) {
        plain <- cap(function() summarise(data, !!!dots, .by = g))
        total <- ordinary$value
        total$g <- "Total"
        total$set <- 2L
        plain$value$set <- 1L
        ordinary$value <- bind_rows(plain$value, total)
        ordinary$value <- select(ordinary$value, all_of(names(margin$value)))
      }
      status <- if (ordinary$ok && margin$ok) {
        if (isTRUE(all.equal(as.data.frame(ordinary$value), as.data.frame(margin$value), check.attributes = FALSE))) "match" else "different"
      } else if (ordinary$ok) "margin_error" else if (margin$ok) "ordinary_error" else "both_error"
      rows[[length(rows) + 1L]] <- data.frame(kind, partition, preceded, named, form, status,
        message = if (!margin$ok) margin$text else "", stringsAsFactors = FALSE)
    }
  }
}
result <- bind_rows(rows)
write.csv(result, file.path(artifact, "pick-forward-results.csv"), row.names = FALSE)
print(with(result, table(form, status)))
cat("cases:", nrow(result), "\n")
stopifnot(all(result$status[result$form %in% names(forms)[1:8]] == "margin_error"))
stopifnot(all(result$status[!result$form %in% names(forms)[1:8]] == "match"))
