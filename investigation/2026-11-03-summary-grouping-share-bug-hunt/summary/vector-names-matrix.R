options(lifecycle_verbosity = "quiet", width = 120)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
artifact <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/summary"
input <- data.frame(h = c("f", "f", "f"), g = c("a", "a", "b"), x = 1:3, y = 5:7)
templates <- c(
  index = "{.col}_{.fn}_{seq_along(.col)}",
  length = "{.col}_{.fn}_{length(.col)}",
  reverse = "{.col}_{.fn}_{rev(.col)}",
  match = "{.col}_{.fn}_{match(.col, .col)}",
  standard = "{.col}_{.fn}",
  elementwise = "{toupper(.col)}_{.fn}"
)
fns <- list(single = rlang::expr(sum), list = rlang::expr(list(total = sum, avg = mean)))
cap <- function(f) tryCatch(list(ok = TRUE, value = f()), error = function(cnd) list(ok = FALSE, text = conditionMessage(cnd)))
rows <- list()
add <- function(lane, backend, template, functions, keys, helper, status, detail = "") {
  rows[[length(rows) + 1L]] <<- data.frame(lane, backend, template, functions, keys, helper, status, detail)
}
for (backend in c("local", "dtplyr", "arrow")) for (t in names(templates)) for (fn in names(fns)) for (keys in c(FALSE, TRUE)) {
  data <- switch(backend, local = input, dtplyr = dtplyr::lazy_dt(input), arrow = arrow::arrow_table(input))
  by <- if (keys) "g" else character()
  q <- rlang::new_quosure(rlang::expr(across(c(x, y), !!fns[[fn]], .names = !!templates[[t]])), environment())
  ordinary <- cap(function() { out <- summarise(data, !!q, .by = all_of(by)); if (backend == "local") out else collect(out) })
  margin <- cap(function() { out <- summarize_with_margins(data, !!q, .by = all_of(by)); if (backend == "local") out else collect(out) })
  status <- if (ordinary$ok && margin$ok) {
    if (identical(names(ordinary$value), names(margin$value))) "match" else "names_differ"
  } else if (ordinary$ok) "margin_error" else if (margin$ok) "ordinary_error" else "both_error"
  add("ordinary", backend, t, fn, keys, "none", status,
      if (ordinary$ok && margin$ok) paste(paste(names(ordinary$value), collapse = ","), paste(names(margin$value), collapse = ","), sep = " vs ") else if (!margin$ok) margin$text else ordinary$text)
}
for (kind in c("frame", "tibble", "table")) for (t in names(templates)) for (fn in names(fns)) for (keys in c(FALSE, TRUE)) for (helper in c("share_of_total", "share_of_parent")) {
  data <- switch(kind, frame = input, tibble = as_tibble(input), table = data.table::as.data.table(input))
  q <- rlang::new_quosure(rlang::expr(across(c(x, y), !!fns[[fn]], .names = !!templates[[t]])), environment())
  ordinary <- summarise(data, !!q)
  source <- tail(names(ordinary), 1L)
  share <- rlang::new_quosure(rlang::call2(helper, rlang::sym(source)), environment())
  by <- if (keys) "h" else character()
  margin <- cap(function() summarize_with_margins(data, !!q, fraction = !!share, .by = all_of(by), .grouping = rollup(g)))
  status <- if (margin$ok) "success" else "margin_error"
  add("source", kind, t, fn, keys, helper, status, if (margin$ok) paste(names(margin$value), collapse = ",") else margin$text)
}
# Share-output naming uses the same scalar-pair helper, with .fn fixed to 1.
for (t in names(templates)) for (helper in c("share_of_total", "share_of_parent")) {
  baseline <- summarise(input, across(c(x, y), sum, .names = templates[[t]]))
  q <- rlang::new_quosure(rlang::expr(across(c(x, y), !!rlang::sym(helper), .names = !!templates[[t]])), environment())
  actual <- cap(function() summarize_with_margins(input, x = sum(x), y = sum(y), !!q, .grouping = rollup(g)))
  status <- if (actual$ok && identical(tail(names(actual$value), 2L), names(baseline))) "match" else if (actual$ok) "names_differ" else "margin_error"
  add("share_output", "local", t, "single", FALSE, helper, status,
      if (actual$ok) paste(names(baseline), tail(names(actual$value), 2L), collapse = "; ") else actual$text)
}
result <- bind_rows(rows)
write.csv(result, file.path(artifact, "vector-names-results.csv"), row.names = FALSE)
print(with(result, table(lane, status)))
print(with(result[result$lane == "ordinary", ], table(backend, functions, status)))
cat("cases:", nrow(result), "\n")
ordinary <- result[result$lane == "ordinary", ]
stopifnot(all(ordinary$status[ordinary$backend != "dtplyr" | ordinary$functions != "single" | ordinary$template %in% c("standard", "elementwise")] == "match"))
stopifnot(all(ordinary$status[ordinary$backend == "dtplyr" & ordinary$functions == "single" & !ordinary$template %in% c("standard", "elementwise")] == "names_differ"))
source <- result[result$lane == "source", ]
stopifnot(all(source$status[source$template %in% c("standard", "elementwise")] == "success"))
stopifnot(all(source$status[!source$template %in% c("standard", "elementwise")] == "margin_error"))
output <- result[result$lane == "share_output", ]
stopifnot(all(output$status[output$template %in% c("standard", "elementwise")] == "match"))
stopifnot(all(output$status[!output$template %in% c("standard", "elementwise")] == "names_differ"))
