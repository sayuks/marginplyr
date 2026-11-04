pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out_dir <- "investigation/2026-11-04-summary-grouping-share-bug-hunt/summary"
data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 4:6)
inputs <- list(data.frame = identity, tibble = tibble::as_tibble,
               data.table = data.table::as.data.table)
refs <- c("..1", "..2", "..01", "..001", "..02", "..+1", ".. 1", "..\t1")
helpers <- c("pick", "across", "if_any", "if_all")
results <- list()
for (input in names(inputs)) {
  source <- inputs[[input]](data)
  for (grain in c("none", "fixed", "rollup")) {
    for (helper in helpers) {
      for (ref in refs) {
        for (qualified in c(FALSE, TRUE)) {
          call <- rlang::call2(helper, as.name(ref),
            .ns = if (qualified) "dplyr" else NULL)
          if (helper == "across") call <- rlang::call_modify(call, .fns = quote(sum))
          if (helper %in% c("if_any", "if_all")) {
            call <- rlang::call_modify(call, .fns = quote(~ .x > 1))
          }
          expr <- rlang::expr((function(...) sum(!!call))(x, y))
          ordinary <- if (grain == "none") {
            rlang::inject(dplyr::summarise(source, value = !!expr))
          } else {
            detail <- rlang::inject(dplyr::summarise(source, value = !!expr, .by = g))
            if (grain == "fixed") detail else {
              total <- rlang::inject(dplyr::summarise(source, value = !!expr))
              total$g <- NA_character_
              dplyr::bind_rows(detail, total)
            }
          }
          margin <- tryCatch({
            if (grain == "none") {
              rlang::inject(marginplyr::summarize_with_margins(source, value = !!expr))
            } else if (grain == "fixed") {
              rlang::inject(marginplyr::summarize_with_margins(source, value = !!expr, .by = g))
            } else {
              rlang::inject(marginplyr::summarize_with_margins(
                source, value = !!expr, .grouping = rollup(g), .margin_label = NULL))
            }
          }, error = identity)
          error <- inherits(margin, "error")
          agrees <- !error && isTRUE(all.equal(
            as.data.frame(ordinary), as.data.frame(margin)[names(ordinary)],
            check.attributes = FALSE))
          results[[length(results) + 1L]] <- data.frame(
            input, grain, helper, ref, qualified,
            expected = paste(ordinary$value, collapse = ","),
            observed = if (error) paste(tail(strsplit(conditionMessage(margin), "\n")[[1L]], 2L), collapse = " ") else paste(margin$value, collapse = ","),
            outcome = if (error) "margin-only-error" else if (agrees) "agree" else "value-difference")
        }
      }
    }
  }
}
result <- dplyr::bind_rows(results)
write.csv(result, file.path(out_dir, "noncanonical-dots-results.csv"), row.names = FALSE)
print(with(result, table(helper, outcome)))
stopifnot(nrow(result) == 576L,
          sum(result$outcome == "margin-only-error") == 432L,
          sum(result$outcome == "agree") == 144L,
          !any(result$outcome == "value-difference"))
