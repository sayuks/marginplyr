pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
strict <- "--assert" %in% commandArgs(trailingOnly = TRUE)
out <- "investigation/2026-11-02-summary-grouping-share-bug-hunt/summary"
fixture <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
classes <- list(frame = identity, tibble = tibble::as_tibble,
                data_table = data.table::as.data.table)
make_expression <- function(helper, spelling, control) {
  forwarded <- if (spelling == "dot1") "..1" else "..."
  selector <- if (control == "none") forwarded else paste0(control, "(", forwarded, ")")
  fn <- if (helper == "across") "sum" else "~ .x > 2"
  call <- switch(spelling,
    positional = paste0("dplyr::", helper, "(", selector, ", ", fn, ")"),
    dot1 = paste0("dplyr::", helper, "(", selector, ", ", fn, ")"),
    named = paste0("dplyr::", helper, "(.cols = ", selector, ", .fns = ", fn, ")"),
    forwarded = paste0("dplyr::", helper, "(...)")
  )
  if (helper != "across") call <- paste0("sum(", call, ")")
  args <- if (spelling == "forwarded") paste0(".cols = 'x', .fns = ", fn) else "'x'"
  str2lang(paste0("(function(...) ", call, ")(", args, ")"))
}
capture <- function(expr) tryCatch(eval(expr), error = identity)
rows <- list()
k <- 0L
for (class in names(classes)) for (helper in c("across", "if_any", "if_all")) {
  for (spelling in c("positional", "named", "forwarded", "dot1")) {
    for (plan in c("none", "fixed", "rollup")) for (named in c(FALSE, TRUE)) {
      for (preceded in c(FALSE, TRUE)) {
        data <- classes[[class]](fixture)
        expr <- make_expression(helper, spelling, "none")
        summaries <- if (preceded) list(prior = quote(sum(y))) else list()
        summaries <- c(summaries, stats::setNames(list(expr), if (named) "answer" else ""))
        ordinary <- if (plan == "none") {
          dplyr::summarise(data, !!!summaries)
        } else {
          detail <- dplyr::summarise(data, !!!summaries, .by = g)
          if (plan == "fixed") detail else dplyr::bind_rows(
            detail, dplyr::mutate(dplyr::summarise(data, !!!summaries), g = "Total")
          )
        }
        actual <- capture(if (plan == "none") {
          quote(summarize_with_margins(data, !!!summaries))
        } else if (plan == "fixed") {
          quote(summarize_with_margins(data, !!!summaries, .by = g))
        } else {
          quote(summarize_with_margins(data, !!!summaries, .grouping = rollup(g)))
        })
        matches <- !inherits(actual, "error") && isTRUE(all.equal(
          as.data.frame(actual)[names(ordinary)], as.data.frame(ordinary)
        ))
        k <- k + 1L
        rows[[k]] <- data.frame(class, helper, spelling, plan, named, preceded,
                               matches, error = if (inherits(actual, "error")) conditionMessage(actual) else "")
      }
    }
  }
}
result <- do.call(rbind, rows)
write.csv(result, file.path(out, "ellipsis-results.csv"), row.names = FALSE)
cat("Forwarding comparisons:", nrow(result), "\n")
print(table(result$matches))
failed <- result[!result$matches, ]
stopifnot(all(ifelse(
  failed$spelling == "dot1",
  grepl("Column `..1` doesn't exist", failed$error, fixed = TRUE),
  grepl("'...' used in an incorrect context", failed$error, fixed = TRUE)
)))

# Wrapping a selection in c/all_of uses ordinary value evaluation and succeeds.
controls <- 0L
for (class in names(classes)) for (helper in c("across", "if_any", "if_all")) {
  for (spelling in c("positional", "named", "dot1")) for (control in c("c", "dplyr::all_of")) {
    if (spelling == "dot1" && control == "c") next
    data <- classes[[class]](fixture)
    expr <- make_expression(helper, spelling, control)
    expected <- dplyr::summarise(data, !!expr, .by = g)
    actual <- summarize_with_margins(data, !!expr, .by = g)
    stopifnot(isTRUE(all.equal(as.data.frame(actual), as.data.frame(expected))))
    controls <- controls + 1L
  }
}
cat("Successful c/all_of controls:", controls, "\n")

data <- data.frame(x = 1:3)
cat("Minimized ordinary result:\n")
print(dplyr::summarise(data, (function(...) dplyr::across(..., sum))("x")))
cat("Minimized Margin result:\n")
print(tryCatch(summarize_with_margins(
  data, (function(...) dplyr::across(..., sum))("x")
), error = conditionMessage))
if (strict) stopifnot(all(result$matches))
